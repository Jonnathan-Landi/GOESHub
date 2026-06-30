.normalize_goes_geos_extent <- function(r) {
  projection <- terra::crs(r, proj = TRUE)
  raster_extent <- as.vector(terra::ext(r))

  is_geostationary <- grepl("(?:^| )\\+proj=geos(?: |$)", projection)
  has_scan_angles <- max(abs(raster_extent), na.rm = TRUE) < 1
  height_match <- regexec("(?:^| )\\+h=([0-9.]+)(?: |$)", projection)
  height_parts <- regmatches(projection, height_match)[[1]]

  if (is_geostationary && has_scan_angles) {
    if (length(height_parts) < 2L) {
      stop("El raster GOES usa angulos de barrido pero su CRS no declara +h")
    }

    perspective_height <- as.numeric(height_parts[[2]])
    if (!is.finite(perspective_height) || perspective_height <= 0) {
      stop("Altura de perspectiva invalida en el CRS GOES: ", height_parts[[2]])
    }

    terra::ext(r) <- terra::ext(r) * perspective_height
  }

  r
}

.build_buffer_zone <- function(province_path, buffer_path, work_epsg, radius_km) {
  radius_m <- suppressWarnings(as.numeric(radius_km)) * 1000
  if (!is.finite(radius_m) || radius_m <= 0) {
    stop("roi$radius_km debe ser un numero positivo.")
  }

  province <- terra::project(terra::vect(province_path), work_epsg)
  province_extent <- terra::ext(province)
  center_x <- (terra::xmin(province_extent) + terra::xmax(province_extent)) / 2
  center_y <- (terra::ymin(province_extent) + terra::ymax(province_extent)) / 2
  half_side <- max(
    terra::xmax(province_extent) - terra::xmin(province_extent),
    terra::ymax(province_extent) - terra::ymin(province_extent)
  ) / 2 + radius_m

  roi <- terra::as.polygons(terra::ext(
    center_x - half_side,
    center_x + half_side,
    center_y - half_side,
    center_y + half_side
  ))
  terra::crs(roi) <- work_epsg
  fs::dir_create(fs::path_dir(buffer_path), recurse = TRUE)
  terra::writeVector(roi, buffer_path, layer = "buffer_zone", overwrite = TRUE)
  roi
}

.process_goes_item <- function(
  item,
  buffer_obj,
  work_epsg,
  output_res_m,
  progress = NULL
) {
  if (is.function(progress)) {
    on.exit(progress(message = item$id), add = TRUE)
  }
  r <- .normalize_goes_geos_extent(terra::rast(item$nc_path))
  buffer_local <- terra::project(buffer_obj, terra::crs(r))
  if (!terra::relate(terra::ext(r), terra::ext(buffer_local), "intersects")) {
    stop(
      "El ROI no intersecta el raster GOES despues de igualar sus CRS. ",
      "Raster: ", paste(as.vector(terra::ext(r)), collapse = ", "),
      "; ROI: ", paste(as.vector(terra::ext(buffer_local)), collapse = ", ")
    )
  }

  # Recortar primero evita enmascarar los ~29 millones de pixeles del disco.
  cropped <- terra::crop(r, buffer_local, snap = "out")
  cmi <- cropped[["CMI"]]
  dqf <- cropped[["DQF"]]
  cmi <- terra::mask(cmi, dqf == 0, maskvalue = FALSE) - 273.15
  cmi_crop <- terra::mask(cmi, buffer_local)
  output <- terra::project(
    cmi_crop,
    work_epsg,
    method = "bilinear",
    res = output_res_m
  )

  fs::dir_create(fs::path_dir(item$tif_path), recurse = TRUE)
  terra::writeRaster(output, as.character(item$tif_path), overwrite = TRUE)

  if (!fs::file_exists(item$tif_path)) {
    stop("No se genero TIF: ", item$tif_path)
  }

  list(
    id = item$id,
    status = "ok",
    nc_path = item$nc_path,
    tif_path = item$tif_path,
    nc_mtime = item$nc_mtime,
    tif_size = as.numeric(fs::file_info(item$tif_path)$size),
    error = NULL
  )
}

run_process_pipeline <- function(cfg) {
  started_at <- Sys.time()
  root_dir <- here::here()
  source(file.path(root_dir, "scripts", "core", "goes_frame_naming.R"))
  source(file.path(root_dir, "scripts", "core", "cache_state.R"))

  log_file <- build_log_file(cfg, stage = "process")
  append_stage_log(log_file, stage = "process", message = "Inicio process pipeline")

  t_utc <- lubridate::now(tzone = cfg$time$tz_global)
  y <- format(t_utc, "%Y")
  m <- format(t_utc, "%m")
  d <- format(t_utc, "%d")
  channel <- sprintf("%02d", as.integer(cfg$goes$channel))

  raw_dir <- fs::path(
    cfg$paths$raw_dir,
    cfg$goes$satellite,
    cfg$goes$product,
    channel,
    y,
    m,
    d
  )

  nc_files <- list.files(raw_dir, pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
  if (length(nc_files) == 0) {
    append_stage_log(log_file, stage = "process", level = "WARN", message = "Sin NetCDF para procesar", fields = list(raw_dir = raw_dir))
    return(invisible(list(log_file = log_file, status = "no_data")))
  }

  terra_threads <- cfg$performance$terra_threads
  if (is.null(terra_threads) || identical(terra_threads, "auto")) {
    terra_threads <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
  } else {
    terra_threads <- suppressWarnings(as.integer(terra_threads))
    if (!is.finite(terra_threads) || is.na(terra_threads) || terra_threads < 1L) {
      terra_threads <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
    }
  }
  terra::terraOptions(threads = terra_threads)

  processed_dir <- fs::path(
    cfg$paths$processed_dir,
    cfg$goes$satellite,
    cfg$goes$product,
    channel,
    y,
    m,
    d
  )
  fs::dir_create(processed_dir, recurse = TRUE)

  state_path <- cache_stage_path(cfg, "process")
  state <- cache_load_day(state_path, t_utc)

  work_epsg <- cfg$roi$work_epsg
  buffer_path <- cfg$roi$buffer_path

  if (!fs::file_exists(buffer_path)) {
    if (!fs::file_exists(cfg$roi$province_path)) {
      stop("No existe la provincia configurada: ", cfg$roi$province_path)
    }
    roi_metric <- .build_buffer_zone(
      cfg$roi$province_path,
      buffer_path,
      work_epsg,
      cfg$roi$radius_km
    )
  } else {
    roi_metric <- terra::project(terra::vect(buffer_path), work_epsg)
  }

  source_raster <- .normalize_goes_geos_extent(terra::rast(nc_files[[1]]))
  buffer <- terra::project(roi_metric, terra::crs(source_raster))

  output_res_m <- suppressWarnings(as.numeric(cfg$processing$res_m))
  if (!is.finite(output_res_m) || output_res_m <= 0) {
    stop("processing$res_m debe ser una resolucion positiva en metros")
  }

  process_workers <- cfg$performance$process_workers
  max_auto_workers <- suppressWarnings(as.integer(cfg$performance$max_auto_workers))
  if (!is.finite(max_auto_workers) || is.na(max_auto_workers) || max_auto_workers < 1L) {
    max_auto_workers <- 4L
  }

  worker_blas_threads <- suppressWarnings(as.integer(cfg$performance$worker_blas_threads))
  if (!is.finite(worker_blas_threads) || is.na(worker_blas_threads) || worker_blas_threads < 1L) {
    worker_blas_threads <- 1L
  }

  if (is.null(process_workers) || identical(process_workers, "auto")) {
    process_workers <- min(max_auto_workers, max(1L, parallel::detectCores(logical = TRUE) - 1L))
  } else {
    process_workers <- suppressWarnings(as.integer(process_workers))
    if (!is.finite(process_workers) || is.na(process_workers) || process_workers < 1L) {
      process_workers <- 1L
    }
  }

  buffer_path <- as.character(buffer_path)
  process_cache_version <- 5L

  roi_extent <- as.vector(terra::ext(roi_metric))
  append_stage_log(
    log_file,
    stage = "process",
    message = "ROI preparado para recorte",
    fields = list(
      roi_path = buffer_path,
      roi_crs = terra::crs(roi_metric, proj = TRUE),
      roi_extent = roi_extent
    )
  )

  ok_count <- 0L
  fail_count <- 0L

  pending <- list()

  for (f in nc_files) {
    id <- goes_extract_stamp(f)
    if (is.na(id)) {
      next
    }

    tif_file <- fs::path(processed_dir, paste0(id, ".tif"))
    nc_info <- fs::file_info(f)
    nc_mtime <- format(nc_info$modification_time, "%Y-%m-%dT%H:%M:%SZ")
    item <- state$items[[id]]

    needs_processing <- FALSE
    if (is.null(item)) {
      needs_processing <- TRUE
    } else if (!identical(item$status, "ok")) {
      needs_processing <- TRUE
    } else if (!fs::file_exists(tif_file)) {
      needs_processing <- TRUE
    } else if (!identical(item$nc_mtime_utc, nc_mtime)) {
      needs_processing <- TRUE
    } else if (!identical(item$processor_version, process_cache_version)) {
      needs_processing <- TRUE
    }

    if (!needs_processing) {
      next
    }

    pending[[length(pending) + 1L]] <- list(
      id = id,
      nc_path = f,
      tif_path = as.character(tif_file),
      nc_mtime = nc_mtime
    )
  }

  results <- list()
  if (length(pending) > 0L) {
    progress <- progressr::progressor(steps = length(pending))
    workers <- min(process_workers, length(pending))

    if (workers <= 1L) {
      for (item in pending) {
        results[[length(results) + 1L]] <- .process_goes_item(
          item, buffer, work_epsg, output_res_m, progress
        )
      }
    } else {
      parallel_ok <- TRUE
      chunk_results <- tryCatch(
        {
          idx_chunks <- split(seq_along(pending), cut(seq_along(pending), breaks = workers, labels = FALSE))
          pending_chunks <- lapply(idx_chunks, function(ix) pending[ix])

          old_plan <- future::plan()
          on.exit(future::plan(old_plan), add = TRUE)
          future::plan(future::multisession, workers = workers)

          future.apply::future_lapply(
            pending_chunks,
            function(chunk) {
              Sys.setenv(
                OPENBLAS_NUM_THREADS = as.character(worker_blas_threads),
                OMP_NUM_THREADS = as.character(worker_blas_threads),
                MKL_NUM_THREADS = as.character(worker_blas_threads)
              )
              terra::terraOptions(threads = 1)

              buffer_local <- terra::vect(buffer_path)
              lapply(
                chunk,
                .process_goes_item,
                buffer_obj = buffer_local,
                work_epsg = work_epsg,
                output_res_m = output_res_m,
                progress = progress
              )
            },
            future.packages = c("fs", "terra"),
            future.seed = FALSE
          )
        },
        error = function(e) {
          parallel_ok <<- FALSE
          append_stage_log(
            log_file,
            stage = "process",
            level = "WARN",
            message = "Fallo procesamiento paralelo; se usa modo serial",
            fields = list(error = conditionMessage(e), requested_workers = workers)
          )
          NULL
        }
      )

      if (isTRUE(parallel_ok) && !is.null(chunk_results)) {
        results <- unlist(chunk_results, recursive = FALSE, use.names = FALSE)
      } else {
        for (item in pending) {
          results[[length(results) + 1L]] <- .process_goes_item(
            item, buffer, work_epsg, output_res_m, progress
          )
        }
      }
    }
  }

  for (res in results) {
    if (identical(res$status, "ok") && fs::file_exists(res$tif_path)) {
      ok_count <- ok_count + 1L
      state$items[[res$id]] <- list(
        status = "ok",
        nc_path = res$nc_path,
        tif_path = res$tif_path,
        processed_at_utc = format(lubridate::now("UTC"), "%Y-%m-%dT%H:%M:%SZ"),
        processor_version = process_cache_version,
        nc_mtime_utc = res$nc_mtime,
        tif_size = res$tif_size
      )
    } else {
      fail_count <- fail_count + 1L
      state$items[[res$id]] <- list(
        status = "failed",
        nc_path = res$nc_path,
        tif_path = res$tif_path,
        processed_at_utc = format(lubridate::now("UTC"), "%Y-%m-%dT%H:%M:%SZ"),
        error = res$error
      )
    }
  }

  cache_save_day(state, state_path)

  append_stage_log(
    log_file,
    stage = "process",
    message = "Fin process pipeline",
    fields = list(
      processed_ok = ok_count,
      processed_failed = fail_count,
      output_dir = processed_dir,
      terra_threads = terra_threads,
      process_workers = process_workers,
      duration_sec = round(as.numeric(difftime(Sys.time(), started_at, units = "secs")), 2)
    )
  )

  invisible(list(log_file = log_file, status = "ok", processed_ok = ok_count, processed_failed = fail_count))
}
