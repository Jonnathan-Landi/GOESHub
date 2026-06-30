.render_goes_item <- function(
  item,
  cfg,
  ecu,
  cant,
  static_assets,
  cols,
  brks,
  progress = NULL
) {
  if (is.function(progress)) {
    on.exit(progress(message = item$id), add = TRUE)
  }
  tryCatch(
    {
      layer <- terra::rast(item$tif)
      terra::time(layer) <- item$t_utc
      layer_range <- as.numeric(
        terra::global(layer, c("min", "max"), na.rm = TRUE)[1, ]
      )

      generate_frames(
        layer = layer,
        cfg = cfg,
        e = terra::ext(layer),
        ecu = ecu,
        cant = cant,
        out_file = item$png,
        static_assets = static_assets,
        cols = cols,
        brks = brks
      )

      if (!fs::file_exists(item$png)) {
        stop("No se genero PNG")
      }

      list(
        id = item$id,
        status = "ok",
        tif = item$tif,
        png = item$png,
        tif_mtime = item$tif_mtime,
        tif_size = item$tif_size,
        tif_min = layer_range[[1]],
        tif_max = layer_range[[2]],
        png_size = as.numeric(fs::file_info(item$png)$size),
        error = NULL
      )
    },
    error = function(e) {
      list(
        id = item$id,
        status = "failed",
        tif = item$tif,
        png = item$png,
        tif_mtime = item$tif_mtime,
        tif_size = item$tif_size,
        png_size = NA_real_,
        error = conditionMessage(e)
      )
    }
  )
}

run_render_pipeline <- function(cfg) {
  started_at <- Sys.time()
  root_dir <- here::here()
  source(file.path(root_dir, "scripts", "core", "cache_state.R"))
  source(file.path(root_dir, "scripts", "core", "state_frames.R"))
  source(file.path(root_dir, "scripts", "core", "goes_frame_naming.R"))
  source(file.path(root_dir, "scripts", "render", "render_frames_png.R"))
  source(file.path(root_dir, "scripts", "render", "render_palette_c13.R"))

  log_file <- build_log_file(cfg, stage = "render")
  append_stage_log(log_file, stage = "render", message = "Inicio render pipeline")

  t_utc <- lubridate::now(tzone = cfg$time$tz_global)
  channel <- sprintf("%02d", as.integer(cfg$goes$channel))
  processed_dir <- fs::path(
    cfg$paths$processed_dir,
    cfg$goes$satellite,
    cfg$goes$product,
    channel,
    format(t_utc, "%Y"),
    format(t_utc, "%m"),
    format(t_utc, "%d")
  )

  tif_files <- list.files(
    processed_dir,
    pattern = "\\.tif$",
    full.names = TRUE,
    recursive = TRUE
  )
  if (length(tif_files) == 0L) {
    append_stage_log(
      log_file,
      stage = "render",
      level = "WARN",
      message = "Sin rasters procesados para render",
      fields = list(processed_dir = processed_dir)
    )
    return(invisible(list(log_file = log_file, status = "no_data")))
  }

  state_path <- state_frames_path(cfg, t_utc)
  state <- state_load(state_path, t_utc)
  render_cache_version <- 4L

  pending <- lapply(tif_files, function(f) {
    id <- tools::file_path_sans_ext(basename(f))
    if (!grepl("^s\\d{13}$", id)) {
      return(NULL)
    }

    png_file <- png_path_from_time(cfg, goes_stamp_to_time(id))
    info <- fs::file_info(f)
    tif_mtime <- format(info$modification_time, "%Y-%m-%dT%H:%M:%SZ")
    cached <- state$items[[id]]
    current <- !is.null(cached) &&
      identical(cached$status, "ok") &&
      fs::file_exists(png_file) &&
      fs::file_info(png_file)$size >= 150 * 1024 &&
      identical(cached$tif_mtime_utc, tif_mtime) &&
      identical(cached$renderer_version, render_cache_version)

    if (current) {
      return(NULL)
    }

    list(
      id = id,
      tif = f,
      png = png_file,
      t_utc = goes_stamp_to_time(id),
      tif_mtime = tif_mtime,
      tif_size = as.numeric(info$size)
    )
  })
  pending <- Filter(Negate(is.null), pending)

  if (length(pending) == 0L) {
    append_stage_log(log_file, stage = "render", message = "Sin pendientes de render")
    return(invisible(list(log_file = log_file, status = "up_to_date")))
  }

  max_workers <- suppressWarnings(as.integer(cfg$performance$max_auto_workers))
  if (!is.finite(max_workers) || max_workers < 1L) max_workers <- 4L
  workers <- cfg$performance$render_workers
  if (is.null(workers) || identical(workers, "auto")) {
    workers <- min(max_workers, max(1L, parallel::detectCores() - 1L))
  }
  workers <- min(max(1L, as.integer(workers)), length(pending))

  load_assets <- function() {
    ecu <- terra::vect(as.character(cfg$roi$country_path))
    cant <- terra::vect(as.character(cfg$roi$cantons_path))
    list(ecu = ecu, cant = cant, static = prepare_render_static_assets(cfg, cant))
  }
  palette <- c13_palette()
  progress <- progressr::progressor(steps = length(pending))

  if (workers == 1L) {
    assets <- load_assets()
    results <- lapply(
      pending,
      .render_goes_item,
      cfg = cfg,
      ecu = assets$ecu,
      cant = assets$cant,
      static_assets = assets$static,
      cols = palette$cols,
      brks = palette$brks,
      progress = progress
    )
  } else {
    chunks <- split(pending, rep_len(seq_len(workers), length(pending)))
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = workers)
    chunk_results <- future.apply::future_lapply(
      chunks,
      function(chunk) {
        terra::terraOptions(threads = 1)
        source(file.path(root_dir, "scripts", "render", "render_frames_png.R"))
        assets <- load_assets()
        lapply(
          chunk,
          .render_goes_item,
          cfg = cfg,
          ecu = assets$ecu,
          cant = assets$cant,
          static_assets = assets$static,
          cols = palette$cols,
          brks = palette$brks,
          progress = progress
        )
      },
      future.packages = c("fs", "png", "terra"),
      future.seed = FALSE
    )
    results <- unlist(chunk_results, recursive = FALSE, use.names = FALSE)
  }

  ok_count <- 0L
  fail_count <- 0L
  for (res in results) {
    now <- format(lubridate::now("UTC"), "%Y-%m-%dT%H:%M:%SZ")
    if (identical(res$status, "ok")) {
      ok_count <- ok_count + 1L
      state$items[[res$id]] <- list(
        status = "ok",
        tif_path = res$tif,
        png_path = res$png,
        processed_at_utc = now,
        renderer_version = render_cache_version,
        tif_mtime_utc = res$tif_mtime,
        tif_size = res$tif_size,
        tif_min = res$tif_min,
        tif_max = res$tif_max,
        png_size = res$png_size
      )
    } else {
      fail_count <- fail_count + 1L
      state$items[[res$id]] <- list(
        status = "failed",
        tif_path = res$tif,
        png_path = res$png,
        processed_at_utc = now,
        error = res$error
      )
      append_stage_log(
        log_file,
        stage = "render",
        level = "ERROR",
        message = "Fallo render de frame",
        fields = list(id = res$id, error = res$error)
      )
    }
  }

  state_save(state, state_path)
  append_stage_log(
    log_file,
    stage = "render",
    message = "Fin render pipeline",
    fields = list(
      rendered_ok = ok_count,
      rendered_failed = fail_count,
      render_workers = workers,
      duration_sec = round(as.numeric(difftime(Sys.time(), started_at, units = "secs")), 2)
    )
  )

  invisible(list(
    log_file = log_file,
    status = "ok",
    rendered_ok = ok_count,
    rendered_failed = fail_count
  ))
}
