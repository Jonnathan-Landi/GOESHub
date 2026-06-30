.parse_lookback <- function(value) {
  value <- trimws(tolower(value))
  match <- regexec("^([0-9]+)\\s*(hour|hours|day|days)$", value)
  parts <- regmatches(value, match)[[1]]
  if (length(parts) == 0L) {
    stop("lookback debe usar el formato 'N hours' o 'N days'.")
  }
  amount <- as.integer(parts[[2]])
  if (parts[[3]] %in% c("day", "days")) lubridate::days(amount) else lubridate::hours(amount)
}

.last_local_goes_time <- function(path) {
  files <- list.files(path, pattern = "\\.nc$", recursive = TRUE)
  stamps <- stringr::str_match(files, "s(\\d{13})")[, 2]
  stamps <- stamps[!is.na(stamps)]
  if (length(stamps) == 0L) return(as.POSIXct(NA))
  stamp <- max(stamps)
  as.POSIXct(
    sprintf(
      "%s-01-01 %s:%s:%s",
      substr(stamp, 1, 4),
      substr(stamp, 8, 9),
      substr(stamp, 10, 11),
      substr(stamp, 12, 13)
    ),
    tz = "UTC"
  ) + lubridate::days(as.integer(substr(stamp, 5, 7)) - 1L)
}

download_recent <- function(log_file, cfg, lookback) {
  source(here::here("scripts", "core", "ops_utils.R"))
  source(here::here("scripts", "ingest", "s3_list_keys.R"))
  source(here::here("scripts", "ingest", "download.R"))

  lookback <- .parse_lookback(lookback)
  if (lookback > lubridate::days(7)) {
    stop("La ventana maxima de ingesta es 7 dias.")
  }

  end_local <- lubridate::now(tzone = cfg$time$tz_local) -
    lubridate::minutes(cfg$goes$latency_min)
  start_utc <- lubridate::with_tz(end_local - lookback, cfg$time$tz_global)
  end_utc <- lubridate::with_tz(end_local, cfg$time$tz_global)
  hours <- seq(
    lubridate::floor_date(start_utc, "hour"),
    lubridate::floor_date(end_utc, "hour"),
    by = "1 hour"
  )

  max_workers <- suppressWarnings(as.integer(cfg$performance$max_auto_workers))
  if (!is.finite(max_workers) || max_workers < 1L) max_workers <- 4L
  workers <- cfg$performance$download_workers
  if (is.null(workers) || identical(workers, "auto")) workers <- max_workers
  workers <- suppressWarnings(as.integer(workers))
  if (!is.finite(workers) || workers < 1L) workers <- 1L

  totals <- c(candidates = 0L, downloaded = 0L, skipped = 0L, errors = 0L)
  core_log_append(
    log_file,
    msg = "Ventana recent calculada",
    fields = list(start_utc = start_utc, end_utc = end_utc, n_hours = length(hours))
  )
  progress <- progressr::progressor(steps = length(hours))

  for (hour in hours) {
    hour <- as.POSIXct(hour, origin = "1970-01-01", tz = cfg$time$tz_global)
    prefix <- sprintf(
      "%s/%s/%s/%s/",
      cfg$goes$product,
      format(hour, "%Y"),
      format(hour, "%j"),
      format(hour, "%H")
    )
    out_dir <- fs::path(
      cfg$paths$raw_dir,
      cfg$goes$satellite,
      cfg$goes$product,
      cfg$goes$channel,
      format(hour, "%Y"),
      format(hour, "%m"),
      format(hour, "%d"),
      format(hour, "%H")
    )

    keys <- tryCatch(
      goes_s3_list_keys_all(
        bucket = cfg$goes$bucket,
        prefix = prefix,
        channel = cfg$goes$channel
      ),
      error = function(e) {
        totals[["errors"]] <<- totals[["errors"]] + 1L
        core_log_append(
          log_file,
          level = "ERROR",
          msg = "Fallo listando S3",
          fields = list(prefix = prefix, error = conditionMessage(e))
        )
        character()
      }
    )
    if (length(keys) == 0L) {
      progress(message = format(hour, "%H:00 UTC"))
      next
    }

    totals[["candidates"]] <- totals[["candidates"]] + length(keys)
    destinations <- fs::path(out_dir, fs::path_file(keys))
    valid <- fs::file_exists(destinations)
    if (any(valid)) {
      valid[valid] <- vapply(
        destinations[valid],
        core_is_file_ok,
        logical(1),
        min_bytes = cfg$goes$min_bytes
      )
    }
    totals[["skipped"]] <- totals[["skipped"]] + sum(valid)
    missing <- keys[!valid]
    if (length(missing) == 0L) {
      progress(message = format(hour, "%H:00 UTC"))
      next
    }

    result <- goes_download_many(
      bucket = cfg$goes$bucket,
      keys = missing,
      out_dir = out_dir,
      overwrite = TRUE,
      min_bytes_ok = cfg$goes$min_bytes,
      parallel_workers = min(workers, length(missing))
    )
    totals[["downloaded"]] <- totals[["downloaded"]] + length(result$downloaded)
    totals[["errors"]] <- totals[["errors"]] + length(result$failed)
    progress(message = format(hour, "%H:00 UTC"))
  }

  if (totals[["downloaded"]] == 0L) {
    last <- .last_local_goes_time(
      fs::path(
        cfg$paths$raw_dir,
        cfg$goes$satellite,
        cfg$goes$product,
        cfg$goes$channel
      )
    )
    if (!is.na(last)) {
      message("GOES: sin actualizacion. Ultimo dato local: ", format(last, tz = "UTC"))
    }
  }

  core_log_append(
    log_file,
    level = if (totals[["errors"]] > 0L) "ERROR" else "INFO",
    msg = "Fin recent",
    fields = as.list(totals)
  )
  invisible(list(
    status = if (totals[["errors"]] > 0L) "error" else "ok",
    totals = as.list(totals)
  ))
}
