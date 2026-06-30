.download_one_goes <- function(
  bucket,
  key,
  dest,
  min_bytes,
  timeout_sec = 240,
  retries = 3L
) {
  url <- paste0("https://", bucket, ".s3.amazonaws.com/", key)
  part <- paste0(dest, ".part")
  fs::dir_create(fs::path_dir(dest), recurse = TRUE)
  on.exit(if (fs::file_exists(part)) fs::file_delete(part), add = TRUE)

  for (attempt in seq_len(retries + 1L)) {
    if (fs::file_exists(part)) fs::file_delete(part)
    response <- tryCatch(
      curl::curl_download(
        url,
        part,
        quiet = TRUE,
        handle = curl::new_handle(
          timeout = timeout_sec,
          connecttimeout = 30,
          useragent = "SWMS-ingest/1.0"
        )
      ),
      error = identity
    )

    if (!inherits(response, "error") &&
        core_is_file_ok(part, min_bytes = min_bytes)) {
      if (fs::file_exists(dest)) fs::file_delete(dest)
      fs::file_move(part, dest)
      return(dest)
    }
    if (attempt <= retries) Sys.sleep(0.5 * 2^(attempt - 1L))
  }

  stop("No se pudo descargar ", fs::path_file(dest), call. = FALSE)
}

goes_download_many <- function(
  bucket,
  keys,
  out_dir,
  overwrite = FALSE,
  verbose = FALSE,
  min_bytes_ok = 50 * 1024,
  fail_fast = FALSE,
  parallel_workers = 4L,
  timeout_sec = 240,
  ...
) {
  fs::dir_create(out_dir, recurse = TRUE)
  if (length(keys) == 0L) {
    return(list(
      downloaded = character(),
      skipped = character(),
      failed = character(),
      out_dir = out_dir
    ))
  }

  destinations <- fs::path(out_dir, fs::path_file(keys))
  valid <- !overwrite & fs::file_exists(destinations)
  if (any(valid)) {
    valid[valid] <- vapply(
      destinations[valid],
      core_is_file_ok,
      logical(1),
      min_bytes = min_bytes_ok
    )
  }

  skipped <- as.character(destinations[valid])
  keys <- keys[!valid]
  destinations <- destinations[!valid]
  if (length(keys) == 0L) {
    return(list(
      downloaded = character(),
      skipped = skipped,
      failed = character(),
      out_dir = out_dir
    ))
  }

  workers <- suppressWarnings(as.integer(parallel_workers))
  if (!is.finite(workers) || workers < 1L) workers <- 1L
  workers <- min(workers, length(keys))
  downloaded <- character()
  failed <- character()

  # curl multi concurre dentro de un solo proceso: menos RAM y startup que PSOCK.
  batches <- split(seq_along(keys), ceiling(seq_along(keys) / workers))
  for (indices in batches) {
    batch_keys <- keys[indices]
    batch_dest <- as.character(destinations[indices])
    parts <- paste0(batch_dest, ".part")
    urls <- paste0("https://", bucket, ".s3.amazonaws.com/", batch_keys)
    unlink(parts)

    if (verbose) message("Descargando ", length(indices), " archivo(s)")
    response <- curl::multi_download(
      urls,
      parts,
      progress = FALSE,
      multi_timeout = timeout_sec,
      timeout = timeout_sec,
      connecttimeout = 30,
      useragent = "SWMS-ingest/1.0"
    )

    for (i in seq_along(indices)) {
      ok <- isTRUE(response$success[[i]]) &&
        response$status_code[[i]] >= 200 &&
        response$status_code[[i]] < 300 &&
        core_is_file_ok(parts[[i]], min_bytes = min_bytes_ok)

      if (ok) {
        if (fs::file_exists(batch_dest[[i]])) fs::file_delete(batch_dest[[i]])
        fs::file_move(parts[[i]], batch_dest[[i]])
        downloaded <- c(downloaded, batch_dest[[i]])
      } else {
        # Reintento individual con backoff para fallos transitorios.
        retry <- tryCatch(
          .download_one_goes(
            bucket,
            batch_keys[[i]],
            batch_dest[[i]],
            min_bytes_ok,
            timeout_sec
          ),
          error = identity
        )
        if (inherits(retry, "error")) {
          failed <- c(failed, batch_dest[[i]])
          if (fail_fast) stop(conditionMessage(retry), call. = FALSE)
        } else {
          downloaded <- c(downloaded, retry)
        }
      }
      if (fs::file_exists(parts[[i]])) fs::file_delete(parts[[i]])
    }
  }

  list(
    downloaded = downloaded,
    skipped = skipped,
    failed = failed,
    out_dir = out_dir
  )
}
