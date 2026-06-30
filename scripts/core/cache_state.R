cache_stage_path <- function(cfg, stage) {
  cache_root <- cfg$paths$cache_dir
  if (is.null(cache_root) || !nzchar(cache_root)) {
    cache_root <- here::here("cache")
  }

  fs::dir_create(cache_root, recurse = TRUE)

  sat <- cfg$goes$satellite
  if (is.null(sat) || !nzchar(sat)) {
    sat <- "goes"
  }

  channel <- sprintf("%02d", as.integer(cfg$goes$channel))
  fs::path(cache_root, paste0(stage, "_c", channel, ".jsonl"))
}

cache_empty_day <- function(date_local) {
  now_utc <- format(lubridate::now("UTC"), "%Y-%m-%dT%H:%M:%SZ")
  list(
    schema_version = 1,
    date_local = format(as.Date(date_local), "%Y-%m-%d"),
    created_utc = now_utc,
    updated_utc = now_utc,
    items = list()
  )
}

cache_load_day <- function(path, date_local) {
  target_date <- format(as.Date(date_local), "%Y-%m-%d")
  if (!fs::file_exists(path)) {
    return(cache_empty_day(target_date))
  }

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0L) {
    return(cache_empty_day(target_date))
  }

  for (line in rev(lines)) {
    item <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (!is.null(item$date_local) && identical(item$date_local, target_date)) {
      return(item)
    }
  }

  cache_empty_day(target_date)
}

cache_save_day <- function(state, path) {
  state$updated_utc <- format(lubridate::now("UTC"), "%Y-%m-%dT%H:%M:%SZ")
  target_date <- state$date_local
  if (is.null(target_date) || !nzchar(target_date)) {
    stop("Cache state must include date_local.", call. = FALSE)
  }

  lines <- character()
  if (fs::file_exists(path)) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    lines <- lines[nzchar(trimws(lines))]
  }

  keep <- logical(length(lines))
  for (i in seq_along(lines)) {
    item <- tryCatch(
      jsonlite::fromJSON(lines[[i]], simplifyVector = FALSE),
      error = function(e) NULL
    )
    keep[[i]] <- is.null(item$date_local) || !identical(item$date_local, target_date)
  }

  new_line <- jsonlite::toJSON(
    state,
    pretty = FALSE,
    auto_unbox = TRUE,
    null = "null"
  )

  out_lines <- c(lines[keep], as.character(new_line))
  tmp <- paste0(path, ".tmp")
  writeLines(out_lines, tmp, useBytes = TRUE)

  if (fs::file_exists(path)) {
    fs::file_delete(path)
  }
  fs::file_move(tmp, path)
}
