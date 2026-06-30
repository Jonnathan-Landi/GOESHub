append_stage_log <- function(log_file, stage, level = "INFO", message, fields = list()) {
  fs::dir_create(fs::path_dir(log_file), recurse = TRUE)
  log_day <- format(as.Date(lubridate::now("UTC")), "%Y-%m-%d")
  day_header <- paste0(
    "############################\n",
    "# Fecha ",
    log_day,
    "\n",
    "############################\n"
  )

  needs_header <- TRUE
  if (fs::file_exists(log_file)) {
    existing <- readLines(log_file, warn = FALSE, encoding = "UTF-8")
    needs_header <- !any(existing == paste0("# Fecha ", log_day))
  }

  if (isTRUE(needs_header)) {
    cat(day_header, file = log_file, append = TRUE)
  }

  entry <- c(
    list(
      ts_utc = format(lubridate::now("UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      stage = stage,
      level = level,
      message = message
    ),
    fields
  )

  line <- jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null")
  cat(paste0(line, "\n"), file = log_file, append = TRUE)
}
