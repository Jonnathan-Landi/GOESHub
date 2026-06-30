run_ingest_pipeline <- function(cfg, lookback = "3 hour") {
  root_dir <- here::here()
  source(file.path(root_dir, "scripts", "core", "ops_utils.R"))
  source(file.path(root_dir, "scripts", "ingest", "recent.R"))

  log_file <- build_log_file(cfg, stage = "ingest")
  append_stage_log(
    log_file,
    stage = "ingest",
    message = "Inicio ingest pipeline",
    fields = list(lookback = lookback)
  )

  download_recent(log_file = log_file, cfg = cfg, lookback = lookback)

  append_stage_log(
    log_file,
    stage = "ingest",
    message = "Fin ingest pipeline",
    fields = list(lookback = lookback)
  )

  invisible(list(log_file = log_file))
}
