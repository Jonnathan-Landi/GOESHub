orch_file <- vapply(
  sys.frames(),
  function(env) if (is.null(env$ofile)) "" else env$ofile,
  character(1)
)
orch_file <- orch_file[nzchar(orch_file)]
orch_file <- if (length(orch_file) > 0) {
  orch_file[[length(orch_file)]]
} else {
  file.path(getwd(), "orchestrator", "orch_pipeline.R")
}

root_dir <- normalizePath(
  file.path(dirname(normalizePath(orch_file, winslash = "/", mustWork = FALSE)), ".."),
  winslash = "/",
  mustWork = FALSE
)

source(file.path(root_dir, "R", "core", "dependencies.R"))
install_and_load_dependencies()

source(file.path(root_dir, "R", "core", "config.R"))
source(file.path(root_dir, "R", "core", "logging.R"))
source(file.path(root_dir, "R", "core", "naming.R"))
source(file.path(root_dir, "R", "pipelines", "ingest_pipeline.R"))
source(file.path(root_dir, "R", "pipelines", "process_pipeline.R"))
source(file.path(root_dir, "R", "pipelines", "render.R"))
source(file.path(root_dir, "R", "pipelines", "publish_pipeline.R"))

run_pipeline <- function(
  lookback = "3 hour",
  create_animation = TRUE,
  stages = c("ingest", "process", "render", "publish")
) {
  started_at <- Sys.time()
  cfg <- load_backend_config(root_dir)
  options(progressr.enable = TRUE)
  progressr::handlers(progressr::handler_progress(
    format = "[:bar] :percent | :current/:total | elapsed :elapsed | eta :eta | :message",
    clear = FALSE
  ))

  force_single_core <- tolower(Sys.getenv(
    "GOES_FORCE_SINGLE_CORE",
    unset = "0"
  )) %in%
    c("1", "true", "yes")
  if (isTRUE(force_single_core)) {
    if (is.null(cfg$performance)) {
      cfg$performance <- list()
    }
    cfg$performance$download_workers <- 1L
    cfg$performance$process_workers <- 1L
    cfg$performance$render_workers <- 1L
    cfg$performance$terra_threads <- 1L
    cfg$performance$worker_blas_threads <- 1L
    cfg$performance$max_auto_workers <- 1L
  }

  if (is.character(stages) && length(stages) == 1) {
    stages <- trimws(unlist(strsplit(stages, ",")))
  }

  valid_stages <- c("ingest", "process", "render", "publish")
  invalid_stages <- setdiff(stages, valid_stages)
  if (length(invalid_stages) > 0L) {
    stop("Etapas no soportadas: ", paste(invalid_stages, collapse = ", "))
  }

  run_stage <- function(stage) {
    stage_started <- Sys.time()
    message("\n[", match(stage, stages), "/", length(stages), "] Iniciando: ", stage)

    result <- switch(
      stage,
      ingest = run_ingest_pipeline(cfg, lookback = lookback),
      process = run_process_pipeline(cfg),
      render = run_render_pipeline(cfg),
      publish = run_publish_pipeline(
        cfg,
        create_animation = create_animation
      )
    )

    stage_elapsed <- round(as.numeric(difftime(Sys.time(), stage_started, units = "secs")), 1)
    message("[", stage, "] Finalizada en ", stage_elapsed, " s")
    result
  }

  invisible(lapply(stages, function(stage) {
    progressr::with_progress(run_stage(stage))
  }))

  elapsed <- round(as.numeric(difftime(Sys.time(), started_at, units = "secs")), 1)
  message("Pipeline finalizado en ", elapsed, " segundos.")

  invisible(TRUE)
}
