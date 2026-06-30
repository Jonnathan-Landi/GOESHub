source(file.path("orchestrator", "orch_pipeline.R"))
run_pipeline(
  lookback = "3 hour",
  create_animation = TRUE,
  stages = c("ingest", "process", "render", "publish")
)
