state_frames_path <- function(cfg, date_local) {
  cache_stage_path(cfg, "frames")
}

state_load <- function(path, date_local) {
  cache_load_day(path, date_local)
}

state_save <- function(state, path) {
  cache_save_day(state, path)
}
