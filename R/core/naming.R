goes_frame_prefix <- function(cfg) {
  configured <- cfg$naming$frame_prefix
  if (!is.null(configured) && nzchar(configured)) {
    return(configured)
  }

  sprintf(
    "%s_c%s",
    cfg$goes$satellite,
    sprintf("%02d", as.integer(cfg$goes$channel))
  )
}

go_frame_pattern <- function(cfg) {
  paste0("^", goes_frame_prefix(cfg), "_\\d{8}_\\d{6}\\.png$")
}

latest_mp4_name <- function(cfg) {
  configured <- cfg$naming$mp4_latest_name
  if (!is.null(configured) && nzchar(configured)) {
    return(configured)
  }
  paste0(goes_frame_prefix(cfg), "_latest_whatsapp.mp4")
}
