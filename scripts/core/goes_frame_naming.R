goes_extract_stamp <- function(nc_file) {
  bn <- basename(nc_file)
  m <- stringr::str_match(bn, "s(\\d{13})")

  if (is.na(m[, 2])) {
    return(NA_character_)
  }

  paste0("s", m[, 2])
}

goes_stamp_to_time <- function(stamp) {
  s <- substring(stamp, 2)

  y <- as.integer(substr(s, 1, 4))
  doy <- as.integer(substr(s, 5, 7))
  hh <- as.integer(substr(s, 8, 9))
  mm <- as.integer(substr(s, 10, 11))
  ss <- as.integer(substr(s, 12, 13))

  as.POSIXct(sprintf("%04d-01-01 00:00:00", y), tz = "UTC") +
    lubridate::days(doy - 1) +
    lubridate::hours(hh) +
    lubridate::minutes(mm) +
    lubridate::seconds(ss)
}

png_path_from_time <- function(cfg, t_utc) {
  frame_prefix <- cfg$naming$frame_prefix
  if (is.null(frame_prefix) || !nzchar(frame_prefix)) {
    satellite <- cfg$goes$satellite
    if (is.null(satellite) || !nzchar(satellite)) {
      satellite <- "goes"
    }
    channel <- sprintf("%02d", as.integer(cfg$goes$channel))
    frame_prefix <- paste0(satellite, "_c", channel)
  }

  fs::path(
    cfg$paths$public_dir,
    paste0(
      frame_prefix,
      "_",
      format(t_utc, "%Y%m%d_%H%M%S", tz = "UTC"),
      ".png"
    )
  )
}
