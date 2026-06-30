load_backend_config <- function(
  root_dir = here::here(),
  config_file = NULL,
  abi_file = file.path(root_dir, "config", "cfg_abi.yml")
) {
  if (is.null(config_file)) {
    config_file <- file.path(root_dir, "config", "config.yml")
  }

  cfg <- yaml::read_yaml(config_file)
  abi_cfg <- yaml::read_yaml(abi_file)
  if (is.null(abi_cfg$abi)) {
    stop("cfg_abi.yml debe contener la seccion 'abi'.", call. = FALSE)
  }
  cfg$goes <- abi_cfg$abi

  resolve_path <- function(path_value) {
    if (is.null(path_value) || !nzchar(path_value)) {
      return(path_value)
    }
    normalizePath(
      file.path(root_dir, path_value),
      winslash = "/",
      mustWork = FALSE
    )
  }

  cfg$paths$raw_dir <- resolve_path(cfg$paths$raw_dir)
  cfg$paths$processed_dir <- resolve_path(cfg$paths$processed_dir)
  cfg$paths$public_dir <- resolve_path(cfg$paths$public_dir)
  cfg$paths$logs_dir <- resolve_path(cfg$paths$logs_dir)
  if (is.null(cfg$paths$cache_dir) || !nzchar(cfg$paths$cache_dir)) {
    cfg$paths$cache_dir <- "cache"
  }
  cfg$paths$cache_dir <- resolve_path(cfg$paths$cache_dir)
  cfg$paths$ffmpeg_path <- resolve_path(cfg$paths$ffmpeg_path)
  cfg$roi$country_path <- resolve_path(cfg$roi$country_path)
  cfg$roi$cantons_path <- resolve_path(cfg$roi$cantons_path)
  cfg$roi$province_path <- resolve_path(cfg$roi$province_path)
  cfg$roi$buffer_path <- resolve_path(cfg$roi$buffer_path)

  cfg
}

build_log_file <- function(cfg, stage, t_utc = lubridate::now("UTC")) {
  fs::dir_create(cfg$paths$logs_dir, recurse = TRUE)
  fs::path(cfg$paths$logs_dir, "pipeline.log")
}
