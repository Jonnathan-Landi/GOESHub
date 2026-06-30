run_publish_pipeline <- function(
  cfg,
  create_animation = TRUE
) {
  started_at <- Sys.time()
  log_file <- build_log_file(cfg, stage = "publish")

  n_frames <- suppressWarnings(as.integer(cfg$publish$n_frames))
  if (!is.finite(n_frames) || is.na(n_frames) || n_frames < 1L) {
    n_frames <- 19L
  }

  fps <- suppressWarnings(as.integer(cfg$publish$fps))
  if (!is.finite(fps) || is.na(fps) || fps < 1L) {
    fps <- 2L
  }

  ffmpeg_preset <- cfg$publish$ffmpeg_preset
  if (is.null(ffmpeg_preset) || !nzchar(ffmpeg_preset)) {
    ffmpeg_preset <- "medium"
  }

  ffmpeg_crf <- suppressWarnings(as.integer(cfg$publish$ffmpeg_crf))
  if (!is.finite(ffmpeg_crf) || is.na(ffmpeg_crf)) {
    ffmpeg_crf <- 20L
  }

  append_stage_log(
    log_file,
    stage = "publish",
    message = "Inicio publish pipeline",
    fields = list(
      create_animation = create_animation,
      n_frames = n_frames,
      fps = fps,
      ffmpeg_preset = ffmpeg_preset,
      ffmpeg_crf = ffmpeg_crf
    )
  )

  if (!isTRUE(create_animation)) {
    append_stage_log(
      log_file,
      stage = "publish",
      message = "Publish sin animación"
    )
    return(invisible(list(log_file = log_file, status = "skipped")))
  }

  root_dir <- here::here()
  frames_dir <- cfg$paths$public_dir
  out_dir <- file.path(root_dir, "data", "out")
  fs::dir_create(out_dir, recurse = TRUE)

  pattern <- go_frame_pattern(cfg)

  frames <- list.files(frames_dir, pattern = pattern, full.names = TRUE)
  if (length(frames) == 0L) {
    append_stage_log(
      log_file,
      stage = "publish",
      level = "WARN",
      message = "Sin frames para publicar",
      fields = list(frames_dir = frames_dir)
    )
    return(invisible(list(log_file = log_file, status = "no_data")))
  }

  frames <- sort(frames)
  frames_last <- utils::tail(frames, n_frames)
  frames_info <- fs::file_info(frames_last)
  frame_signature <- paste(
    basename(frames_last),
    as.numeric(frames_info$size),
    as.numeric(frames_info$modification_time),
    collapse = "|"
  )

  publish_cache_dir <- cfg$paths$cache_dir
  fs::dir_create(publish_cache_dir, recurse = TRUE)
  publish_cache_path <- fs::path(
    publish_cache_dir,
    paste0(
      "publish_c",
      sprintf("%02d", as.integer(cfg$goes$channel)),
      "_latest.json"
    )
  )

  previous_signature <- NULL
  if (fs::file_exists(publish_cache_path)) {
    previous <- tryCatch(
      jsonlite::fromJSON(publish_cache_path, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (!is.null(previous$frame_signature)) {
      previous_signature <- as.character(previous$frame_signature)
    }
  }

  if (identical(previous_signature, frame_signature)) {
    append_stage_log(
      log_file,
      stage = "publish",
      message = "Sin cambios en frames recientes; se omite re-encode",
      fields = list(n_frames_considered = length(frames_last))
    )
    return(invisible(list(log_file = log_file, status = "up_to_date")))
  }

  source(file.path(
    root_dir,
    "scripts",
    "publish",
    "encode_mp4_whatsapp_latest.R"
  ))

  fs::dir_create(file.path(out_dir, "gifs_WhatsApp"), recurse = TRUE)

  make_latest_mp4_whatsapp(
    frames_dir = frames_dir,
    out_dir = file.path(out_dir, "gifs_WhatsApp"),
    n = n_frames,
    pattern = pattern,
    fps = fps,
    width_px = 960,
    crf = ffmpeg_crf,
    preset = ffmpeg_preset,
    ffmpeg_bin = cfg$paths$ffmpeg_path,
    out_name = latest_mp4_name(cfg)
  )

  jsonlite::write_json(
    list(
      updated_utc = format(lubridate::now("UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      frame_signature = frame_signature,
      n_frames_considered = length(frames_last)
    ),
    publish_cache_path,
    auto_unbox = TRUE,
    pretty = FALSE
  )

  append_stage_log(
    log_file,
    stage = "publish",
    message = "Fin publish pipeline",
    fields = list(
      duration_sec = round(
        as.numeric(difftime(Sys.time(), started_at, units = "secs")),
        2
      ),
      ffmpeg_preset = ffmpeg_preset,
      ffmpeg_crf = ffmpeg_crf
    )
  )
  invisible(list(log_file = log_file, status = "ok"))
}
