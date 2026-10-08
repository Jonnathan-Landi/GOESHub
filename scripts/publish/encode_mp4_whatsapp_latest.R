make_latest_mp4_whatsapp <- function(
  frames_dir,
  out_dir,
  n = 13,
  pattern = "^goes19_c13_\\d{8}_\\d{6}\\.png$",
  out_name = "cuenca_c13_latest_whatsapp.mp4",
  fps = 2,
  width_px = 960,
  crf = 18,
  preset = "slow",
  ffmpeg_bin = Sys.which("ffmpeg"),
  overwrite = TRUE,
  order_by = c("name", "mtime") # <- robustez: por nombre o por fecha del archivo
) {
  order_by <- match.arg(order_by)
  stopifnot(dir.exists(frames_dir))

  if (!dir.exists(out_dir)) {
    fs::dir_create(out_dir, recurse = TRUE)
  }

  # 1) Listar frames
  files <- list.files(frames_dir, pattern = pattern, full.names = TRUE)
  if (length(files) < 2) {
    stop("No hay suficientes frames PNG para crear MP4.")
  }

  # 2) Ordenar y escoger últimos n
  if (order_by == "name") {
    files <- sort(files)
  } else {
    info <- file.info(files)
    files <- rownames(info)[order(info$mtime)]
  }
  files_last <- utils::tail(files, n)
  if (length(files_last) < 2) {
    stop("No hay suficientes frames en los 'últimos n' para crear MP4.")
  }

  # 3) Crear carpeta temporal con nombres secuenciales (image2)
  tmp_dir <- tempfile("frames_seq_")
  fs::dir_create(tmp_dir)

  # Copiar/renombrar a frame_0001.png ... frame_0012.png
  seq_names <- sprintf("frame_%04d.png", seq_along(files_last))
  seq_paths <- fs::path(tmp_dir, seq_names)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  ok <- file.copy(from = files_last, to = seq_paths, overwrite = TRUE)
  if (!all(ok)) {
    stop("Falló la copia/renombrado de algunos frames a la carpeta temporal.")
  }

  # 4) Salida (evitar normalizePath que exige existencia)
  out_file <- fs::path(out_dir, out_name)
  out_file_ff <- gsub("\\\\", "/", fs::path_abs(out_file))

  # 5) Verificar ffmpeg
  configured_ffmpeg <- if (length(ffmpeg_bin) > 0L) {
    as.character(ffmpeg_bin)[[1]]
  } else {
    ""
  }
  configured_ffmpeg <- if (is.na(configured_ffmpeg)) "" else configured_ffmpeg
  ffmpeg_candidates <- unique(c(
    configured_ffmpeg,
    Sys.which("ffmpeg")
  ))
  ffmpeg_candidates <- ffmpeg_candidates[
    nzchar(ffmpeg_candidates) & file.exists(ffmpeg_candidates)
  ]
  if (length(ffmpeg_candidates) == 0L) {
    stop(
      "No se encontró ffmpeg. Ruta configurada: ",
      ifelse(nzchar(configured_ffmpeg), configured_ffmpeg, "<vacía>"),
      ". Configure paths$ffmpeg_path con una ruta válida o agregue ffmpeg al PATH.",
      call. = FALSE
    )
  }
  ffmpeg_bin <- normalizePath(ffmpeg_candidates[[1]], winslash = "/", mustWork = TRUE)

  ffmpeg_check <- tryCatch(
    system2(ffmpeg_bin, args = c("-hide_banner", "-version"), stdout = TRUE, stderr = TRUE),
    error = function(e) e
  )
  if (inherits(ffmpeg_check, "error")) {
    stop(
      "Windows no pudo iniciar ffmpeg en '", ffmpeg_bin, "'. ",
      "Verifique que el ejecutable no esté bloqueado y que sus dependencias estén disponibles. ",
      "Detalle: ", conditionMessage(ffmpeg_check),
      call. = FALSE
    )
  }

  # 6) Comando ffmpeg con image2
  # -framerate controla la cadencia de entrada
  # -pix_fmt yuv420p para compatibilidad
  vf <- sprintf("scale=%d:-2:flags=lanczos,format=yuv420p", width_px)
  in_pattern <- gsub(
    "\\\\",
    "/",
    fs::path_abs(fs::path(tmp_dir, "frame_%04d.png"))
  )

  args <- c(
    if (overwrite) "-y" else "-n",
    "-hide_banner",
    "-loglevel",
    "error",
    "-framerate",
    as.character(fps),
    "-i",
    in_pattern,
    "-vf",
    vf,
    "-c:v",
    "libx264",
    "-profile:v",
    "high",
    "-level",
    "4.1",
    "-preset",
    preset,
    "-crf",
    as.character(crf),
    "-movflags",
    "+faststart",
    out_file_ff
  )

  res <- system2(ffmpeg_bin, args = args, stdout = TRUE, stderr = TRUE)
  exit_status <- attr(res, "status", exact = TRUE)

  if (!is.null(exit_status) || !file.exists(out_file)) {
    status_text <- if (is.null(exit_status)) {
      "salida no encontrada"
    } else {
      paste0("código de salida ", exit_status)
    }
    stop(
      "FFmpeg falló (", status_text, "). Mensaje:\n",
      paste(res, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(out_file)
}
