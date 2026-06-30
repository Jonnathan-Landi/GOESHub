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
  ok <- file.copy(from = files_last, to = seq_paths, overwrite = TRUE)
  if (!all(ok)) {
    stop("Falló la copia/renombrado de algunos frames a la carpeta temporal.")
  }

  # 4) Salida (evitar normalizePath que exige existencia)
  out_file <- fs::path(out_dir, out_name)
  out_file_ff <- gsub("\\\\", "/", fs::path_abs(out_file))

  # 5) Verificar ffmpeg
  ffmpeg_bin <- as.character(ffmpeg_bin)[[1]]
  if (!nzchar(ffmpeg_bin) || !file.exists(ffmpeg_bin)) {
    stop(
      "No se encontro ffmpeg en la ruta esperada: ", ffmpeg_bin, "\n",
      "Configure paths$ffmpeg_path con la ruta absoluta o relativa al ejecutable."
    )
  }
  ffmpeg_bin <- normalizePath(ffmpeg_bin, winslash = "/", mustWork = TRUE)

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

  if (!file.exists(out_file)) {
    stop("FFmpeg no generó el MP4. Mensaje:\n", paste(res, collapse = "\n"))
  }

  invisible(out_file)
}
