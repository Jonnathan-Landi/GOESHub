core_is_file_ok <- function(
  filepath,
  min_bytes = 50 * 1024,
  check_header = TRUE
) {
  # Existencia
  if (!fs::file_exists(filepath)) {
    return(FALSE)
  }

  # ---- validar min_bytes de forma segura ----
  if (is.null(min_bytes)) {
    return(FALSE)
  }
  if (length(min_bytes) != 1L) {
    return(FALSE)
  }

  min_bytes <- suppressWarnings(as.numeric(min_bytes))
  if (!is.finite(min_bytes) || is.na(min_bytes) || min_bytes <= 0) {
    return(FALSE)
  }

  # ---- tamaño de archivo (seguro) ----
  info <- tryCatch(fs::file_info(filepath), error = function(e) NULL)
  if (is.null(info) || nrow(info) == 0L) {
    return(FALSE)
  }

  size <- info$size[1]
  if (is.null(size) || length(size) != 1L) {
    return(FALSE)
  }

  size <- suppressWarnings(as.numeric(size))
  if (!is.finite(size) || is.na(size) || size < min_bytes) {
    return(FALSE)
  }

  # ---- chequeo cabecera (opcional) ----
  if (isTRUE(check_header)) {
    con <- tryCatch(file(filepath, "rb"), error = function(e) NULL)
    if (is.null(con)) {
      return(FALSE)
    }
    on.exit(close(con), add = TRUE)

    sig <- tryCatch(readBin(con, what = "raw", n = 8), error = function(e) {
      raw(0)
    })

    # HDF5 signature (NetCDF4)
    hdf5_sig <- as.raw(c(0x89, 0x48, 0x44, 0x46, 0x0d, 0x0a, 0x1a, 0x0a))
    if (length(sig) == 8 && identical(sig, hdf5_sig)) {
      return(TRUE)
    }

    # NetCDF classic signatures
    if (length(sig) >= 4) {
      if (
        identical(sig[1:4], charToRaw("CDF\001")) ||
          identical(sig[1:4], charToRaw("CDF\002"))
      ) {
        return(TRUE)
      }
    }
    return(FALSE)
  }

  TRUE
}
# -----------------------------------
# generador de Logs
# -----------------------------------

core_log_append <- function(log_file, level = "INFO", msg, fields = list()) {
  fs::dir_create(fs::path_dir(log_file), recurse = TRUE)

  entry <- c(
    list(
      ts_utc = format(lubridate::now(tzone = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      level = level,
      message = msg
    ),
    fields
  )

  # Para auditoría considero que es suficiente; luego si es necesario se planteara migrar a jsonlite.
  line <- paste0(
    paste(
      names(entry),
      vapply(entry, as.character, ""),
      sep = "=",
      collapse = " | "
    ),
    "\n"
  )
  cat(line, file = log_file, append = TRUE)
}
