goes_norm_channel <- function(channel) {
  if (is.null(channel) || length(channel) != 1) {
    stop("channel inválido")
  }
  ch <- suppressWarnings(as.integer(channel))
  if (is.na(ch) || ch < 1 || ch > 16) {
    stop("channel debe estar entre 1 y 16")
  }
  sprintf("%02d", ch)
}

# ListObjectsV2 con filtro cliente
# - Si return_meta=FALSE: retorna character vector de keys filtradas
# - Si return_meta=TRUE : retorna data.frame key/size/last_modified
goes_s3_list_keys_all <- function(
  bucket,
  prefix,
  channel,
  max_keys = 1000,
  timeout_sec = 30,
  max_tries = 5,
  pause_base = 0.5,
  return_meta = FALSE
) {
  stopifnot(is.character(bucket), length(bucket) == 1, nzchar(bucket))
  stopifnot(is.character(prefix), length(prefix) == 1)
  stopifnot(is.numeric(max_keys), max_keys >= 1, max_keys <= 1000)

  ch2 <- goes_norm_channel(channel)
  channel_pattern <- paste0("C", ch2, "_G")

  base_url <- paste0("https://", bucket, ".s3.amazonaws.com/")
  token <- NULL

  out_keys <- list()
  out_sizes <- list()
  out_lm <- list()
  idx <- 0L

  repeat {
    query <- list(
      "list-type" = 2,
      prefix = prefix,
      "max-keys" = as.integer(max_keys)
    )
    if (!is.null(token)) {
      query[["continuation-token"]] <- token
    }

    # httr::RETRY maneja backoff
    r <- httr::RETRY(
      verb = "GET",
      url = base_url,
      query = query,
      times = max_tries,
      pause_base = pause_base,
      terminate_on = c(400, 401, 403, 404),
      httr::timeout(timeout_sec)
    )
    httr::stop_for_status(r)

    txt <- httr::content(r, as = "text", encoding = "UTF-8")
    doc <- xml2::read_xml(txt)
    ns <- xml2::xml_ns(doc)

    # Traigo nodos Contents una sola vez
    contents <- xml2::xml_find_all(doc, ".//d1:Contents", ns = ns)
    if (length(contents) > 0) {
      # Key/Size/LastModified por item
      key_nodes <- xml2::xml_find_all(contents, ".//d1:Key", ns = ns)
      size_nodes <- xml2::xml_find_all(contents, ".//d1:Size", ns = ns)
      lm_nodes <- xml2::xml_find_all(contents, ".//d1:LastModified", ns = ns)

      keys <- xml2::xml_text(key_nodes)

      # Filtro aplicado aquí
      keep <- grepl("\\.nc$", keys, ignore.case = TRUE) &
        grepl(channel_pattern, keys)

      if (any(keep)) {
        idx <- idx + 1L
        out_keys[[idx]] <- keys[keep]

        if (return_meta) {
          sizes <- suppressWarnings(as.numeric(xml2::xml_text(size_nodes)))
          lms <- xml2::xml_text(lm_nodes)

          out_sizes[[idx]] <- sizes[keep]
          out_lm[[idx]] <- lms[keep]
        }
      }
    }

    is_truncated <- xml2::xml_text(xml2::xml_find_first(
      doc,
      ".//d1:IsTruncated",
      ns = ns
    ))
    if (!identical(tolower(is_truncated), "true")) {
      break
    }

    token <- xml2::xml_text(xml2::xml_find_first(
      doc,
      ".//d1:NextContinuationToken",
      ns = ns
    ))
    if (is.na(token) || token == "") break
  }

  keys_out <- unlist(out_keys, use.names = FALSE)
  if (!return_meta) {
    return(keys_out)
  }

  data.frame(
    key = keys_out,
    size = unlist(out_sizes, use.names = FALSE),
    last_modified = unlist(out_lm, use.names = FALSE),
    stringsAsFactors = FALSE
  )
}
