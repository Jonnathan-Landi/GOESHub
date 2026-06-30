# ------------------------------------------------------------
# Rectángulo redondeado en base R
# ------------------------------------------------------------
.round_rect <- function(x0, y0, x1, y1, r, col, border, lwd = 1) {
  r <- max(0, min(r, (x1 - x0) / 2, (y1 - y0) / 2))
  if (r == 0) {
    rect(x0, y0, x1, y1, col = col, border = border, lwd = lwd)
    return(invisible(NULL))
  }

  n <- 18
  th <- seq(0, pi / 2, length.out = n)

  # esquinas (arcos)
  bl <- cbind(x0 + r + r * cos(pi + th), y0 + r + r * sin(pi + th))
  br <- cbind(
    x1 - r + r * cos(3 * pi / 2 + th),
    y0 + r + r * sin(3 * pi / 2 + th)
  )
  tr <- cbind(x1 - r + r * cos(0 + th), y1 - r + r * sin(0 + th))
  tl <- cbind(x0 + r + r * cos(pi / 2 + th), y1 - r + r * sin(pi / 2 + th))

  p <- rbind(bl, br, tr, tl)
  polygon(p[, 1], p[, 2], col = col, border = border, lwd = lwd)
  invisible(NULL)
}

# ------------------------------------------------------------
# Rectángulo redondeado en base R
# ------------------------------------------------------------
draw_inset_legend_c13_acrylic_base <- function(
  e,
  cols,
  brks,
  # ---- POSICIÓN ----
  margin = 0.010, # separación del borde (fracción de xr/yr)
  # ---- TAMAÑO: el ancho lo domina la barra ----
  bar_w_ratio = 0.26, # largo total de la barra (fracción de xr)  << AJUSTE PRINCIPAL
  side_pad_ratio = 0.06, # padding lateral del recuadro respecto a la barra (fracción de bar_w)
  box_h = 0.120, # alto del recuadro (fracción de yr)
  # ---- PADDING INTERNO ----
  pad_x_ratio = 0.06, # padding interno horizontal (fracción de W)
  pad_y_ratio = 0.09, # padding interno vertical (fracción de H)
  # ---- ESTILO “ACRYLIC” ----
  bg_gray = 0.92,
  bg_alpha = 0.86,
  border_alpha = 0.25,
  border_lwd = 1.2,
  corner_ratio = 0.070, # radio relativo a H (menos = menos redondeado)
  sheen_alpha = 0.14,
  sheen_stripes = 10,
  # ---- TEXTO ----
  cex_title = 1.15, # "Banda 13:" (más grande)
  cex_sub = 1.02,
  extra_gap_ratio = 0.012, # espacio entre líneas (fracción de yr)  << AJUSTE FINO
  # ---- ESPACIO ENTRE SUBTÍTULO Y BARRA ----
  gap_under_sub_mult = 1.6, # <-- AUMENTE si quiere más aire (1.6–2.2)
  # ---- BARRA / TICKS ----
  bar_h_ratio = 0.34, # alto barra dentro del bloque inferior
  ticks = seq(-90, 50, by = 15),
  cex_ticks = 0.86,
  tick_lwd = 1,
  tick_len_ratio = 0.30, # largo del tick como fracción de barH (antes fijo 0.30)
  # ---- FLECHAS ----
  arrow_w_ratio = 0.06 # ancho de cada flecha (fracción del ancho TOTAL de la barra)
) {
  # ---------------------------
  # Helpers
  # ---------------------------
  .round_rect <- function(x0, y0, x1, y1, r, col, border, lwd = 1) {
    r <- max(0, min(r, (x1 - x0) / 2, (y1 - y0) / 2))
    if (r == 0) {
      rect(x0, y0, x1, y1, col = col, border = border, lwd = lwd)
      return(invisible(NULL))
    }
    n <- 18
    th <- seq(0, pi / 2, length.out = n)
    bl <- cbind(x0 + r + r * cos(pi + th), y0 + r + r * sin(pi + th))
    br <- cbind(
      x1 - r + r * cos(3 * pi / 2 + th),
      y0 + r + r * sin(3 * pi / 2 + th)
    )
    tr <- cbind(x1 - r + r * cos(0 + th), y1 - r + r * sin(0 + th))
    tl <- cbind(x0 + r + r * cos(pi / 2 + th), y1 - r + r * sin(pi / 2 + th))
    p <- rbind(bl, br, tr, tl)
    polygon(p[, 1], p[, 2], col = col, border = border, lwd = lwd)
    invisible(NULL)
  }

  # ---------------------------
  # Geometría base
  # ---------------------------
  xr <- terra::xmax(e) - terra::xmin(e)
  yr <- terra::ymax(e) - terra::ymin(e)

  # ancho de barra en unidades "user"
  bar_w <- bar_w_ratio * xr

  # recuadro apenas más ancho que la barra
  side_pad <- side_pad_ratio * bar_w
  W <- bar_w + 2 * side_pad
  H <- box_h * yr

  x0 <- terra::xmin(e) + margin * xr
  y0 <- terra::ymin(e) + margin * yr
  x1 <- x0 + W
  y1 <- y0 + H

  # ---------------------------
  # Fondo acrylic
  # ---------------------------
  bg_col <- grDevices::rgb(bg_gray, bg_gray, bg_gray, alpha = bg_alpha)
  border_col <- grDevices::rgb(1, 1, 1, alpha = border_alpha)
  r <- corner_ratio * H

  .round_rect(
    x0,
    y0,
    x1,
    y1,
    r = r,
    col = bg_col,
    border = border_col,
    lwd = border_lwd
  )

  # brillo superior por franjas
  for (i in seq_len(sheen_stripes)) {
    t <- (i - 1) / (sheen_stripes - 1)
    a <- sheen_alpha * (1 - t)^2
    col_sheen <- grDevices::rgb(0.98, 0.98, 1.00, alpha = a)
    sy0 <- y0 + (0.55 + 0.45 * t) * H
    sy1 <- y0 + (0.55 + 0.45 * (t + 1 / sheen_stripes)) * H
    rect(x0, sy0, x1, sy1, col = col_sheen, border = NA)
  }

  # ---------------------------
  # Área interna
  # ---------------------------
  px <- pad_x_ratio * W
  py <- pad_y_ratio * H

  ix0 <- x0 + px
  ix1 <- x1 - px
  iy0 <- y0 + py
  iy1 <- y1 - py

  iW <- ix1 - ix0
  iH <- iy1 - iy0

  # ---------------------------
  # Texto con espaciado real
  # ---------------------------
  title_txt <- "Banda 13:"
  sub_txt <- "Canal 13 (10.3 µm) Ventana IR (limpia)"

  h_title <- strheight(title_txt, cex = cex_title)
  extra_gap <- extra_gap_ratio * yr

  y_title <- iy1
  text(
    ix0,
    y_title,
    labels = title_txt,
    adj = c(0, 1),
    cex = cex_title,
    font = 2,
    col = "black"
  )

  y_sub <- y_title - h_title - extra_gap
  text(
    ix0,
    y_sub,
    labels = sub_txt,
    adj = c(0, 1),
    cex = cex_sub,
    font = 1,
    col = "black"
  )

  # ---------------------------
  # Bloque inferior: start debajo del subtítulo (más aire)
  # ---------------------------
  gap_under_sub <- gap_under_sub_mult * extra_gap
  y_block_top <- y_sub - gap_under_sub

  # Protección mínima para (labels + barra)
  min_restH <- 0.36 * iH
  if ((y_block_top - iy0) < min_restH) {
    y_block_top <- iy0 + min_restH
  }

  # ---------------------------
  # Bloque inferior: barra y etiquetas
  # ---------------------------
  restH <- y_block_top - iy0
  barH <- bar_h_ratio * restH
  labH <- restH - barH

  by0 <- iy0 + labH
  by1 <- by0 + barH

  # Barra centrada, con ancho EXACTO bar_w
  bar_x0 <- (x0 + x1) / 2 - bar_w / 2
  bar_x1 <- (x0 + x1) / 2 + bar_w / 2

  # Flechas ocupan ancho dentro del total de la barra
  aw <- arrow_w_ratio * (bar_x1 - bar_x0)

  bx0 <- bar_x0 + aw
  bx1 <- bar_x1 - aw

  # Colorear barra usando brks/cols
  tmin <- min(brks)
  tmax <- max(brks)
  x_brks <- bx0 + (brks - tmin) / (tmax - tmin) * (bx1 - bx0)

  for (i in seq_along(cols)) {
    rect(x_brks[i], by0, x_brks[i + 1], by1, col = cols[i], border = NA)
  }
  rect(bx0, by0, bx1, by1, border = "black", lwd = 1)

  # Flechas altas (izq/der)
  yc <- (by0 + by1) / 2
  polygon(
    x = c(bar_x0 + 0.08 * aw, bar_x0 + 0.98 * aw, bar_x0 + 0.98 * aw),
    y = c(yc, by1, by0),
    col = "black",
    border = NA
  )
  polygon(
    x = c(bar_x1 - 0.08 * aw, bar_x1 - 0.98 * aw, bar_x1 - 0.98 * aw),
    y = c(yc, by1, by0),
    col = "black",
    border = NA
  )

  # ---------------------------
  # Ticks y números (ticks hacia ABAJO)
  # ---------------------------
  ticks <- ticks[ticks >= tmin & ticks <= tmax]
  if (length(ticks) > 0) {
    x_ticks <- bx0 + (ticks - tmin) / (tmax - tmin) * (bx1 - bx0)

    tick_len <- tick_len_ratio * barH

    # ticks hacia abajo desde el borde inferior de la barra
    segments(
      x0 = x_ticks,
      y0 = by0,
      x1 = x_ticks,
      y1 = by0 - tick_len,
      col = "black",
      lwd = tick_lwd
    )

    # números debajo del tick (dentro del recuadro)
    y_lab <- max(iy0 + 0.10 * labH, (by0 - tick_len) - 0.18 * barH)

    text(
      x = x_ticks,
      y = y_lab,
      labels = ticks,
      cex = cex_ticks,
      col = "black",
      font = 2,
      adj = c(0.5, 1)
    )
  }

  invisible(NULL)
}

prepare_render_static_assets <- function(cfg, cant, title_txt = "Nubosidad con potencial de tormentas (GOES-19)") {
  wrap_two_lines <- function(txt, max_chars) {
    if (nchar(txt) <= max_chars) {
      return(txt)
    }
    words <- strsplit(txt, "\\s+")[[1]]
    line1 <- ""
    line2 <- ""
    for (w in words) {
      if (nchar(line1) + nchar(w) + 1 <= max_chars) {
        line1 <- trimws(paste(line1, w))
      } else {
        line2 <- trimws(paste(line2, w))
      }
    }
    paste0(line1, "\n", line2)
  }

  cuenca <- cant[cant$CANTON == "CUENCA", ]
  centroid <- NULL
  if (nrow(cuenca) == 1) {
    centroid <- as.numeric(terra::crds(terra::centroids(cuenca))[1, ])
  }

  logo_path <- here::here("assets", "img", "logos", "Log_Etapa.png")
  logo_img <- png::readPNG(logo_path)
  logo_ratio <- dim(logo_img)[2] / dim(logo_img)[1]

  list(
    cuenca = cuenca,
    cuenca_centroid = centroid,
    title_txt2 = wrap_two_lines(title_txt, 34),
    logo_img = logo_img,
    logo_ratio = logo_ratio
  )
}

# ------------------------------------------------------------
# Rectángulo redondeado en base R
# ------------------------------------------------------------
generate_frames <- function(
  layer,
  cfg,
  e,
  ecu,
  cant,
  out_file = NULL,
  static_assets = NULL,
  cols,
  brks,
  # ---- PARÁMETROS VISUALES CONTROLABLES ----
  cex_title = 1.8,
  cex_time = 1.5,
  cex_legend = 0.95,
  cex_cuenca = 1.75,
  bar_height_ratio = 0.09,
  legend_height_ratio = 0.028,
  pad_x = 0.0001,
  # ---- ENCUADRE: asimetría norte/sur (mantiene “más sur”)
  view_up_ratio = 0.35,
  view_down_ratio = 0.65,
  # Pal zoom
  zoom = 1.7, # >1 = más zoom , <1 = más lejos
  pan_x_ratio = 0, # + derecha, - izquierda
  pan_y_ratio = 0.15 # + arriba (norte), - abajo (sur)
) {
  layer_time <- terra::time(layer)[1]
  layer <- terra::clamp(
    layer,
    lower = min(brks),
    upper = max(brks),
    values = TRUE
  )
  terra::time(layer) <- layer_time

  # ------------------------------------------------------------
  # Defino salida y abro dispositivo PNG (No cambiar a ggplot, pruebas indican que no funciona bien)
  # ------------------------------------------------------------
  if (is.null(out_file)) {
    out_dir <- cfg$paths$public_dir
    fs::dir_create(out_dir, recurse = TRUE)
    t_utc <- terra::time(layer)[1]
    out_file <- fs::path(
      out_dir,
      paste0("goes19_c13_", format(t_utc, "%Y%m%d_%H%M%S", tz = "UTC"), ".png")
    )
  } else {
    fs::dir_create(fs::path_dir(out_file), recurse = TRUE)
  }

  png(
    filename = out_file,
    width = 2000,
    height = 2000,
    res = 150,
    type = "cairo",
    bg = "black"
  )

  png_device <- dev.cur()

  # Elimina márgenes del dispositivo para que el mapa ocupe todo el lienzo.
  op_par <- par(no.readonly = TRUE)
  on.exit({
    if (png_device %in% dev.list()) {
      dev.set(png_device)
      par(op_par)
      dev.off()
    }
  }, add = TRUE)
  par(
    mar = c(0, 0, 0, 0),
    oma = c(0, 0, 0, 0),
    plt = c(0, 1, 0, 1),
    fig = c(0, 1, 0, 1),
    xaxs = "i",
    yaxs = "i"
  )

  # ------------------------------------------------------------
  # ENCUADRE: construir e_plot con asimetría + zoom + pan
  # ------------------------------------------------------------
  e0 <- e

  # Clamp ratios
  view_up_ratio <- max(0.05, min(0.95, view_up_ratio))
  view_down_ratio <- max(0.05, min(0.95, view_down_ratio))

  # Clamp zoom
  zoom <- max(0.30, min(4.00, zoom))

  if (!is.null(static_assets$cuenca)) {
    cuenca <- static_assets$cuenca
  } else {
    cuenca <- cant[cant$CANTON == "CUENCA", ]
  }

  if (nrow(cuenca) == 1 && !is.null(static_assets$cuenca_centroid)) {
    cx <- static_assets$cuenca_centroid[1]
    cy <- static_assets$cuenca_centroid[2]
  } else if (nrow(cuenca) == 1) {
    cen <- terra::centroids(cuenca)
    xy <- terra::crds(cen)[1, ]
    cx <- xy[1]
    cy <- xy[2]
  } else {
    # fallback: centro del extent original
    cx <- (terra::xmin(e0) + terra::xmax(e0)) / 2
    cy <- (terra::ymin(e0) + terra::ymax(e0)) / 2
  }

  # tamaño base tomado del raster/extent entregado (lo que viene del crop/projection)
  xr0 <- terra::xmax(e0) - terra::xmin(e0)
  yr0 <- terra::ymax(e0) - terra::ymin(e0)

  # aplicar zoom: zoom>1 reduce ventana, zoom<1 amplía
  xrZ <- xr0 / zoom
  yrZ <- yr0 / zoom

  # Igualar aspecto del extent al del dispositivo para evitar bandas laterales.
  dev_aspect <- 2000 / 2000
  ext_aspect <- xrZ / yrZ
  if (ext_aspect < dev_aspect) {
    xrZ <- yrZ * dev_aspect
  } else if (ext_aspect > dev_aspect) {
    yrZ <- xrZ / dev_aspect
  }

  # asimetría vertical (más sur que norte)
  y_min <- cy - view_down_ratio * yrZ
  y_max <- cy + view_up_ratio * yrZ

  # por defecto el ancho queda centrado en cx
  x_min <- cx - 0.50 * xrZ
  x_max <- cx + 0.50 * xrZ

  # pan (desplazamiento) en unidades del encuadre final
  dx <- pan_x_ratio * (x_max - x_min)
  dy <- pan_y_ratio * (y_max - y_min)

  e_plot <- terra::ext(x_min + dx, x_max + dx, y_min + dy, y_max + dy)

  # Helpers geométricos del encuadre final
  xmn <- terra::xmin(e_plot)
  xmx <- terra::xmax(e_plot)
  ymn <- terra::ymin(e_plot)
  ymx <- terra::ymax(e_plot)
  xr <- xmx - xmn
  yr <- ymx - ymn

  # ------------------------------------------------------------
  # Plot base (usar ext = e_plot)
  # ------------------------------------------------------------
  terra::plot(
    layer,
    col = cols,
    breaks = brks,
    mar = c(0, 0, 0, 0),
    axes = FALSE,
    legend = FALSE,
    asp = 1,
    buffer = FALSE,
    ext = e_plot
  )

  # ...existing code...

  # ------------------------------------------------------------
  # Límites país y provincia
  # ------------------------------------------------------------
  terra::lines(ecu, col = "white", lwd = 4, lty = 1)

  # ------------------------------------------------------------
  # Cantones / Cuenca destacado
  # ------------------------------------------------------------
  if (nrow(cuenca) == 1) {
    terra::lines(cuenca, col = "white", lwd = 9)
    terra::lines(cuenca, col = "black", lwd = 5)

    xy <- terra::crds(terra::centroids(cuenca))[1, ]

    label_txt <- "CUENCA"
    offs <- rbind(
      c(-1, 0),
      c(1, 0),
      c(0, -1),
      c(0, 1),
      c(-1, -1),
      c(-1, 1),
      c(1, -1),
      c(1, 1)
    )
    hx <- 0.0022 * xr
    hy <- 0.0022 * yr

    for (i in seq_len(nrow(offs))) {
      text(
        xy[1] + offs[i, 1] * hx,
        xy[2] + offs[i, 2] * hy,
        labels = label_txt,
        col = "black",
        cex = cex_cuenca,
        font = 2
      )
    }
    text(
      xy[1],
      xy[2],
      labels = label_txt,
      col = "white",
      cex = cex_cuenca,
      font = 2
    )
  }

  # ------------------------------------------------------------
  # Barra superior
  # ------------------------------------------------------------
  title_txt <- "Nubosidad con potencial de tormentas (GOES-19)"

  t_utc <- terra::time(layer)[1]
  # lab_utc se elimina si no se usa
  lab_ec <- format(t_utc, "%Y-%m-%d %H:%M", tz = "America/Guayaquil")

  bar_h <- bar_height_ratio * yr
  y1 <- ymx
  y0 <- y1 - bar_h

  rect(xmn, y0, xmx, y1, col = "black", border = NA)

  pad_x_u <- pad_x * xr

  p_left <- 0.28
  p_mid <- 0.42
  # p_right se elimina si no se usa

  xL0 <- xmn
  xL1 <- xL0 + p_left * xr
  xM0 <- xL1
  xM1 <- xM0 + p_mid * xr
  # xR0 y xR1 se eliminan si no se usan

  # LOGO
  if (!is.null(static_assets$logo_img) && !is.null(static_assets$logo_ratio)) {
    logo_img <- static_assets$logo_img
    logo_ratio <- static_assets$logo_ratio
  } else {
    logo_path <- here::here("assets", "img", "logos", "Log_Etapa.png")
    logo_img <- png::readPNG(logo_path)
    logo_ratio <- dim(logo_img)[2] / dim(logo_img)[1]
  }

  cell_w_left <- (xL1 - xL0)
  cell_h <- (y1 - y0)

  logo_scale <- 1.50
  pad_y_logo <- 0.06 * bar_h

  logo_h_base <- cell_h - 2 * pad_y_logo
  logo_h <- min(logo_h_base * logo_scale, cell_h * 0.98)
  logo_w <- min(logo_h * logo_ratio, cell_w_left - 2 * pad_x_u)

  lx0 <- xL0 + pad_x_u
  lx1 <- lx0 + logo_w
  ly0 <- y0 + (cell_h - logo_h) / 2
  ly1 <- ly0 + logo_h

  rasterImage(logo_img, lx0, ly0, lx1, ly1, interpolate = FALSE)

  # TÍTULO (halo)
  if (!is.null(static_assets$title_txt2)) {
    title_txt2 <- static_assets$title_txt2
  } else {
    words <- strsplit(title_txt, "\\s+")[[1]]
    line1 <- ""
    line2 <- ""
    for (w in words) {
      if (nchar(line1) + nchar(w) + 1 <= 34) {
        line1 <- trimws(paste(line1, w))
      } else {
        line2 <- trimws(paste(line2, w))
      }
    }
    title_txt2 <- if (nzchar(line2)) paste0(line1, "\n", line2) else line1
  }

  hx_t <- 0.0016 * xr
  hy_t <- 0.0016 * yr
  offs_t <- rbind(
    c(-1, 0),
    c(1, 0),
    c(0, -1),
    c(0, 1),
    c(-1, -1),
    c(-1, 1),
    c(1, -1),
    c(1, 1)
  )

  x_title <- (xM0 + xM1) / 2
  y_title <- y0 + 0.55 * bar_h

  for (i in seq_len(nrow(offs_t))) {
    text(
      x_title + offs_t[i, 1] * hx_t,
      y_title + offs_t[i, 2] * hy_t,
      labels = title_txt2,
      col = "black",
      cex = cex_title,
      font = 2,
      adj = c(0.5, 0.5)
    )
  }
  text(
    x_title,
    y_title,
    labels = title_txt2,
    col = "white",
    cex = cex_title,
    font = 2,
    adj = c(0.5, 0.5)
  )

  # Hora Ecuador alineada a derecha
  line_ec <- paste0(lab_ec, "  [Hora Ecuador]")
  par(xpd = NA)

  w_in <- strwidth(line_ec, units = "inches", cex = cex_time, font = 2)
  w_ndc <- w_in / par("pin")[1]
  right_margin_ndc <- 0.008
  x_left_ndc <- 1 - right_margin_ndc - w_ndc

  y0_ndc <- grconvertY(y0, from = "user", to = "ndc")
  y1_ndc <- grconvertY(y1, from = "user", to = "ndc")
  y_mid_ndc <- y0_ndc + 0.50 * (y1_ndc - y0_ndc)

  x_left_user <- grconvertX(x_left_ndc, from = "ndc", to = "user")
  y_mid_user <- grconvertY(y_mid_ndc, from = "ndc", to = "user")

  text(
    x = x_left_user,
    y = y_mid_user,
    labels = line_ec,
    col = "white",
    cex = cex_time,
    font = 2,
    adj = c(0, 0.5)
  )

  # ------------------------------------------------------------
  # Leyenda inferior
  # ------------------------------------------------------------
  par(xpd = NA)

  draw_inset_legend_c13_acrylic_base(
    e = e_plot,
    cols = cols,
    brks = brks,
    margin = 0.010,
    bar_w_ratio = 0.30,
    side_pad_ratio = 0.05,
    box_h = 0.125,
    extra_gap_ratio = 0.016
  )

  invisible(out_file)
}
