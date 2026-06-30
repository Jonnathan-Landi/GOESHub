# -----------------------------
# Paleta C13 (BT en °C)
# -----------------------------
c13_palette <- function() {
  # breaks a 1°C: de -90 a 50
  brks <- seq(-90, 50, by = 1)

  # midpoints para asignar color por intervalo
  mids <- (brks[-1] + brks[-length(brks)]) / 2

  # función auxiliar: interpola colores (RGB) en un tramo [x0,x1]
  interp_hex <- function(x, x0, x1, c0, c1) {
    # x: vector en [x0,x1]
    rgb0 <- grDevices::col2rgb(c0)
    rgb1 <- grDevices::col2rgb(c1)

    t <- (x - x0) / (x1 - x0)
    r <- as.integer(round(rgb0[1, 1] + t * (rgb1[1, 1] - rgb0[1, 1])))
    g <- as.integer(round(rgb0[2, 1] + t * (rgb1[2, 1] - rgb0[2, 1])))
    b <- as.integer(round(rgb0[3, 1] + t * (rgb1[3, 1] - rgb0[3, 1])))

    grDevices::rgb(r, g, b, maxColorValue = 255)
  }

  cols <- character(length(mids))

  # -----------------------------
  # Grises: 50 -> -10 (como referencia use SENAMHI)
  # -----------------------------
  idx <- mids >= -10
  cols[idx] <- interp_hex(
    mids[idx],
    x0 = 50,
    x1 = -10,
    c0 = "#020303",
    c1 = "#807f7f"
  )

  # -----------------------------
  # -10 -> -35: marrón -> naranja -> amarillo
  #    Nota: (2 subtramos para controlar curvatura visual)
  # -----------------------------
  idx1 <- mids < -10 & mids >= -20
  cols[idx1] <- interp_hex(mids[idx1], -10, -20, "#655546", "#a95525")

  idx2 <- mids < -20 & mids >= -30
  cols[idx2] <- interp_hex(mids[idx2], -20, -30, "#a95525", "#f0a627")

  idx3 <- mids < -30 & mids >= -35
  cols[idx3] <- interp_hex(mids[idx3], -30, -35, "#f0a627", "#efc458")

  # -----------------------------
  # -35 -> -55: amarillo/verde
  #    (siguiendo nomenclatura SENAMHI: -35, -40, -45, -50, -55)
  # -----------------------------
  idx4 <- mids < -35 & mids >= -40
  cols[idx4] <- interp_hex(mids[idx4], -35, -40, "#efc458", "#d5d528")

  idx5 <- mids < -40 & mids >= -45
  cols[idx5] <- interp_hex(mids[idx5], -40, -45, "#d5d528", "#f1ed76")

  idx6 <- mids < -45 & mids >= -50
  cols[idx6] <- interp_hex(mids[idx6], -45, -50, "#f1ed76", "#59b948")

  idx7 <- mids < -50 & mids >= -55
  cols[idx7] <- interp_hex(mids[idx7], -50, -55, "#59b948", "#18773c")

  # -----------------------------
  # -55 -> -70: cian/azules
  # -----------------------------
  idx8 <- mids < -55 & mids >= -60
  cols[idx8] <- interp_hex(mids[idx8], -55, -60, "#31b9eb", "#387ec1")

  idx9 <- mids < -60 & mids >= -70
  cols[idx9] <- interp_hex(mids[idx9], -60, -70, "#387ec1", "#233f97")

  # -----------------------------
  #  Fuerzo el salto: a partir de mids <= -70.5 entra rampa roja.
  # -----------------------------
  idx10 <- mids < -70 & mids >= -80
  # arranque rojo (observado ~-71)
  cols[idx10] <- interp_hex(mids[idx10], -71, -80, "#f38375", "#bd1f24")

  # -----------------------------
  # -80 -> -90: rojos -> púrpuras
  # -----------------------------
  idx11 <- mids < -80 & mids >= -85
  cols[idx11] <- interp_hex(mids[idx11], -80, -85, "#bd1f24", "#854c9d")

  idx12 <- mids < -85
  cols[idx12] <- interp_hex(mids[idx12], -85, -90, "#854c9d", "#5f3b96")

  list(cols = cols, brks = brks)
}
