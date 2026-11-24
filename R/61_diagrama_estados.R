# 61_diagrama_estados.R
#
# Diagram of the five smoking states and the transitions between them (online supplemental
# figure S1).
# Usage:  Rscript R/61_diagrama_estados.R      (English)
#         Rscript R/61_diagrama_estados.R es   (Spanish)

library(here)
library(data.table)
library(ggplot2)

here::i_am("R/61_diagrama_estados.R")

ARGS   <- commandArgs(trailingOnly = TRUE)
IDIOMA <- if (length(ARGS) >= 1L && ARGS[1] %in% c("en", "es")) ARGS[1] else "en"

if (IDIOMA == "en") {
  fig_dir  <- here::here("paper", "TC", "figures")
  FORMATOS <- c("pdf", "tiff", "png")
  NOMBRE   <- "figureS1_model_states"
} else {
  fig_dir  <- here::here("informe", "figuras")
  FORMATOS <- c("svg", "png300", "pdf")
  NOMBRE   <- "figura0_estados_modelo"
  if (!requireNamespace("svglite", quietly = TRUE))
    stop("falta el paquete svglite.")
}
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

log_con <- file(here::here("logs", sprintf("61_diagrama_estados_%s.log", IDIOMA)),
                open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 61_diagrama_estados.R (idioma = %s) -- %s ===", IDIOMA, format(Sys.time()))

es <- IDIOMA == "es"

ESTADOS <- data.table(
  id  = 1:5,
  x   = c(5.0, 2.2, 7.8, 2.2, 7.8),
  y   = c(8.6, 5.9, 5.9, 2.6, 2.6),
  eti = if (es) c("1\nNunca fumó", "2\nFumador\nocasional", "3\nFumador\ndiario",
                  "4\nExfumador\nreciente\n(<1 año)", "5\nExfumador\nconsolidado\n(≥1 año)")
        else   c("1\nNever\nsmoked", "2\nSmokes\noccasionally", "3\nSmokes\ndaily",
                 "4\nQuit\n<1 year ago", "5\nQuit\n≥1 year ago")
)
ANCHO <- 2.3
ALTO  <- 1.5

TR <- data.table(
  desde = c(1L, 2L, 3L, 2L, 4L, 3L, 4L, 4L, 5L, 5L),
  hasta = c(2L, 3L, 2L, 4L, 2L, 4L, 3L, 5L, 3L, 2L),
  curv  = c(0.00, -0.30, -0.30, -0.30, -0.30, -0.13, -0.13, 0.00, 0.00, 0.00),
  eti   = if (es) c("iniciación", "escalada", "descenso",
                    "cesación", "recaída", "cesación", "recaída",
                    "≥1 año sin fumar", "recaída", "recaída")
          else   c("initiation", "escalation", "downshift",
                   "cessation", "relapse", "cessation", "relapse",
                   "≥1 year abstinent", "relapse", "relapse"),
  tt = c(0.50, 0.50, 0.50, 0.50, 0.50, 0.68, 0.68, 0.50, 0.50, 0.82),
  dx = c(0.00, 0.00, 0.00, 0.60, -0.60, 0.10, -0.10, 0.00, 0.46, 0.00),
  dy = c(0.00, 0.56, -0.56, 0.00, 0.00, -0.40, 0.40, 0.00, 0.00, 0.00),
  tipo = c("est", "cal", "cal", "est", "est", "est", "est", "det", "est", "est")
)
TIPOS <- if (es) c(est = "Tasa estimada", cal = "Tasa calibrada", det = "Paso determinístico") else
                 c(est = "Estimated rate", cal = "Calibrated rate", det = "Deterministic step")

borde <- function(x0, y0, x1, y1, w = ANCHO / 2 + 0.18, h = ALTO / 2 + 0.18) {
  dx <- x1 - x0; dy <- y1 - y0
  if (dx == 0 && dy == 0) return(c(x0, y0))
  t <- min(if (dx != 0) abs(w / dx) else Inf, if (dy != 0) abs(h / dy) else Inf)
  c(x0 + t * dx, y0 + t * dy)
}

seg <- rbindlist(lapply(seq_len(nrow(TR)), function(i) {
  a <- ESTADOS[id == TR$desde[i]]; b <- ESTADOS[id == TR$hasta[i]]
  p0 <- borde(a$x, a$y, b$x, b$y)
  p1 <- borde(b$x, b$y, a$x, a$y)
  data.table(x = p0[1], y = p0[2], xend = p1[1], yend = p1[2],
             curv = TR$curv[i], eti = TR$eti[i],
             tipo = factor(TIPOS[TR$tipo[i]], levels = TIPOS),
             lx = p0[1] + TR$tt[i] * (p1[1] - p0[1]) + TR$dx[i],
             ly = p0[2] + TR$tt[i] * (p1[2] - p0[2]) + TR$dy[i])
}))

FLECHA <- arrow(length = unit(0.16, "cm"), type = "closed")
FAM <- "serif"

p <- ggplot() +
  geom_tile(data = ESTADOS, aes(x, y), width = ANCHO, height = ALTO,
            fill = "grey96", colour = "grey30", linewidth = 0.35)

for (cv in unique(seg$curv)) {
  p <- p + geom_curve(data = seg[curv == cv],
                      aes(x = x, y = y, xend = xend, yend = yend, colour = tipo),
                      curvature = cv, arrow = FLECHA, linewidth = 0.45)
}
p <- p + scale_colour_manual(values = setNames(c("grey15", "#2166AC", "grey60"), TIPOS),
                             drop = FALSE, name = NULL)

p <- p +
  geom_text(data = ESTADOS, aes(x, y, label = eti), size = 2.8, family = FAM, lineheight = 0.95) +
  geom_label(data = seg[eti != ""], aes(lx, ly, label = eti), size = 2.5, family = FAM,
             label.size = 0, label.padding = unit(0.06, "lines"),
             fill = "white", colour = "grey15") +
  coord_equal(xlim = c(0.2, 9.8), ylim = c(1.5, 9.6), expand = FALSE) +
  theme_void(base_size = 9, base_family = FAM)

p <- p + theme(plot.margin = margin(4, 4, 4, 4),
               legend.position = "bottom",
               legend.text = element_text(size = 7.5),
               legend.key.width = unit(1.1, "cm"),
               legend.margin = margin(0, 0, 0, 0))

guardar <- function(p, nombre, ancho_mm, alto_mm) {
  pulg_w <- ancho_mm / 25.4; pulg_h <- alto_mm / 25.4
  if ("pdf" %in% FORMATOS)
    ggsave(file.path(fig_dir, paste0(nombre, ".pdf")), p, width = ancho_mm,
           height = alto_mm, units = "mm", device = cairo_pdf)
  if ("tiff" %in% FORMATOS)
    ggsave(file.path(fig_dir, paste0(nombre, ".tiff")), p, width = ancho_mm,
           height = alto_mm, units = "mm", dpi = 300, compression = "lzw")
  if ("svg" %in% FORMATOS) {
    svglite::svglite(file.path(fig_dir, paste0(nombre, ".svg")), width = pulg_w, height = pulg_h)
    print(p); dev.off()
  }
  if ("png300" %in% FORMATOS)
    ggsave(file.path(fig_dir, paste0(nombre, ".png")), p, width = ancho_mm,
           height = alto_mm, units = "mm", dpi = 300)
  if ("png" %in% FORMATOS) {
    dir.create(file.path(fig_dir, "preview"), showWarnings = FALSE)
    ggsave(file.path(fig_dir, "preview", paste0(nombre, ".png")), p, width = ancho_mm,
           height = alto_mm, units = "mm", dpi = 200)
  }
  log_msg("  %s  (%d x %d mm)  ->  %s", nombre, ancho_mm, alto_mm, paste(FORMATOS, collapse = " "))
}

guardar(p, NOMBRE, 150, 130)
log_msg("\n=== output en %s ===", fig_dir)
close(log_con)
