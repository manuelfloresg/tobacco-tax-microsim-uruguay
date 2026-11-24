# 55_figuras_articulo.R
#
# Figures of the article (English) and of the working paper (Spanish). Does not run the model.
# Usage:  Rscript R/55_figuras_articulo.R      (article: paper/TC/figures/)
#         Rscript R/55_figuras_articulo.R es   (working paper: informe/figuras/)
# Input:  output/tables/59_*, 60_*, 77_*, 79_*, 88_*, 95_*
# Output: figures in PDF, TIFF, PNG and SVG, and numeros_graficados.csv with every plotted value

library(here)
library(data.table)
library(ggplot2)
library(patchwork)

here::i_am("R/55_figuras_articulo.R")

ARGS   <- commandArgs(trailingOnly = TRUE)
IDIOMA <- if (length(ARGS) >= 1L && ARGS[1] %in% c("en", "es")) ARGS[1] else "en"

if (IDIOMA == "en") {
  fig_dir  <- here::here("paper", "TC", "figures")
  FORMATOS <- c("pdf", "tiff", "png")
} else {
  fig_dir  <- here::here("informe", "figuras")
  FORMATOS <- c("svg", "png300", "pdf")
  if (!requireNamespace("svglite", quietly = TRUE))
    stop("falta el paquete svglite, necesario para emitir las figuras del informe en SVG.")
}

log_con <- file(here::here("logs", sprintf("55_figuras_articulo_%s.log", IDIOMA)),
                open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 55_figuras_articulo.R (idioma = %s) -- %s ===", IDIOMA, format(Sys.time()))

tab_dir <- here::here("output", "tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

ESC_PRINCIPAL <- "conv75_gradual"
ANIOS_BANDA   <- c(2030L, 2045L, 2060L)

leer <- function(f) {
  p <- file.path(tab_dir, f)
  if (!file.exists(p)) stop("falta ", f, ": correr el script que lo produce antes que este.")
  fread(p)
}

DIC <- list(
  rama_base   = c(en = "Status quo",                  es = "Statu quo"),
  rama_g75    = c(en = "WHO 75%, gradual",            es = "OMS 75%, gradual"),
  rama_a75    = c(en = "WHO 75%, abrupt",             es = "OMS 75%, abrupto"),
  rama_gEU    = c(en = "EU average 81.55%, gradual",  es = "Promedio UE 81,55%, gradual"),
  y_consumo   = c(en = "Cigarette consumption\n(millions of sticks)",
                  es = "Consumo de cigarrillos\n(millones de unidades)"),
  y_prev      = c(en = "Adult smoking prevalence\n(%, aged 15+)",
                  es = "Prevalencia de tabaquismo\n(%, 15 años y más)"),
  y_aporte    = c(en = "Contribution to the fall in\nconsumption (log points)",
                  es = "Aporte a la caída del\nconsumo (puntos log)"),
  y_share_ext = c(en = "Extensive margin as a share of\nthe total fall (%)",
                  es = "Margen extensivo como proporción\nde la caída total (%)"),
  y_prev_ter  = c(en = "Smoking prevalence (%, aged 15+)",
                  es = "Prevalencia de tabaquismo (%, 15 años y más)"),
  y_brecha    = c(en = "T1 − T3 prevalence gap (pp)",
                  es = "Brecha de prevalencia T1 − T3 (pp)"),
  y_idx_prec  = c(en = "Index (2003 = 100)", es = "Índice (2003 = 100)"),
  y_idx_vol   = c(en = "Index (2003 = 100 DGI;\n2006 = 100 prevalence)",
                  es = "Índice (2003 = 100 DGI;\n2006 = 100 prevalencia)"),
  marg_ext    = c(en = "Extensive", es = "Extensivo (cesación + no inicio)"),
  comp_ces    = c(en = "Cessation", es = "Cesación"),
  comp_ini    = c(en = "Avoided initiation", es = "Iniciación evitada"),
  y_comp_ext  = c(en = "Share of the extensive margin (%)",
                  es = "Participación en el margen extensivo (%)"),
  marg_int    = c(en = "Intensive", es = "Intensivo (menos cigarrillos)"),
  ter_1       = c(en = "T1 (lowest income)",  es = "T1 (ingreso más bajo)"),
  ter_2       = c(en = "T2",                  es = "T2"),
  ter_3       = c(en = "T3 (highest income)", es = "T3 (ingreso más alto)"),
  ev_fctc     = c(en = "FCTC",               es = "CMCT"),
  ev_decreto  = c(en = "Excise decree",      es = "Decreto IMESI"),
  ev_iva      = c(en = "VAT 22%",            es = "IVA 22%"),
  ev_ley      = c(en = "Law 18,256",         es = "Ley 18.256"),
  ev_unif     = c(en = "Excise unified 70%", es = "IMESI unificado 70%"),
  ev_suba     = c(en = "Excise hike Feb-10", es = "Suba IMESI feb-10"),
  s_precio    = c(en = "Real price",              es = "Precio real"),
  s_imesi     = c(en = "Real tax per pack",       es = "IMESI real"),
  s_dgi       = c(en = "Legal sales (DGI)",       es = "Ventas legales (DGI)"),
  s_ech       = c(en = "Survey prevalence (ECH)", es = "Prevalencia ECH"),
  s_jnd       = c(en = "Survey prevalence (JND)", es = "Prevalencia JND"),
  cap_s1      = c(en = paste("Uruguay 2001-2025. Dashed vertical lines mark policy breaks:",
                             "FCTC ratification (2004), excise decrees (2005), VAT (2007),",
                             "Law 18,256 (2008), excise unification (Jul 2009) and a further",
                             "hike (Feb 2010)."),
                  es = paste("Uruguay 2001-2025. Las líneas verticales punteadas marcan los",
                             "quiebres de política: ratificación del CMCT (2004), decretos de",
                             "IMESI (2005), IVA (2007), Ley 18.256 (2008), unificación del",
                             "IMESI (jul-2009) y una suba adicional (feb-2010)."))
)

L <- function(clave) {
  v <- DIC[[clave]]
  if (is.null(v)) stop("falta la clave de traduccion: ", clave)
  unname(v[[IDIOMA]])
}

NOM <- if (IDIOMA == "en") {
  c(f1 = "figure1_consumption_prevalence", f2 = "figure2_extensive_intensive",
    f2b = "figure2b_extensive_composition",
    f3 = "figure3_tertile_prevalence",     fS1 = "figureS2_historical_validation")
} else {
  c(f1 = "figura1_consumo_prevalencia",    f2 = "figura2_extensivo_intensivo",
    f2b = "figura2b_composicion_extensivo",
    f3 = "figura3_prevalencia_tercil",     fS1 = "figuraA1_validacion_historica")
}

ETIQ_RAMA <- c(base = L("rama_base"), conv75_gradual = L("rama_g75"),
               conv75_abrupto = L("rama_a75"), convEU_gradual = L("rama_gEU"))
ORDEN_RAMA <- names(ETIQ_RAMA)

PAL_TC <- list(
  rama = c("#3B3B3B", "#0072B2", "#56B4E9", "#D55E00"),
  ter  = c("#7F2704", "#D94801", "#FD9A4E"),
  int  = "#9ECAE1", ces = "#1B7A44", ini = "#8FD19E"
)
COL_RAMA <- setNames(if (IDIOMA == "en") PAL_TC$rama else c("#000000", "#0072B2", "#D55E00", "#009E73"),
                     unname(ETIQ_RAMA))
LTY_RAMA <- setNames(c("solid", "longdash", "dotted", "dotdash"),   unname(ETIQ_RAMA))
SHP_RAMA <- setNames(c(16, 17, 15, 4),                              unname(ETIQ_RAMA))

tema <- theme_bw(base_size = 9, base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, colour = "grey88"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(1.1, "cm"),
    legend.margin = margin(t = -2),
    strip.background = element_rect(fill = "grey94", colour = NA),
    plot.tag = element_text(face = "bold", size = 10)
  )
if (IDIOMA == "en") tema <- tema + theme(panel.grid.major.x = element_blank())
tema_barras <- if (IDIOMA == "en") tema + theme(panel.grid.major = element_blank()) else tema

guardar <- function(p, nombre, ancho_mm, alto_mm) {
  pulg_w <- ancho_mm / 25.4
  pulg_h <- alto_mm / 25.4
  if ("pdf" %in% FORMATOS)
    ggsave(file.path(fig_dir, paste0(nombre, ".pdf")), p,
           width = ancho_mm, height = alto_mm, units = "mm", device = cairo_pdf)
  if ("tiff" %in% FORMATOS)
    ggsave(file.path(fig_dir, paste0(nombre, ".tiff")), p,
           width = ancho_mm, height = alto_mm, units = "mm", dpi = 300,
           compression = "lzw")
  if ("svg" %in% FORMATOS) {
    svglite::svglite(file.path(fig_dir, paste0(nombre, ".svg")),
                     width = pulg_w, height = pulg_h)
    print(p); dev.off()
  }
  if ("png300" %in% FORMATOS) {
    ggsave(file.path(fig_dir, paste0(nombre, ".png")), p,
           width = ancho_mm, height = alto_mm, units = "mm", dpi = 300)
  }
  if ("png" %in% FORMATOS) {
    dir.create(file.path(fig_dir, "preview"), showWarnings = FALSE)
    ggsave(file.path(fig_dir, "preview", paste0(nombre, ".png")), p,
           width = ancho_mm, height = alto_mm, units = "mm", dpi = 200)
  }
  log_msg("  %s  (%d x %d mm)  ->  %s", nombre, ancho_mm, alto_mm,
          paste(FORMATOS, collapse = " "))
}

graficadas <- list()
anotar_serie <- function(figura, d) {
  graficadas[[length(graficadas) + 1L]] <<- cbind(figura = figura, as.data.table(d))
  invisible(NULL)
}

log_msg("\n--- Figure 1 ---")

agg <- leer("59_espacio_escenarios_agregado.csv")
if (IDIOMA == "en") agg <- agg[escenario != "convEU_gradual"]
if (IDIOMA == "en") {
  anual <- leer("95_tc_anual_por_semilla.csv")[variante %in% c("principal", "abrupto") &
                                                 semilla == 20260809L]
  anual <- rbind(
    anual[variante == "principal" & rama == "base", .(escenario = "base", anio, consumo_mill, prevalencia)],
    anual[variante == "principal" & rama == "esc", .(escenario = "conv75_gradual", anio, consumo_mill, prevalencia)],
    anual[variante == "abrupto" & rama == "esc", .(escenario = "conv75_abrupto", anio, consumo_mill, prevalencia)])
  chk <- merge(anual, agg[, .(escenario, anio, c5 = consumo_mill, p5 = prevalencia)],
               by = c("escenario", "anio"))
  if (nrow(chk) != nrow(agg) || chk[, max(abs(consumo_mill - c5), abs(prevalencia - p5))] > 1e-9)
    stop("La serie anual de R/78 no reproduce 59_espacio_escenarios_agregado.csv")
  agg <- anual
}
agg[, rama := factor(ETIQ_RAMA[escenario], levels = unname(ETIQ_RAMA[ORDEN_RAMA]))]
agg[, prevalencia_pct := 100 * prevalencia]

bandas <- leer("77_mc_bandas.csv")[fuente == "capa 1 + prior del hazard"]
nivel_base <- agg[escenario == "base", .(anio, consumo_base = consumo_mill,
                                         prev_base = prevalencia_pct)]
b_cons <- merge(bandas[resultado == "caida_consumo_pct", .(anio, lo = p2.5, hi = p97.5)],
                nivel_base, by = "anio")
b_cons[, `:=`(y_lo = consumo_base * (1 + lo / 100), y_hi = consumo_base * (1 + hi / 100))]
b_prev <- merge(bandas[resultado == "caida_prevalencia_pct", .(anio, lo = p2.5, hi = p97.5)],
                nivel_base, by = "anio")
b_prev[, `:=`(y_lo = prev_base * (1 + lo / 100), y_hi = prev_base * (1 + hi / 100))]

panel_serie <- function(col_y, etiqueta_y, banda) {
  puntos <- if (IDIOMA == "en") agg[anio %% 5L == 0L] else agg
  sombra <- if (IDIOMA == "en")
    list(annotate("rect", xmin = 2027, xmax = 2031, ymin = -Inf, ymax = Inf, fill = "grey88"),
         annotate("text", x = 2031.4, y = -Inf, label = "Tax increase\n(gradual scenario)",
                  hjust = 0, vjust = -0.3, size = 2.6, family = "serif", lineheight = 0.9,
                  colour = "grey35")) else NULL
  ggplot(agg, aes(anio, .data[[col_y]], colour = rama, linetype = rama, shape = rama)) +
    sombra +
    geom_line(linewidth = 0.5) +
    geom_point(data = puntos, size = 1.4) +
    geom_errorbar(data = banda, inherit.aes = FALSE,
                  aes(x = anio, ymin = y_lo, ymax = y_hi),
                  width = 1.6, linewidth = 0.35, colour = COL_RAMA[[2]]) +
    scale_colour_manual(values = COL_RAMA) +
    scale_linetype_manual(values = LTY_RAMA) +
    scale_shape_manual(values = SHP_RAMA) +
    scale_x_continuous(breaks = seq(2025, 2060, 10)) +
    labs(x = NULL, y = etiqueta_y) +
    tema
}

f1a <- panel_serie("consumo_mill", L("y_consumo"), b_cons)
f1b <- panel_serie("prevalencia_pct", L("y_prev"), b_prev)
if (IDIOMA == "en") {
  f1a <- f1a + scale_x_continuous(breaks = seq(2025, 2060, 5))
  f1b <- f1b + scale_x_continuous(breaks = seq(2025, 2060, 5))
  fig1 <- (f1a / f1b) + plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(legend.position = "bottom")
  guardar(fig1, NOM[["f1"]], 170, 170)
} else {
  fig1 <- (f1a + f1b) + plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(legend.position = "bottom")
  guardar(fig1, NOM[["f1"]], 180, 90)
}
anotar_serie("Figure 1", agg[, .(escenario, anio, consumo_mill, prevalencia_pct)])
anotar_serie("Figure 1 error bars, consumption", b_cons[, .(anio, y_lo, y_hi)])
anotar_serie("Figure 1 error bars, prevalence", b_prev[, .(anio, y_lo, y_hi)])

log_msg("\n--- Figure 2 ---")

cmp <- leer("59_espacio_escenarios_comparado.csv")[escenario == ESC_PRINCIPAL & anio > 2025L]
mar <- melt(cmp[, .(anio, EXT = aporte_extensivo, INT = aporte_intensivo)],
            id.vars = "anio", variable.name = "margen", value.name = "aporte_log")
mar[, reduccion_pct := -100 * aporte_log]
mar[, margen := factor(margen, levels = c("INT", "EXT"),
                       labels = c(L("marg_int"), L("marg_ext")))]

cmp[, share_ext_pct := 100 * aporte_extensivo / d_ln_consumo]

f2a <- ggplot(mar, aes(anio, reduccion_pct, fill = margen)) +
  geom_col(width = 3.4, colour = "grey25", linewidth = 0.2) +
  scale_fill_manual(values = setNames(c("grey78", "grey32"),
                                      c(L("marg_int"), L("marg_ext")))) +
  scale_x_continuous(breaks = seq(2030, 2060, 10)) +
  labs(x = NULL, y = L("y_aporte")) +
  tema

f2b <- ggplot(cmp, aes(anio, share_ext_pct)) +
  geom_line(linewidth = 0.5, colour = "grey20") +
  geom_point(size = 1.5, shape = 16, colour = "grey20") +
  scale_x_continuous(breaks = seq(2030, 2060, 10)) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = NULL, y = L("y_share_ext")) +
  tema

if (IDIOMA == "es") {
  fig2 <- (f2a + f2b) + plot_annotation(tag_levels = "A")
  guardar(fig2, NOM[["f2"]], 180, 80)
  anotar_serie("Figure 2", mar[, .(anio, margen, reduccion_pct)])
  anotar_serie("Figure 2 share", cmp[, .(anio, share_ext_pct)])
} else {
  desc0 <- leer("88_descomposicion_margen_extensivo.csv")[, .(anio, pct_cesacion, pct_iniciacion)]
  mec <- merge(cmp[, .(anio, d_ln_consumo, aporte_extensivo, aporte_intensivo)], desc0, by = "anio")
  chk <- mec[, max(abs(aporte_extensivo + aporte_intensivo - d_ln_consumo) / abs(d_ln_consumo))]
  if (chk > 0.02) stop("los aportes por margen se apartan del cambio total en ", chk)
  mec[, total_pct := -100 * (exp(d_ln_consumo) - 1)]
  mec[, `:=`(INT = total_pct * aporte_intensivo / (aporte_intensivo + aporte_extensivo),
             EXT = total_pct * aporte_extensivo / (aporte_intensivo + aporte_extensivo))]
  mec[, `:=`(CES = EXT * pct_cesacion / 100, INI = EXT * pct_iniciacion / 100)]
  mec_l <- melt(mec[, .(anio, total_pct, INT, CES, INI)], id.vars = c("anio", "total_pct"),
                variable.name = "mecanismo", value.name = "pct")
  ETQ_MEC <- c(INT = "Fewer cigarettes per day (intensive)",
               CES = "Quitting (extensive)", INI = "Not starting (extensive)")
  mec_l[, mecanismo := factor(ETQ_MEC[as.character(mecanismo)], levels = rev(unname(ETQ_MEC)))]
  mec_l[, share := 100 * pct / total_pct]
  COL_MEC <- setNames(c(PAL_TC$int, PAL_TC$ces, PAL_TC$ini), unname(ETQ_MEC))

  barras <- function(col_y, etiqueta_y) {
    ggplot(mec_l, aes(anio, .data[[col_y]], fill = mecanismo)) +
      geom_col(width = 3.4, colour = "grey20", linewidth = 0.2) +
      scale_fill_manual(values = COL_MEC, breaks = unname(ETQ_MEC)) +
      scale_x_continuous(breaks = seq(2030, 2060, 10)) +
      labs(x = NULL, y = etiqueta_y) +
      tema_barras
  }
  f2a_tc <- barras("pct", "Fall in cigarette consumption\nrelative to the status quo (%)") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.04)))
  f2b_tc <- barras("share", "Share of the fall in consumption (%)") +
    scale_y_continuous(breaks = seq(0, 100, 25), expand = c(0, 0)) +
    coord_cartesian(ylim = c(0, 100))
  fig2 <- (f2a_tc + f2b_tc) + plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")
  guardar(fig2, NOM[["f2"]], 180, 88)
  anotar_serie("Figure 2", mec_l[, .(anio, mecanismo, pct, share)])
  log_msg("  Figura 2 (TC): 2060 intensivo %.1f%%, cesacion %.1f%%, no inicio %.1f%% de la caida",
          mec_l[anio == 2060 & grepl("intensive", mecanismo), share],
          mec_l[anio == 2060 & grepl("Quitting", mecanismo), share],
          mec_l[anio == 2060 & grepl("starting", mecanismo), share])
}

if (IDIOMA != "en") {
log_msg("
--- Figure 2b ---")

desc <- leer("88_descomposicion_margen_extensivo.csv")
comp <- melt(desc[, .(anio, ces = pct_cesacion, ini = pct_iniciacion)],
             id.vars = "anio", variable.name = "mecanismo", value.name = "pct")
comp[, mecanismo := factor(mecanismo, levels = c("ini", "ces"),
                           labels = c(L("comp_ini"), L("comp_ces")))]

fig2b <- ggplot(comp, aes(anio, pct, fill = mecanismo)) +
  geom_col(width = 3.4, colour = "grey25", linewidth = 0.2) +
  geom_hline(yintercept = 50, linetype = "22", linewidth = 0.3, colour = "grey45") +
  scale_fill_manual(values = setNames(if (IDIOMA == "en") c(PAL_TC$ini, PAL_TC$ces) else c("grey78", "grey32"),
                                      c(L("comp_ini"), L("comp_ces")))) +
  scale_x_continuous(breaks = seq(2030, 2060, 5)) +
  scale_y_continuous(breaks = seq(0, 100, 25), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = NULL, y = L("y_comp_ext")) +
  tema
guardar(fig2b, NOM[["f2b"]], 150, 80)
anotar_serie("Figure 2b", comp[, .(anio, mecanismo, pct)])
}

log_msg("\n--- Figure 3 ---")

ram <- leer("79_razon_prevalencia_ramas.csv")[escenario %in% c("base", ESC_PRINCIPAL)]
ram[, rama := factor(fifelse(escenario == "base", L("rama_base"), L("rama_g75")),
                     levels = c(L("rama_base"), L("rama_g75")))]

ter <- melt(ram, id.vars = c("rama", "anio"),
            measure.vars = c("prev_T1", "prev_T2", "prev_T3"),
            variable.name = "tercil", value.name = "prev")
ter[, prev_pct := 100 * prev]
ter[, tercil := factor(tercil, levels = c("prev_T1", "prev_T2", "prev_T3"),
                       labels = c(L("ter_1"), L("ter_2"), L("ter_3")))]

COL_T  <- setNames(if (IDIOMA == "en") PAL_TC$ter else c("#000000", "#0072B2", "#D55E00"),
                   c(L("ter_1"), L("ter_2"), L("ter_3")))
SHP_T  <- setNames(c(16, 17, 15),                      c(L("ter_1"), L("ter_2"), L("ter_3")))
LTY_R2 <- setNames(c("solid", "longdash"),             c(L("rama_base"), L("rama_g75")))
SHP_R2 <- setNames(c(16, 17),                          c(L("rama_base"), L("rama_g75")))

f3a <- ggplot(ter, aes(anio, prev_pct, colour = tercil, shape = tercil, linetype = rama)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.4) +
  scale_colour_manual(values = COL_T) +
  scale_shape_manual(values = SHP_T) +
  scale_linetype_manual(values = LTY_R2) +
  scale_x_continuous(breaks = seq(2025, 2060, 10)) +
  labs(x = NULL, y = L("y_prev_ter")) +
  guides(colour = guide_legend(order = 1, nrow = 1),
         shape  = guide_legend(order = 1, nrow = 1),
         linetype = guide_legend(order = 2, nrow = 1)) +
  tema + theme(legend.position = "bottom", legend.box = "vertical",
               legend.margin = margin(t = 0, b = 0),
               legend.key.width = unit(0.9, "cm"))

COL_R2 <- setNames(if (IDIOMA == "en") PAL_TC$rama[1:2] else c("grey15", "grey15"),
                   c(L("rama_base"), L("rama_g75")))
f3b <- ggplot(ram, aes(anio, brecha_T1_T3_pp, linetype = rama, shape = rama, colour = rama)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = COL_R2) +
  scale_linetype_manual(values = LTY_R2) +
  scale_shape_manual(values = SHP_R2) +
  scale_x_continuous(breaks = seq(2025, 2060, 10)) +
  labs(x = NULL, y = L("y_brecha")) +
  guides(linetype = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)) +
  tema + theme(legend.position = "bottom",
               legend.margin = margin(t = 0, b = 0),
               legend.key.width = unit(0.9, "cm"))

fig3 <- (f3a | f3b) + plot_annotation(tag_levels = "A")
guardar(fig3, NOM[["f3"]], 180, 105)
anotar_serie("Figure 3", ter[, .(rama, anio, tercil, prev_pct)])
anotar_serie("Figure 3 gap", ram[, .(rama, anio, brecha_T1_T3_pp)])

log_msg("\n--- Figure S1 (supplement) ---")

hist_s <- leer("60_validacion_historica_fctc_series.csv")[anio >= 2001 & anio <= 2025]
EVENTOS <- data.table(
  anio = c(2004, 2005, 2007, 2008, 2009, 2010),
  etiqueta = c(L("ev_fctc"), L("ev_decreto"), L("ev_iva"), L("ev_ley"),
               L("ev_unif"), L("ev_suba"))
)

s1_eventos <- ggplot(EVENTOS, aes(x = anio, xend = anio)) +
  geom_vline(aes(xintercept = anio), colour = "grey75", linetype = "dashed", linewidth = 0.3) +
  geom_text(aes(y = 0, label = etiqueta), angle = 90, hjust = 0, vjust = 0.5,
           size = 2.3, family = "serif") +
  scale_x_continuous(breaks = seq(2001, 2025, 4), limits = c(2001, 2025)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_void(base_size = 9) +
  theme(plot.margin = margin(t = 2, r = 4, b = 0, l = 46))

s1a <- ggplot(hist_s, aes(anio)) +
  geom_vline(data = EVENTOS, aes(xintercept = anio), colour = "grey75",
             linetype = "dashed", linewidth = 0.3) +
  geom_line(aes(y = idx_precio, colour = L("s_precio")), linewidth = 0.6) +
  geom_line(aes(y = idx_impuesto, colour = L("s_imesi")), linewidth = 0.6) +
  scale_colour_manual(values = setNames(c("#0072B2", "#D55E00"),
                                        c(L("s_precio"), L("s_imesi")))) +
  scale_x_continuous(breaks = seq(2001, 2025, 4), limits = c(2001, 2025)) +
  labs(x = NULL, y = L("y_idx_prec"), colour = NULL) +
  tema + theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1),
              plot.margin = margin(t = 2, r = 4, b = 4, l = 4))

f_ech <- hist_s[anio == 2006, prev_ech]
f_jnd <- hist_s[anio == 2006, prev_jnd]
hist_s[, `:=`(idx_prev_ech = 100 * prev_ech / f_ech, idx_prev_jnd = 100 * prev_jnd / f_jnd)]
hist_long <- melt(hist_s[, .(anio, DGI = idx_dgi, ECH = idx_prev_ech, JND = idx_prev_jnd)],
                  id.vars = "anio", variable.name = "serie", value.name = "idx")
hist_long[, serie := factor(serie, levels = c("DGI", "ECH", "JND"),
                            labels = c(L("s_dgi"), L("s_ech"), L("s_jnd")))]

s1b <- ggplot(hist_long, aes(anio, idx, colour = serie, shape = serie, linetype = serie)) +
  geom_vline(data = EVENTOS, aes(xintercept = anio), colour = "grey75",
             linetype = "dashed", linewidth = 0.3) +
  geom_line(linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 1.3, na.rm = TRUE) +
  scale_colour_manual(values = setNames(c("#0072B2", "#009E73", "#8e5b2d"),
                                        c(L("s_dgi"), L("s_ech"), L("s_jnd")))) +
  scale_shape_manual(values = setNames(c(NA, 16, 17),
                                       c(L("s_dgi"), L("s_ech"), L("s_jnd")))) +
  scale_linetype_manual(values = setNames(c("solid", "dashed", "dashed"),
                                          c(L("s_dgi"), L("s_ech"), L("s_jnd")))) +
  scale_x_continuous(breaks = seq(2001, 2025, 4)) +
  labs(x = NULL, y = L("y_idx_vol"), colour = NULL, shape = NULL, linetype = NULL) +
  tema + theme(legend.position = c(0.02, 0.02), legend.justification = c(0, 0))

cap_txt <- if (IDIOMA == "es") NULL else paste(strwrap(L("cap_s1"), width = 78), collapse = "\n")

figS1 <- (s1_eventos / s1a / s1b) +
  plot_layout(heights = c(0.5, 1, 1)) +
  plot_annotation(
    tag_levels = list(c("", "A", "B")),
    caption = cap_txt,
    theme = theme(plot.caption = element_text(hjust = 0, size = 7, family = "serif"))
  )
guardar(figS1, NOM[["fS1"]], 150, if (IDIOMA == "es") 175 else 190)
anotar_serie("Figure S1 prices", hist_s[, .(anio, idx_precio, idx_impuesto)])
anotar_serie("Figure S1 volume/prevalence", hist_long[, .(anio, serie, idx)])

gr <- rbindlist(graficadas, fill = TRUE)
fwrite(gr, file.path(fig_dir, "numeros_graficados.csv"))
if (IDIOMA == "en") fwrite(gr, file.path(tab_dir, "83_numeros_graficados.csv"))
log_msg("\nVolcado de series graficadas: %d filas -> %s/numeros_graficados.csv",
        nrow(gr), basename(fig_dir))

log_msg("\n=== outputs en %s: %s ===", fig_dir, paste(NOM, collapse = ", "))
close(log_con)
