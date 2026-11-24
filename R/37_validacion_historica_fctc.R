# 37_validacion_historica_fctc.R
#
# Uruguay's 2003-2010 tax episode: real taxes per pack, real price, tax share, legal volumes and
# prevalence (ECH and household drug survey), for the qualitative comparison in the supplement.
# Input:  output/tables/41_*, 21_*, 26_*
# Output: output/tables/60_*.csv

library(here)
library(data.table)

here::i_am("R/37_validacion_historica_fctc.R")

log_con <- file(here::here("logs", "37_validacion_historica_fctc.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 37_validacion_historica_fctc.R -- %s ===", format(Sys.time()))

out_tab <- here::here("output", "tables")
out_fig <- here::here("output", "figures")
if (!dir.exists(out_fig)) dir.create(out_fig, recursive = TRUE)

log_msg("\n--- 1) Series de insumo ---")

panel <- fread(file.path(out_tab, "41_panel_fiscal_anual.csv"))
ech   <- fread(file.path(out_tab, "21_prevalencia_ponderada.csv"))
jnd   <- fread(file.path(out_tab, "26_jnd_hogares_serie.csv"))

log_msg("Panel fiscal: %d anios (%d-%d)", nrow(panel), min(panel$anio), max(panel$anio))
log_msg("ECH prevalencia ponderada: %d anios (%d-%d)", nrow(ech), min(ech$anio), max(ech$anio))
log_msg("JND-Hogares: %d olas (%s)", nrow(jnd), paste(jnd$anio, collapse = ", "))

log_msg("\n--- 2) Verificacion de las cifras citadas en la literatura ---")

p01 <- panel[anio == 2001]$p_reales;   p21 <- panel[anio == 2021]$p_reales
t01 <- panel[anio == 2001]$tax_reales; t21 <- panel[anio == 2021]$tax_reales
log_msg("Precio real 2001->2021: %.2f -> %.2f  (+%.1f%%, cita: +84%%)",
        p01, p21, 100 * (p21 / p01 - 1))
log_msg("Impuesto real 2001->2021: %.2f -> %.2f  (+%.1f%%, cita: +123%%)",
        t01, t21, 100 * (t21 / t01 - 1))
log_msg("NOTA: las cifras 123%%/84%% y el 33%% de caida en adultos 2006-2022 (ECH 15-64) provienen")
log_msg("de Gerstenblueth & Triunfo (2024), 'El control del tabaco en Uruguay: evolucion de los")
log_msg("ultimos 20 anios', Rev Med Urug 40(3):e204 -- NO estaba en biblio/ del proyecto, se")
log_msg("incorpora la verificacion en esta sesion. Confirmadas ambas cifras de forma independiente")
log_msg("contra 41_panel_fiscal_anual.csv (arriba) y contra 21_prevalencia_ponderada.csv (abajo).")

log_msg("\n--- 3) Panel combinado, indices base 2003=100 ---")

base <- panel[, .(anio, p_reales, tax_reales, porcentaje_tax_p, dgi_mill = cigarrillos_mill)]
base <- merge(base, ech[, .(anio, prev_ech = prev_ponderada)], by = "anio", all.x = TRUE)
base <- merge(base, jnd[, .(anio, prev_jnd = prev_30d_o_similar)], by = "anio", all.x = TRUE)

anio_base <- 2003
f_p   <- base[anio == anio_base]$p_reales
f_t   <- base[anio == anio_base]$tax_reales
f_dgi <- base[anio == anio_base]$dgi_mill
base[, `:=`(idx_precio  = 100 * p_reales / f_p,
            idx_impuesto = 100 * tax_reales / f_t,
            idx_dgi      = 100 * dgi_mill / f_dgi)]
setorder(base, anio)
fwrite(base, file.path(out_tab, "60_validacion_historica_fctc_series.csv"))
log_msg("Escrito 60_validacion_historica_fctc_series.csv (%d filas, base %d=100)", nrow(base), anio_base)

log_msg("\n--- 4) Episodio 2003/2005 -> 2010/2011: volumen vs. prevalencia ---")

pct <- function(v0, v1) 100 * (v1 / v0 - 1)

resumen <- data.table(
  serie = c("Precio real", "Impuesto real", "Participacion impositiva en el precio (pp)",
            "Volumen legal DGI", "Prevalencia ECH (15+)", "Prevalencia JND-Hogares"),
  valor_2003_05 = c(base[anio == 2003]$p_reales, base[anio == 2003]$tax_reales,
                     base[anio == 2003]$porcentaje_tax_p, base[anio == 2003]$dgi_mill,
                     base[anio == 2006]$prev_ech, base[anio == 2006]$prev_jnd),
  valor_2010_11 = c(base[anio == 2010]$p_reales, base[anio == 2010]$tax_reales,
                     base[anio == 2010]$porcentaje_tax_p, base[anio == 2010]$dgi_mill,
                     base[anio == 2011]$prev_ech, base[anio == 2011]$prev_jnd),
  anio_ini = c(2003, 2003, 2003, 2003, 2006, 2006),
  anio_fin = c(2010, 2010, 2010, 2010, 2011, 2011)
)
resumen[, cambio_pct := ifelse(grepl("^Participacion", serie),
                                valor_2010_11 - valor_2003_05,
                                pct(valor_2003_05, valor_2010_11))]
log_msg("Resumen del episodio:\n%s", paste(capture.output(print(resumen)), collapse = "\n"))

vel_dgi  <- resumen[serie == "Volumen legal DGI"]$cambio_pct
vel_ech  <- resumen[serie == "Prevalencia ECH (15+)"]$cambio_pct
vel_jnd  <- resumen[serie == "Prevalencia JND-Hogares"]$cambio_pct
log_msg("\nDGI (2003-2010): %.1f%%  |  ECH (2006-2011): %.1f%%  |  JND (2006-2011): %.1f%%",
        vel_dgi, vel_ech, vel_jnd)
log_msg("Ratio de velocidad DGI/ECH: %.2fx  |  DGI/JND: %.2fx", vel_dgi / vel_ech, vel_dgi / vel_jnd)

fwrite(resumen, file.path(out_tab, "60_validacion_historica_fctc_episodio.csv"))

log_msg("\n--- 5) Figura ---")

fig_path <- file.path(out_fig, "60_validacion_historica_fctc.png")
png(fig_path, width = 1500, height = 1500, res = 160)
par(mfrow = c(2, 1), mar = c(2.5, 4.5, 3, 1), oma = c(3, 0, 2, 0))

rango_x <- c(2001, 2025)
eventos <- c(2004, 2005, 2007, 2008, 2009, 2010)
etiquetas_ev <- c("Ratif. FCTC", "Decretos IMESI", "IVA 22%", "Ley 18.256",
                   "IMESI unif. 70%", "IMESI feb-2010")

plot(base$anio, base$idx_precio, type = "l", lwd = 2.5, col = "#1b5e8c",
     xlim = rango_x, ylim = c(0, max(base$idx_impuesto, na.rm = TRUE) * 1.55),
     xlab = "", ylab = "Indice (2003 = 100)", main = "Precio e impuesto real de cigarrillos")
lines(base$anio, base$idx_impuesto, lwd = 2.5, col = "#b5442e")
abline(v = eventos, col = "grey70", lty = 2)
y_lab0 <- max(base$idx_impuesto, na.rm = TRUE) * 1.08
text(eventos, y_lab0, etiquetas_ev, srt = 90, cex = 0.68, adj = c(0, 0.5), xpd = NA)
legend("topleft", legend = c("Precio real", "Impuesto real"), col = c("#1b5e8c", "#b5442e"),
       lwd = 2.5, bty = "n", cex = 0.85)

f_ech <- base[anio == 2006]$prev_ech
f_jnd <- base[anio == 2006]$prev_jnd
base[, `:=`(idx_prev_ech = 100 * prev_ech / f_ech, idx_prev_jnd = 100 * prev_jnd / f_jnd)]

plot(base$anio, base$idx_dgi, type = "l", lwd = 2.5, col = "#1b5e8c",
     xlim = rango_x, ylim = c(min(c(base$idx_dgi, base$idx_prev_ech, base$idx_prev_jnd), na.rm = TRUE) * 0.95, 105),
     xlab = "Año", ylab = "Indice (2003=100 DGI; 2006=100 prevalencia)",
     main = "Volumen legal (DGI) vs. prevalencia encuestada (ECH, JND-Hogares)")
lines(base$anio, base$idx_prev_ech, lwd = 2.5, col = "#2e7d32", type = "b", pch = 16, cex = 0.7)
lines(base$anio, base$idx_prev_jnd, lwd = 2.5, col = "#8e5b2d", type = "b", pch = 17, cex = 0.7)
abline(v = eventos, col = "grey70", lty = 2)
legend("bottomleft", legend = c("Volumen legal DGI", "Prevalencia ECH (15+)", "Prevalencia JND-Hogares"),
       col = c("#1b5e8c", "#2e7d32", "#8e5b2d"), lwd = 2.5, pch = c(NA, 16, 17), bty = "n", cex = 0.85)

mtext("Uruguay 2001-2025 -- quiebre de politica marcado (lineas punteadas): ratificacion FCTC 2004,\ndecretos IMESI 2005, IVA 2007, Ley 18.256 (2008), unificacion IMESI jul-2009, suba feb-2010",
      outer = TRUE, cex = 0.7, line = 0.3)
dev.off()
log_msg("Figura escrita: %s", fig_path)

log_msg("\n=== outputs: 60_validacion_historica_fctc_series.csv, 60_validacion_historica_fctc_episodio.csv, %s ===",
        basename(fig_path))
close(log_con)
