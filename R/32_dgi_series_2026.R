# 32_dgi_series_2026.R
#
# Monthly tax authority (DGI) series of legal volumes and excise revenue, validated against the
# existing sources and used to extend the annual fiscal panel to 2026. Aborts if any existing
# value would change.
# Input:  data/raw/DGI_series_2026/, output/tables/41_panel_fiscal_anual.csv
# Output: output/tables/41_panel_fiscal_anual.csv (extended), 54_*.csv

library(here)
library(data.table)
library(haven)

here::i_am("R/32_dgi_series_2026.R")

log_con <- file(here::here("logs", "32_dgi_series_2026.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 32_dgi_series_2026.R -- %s ===", format(Sys.time()))

raw_dgi <- here::here("data", "raw", "DGI_series_2026")
out_dir <- here::here("output", "tables")

leer_latin1 <- function(f) iconv(readLines(f, warn = FALSE), from = "latin1", to = "UTF-8")

num_dgi <- function(x) {
  x <- trimws(x)
  x[x %in% c("", "-", "--")] <- NA_character_
  suppressWarnings(as.numeric(gsub(".", "", x, fixed = TRUE)))
}

MESES_ES <- c(ene = 1, feb = 2, mar = 3, abr = 4, may = 5, jun = 6,
              jul = 7, ago = 8, set = 9, sep = 9, oct = 10, nov = 11, dic = 12)

parsear_fecha_dgi <- function(x) {
  p <- tstrsplit(trimws(x), "-", fixed = TRUE)
  mes <- MESES_ES[tolower(p[[1]])]
  yy <- suppressWarnings(as.integer(p[[2]]))
  anio <- fifelse(yy >= 30L, 1900L + yy, 2000L + yy)
  data.table(anio = anio, mes = as.integer(mes))
}

log_msg("\n--- 1) Volumen fisico mensual (IMESI_vol_fisico_series_mensuales_2026-07.csv) ---")

f_vol <- file.path(raw_dgi, "IMESI_vol_fisico_series_mensuales_2026-07.csv")
LV <- leer_latin1(f_vol)
log_msg("Lineas: %d", length(LV))

fila_subrubro <- strsplit(LV[2], ";", fixed = TRUE)[[1]]
fila_nivel3   <- strsplit(LV[3], ";", fixed = TRUE)[[1]]
col_cig <- grep("^Cigarrillos$", trimws(fila_subrubro))
col_tab <- grep("^Tabacos$", trimws(fila_subrubro))
if (length(col_cig) != 1 || length(col_tab) != 1)
  stop("32: no se identificaron univocamente las columnas Cigarrillos/Tabacos en el encabezado")
log_msg("Columnas localizadas por etiqueta: Cigarrillos en %d-%d, Tabacos en %d-%d",
        col_cig, col_cig + 2, col_tab, col_tab + 2)
stopifnot(identical(trimws(fila_nivel3[col_cig:(col_cig + 2)]), c("Nacionales", "Importados", "Total")),
          identical(trimws(fila_nivel3[col_tab:(col_tab + 2)]), c("Nacionales", "Importados", "Total")))

datos_vol <- LV[7:length(LV)]
campos <- tstrsplit(datos_vol, ";", fixed = TRUE)
VOL <- cbind(
  parsear_fecha_dgi(campos[[1]]),
  data.table(
    cigarrillos_nacionales = num_dgi(campos[[col_cig]]),
    cigarrillos_importados = num_dgi(campos[[col_cig + 1]]),
    cigarrillos_total      = num_dgi(campos[[col_cig + 2]]),
    tabaco_nacional        = num_dgi(campos[[col_tab]]),
    tabaco_importado       = num_dgi(campos[[col_tab + 1]]),
    tabaco_total           = num_dgi(campos[[col_tab + 2]])
  )
)
VOL <- VOL[!is.na(anio) & !is.na(mes)]
log_msg("Filas parseadas: %d, rango %d-%02d a %d-%02d",
        nrow(VOL), min(VOL$anio), VOL[anio == min(anio)]$mes[1], max(VOL$anio), VOL[anio == max(anio)]$mes[.N])
log_msg("Meses con cigarrillos_total no NA: %d (primero %d, ultimo %d)",
        VOL[!is.na(cigarrillos_total), .N], VOL[!is.na(cigarrillos_total)][1]$anio,
        VOL[!is.na(cigarrillos_total)][.N]$anio)
log_msg("Meses con tabaco_total no NA: %d (primero %d, ultimo %d)",
        VOL[!is.na(tabaco_total), .N], VOL[!is.na(tabaco_total)][1]$anio,
        VOL[!is.na(tabaco_total)][.N]$anio)
fwrite(VOL, file.path(out_dir, "54_dgi_vol_mensual.csv"))

VOL_ANUAL_DGI <- VOL[, .(
  cigarrillos_total = sum(cigarrillos_total, na.rm = TRUE),
  tabaco_total      = sum(tabaco_total, na.rm = TRUE),
  n_meses_cig       = sum(!is.na(cigarrillos_total)),
  n_meses_tab       = sum(!is.na(tabaco_total))
), by = anio]

log_msg("\n--- 2) Recaudacion mensual (IMESI_recaudacion_por_rubros_series_mensuales_2026-07.csv) ---")

f_rec <- file.path(raw_dgi, "IMESI_recaudacion_por_rubros_series_mensuales_2026-07.csv")
LR <- leer_latin1(f_rec)
enc_rec <- trimws(strsplit(LR[1], ";", fixed = TRUE)[[1]])
col_tabcig <- grep("^Tabacos y cigarrillos$", enc_rec)
if (length(col_tabcig) != 1) stop("32: no se identifico la columna 'Tabacos y cigarrillos' en recaudacion")
log_msg("Columna 'Tabacos y cigarrillos' localizada por etiqueta en la posicion %d", col_tabcig)

campos_r <- tstrsplit(LR[4:length(LR)], ";", fixed = TRUE)
REC <- cbind(parsear_fecha_dgi(campos_r[[1]]),
             data.table(recaudacion_tabaco = num_dgi(campos_r[[col_tabcig]])))
REC <- REC[!is.na(anio) & !is.na(mes)]
log_msg("Filas parseadas: %d, rango %d-%02d a %d-%02d; meses con dato: %d",
        nrow(REC), min(REC$anio), REC[anio == min(anio)]$mes[1], max(REC$anio),
        REC[anio == max(anio)]$mes[.N], REC[!is.na(recaudacion_tabaco), .N])
fwrite(REC, file.path(out_dir, "54_dgi_recaudacion_mensual.csv"))

REC_ANUAL_DGI <- REC[!is.na(recaudacion_tabaco), .(
  recaudacion_nominal_dgi = sum(recaudacion_tabaco), n_meses_rec = .N), by = anio]

log_msg("\n--- 3) Validacion del solape ---")

panel_viejo <- fread(file.path(out_dir, "41_panel_fiscal_anual.csv"))

vol_viejo <- fread(file.path(out_dir, "41_volumen_fisico_mensual_a_anual.csv"))
chk_a <- merge(vol_viejo[n_meses == 12, .(anio, cig_viejo = cigarrillos_total_mensual_sum,
                                           tab_viejo = tabaco_total_mensual_sum)],
               VOL_ANUAL_DGI[n_meses_cig == 12, .(anio, cig_nuevo = cigarrillos_total,
                                                   tab_nuevo = tabaco_total)], by = "anio")
chk_a[, `:=`(ratio_cig = cig_nuevo / cig_viejo, ratio_tab = tab_nuevo / tab_viejo)]
log_msg("3a) CSV nuevo vs. XLSX viejo, anios completos en ambos (%d anios):", nrow(chk_a))
log_msg("    cigarrillos: ratio min=%.6f max=%.6f | tabaco: ratio min=%.6f max=%.6f",
        min(chk_a$ratio_cig), max(chk_a$ratio_cig), min(chk_a$ratio_tab), max(chk_a$ratio_tab))
desvio_a <- max(abs(chk_a$ratio_cig - 1), abs(chk_a$ratio_tab - 1))
log_msg("    desvio maximo respecto de 1: %.2e", desvio_a)
if (desvio_a < 1e-6) {
  log_msg("    => IDENTICO en cigarrillos Y en tabaco: el CSV nuevo es la misma serie que el XLSX")
  log_msg("       viejo y el parseo (encabezado multinivel, separador de miles, fechas) es correcto.")
} else {
  log_msg("    => ATENCION: no coincide con la fuente vieja. Revisar el parseo antes de usar nada.")
}

chk_b <- merge(panel_viejo[!is.na(cigarrillos_mill), .(anio, cigarrillos_mill, tabaco_mill)],
               VOL_ANUAL_DGI[n_meses_cig == 12], by = "anio")
chk_b[, `:=`(ratio_cig = cigarrillos_total / (cigarrillos_mill * 1e6),
             ratio_tab = tabaco_total / (tabaco_mill * 1e6))]
log_msg("3b) CSV nuevo vs. cigarrostabaco.dta (%d anios de solape, %d-%d):",
        nrow(chk_b), min(chk_b$anio), max(chk_b$anio))
log_msg("    cigarrillos: ratio medio=%.6f (min %.6f, max %.6f)",
        mean(chk_b$ratio_cig), min(chk_b$ratio_cig), max(chk_b$ratio_cig))
log_msg("    tabaco:      ratio medio=%.6f (min %.6f, max %.6f) | 4/9 = %.6f",
        mean(chk_b$ratio_tab), min(chk_b$ratio_tab), max(chk_b$ratio_tab), 4/9)
log_msg("    Desvio maximo del ratio de tabaco respecto de 4/9: %.2e",
        max(abs(chk_b$ratio_tab - 4/9)))

chk_c <- merge(panel_viejo[!is.na(recaudacion_nominal) & n_meses == 12,
                            .(anio, rec_viejo = recaudacion_nominal)],
               REC_ANUAL_DGI[n_meses_rec == 12], by = "anio")
chk_c[, ratio_rec := recaudacion_nominal_dgi / rec_viejo]
log_msg("3c) Recaudacion nueva vs. vieja (%d anios completos): ratio min=%.6f max=%.6f",
        nrow(chk_c), min(chk_c$ratio_rec), max(chk_c$ratio_rec))

chequeos <- rbindlist(list(
  chk_a[, .(anio, chequeo = "vol_nuevo_vs_xlsx_viejo", ratio_cigarrillos = ratio_cig, ratio_tabaco = ratio_tab)],
  chk_b[, .(anio, chequeo = "vol_nuevo_vs_cigarrostabaco", ratio_cigarrillos = ratio_cig, ratio_tabaco = ratio_tab)],
  chk_c[, .(anio, chequeo = "recaudacion_nueva_vs_vieja", ratio_cigarrillos = ratio_rec, ratio_tabaco = NA_real_)]
), fill = TRUE)
fwrite(chequeos, file.path(out_dir, "54_chequeo_dgi_vs_fuentes.csv"))

log_msg("\n--- 4) Extension de 41_panel_fiscal_anual.csv ---")

nuevo <- merge(VOL_ANUAL_DGI, REC_ANUAL_DGI, by = "anio", all = TRUE)
nuevo_completo <- nuevo[n_meses_cig == 12 | n_meses_rec == 12]
log_msg("Anios con doce meses en la fuente nueva: %d a %d (2026 queda afuera: %d meses de volumen)",
        min(nuevo_completo$anio), max(nuevo_completo$anio), nuevo[anio == 2026]$n_meses_cig)

panel <- copy(panel_viejo)
if (!"cigarrillos_dgi_mill" %in% names(panel)) panel[, `:=`(cigarrillos_dgi_mill = NA_real_, tabaco_dgi_mill = NA_real_)]
panel <- merge(panel, nuevo_completo[, .(anio, cig_new = cigarrillos_total, tab_new = tabaco_total,
                                          rec_new = recaudacion_nominal_dgi, n_meses_new = n_meses_rec,
                                          n_meses_cig_new = n_meses_cig)],
               by = "anio", all = TRUE)

registro <- list()
anotar <- function(col, anio, viejo, nuevo_v, motivo) {
  registro[[length(registro) + 1]] <<- data.table(columna = col, anio = anio, valor_viejo = viejo,
                                                   valor_nuevo = nuevo_v, motivo = motivo)
}

panel[!is.na(cig_new) & n_meses_cig_new == 12, cigarrillos_dgi_mill := cig_new / 1e6]
panel[!is.na(tab_new) & n_meses_cig_new == 12, tabaco_dgi_mill := tab_new / 1e6]

ext_cig <- panel[is.na(cigarrillos_mill) & !is.na(cig_new) & n_meses_cig_new == 12]$anio
for (a in ext_cig) anotar("cigarrillos_mill", a, NA_real_, panel[anio == a]$cig_new / 1e6, "extension DGI (ratio 1,000)")
panel[anio %in% ext_cig, cigarrillos_mill := cig_new / 1e6]
log_msg("4b) cigarrillos_mill extendido en %d anios: %s", length(ext_cig), paste(ext_cig, collapse = ", "))

FACTOR_TABACO_DGI_A_CIGARROSTABACO <- 45 / 20
ext_tab <- panel[is.na(tabaco_mill) & !is.na(tab_new) & n_meses_cig_new == 12]$anio
for (a in ext_tab) anotar("tabaco_mill", a, NA_real_,
                           panel[anio == a]$tab_new / 1e6 * FACTOR_TABACO_DGI_A_CIGARROSTABACO,
                           "extension DGI (factor 45/20, Decreto 357/024)")
panel[anio %in% ext_tab, tabaco_mill := tab_new / 1e6 * FACTOR_TABACO_DGI_A_CIGARROSTABACO]
log_msg("4c) tabaco_mill extendido en %d anios con el factor 45/20 = %.4f (Decreto 357/024 art. 1,",
        length(ext_tab), FACTOR_TABACO_DGI_A_CIGARROSTABACO)
log_msg("    verificado contra el texto original: cajilla de 20 unidades / paquete de 45 gramos).")
log_msg("    Anios: %s. El dato crudo sin convertir queda en tabaco_dgi_mill.", paste(ext_tab, collapse = ", "))
log_msg("    Residuo sin verificar: que la unidad de tabaco de cigarrostabaco.dta sea la de base 20.")

cambios_rec <- panel[!is.na(rec_new) & n_meses_new == 12 &
                       (is.na(recaudacion_nominal) | n_meses < 12)]$anio
for (a in cambios_rec) {
  motivo <- fifelse(is.na(panel[anio == a]$recaudacion_nominal), "extension DGI",
                    "anio completado (antes parcial)")
  anotar("recaudacion_nominal", a, panel[anio == a]$recaudacion_nominal, panel[anio == a]$rec_new, motivo)
  anotar("n_meses", a, panel[anio == a]$n_meses, panel[anio == a]$n_meses_new, motivo)
}
panel[anio %in% cambios_rec, `:=`(recaudacion_nominal = rec_new, n_meses = n_meses_new)]
log_msg("4d) recaudacion_nominal extendida/completada en %d anios: %s",
        length(cambios_rec), paste(cambios_rec, collapse = ", "))
log_msg("    De esos, anios que ANTES estaban incompletos y ahora se completan: %s",
        paste(panel_viejo[anio %in% cambios_rec & !is.na(recaudacion_nominal),
                          sprintf("%d (%d->12 meses)", anio, n_meses)], collapse = ", "))

real_inconsistente <- panel[anio %in% cambios_rec & !is.na(recaudacion_real)]$anio
for (a in real_inconsistente) anotar("recaudacion_real", a, panel[anio == a]$recaudacion_real, NA_real_,
                                      "nominal completado; no recalculable con IPC mensual (corta 2022-06)")
panel[anio %in% cambios_rec, recaudacion_real := NA_real_]
log_msg("4d-bis) recaudacion_real puesta en NA en %d anios cuyo nominal cambio y no es recalculable: %s",
        length(real_inconsistente), paste(real_inconsistente, collapse = ", "))
log_msg("        Se recupera en cuanto se cierre el hueco de IPC (misma dependencia que p_reales).")

comunes <- intersect(names(panel_viejo), names(panel))
comunes <- setdiff(comunes, "anio")
violaciones <- rbindlist(lapply(comunes, function(v) {
  cmp <- merge(panel_viejo[, .(anio, viejo = get(v))], panel[, .(anio, nuevo = get(v))], by = "anio")
  cmp <- cmp[!is.na(viejo) & (is.na(nuevo) | abs(nuevo - viejo) > 1e-9)]
  if (nrow(cmp) == 0) return(NULL)
  cmp[, columna := v][]
}), fill = TRUE)
esperados <- rbindlist(registro, fill = TRUE)
if (nrow(violaciones) > 0) {
  inesperadas <- violaciones[!paste(columna, anio) %in% paste(esperados$columna, esperados$anio)]
  log_msg("4e) Valores preexistentes que cambian: %d (esperados: %d, INESPERADOS: %d)",
          nrow(violaciones), nrow(violaciones) - nrow(inesperadas), nrow(inesperadas))
  log_msg("%s", paste(capture.output(print(violaciones)), collapse = "\n"))
  if (nrow(inesperadas) > 0) stop("32: cambios inesperados en el panel, no se reescribe nada")
} else {
  log_msg("4e) Ningun valor preexistente cambia.")
}

panel[, c("cig_new", "tab_new", "rec_new", "n_meses_new", "n_meses_cig_new") := NULL]
setorder(panel, anio)
fwrite(panel, file.path(out_dir, "41_panel_fiscal_anual.csv"))
if (length(registro) > 0) {
  fwrite(esperados, file.path(out_dir, "54_registro_extension_panel.csv"))
  log_msg("Registro de la extension:\n%s", paste(capture.output(print(esperados)), collapse = "\n"))
}
log_msg("Panel reescrito: %d filas (antes %d), rango %d-%d", nrow(panel), nrow(panel_viejo),
        min(panel$anio), max(panel$anio))
log_msg("Cobertura por columna despues de extender:\n%s",
        paste(capture.output(print(panel[, .(anio, cigarrillos_mill = !is.na(cigarrillos_mill),
                                              tabaco_mill = !is.na(tabaco_mill),
                                              cig_dgi = !is.na(cigarrillos_dgi_mill),
                                              recaud = !is.na(recaudacion_nominal),
                                              p_reales = !is.na(p_reales))][anio >= 2018])), collapse = "\n"))

log_msg("\nAVISO: el hueco de PRECIOS 2022-2025 sigue abierto. Estas dos fuentes cierran volumen")
log_msg("fisico y recaudacion, no precios. p_reales / porcentaje_tax_p siguen cortando en 2022-06.")

log_msg("\n=== outputs: 54_dgi_vol_mensual.csv, 54_dgi_recaudacion_mensual.csv, 54_chequeo_dgi_vs_fuentes.csv, 54_registro_extension_panel.csv, 41_panel_fiscal_anual.csv (reescrito) ===")
close(log_con)
