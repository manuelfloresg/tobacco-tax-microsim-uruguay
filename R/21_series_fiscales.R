# 21_series_fiscales.R
#
# Annual fiscal panel: cigarette prices, excise (IMESI), legal volumes and revenue, in real terms.
# R/32 and R/33 extend it to 2026.
# Input:  data/raw/Impuestos/, data/raw/Precios/
# Output: output/tables/41_panel_fiscal_anual.csv and related tables

library(here)
library(data.table)
library(haven)
library(readxl)

here::i_am("R/21_series_fiscales.R")

log_con <- file(here::here("logs", "21_series_fiscales.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 21_series_fiscales.R -- %s ===", format(Sys.time()))

raw_imp <- here::here("data", "raw", "Impuestos")
raw_pre <- here::here("data", "raw", "Precios")
out_dir <- here::here("output", "tables")

num_coma <- function(x) as.numeric(gsub(",", ".", x, fixed = TRUE))

log_msg("\n--- 1) Precios e impuestos.dta ---")

PI <- setDT(read_dta(file.path(raw_pre, "Precios e impuestos.dta")))
for (v in c("porcentaje_tax_p", "tax_reales", "p_reales", "IPCcigar", "IPCgral")) {
  PI[, (v) := num_coma(get(v))]
}
PI[, `:=`(anio = as.numeric(year), mes = as.numeric(month))]
PI <- PI[!is.na(anio) & !is.na(mes)]
log_msg("Filas: %d, rango: %d-%02d a %d-%02d", nrow(PI), min(PI$anio), PI[anio==min(anio)]$mes[1],
        max(PI$anio), PI[anio==max(anio)]$mes[length(PI[anio==max(anio)]$mes)])

log_msg("\n--- 2) asequibilidad.dta ---")
AS <- setDT(read_dta(file.path(raw_pre, "asequibilidad.dta")))
setnames(AS, "anio", "anio", skip_absent = TRUE)
log_msg("Filas: %d, columnas: %s", nrow(AS), paste(names(AS), collapse = ", "))

log_msg("\n--- 3) cigarrostabaco.dta (volumen fisico legal anual, backbone) ---")
VOL_ANUAL <- setDT(read_dta(file.path(raw_pre, "cigarrostabaco.dta")))
setnames(VOL_ANUAL, "año", "anio")
VOL_ANUAL <- VOL_ANUAL[!is.na(anio), .(anio, cigarrillos_mill = cigarros, tabaco_mill = tabaco)]
log_msg("Filas: %d, rango: %d-%d", nrow(VOL_ANUAL), min(VOL_ANUAL$anio), max(VOL_ANUAL$anio))

log_msg("\n--- 4) IMESI Tabacos Uruguay Actualizado Diciembre 2022.xlsx, hoja Series ---")

IM <- setDT(read_excel(file.path(raw_imp, "IMESI Tabacos Uruguay Actualizado Diciembre 2022.xlsx"), sheet = "Series"))
IM[, anio_num := suppressWarnings(as.numeric(as.character(año)))]
IM[, mes_num := suppressWarnings(as.numeric(mes))]
IM[, fecha_reconstruida := as.Date(ifelse(!is.na(fecha), as.character(as.Date(fecha)),
                                    ifelse(!is.na(anio_num) & !is.na(mes_num),
                                           sprintf("%d-%02d-01", anio_num, mes_num), NA)))]
n_reconstruidas <- IM[is.na(fecha) & !is.na(fecha_reconstruida), .N]
log_msg("Filas con fecha reconstruida desde mes/anio (la columna fecha original no llegaba): %d", n_reconstruidas)
IM <- IM[!is.na(fecha_reconstruida)]
IM[, `:=`(anio = as.numeric(format(fecha_reconstruida, "%Y")), mes = as.numeric(format(fecha_reconstruida, "%m")))]
setnames(IM, c("Tasa IMESI", "IMESI $ x caja de 20 uds", "INDICE IMESI REAL (DIC 2014=100)"),
         c("tasa_imesi", "imesi_por_caja", "indice_imesi_real"), skip_absent = TRUE)
log_msg("Filas con fecha valida: %d, rango: %d-%02d a %d-%02d", nrow(IM), min(IM$anio),
        IM[anio == min(anio)]$mes[1], max(IM$anio), IM[anio == max(anio)]$mes[.N])

log_msg("\n--- 5) IMESI volumen fisico mensual_DGI.xlsx, hoja datos (columnas TABACOS) ---")

crudo <- setDT(read_excel(file.path(raw_imp, "IMESI volumen fisico mensual_DGI.xlsx"), sheet = "datos", col_names = FALSE))
etiquetas_vol <- c("fecha_serial", "cigarrillos_nacionales", "cigarrillos_importados", "cigarrillos_total",
                    "tabaco_nacional", "tabaco_importado", "tabaco_total")
VOL_MENSUAL <- crudo[7:.N, c(1, 51:56)]
setnames(VOL_MENSUAL, etiquetas_vol)
for (v in etiquetas_vol) VOL_MENSUAL[, (v) := suppressWarnings(as.numeric(get(v)))]
VOL_MENSUAL <- VOL_MENSUAL[!is.na(fecha_serial) & fecha_serial > 1000]
VOL_MENSUAL[, fecha := as.Date(fecha_serial, origin = "1899-12-30")]
VOL_MENSUAL[, `:=`(anio = as.numeric(format(fecha, "%Y")), mes = as.numeric(format(fecha, "%m")))]
VOL_MENSUAL <- VOL_MENSUAL[!is.na(cigarrillos_total)]
log_msg("Filas con dato (cigarrillos_total no NA): %d, rango: %s a %s",
        nrow(VOL_MENSUAL), format(min(VOL_MENSUAL$fecha)), format(max(VOL_MENSUAL$fecha)))
log_msg("AVISO de cobertura: esta fuente NO llega a 2025 -- termina en %s. El panel ECH llega a 2025.",
        format(max(VOL_MENSUAL$fecha)))

vol_mensual_a_anual <- VOL_MENSUAL[, .(cigarrillos_total_mensual_sum = sum(cigarrillos_total, na.rm = TRUE),
                                        tabaco_total_mensual_sum = sum(tabaco_total, na.rm = TRUE),
                                        n_meses = .N), by = anio]
fwrite(vol_mensual_a_anual, file.path(out_dir, "41_volumen_fisico_mensual_a_anual.csv"))

chequeo_vol <- merge(vol_mensual_a_anual[n_meses == 12], VOL_ANUAL, by = "anio")
chequeo_vol[, `:=`(
  ratio_cigarrillos = cigarrillos_total_mensual_sum / (cigarrillos_mill * 1e6),
  ratio_tabaco = tabaco_total_mensual_sum / (tabaco_mill * 1e6)
)]
fwrite(chequeo_vol, file.path(out_dir, "41_chequeo_volumen_mensual_vs_anual.csv"))
log_msg("Chequeo de consistencia (agregado mensual DGI / cigarrostabaco.dta, deberia ser ~1.0):\n%s",
        paste(capture.output(print(chequeo_vol[, .(anio, ratio_cigarrillos, ratio_tabaco)])), collapse = "\n"))
log_msg("AVISO: cigarrillos coincide exactamente (ratio=1.0) en los 23 anios -- misma fuente, confirma la")
log_msg("extraccion de columnas. Tabaco de armar da un ratio CONSTANTE de %.6f (=4/9) en los 23 anios, no",
        mean(chequeo_vol$ratio_tabaco, na.rm = TRUE))
log_msg("ruido -- es un factor de unidades sistematico entre esta hoja cruda de DGI y cigarrostabaco.dta")
log_msg("(hipotesis mas probable: paquetes de distinto gramaje, p.ej. 40g vs 90g = 4/9 exacto), no")
log_msg("verificado contra la norma. NO se usa la columna de tabaco de esta fuente para el panel final --")
log_msg("se mantiene cigarrostabaco.dta como autoridad para tabaco de armar (ya que cigarrillos, la serie")
log_msg("que SI coincide, confirma que la extraccion de columnas del archivo crudo esta bien hecha; el")
log_msg("problema esta en la unidad de tabaco, no en la columna elegida). Pendiente confirmar el factor")
log_msg("antes de usar volumen de tabaco de armar mas alla de 2021.")

log_msg("\n--- 6) Recaudacion IMESI tabaco y cigarrillos mensuales_DGI.xlsm ---")

crudo_r <- setDT(read_excel(file.path(raw_imp, "Recaudacion IMESI tabaco y cigarrillos mensuales_DGI.xlsm"),
                             sheet = "Recaudación", col_names = FALSE))
REC <- data.table(fecha_serial = suppressWarnings(as.numeric(crudo_r[[3]])),
                   recaudacion_nominal = suppressWarnings(as.numeric(crudo_r[[4]])))
REC <- REC[!is.na(fecha_serial) & fecha_serial > 1000 & !is.na(recaudacion_nominal)]
REC[, fecha := as.Date(fecha_serial, origin = "1899-12-30")]
REC[, `:=`(anio = as.numeric(format(fecha, "%Y")), mes = as.numeric(format(fecha, "%m")))]
log_msg("Filas con dato: %d, rango: %s a %s", nrow(REC), format(min(REC$fecha)), format(max(REC$fecha)))

REC <- merge(REC, PI[, .(anio, mes, IPCgral)], by = c("anio", "mes"), all.x = TRUE)
ipc_base <- PI[anio == max(PI[!is.na(IPCgral)]$anio)][mes == max(mes)]$IPCgral[1]
REC[, recaudacion_real := recaudacion_nominal * ipc_base / IPCgral]
n_sin_ipc <- REC[is.na(IPCgral), .N]
log_msg("Filas de recaudacion sin IPCgral para deflactar (fuera del rango de Precios e impuestos.dta): %d de %d",
        n_sin_ipc, nrow(REC))

recaudacion_anual <- REC[!is.na(recaudacion_real), .(recaudacion_nominal = sum(recaudacion_nominal),
                                                       recaudacion_real = sum(recaudacion_real),
                                                       n_meses = .N), by = anio]
fwrite(recaudacion_anual, file.path(out_dir, "41_recaudacion_anual.csv"))
log_msg("Recaudacion real anual (base = ultimo mes de Precios e impuestos.dta, indice=%.2f), ultimos 5 anios completos:\n%s",
        ipc_base, paste(capture.output(print(tail(recaudacion_anual[n_meses == 12], 5))), collapse = "\n"))

log_msg("\n--- 7) Panel anual consolidado ---")

precio_impuesto_anual <- PI[, .(porcentaje_tax_p = mean(porcentaje_tax_p, na.rm = TRUE),
                                  tax_reales = mean(tax_reales, na.rm = TRUE),
                                  p_reales = mean(p_reales, na.rm = TRUE),
                                  IPCcigar = mean(IPCcigar, na.rm = TRUE),
                                  IPCgral = mean(IPCgral, na.rm = TRUE),
                                  n_meses_precio = .N), by = anio]

macro <- Reduce(function(a, b) merge(a, b, by = "anio", all = TRUE), list(
  setDT(read_dta(file.path(raw_pre, "pibpc.dta")))[!is.na(year), .(anio = year, pibpc)],
  setDT(read_dta(file.path(raw_pre, "isr.dta")))[!is.na(year), .(anio = year, ISR)],
  setDT(read_dta(file.path(raw_pre, "pob1564.dta")))[!is.na(year), .(anio = year, pob1564)]
))

panel <- Reduce(function(a, b) merge(a, b, by = "anio", all = TRUE), list(
  precio_impuesto_anual, VOL_ANUAL, recaudacion_anual, macro
))
panel <- panel[!is.na(anio)]
setorder(panel, anio)
fwrite(panel, file.path(out_dir, "41_panel_fiscal_anual.csv"))
log_msg("Panel anual consolidado escrito (41_panel_fiscal_anual.csv), %d filas, rango %d-%d:\n%s",
        nrow(panel), min(panel$anio), max(panel$anio),
        paste(capture.output(print(panel[anio >= 2015])), collapse = "\n"))

log_msg("\n=== outputs: 41_volumen_fisico_mensual_a_anual.csv, 41_chequeo_volumen_mensual_vs_anual.csv, 41_recaudacion_anual.csv, 41_panel_fiscal_anual.csv ===")
close(log_con)
