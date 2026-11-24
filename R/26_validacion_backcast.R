# 26_validacion_backcast.R
#
# Retrospective run 2011-2025 with the observed real price path, compared with ECH prevalence by
# year and with legal consumption from the tax authority.
# Input:  synthetic population 2011, output/tables/43_parametros_calibrados.csv, 41_*, 35_*
# Output: output/tables/44_*.csv

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/26_validacion_backcast.R")

log_con <- file(here::here("logs", "26_validacion_backcast.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 26_validacion_backcast.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

source(here::here("R", "24_motor_transiciones.R"))

ANIOS_VALIDACION <- c(2014, 2017, 2018, 2019, 2021, 2022, 2023, 2024, 2025)

log_msg("\n--- 1) Corrida completa 2011-2025 ---")

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor,
            params_cal[parametro == "raw_par3"]$valor)
params_escalada <- params[1:2]
param_downshift <- params[3]
log_msg("Parametros calibrados cargados: escalada_15=%.4f, decaimiento=%.4f, downshift=%.4f",
        plogis(params[1]), exp(params[2]), plogis(params[3]))

set.seed(20260809)
pop <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"))
pop[, sexo := factor(sexo, levels = c("varon", "mujer"))]
pop[, tercil := factor(tercil, levels = c(1, 2, 3))]
insumos <- cargar_insumos_motor()

anio_actual <- 2011
snapshots <- list()
t0 <- Sys.time()
for (anio_obj in ANIOS_VALIDACION) {
  while (anio_actual < anio_obj) {
    pop <- ciclo_anual(pop, anio_actual, insumos$modelos, insumos$tabla_superv, insumos$precio_lookup,
                        insumos$pob_ine, insumos$entrada_15_por_anio, params_escalada, param_downshift)
    anio_actual <- anio_actual + 1
  }
  snapshots[[as.character(anio_obj)]] <- copy(pop)[, anio_snapshot := anio_obj]
  log_msg("Snapshot %d: %d individuos", anio_obj, nrow(pop))
}
log_msg("Corrida completa terminada en %.1f segundos", as.numeric(difftime(Sys.time(), t0, units = "secs")))

log_msg("\n--- 2) Prevalencia simulada vs. ECH ---")

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))
P[, grupo_etario := fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")]
observado <- P[anio %in% ANIOS_VALIDACION & !is.na(tercil), .(
  prevalencia_obs = sum(peso * (estado %in% c(2, 3))) / sum(peso),
  N_pond = sum(peso)
), by = .(anio, grupo_etario, sexo, tercil)]

simulado <- rbindlist(lapply(snapshots, function(dt) {
  dt[, grupo_etario := fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")]
  dt[, .(prevalencia_sim = mean(estado %in% c(2, 3)), N = .N), by = .(anio = anio_snapshot, grupo_etario, sexo, tercil)]
}))
simulado[, tercil := as.integer(as.character(tercil))]
simulado[, sexo := as.character(sexo)]

comparacion_prevalencia <- merge(observado, simulado, by = c("anio", "grupo_etario", "sexo", "tercil"))
comparacion_prevalencia[, diferencia := prevalencia_sim - prevalencia_obs]
setorder(comparacion_prevalencia, anio, grupo_etario, sexo, tercil)
fwrite(comparacion_prevalencia, file.path(out_dir, "44_validacion_prevalencia.csv"))

resumen_por_anio <- comparacion_prevalencia[, .(
  prevalencia_obs_pond = weighted.mean(prevalencia_obs, N_pond),
  prevalencia_sim_pond = weighted.mean(prevalencia_sim, N_pond),
  dif_abs_media = mean(abs(diferencia))
), by = anio]
setorder(resumen_por_anio, anio)
fwrite(resumen_por_anio, file.path(out_dir, "44_validacion_prevalencia_resumen_anual.csv"))
log_msg("Prevalencia agregada, simulada vs. observada, por anio:\n%s",
        paste(capture.output(print(resumen_por_anio)), collapse = "\n"))
log_msg("Diferencia absoluta media (todas las celdas edad x sexo x tercil x anio): %.4f, maxima: %.4f",
        mean(abs(comparacion_prevalencia$diferencia)), max(abs(comparacion_prevalencia$diferencia)))

totales_obs_anio <- P[anio %in% ANIOS_VALIDACION, .(total_pond = sum(peso)), by = anio]
estados_obs <- P[anio %in% ANIOS_VALIDACION, .(N_pond = sum(peso)), by = .(anio, estado)]
estados_obs <- merge(estados_obs, totales_obs_anio, by = "anio")
estados_obs[, share_obs := N_pond / total_pond]
estados_obs[, c("N_pond", "total_pond") := NULL]
estados_sim <- rbindlist(lapply(snapshots, function(dt) dt[, .N, by = .(anio = anio_snapshot, estado)][, share_sim := N / sum(N)][, N := NULL]))
estados_comp <- merge(estados_obs, estados_sim, by = c("anio", "estado"))
estados_comp[, diferencia := share_sim - share_obs]
setorder(estados_comp, anio, estado)
fwrite(estados_comp, file.path(out_dir, "44_validacion_por_estado.csv"))
log_msg("Desglose por los 5 estados, simulado vs observado (share nacional, sin desagregar por edad/sexo/tercil):\n%s",
        paste(capture.output(print(estados_comp)), collapse = "\n"))

log_msg("\n--- 3) Consumo total simulado vs. DGI (solo cigarrillos, hasta 2022) ---")

intensidad <- fread(file.path(out_dir, "35_intensidad_condicional_gats.csv"))
intensidad <- intensidad[nivel == "frecuencia_sexo_edad" & celda_chica == FALSE & !is.na(media_pond)]
intensidad_pooled <- intensidad[, .(cig_dia = weighted.mean(media_pond, n_pond)), by = .(frecuencia, sexo, grupo_etario)]
log_msg("Intensidad pooled (GATS 2009+2017, cig/dia) usada para el consumo simulado:\n%s",
        paste(capture.output(print(intensidad_pooled)), collapse = "\n"))

panel_fiscal <- fread(file.path(out_dir, "41_panel_fiscal_anual.csv"))

consumo_simulado <- rbindlist(lapply(snapshots, function(dt) {
  dt2 <- copy(dt)
  dt2[, grupo_etario := fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")]
  dt2[, sexo := as.character(sexo)]
  dt2[, frecuencia := fcase(estado == 2, "ocasional", estado == 3, "diario", default = NA_character_)]
  dt2 <- dt2[!is.na(frecuencia)]
  dt2 <- merge(dt2, intensidad_pooled, by = c("frecuencia", "sexo", "grupo_etario"), all.x = TRUE)
  dt2[, .(cigarrillos_anuales_mill = sum(cig_dia * 365, na.rm = TRUE) / 1e6, n_sin_intensidad = sum(is.na(cig_dia))),
      by = .(anio = anio_snapshot)]
}))

comparacion_consumo <- merge(consumo_simulado, panel_fiscal[, .(anio, cigarrillos_mill)], by = "anio", all.x = TRUE)
comparacion_consumo[, cigarrillos_mill_individuales := cigarrillos_mill * 20]
comparacion_consumo[, ratio_sim_sobre_dgi := cigarrillos_anuales_mill / cigarrillos_mill_individuales]
setorder(comparacion_consumo, anio)
fwrite(comparacion_consumo, file.path(out_dir, "44_validacion_consumo_vs_dgi.csv"))
log_msg("Consumo total simulado (cig/dia x 365, poblacion completa) vs. volumen legal DGI (ya corregido x20, cajas -> cigarrillos individuales):\n%s",
        paste(capture.output(print(comparacion_consumo)), collapse = "\n"))
log_msg("NOTA: el ratio simulado/DGI > 1 es ESPERADO y es justamente el diagnostico esperado --")
log_msg("el consumo total (declarado en encuestas) excede las ventas legales por la participacion")
log_msg("ilicita, que este ratio no separa todavia (no se estimo la brecha de Gerstenblueth et al.,")
log_msg("eso requiere mas trabajo posterior). Un ratio MENOR a 1 si seria un problema a investigar.")

log_msg("\n=== outputs: 44_validacion_prevalencia.csv, 44_validacion_prevalencia_resumen_anual.csv, 44_validacion_consumo_vs_dgi.csv ===")
close(log_con)
