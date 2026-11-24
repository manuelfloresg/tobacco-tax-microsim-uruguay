# 25_calibracion_escalada.R
#
# Calibration of the occasional-to-daily (two parameters) and daily-to-occasional (one parameter)
# transitions by the method of moments: the full model is run from 2011 and occasional and daily
# prevalence by age group is matched to the ECH, 2011-2025.
# Usage:  Rscript R/25_calibracion_escalada.R               (primary calibration)
#         Rscript R/25_calibracion_escalada.R alt <value>   (alternative point: downshift fixed)
# Input:  panel from R/04, synthetic population 2011
# Output: output/tables/43_parametros_calibrados*.csv, 43_*.csv

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/25_calibracion_escalada.R")

ARGS25 <- commandArgs(trailingOnly = TRUE)
MODO_ALT <- length(ARGS25) >= 2 && ARGS25[1] == "alt"
DESCENSO_FIJO <- if (MODO_ALT) as.numeric(ARGS25[2]) else NA_real_
SUF <- if (MODO_ALT) "_alt" else ""

log_con <- file(here::here("logs", sprintf("25_calibracion_escalada%s.log", SUF)), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 25_calibracion_escalada.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

source(here::here("R", "24_motor_transiciones.R"))

N_SUBMUESTRA <- 200000
ANIOS_VALIDACION <- c(2014, 2017, 2018, 2019, 2021, 2022, 2023, 2024, 2025)

log_msg("\n--- 1) Momentos observados (ECH 2011-2025) ---")

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))
P[, grupo_etario := fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")]
observado <- P[anio %in% ANIOS_VALIDACION, .(
  N_pond = sum(peso),
  prop_ocasional = sum(peso * (estado == 2)) / sum(peso),
  prop_diario = sum(peso * (estado == 3)) / sum(peso)
), by = .(anio, grupo_etario)]
setorder(observado, anio, grupo_etario)
if (!MODO_ALT) fwrite(observado, file.path(out_dir, "43_momentos_observados_ech.csv"))
log_msg("Momentos observados escritos (43_momentos_observados_ech.csv, %d filas)", nrow(observado))

log_msg("\n--- 2) Submuestra y carga de insumos ---")

set.seed(20260809)
pop_completa <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"))
pop_base <- pop_completa[sample(.N, N_SUBMUESTRA)]
pop_base[, sexo := factor(sexo, levels = c("varon", "mujer"))]
pop_base[, tercil := factor(tercil, levels = c(1, 2, 3))]
log_msg("Submuestra: %d de %d individuos (%.2f%%)", nrow(pop_base), nrow(pop_completa), 100 * nrow(pop_base) / nrow(pop_completa))

insumos <- cargar_insumos_motor()

log_msg("\n--- 3) Funcion de perdida y optimizacion ---")

simular_hasta <- function(params, anios_objetivo, pop_inicial, insumos, semilla = 999) {
  set.seed(semilla)
  params_escalada <- params[1:2]
  param_downshift <- params[3]
  pop <- copy(pop_inicial)
  anio_actual <- 2011
  resultados <- list()
  for (anio_obj in anios_objetivo) {
    while (anio_actual < anio_obj) {
      pop <- ciclo_anual(pop, anio_actual, insumos$modelos, insumos$tabla_superv, insumos$precio_lookup,
                          insumos$pob_ine, insumos$entrada_15_por_anio, params_escalada, param_downshift)
      anio_actual <- anio_actual + 1
    }
    pop[, grupo_etario := fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")]
    resultados[[as.character(anio_obj)]] <- pop[, .(
      N = .N,
      prop_ocasional = mean(estado == 2),
      prop_diario = mean(estado == 3)
    ), by = grupo_etario][, anio := anio_obj]
  }
  rbindlist(resultados)
}

perdida <- function(params) {
  sim <- simular_hasta(params, ANIOS_VALIDACION, pop_base, insumos)
  comp <- merge(sim, observado, by = c("anio", "grupo_etario"), suffixes = c("_sim", "_obs"))
  sum((comp$prop_ocasional_sim - comp$prop_ocasional_obs)^2 + (comp$prop_diario_sim - comp$prop_diario_obs)^2)
}

params_iniciales <- c(qlogis(0.05), log(0.05), qlogis(0.03))
perdida_inicial <- perdida(params_iniciales)
log_msg("Perdida con parametros iniciales (5%% escalada a los 15, decaimiento 5%%/anio, downshift 3%%): %.5f", perdida_inicial)

t0 <- Sys.time()
if (!MODO_ALT) {
  opt <- optim(params_iniciales, perdida, method = "Nelder-Mead",
               control = list(maxit = 300, reltol = 1e-6))
} else {
  log_msg("MODO ALT: descenso fijo en %.6g; se recalibra solo la escalada", DESCENSO_FIJO)
  raw3 <- qlogis(DESCENSO_FIJO)
  opt <- optim(params_iniciales[1:2], function(p) perdida(c(p, raw3)), method = "Nelder-Mead",
               control = list(maxit = 300, reltol = 1e-6))
  opt$par <- c(opt$par, raw3)
}
log_msg("Optimizacion terminada en %.1f segundos. Convergencia (0 = ok): %d. Perdida final: %.5f (inicial: %.5f)",
        as.numeric(difftime(Sys.time(), t0, units = "secs")), opt$convergence, opt$value, perdida_inicial)

p0_escalada_final <- plogis(opt$par[1])
k_escalada_final <- exp(opt$par[2])
p_downshift_final <- plogis(opt$par[3])
log_msg("Parametros calibrados: escalada a los 15 anios = %.4f, decaimiento anual = %.4f, downshift (diario->ocasional) = %.4f",
        p0_escalada_final, k_escalada_final, p_downshift_final)

parametros_calibrados <- data.table(
  parametro = c("escalada_nivel_15", "escalada_decaimiento", "downshift_constante",
                "raw_par1", "raw_par2", "raw_par3"),
  valor = c(p0_escalada_final, k_escalada_final, p_downshift_final, opt$par[1], opt$par[2], opt$par[3])
)
fwrite(parametros_calibrados, file.path(out_dir, sprintf("43_parametros_calibrados%s.csv", SUF)))
log_msg("Escrito 43_parametros_calibrados.csv")

log_msg("\n--- 4) Bondad de ajuste con parametros calibrados ---")

sim_final <- simular_hasta(opt$par, ANIOS_VALIDACION, pop_base, insumos)
comp_final <- merge(sim_final, observado, by = c("anio", "grupo_etario"), suffixes = c("_sim", "_obs"))
comp_final[, `:=`(dif_ocasional = prop_ocasional_sim - prop_ocasional_obs,
                   dif_diario = prop_diario_sim - prop_diario_obs)]
setorder(comp_final, anio, grupo_etario)
fwrite(comp_final, file.path(out_dir, sprintf("43_bondad_ajuste_calibracion%s.csv", SUF)))
log_msg("Bondad de ajuste (submuestra de calibracion, no la corrida final de R/26):\n%s",
        paste(capture.output(print(comp_final[, .(anio, grupo_etario, prop_ocasional_sim, prop_ocasional_obs,
                                                     prop_diario_sim, prop_diario_obs)])), collapse = "\n"))
log_msg("Diferencia absoluta media: ocasional %.4f, diario %.4f",
        mean(abs(comp_final$dif_ocasional)), mean(abs(comp_final$dif_diario)))

log_msg("\n=== outputs: 43_momentos_observados_ech.csv, 43_parametros_calibrados.csv, 43_bondad_ajuste_calibracion.csv ===")
close(log_con)
