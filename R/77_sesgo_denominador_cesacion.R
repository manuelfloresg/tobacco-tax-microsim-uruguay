# 77_sesgo_denominador_cesacion.R
#
# Bias of the cessation rate from its denominator: the ECH pool of people who smoke or quit in the
# past year includes people who started or relapsed during that year. Measured with the model in
# the retrospective run.
# Input:  synthetic population 2011, output/tables/43_parametros_calibrados.csv
# Output: output/tables/99_sesgo_denominador_cesacion.csv

library(here); library(data.table)
here::i_am("R/77_sesgo_denominador_cesacion.R")
int_dir <- here::here("data", "interim"); out_dir <- here::here("output", "tables")
log_con <- file(here::here("logs", "77_sesgo_denominador_cesacion.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { m <- sprintf(...); cat(m, "\n"); cat(m, "\n", file = log_con) }
log_msg("=== 77_sesgo_denominador_cesacion.R -- %s ===", format(Sys.time()))
source(here::here("R", "24_motor_transiciones.R"))

CONT <- new.env(); CONT$ini <- 0L; CONT$rec <- 0L
orig_iniciacion <- aplicar_iniciacion
orig_recaida <- aplicar_recaida
aplicar_iniciacion <- function(pop, ...) {
  antes <- copy(pop$estado)
  pop <- orig_iniciacion(pop, ...)
  stopifnot(length(antes) == nrow(pop))
  CONT$ini <- CONT$ini + sum(antes == 1L & pop$estado %in% c(2L, 3L))
  pop
}
aplicar_recaida <- function(pop, ...) {
  antes <- copy(pop$estado)
  pop <- orig_recaida(pop, ...)
  stopifnot(length(antes) == nrow(pop))
  CONT$rec <- CONT$rec + sum(antes %in% c(4L, 5L) & pop$estado %in% c(2L, 3L))
  pop
}

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
pe <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor)
pd <- params_cal[parametro == "raw_par3"]$valor
insumos <- cargar_insumos_motor()
set.seed(20260809)
pop <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"))
pop[, sexo := factor(sexo, levels = c("varon", "mujer"))]
pop[, tercil := factor(tercil, levels = c(1, 2, 3))]
res <- list()
for (a in 2011L:2024L) {
  CONT$ini <- 0L; CONT$rec <- 0L
  pop <- ciclo_anual(pop, a, insumos$modelos, insumos$tabla_superv, insumos$precio_lookup,
                     insumos$pob_ine, insumos$entrada_15_por_anio, pe, pd)
  den <- pop[estado %in% c(2L, 3L, 4L), .N]
  ent15 <- pop[edad == 15L & estado %in% c(2L, 3L), .N]
  res[[length(res) + 1L]] <- data.table(anio = a + 1L, denominador = den, ex_recientes = pop[estado == 4L, .N],
                                        iniciaciones = CONT$ini, recaidas = CONT$rec, entran_15_fumando = ent15)
}
R <- rbindlist(res)
R[, `:=`(pct_iniciacion = 100 * iniciaciones / denominador, pct_recaida = 100 * recaidas / denominador,
         pct_entrada_15 = 100 * entran_15_fumando / denominador)]
R[, pct_total := pct_iniciacion + pct_recaida + pct_entrada_15]
R[, `:=`(tasa_estimada = ex_recientes / denominador,
         tasa_sin_entradas = ex_recientes / (denominador - iniciaciones - recaidas - entran_15_fumando))]
log_msg("%s", paste(capture.output(print(R, digits = 4)), collapse = "\n"))
log_msg("Proporcion media de entradas del anio en el denominador: %.2f%% (iniciacion %.2f, recaida %.2f, 15 anios %.2f)",
        mean(R$pct_total), mean(R$pct_iniciacion), mean(R$pct_recaida), mean(R$pct_entrada_15))
log_msg("Sesgo relativo medio de la tasa de cesacion: -%.1f%%", 100 * mean(1 - R$tasa_estimada / R$tasa_sin_entradas))
fwrite(R, file.path(out_dir, "99_sesgo_denominador_cesacion.csv"))
close(log_con)
