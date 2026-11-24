# 74_descomposicion_alguna_vez.R
#
# Decomposition of the error of the retrospective run into the share who ever smoked and the share
# of those who still smoke, with the current model (one run, seed 20260809).
# Input:  synthetic population 2011, output/tables/43_*, 86_descomposicion_ever_split.csv
# Output: output/tables/95_descomposicion_alguna_vez_vigente.csv

library(here); library(data.table)
here::i_am("R/74_descomposicion_alguna_vez.R")
int_dir <- here::here("data", "interim"); out_dir <- here::here("output", "tables")
log_con <- file(here::here("logs", "74_descomposicion_alguna_vez.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { m <- sprintf(...); cat(m, "\n"); cat(m, "\n", file = log_con) }
log_msg("=== 74_descomposicion_alguna_vez.R -- %s ===", format(Sys.time()))
source(here::here("R", "24_motor_transiciones.R"))
params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
pe <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor)
pd <- params_cal[parametro == "raw_par3"]$valor
insumos <- cargar_insumos_motor()
ANIOS <- c(2014L, 2017L, 2018L, 2019L, 2021L, 2022L, 2023L, 2024L, 2025L)
set.seed(20260809)
pop <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"))
pop[, sexo := factor(sexo, levels = c("varon", "mujer"))]
pop[, tercil := factor(tercil, levels = c(1, 2, 3))]
res <- list(); a <- 2011L
for (obj in ANIOS) {
  while (a < obj) {
    pop <- ciclo_anual(pop, a, insumos$modelos, insumos$tabla_superv, insumos$precio_lookup,
                       insumos$pob_ine, insumos$entrada_15_por_anio, pe, pd)
    a <- a + 1L
  }
  res[[as.character(obj)]] <- pop[, .(ever_sim = mean(estado != 1L), prev_sim = mean(estado %in% c(2L, 3L))),
    by = .(ge = fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+"))][, anio := obj]
}
S <- rbindlist(res)[, phi_sim := prev_sim / ever_sim]
O <- fread(file.path(out_dir, "86_descomposicion_ever_split.csv"))[, .(anio, ge, ever_obs, phi_obs, prev_obs)]
X <- merge(S, O, by = c("anio", "ge"))
X[, `:=`(error_total = prev_sim - prev_obs,
         comp_ever = (ever_sim - ever_obs) * (phi_sim + phi_obs) / 2,
         comp_split = (phi_sim - phi_obs) * (ever_sim + ever_obs) / 2)]
X[, pct_split := 100 * comp_split / error_total]
fwrite(X, file.path(out_dir, "95_descomposicion_alguna_vez_vigente.csv"))
log_msg("%s", paste(capture.output(print(X[ge %in% c("45-64", "65+"), .(anio, ge, err = round(100 * error_total, 2),
  ever = round(100 * comp_ever, 2), split = round(100 * comp_split, 2), pct_split = round(pct_split, 1),
  phi_obs = round(phi_obs, 3), phi_sim = round(phi_sim, 3))])), collapse = "\n"))
log_msg("Rango pct_split 65+: %s; 45-64: %s", paste(round(range(X[ge == "65+"]$pct_split), 1), collapse = " a "),
        paste(round(range(X[ge == "45-64"]$pct_split), 1), collapse = " a "))
close(log_con)
