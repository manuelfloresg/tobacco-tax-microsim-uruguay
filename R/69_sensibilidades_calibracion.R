# 69_sensibilidades_calibracion.R
#
# Two sensitivity analyses run as paired comparisons with the primary specification: an
# alternative calibration point for the occasional/daily transitions, and a former-smoker mortality
# risk graded by time since quitting.
# Usage:  Rscript R/69_sensibilidades_calibracion.R <principal|punto_alt|rr_graduado> <seed>
#         Rscript R/69_sensibilidades_calibracion.R fit
# Input:  output/tables/42_supervivencia_por_estado_graduado.csv, 43_parametros_calibrados*.csv
# Output: output/tables/93_sensibilidades_calibracion.csv

library(here)
library(data.table)
here::i_am("R/69_sensibilidades_calibracion.R")

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")
dir_r69 <- file.path(int_dir, "r69")
dir.create(dir_r69, showWarnings = FALSE)

ARGS <- commandArgs(trailingOnly = TRUE)
VARIANTES <- c("principal", "punto_alt", "rr_graduado")
SEMILLAS  <- 20260809 + 0:2
ANIOS_SNAP <- c(2030L, 2045L, 2060L)
ANIO_INICIO_MUERTES <- 2027L
ESC_PRINCIPAL <- "conv75_gradual"

if (length(ARGS) >= 1 && ARGS[1] == "fit") {
  log_con <- file(here::here("logs", "69_sensibilidades_calibracion_fit.log"), open = "wt", encoding = "UTF-8")
  log_msg <- function(...) { m <- sprintf(...); cat(m, "\n"); cat(m, "\n", file = log_con) }
  fs <- list.files(dir_r69, pattern = "\\.csv$", full.names = TRUE)
  d <- rbindlist(lapply(fs, fread))
  log_msg("Corridas leidas: %d (%s)", length(fs), paste(basename(fs), collapse = ", "))
  porsem <- d[, .(
    cons_pct  = 100 * (consumo_mill[rama == "esc"] / consumo_mill[rama == "base"] - 1),
    prev_pp   = 100 * (prevalencia[rama == "esc"] - prevalencia[rama == "base"]),
    prev_base = 100 * prevalencia[rama == "base"],
    muertes_evitadas = muertes_acum[rama == "base"] - muertes_acum[rama == "esc"]),
    by = .(variante, semilla, anio)]
  res <- porsem[, .(n_semillas = .N,
                    cons_pct = mean(cons_pct), cons_pct_ee = sd(cons_pct) / sqrt(.N),
                    prev_pp = mean(prev_pp), prev_base = mean(prev_base),
                    muertes_evitadas = mean(muertes_evitadas),
                    muertes_evitadas_ee = sd(muertes_evitadas) / sqrt(.N)),
                by = .(variante, anio)]
  pr <- porsem[variante == "principal", .(semilla, anio, c0 = cons_pct, p0 = prev_pp, m0 = muertes_evitadas)]
  dif <- merge(porsem[variante != "principal"], pr, by = c("semilla", "anio"))
  dif <- dif[, .(d_cons_pct = mean(cons_pct - c0), d_cons_pct_ee = sd(cons_pct - c0) / sqrt(.N),
                 d_prev_pp = mean(prev_pp - p0),
                 d_muertes = mean(muertes_evitadas - m0), d_muertes_ee = sd(muertes_evitadas - m0) / sqrt(.N)),
             by = .(variante, anio)]
  res <- merge(res, dif, by = c("variante", "anio"), all.x = TRUE)
  setorder(res, variante, anio)
  log_msg("%s", paste(capture.output(print(res, digits = 4)), collapse = "\n"))
  fwrite(res, file.path(out_dir, "93_sensibilidades_calibracion.csv"))
  log_msg("=== output: 93_sensibilidades_calibracion.csv ===")
  close(log_con)
  quit(save = "no")
}

VARIANTE <- ARGS[1]; SEMILLA <- as.numeric(ARGS[2])
stopifnot(VARIANTE %in% VARIANTES, !is.na(SEMILLA))
log_con <- file(here::here("logs", sprintf("69_sensibilidades_calibracion_%s_%d.log", VARIANTE, SEMILLA)),
                open = "wt", encoding = "UTF-8")
log_msg <- function(...) { m <- sprintf(...); cat(m, "\n"); cat(m, "\n", file = log_con) }
log_msg("=== 69_sensibilidades_calibracion.R %s %d -- %s ===", VARIANTE, SEMILLA, format(Sys.time()))
source(here::here("R", "24_motor_transiciones.R"))

archivo_par <- if (VARIANTE == "punto_alt") "43_parametros_calibrados_alt.csv" else "43_parametros_calibrados.csv"
params_cal <- fread(file.path(out_dir, archivo_par))
params_escalada <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor)
param_downshift <- params_cal[parametro == "raw_par3"]$valor
log_msg("Parametros de %s: escalada %.6f / %.6f, descenso %.6g", archivo_par,
        params_cal[parametro == "escalada_nivel_15"]$valor, params_cal[parametro == "escalada_decaimiento"]$valor,
        params_cal[parametro == "downshift_constante"]$valor)

insumos <- cargar_insumos_motor(regla_precio = "mantenimiento_politica", anio_max_precio = 2070L)
if (VARIANTE == "rr_graduado") {
  insumos$tabla_superv <- fread(file.path(out_dir, "42_supervivencia_por_estado_graduado.csv"))
  log_msg("Tabla de supervivencia: 42_supervivencia_por_estado_graduado.csv")
}
precio_base <- insumos$precio_lookup
tray <- fread(file.path(out_dir, "59_trayectorias_impuesto.csv"))[
  escenario_impuesto == ESC_PRINCIPAL, .(anio, factor_impuesto_real)]
precio_esc <- construir_precio_lookup(2010, 2070L, regla = "mantenimiento_politica", trayectoria_impuesto = tray)

correr <- function(precio_lookup, precio_base = NULL) {
  set.seed(SEMILLA)
  pop <- cargar_poblacion_proyeccion()
  snaps <- list(); a <- ANIO_POB_PROYECCION
  for (obj in ANIOS_SNAP) {
    while (a < obj) {
      ov <- if (is.null(precio_base)) NULL else {
        r <- precio_lookup[anio == a]$p_reales / precio_base[anio == a]$p_reales
        if (isTRUE(all.equal(r, 1, tolerance = 1e-12))) NULL else r
      }
      pop <- ciclo_anual(pop, a, insumos$modelos, insumos$tabla_superv, precio_lookup,
                         insumos$pob_ine, insumos$entrada_15_por_anio, params_escalada,
                         param_downshift, precio_ratio_override = ov)
      a <- a + 1L
    }
    snaps[[as.character(obj)]] <- copy(pop)[, anio_snapshot := obj]
  }
  snaps
}

out <- list()
for (rama in c("base", "esc")) {
  pl <- if (rama == "base") precio_base else precio_esc
  t0 <- Sys.time()
  activar_registro_muertes()
  snaps <- correr(pl, if (rama == "base") NULL else precio_base)
  m <- muertes_registradas(); desactivar_registro_muertes()
  out[[rama]] <- rbindlist(lapply(snaps, function(dt) {
    a <- dt$anio_snapshot[1]
    cig <- calcular_cig_dia(dt, a, insumos$intensidad, pl)
    data.table(variante = VARIANTE, semilla = SEMILLA, rama = rama, anio = a,
               prevalencia = mean(cig > 0), consumo_mill = sum(cig) * 365 / 1e6,
               muertes_acum = m[anio >= ANIO_INICIO_MUERTES & anio < a, sum(muertes)])
  }))
  log_msg("Rama %s: %.1f min", rama, as.numeric(difftime(Sys.time(), t0, units = "mins")))
}
res <- rbindlist(out)
log_msg("%s", paste(capture.output(print(res)), collapse = "\n"))
fwrite(res, file.path(dir_r69, sprintf("%s_%d.csv", VARIANTE, SEMILLA)))
close(log_con)
