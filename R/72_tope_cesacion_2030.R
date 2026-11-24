# 72_tope_cesacion_2030.R
#
# Sensitivity analysis: the calendar trend of the cessation rate is held from 2030 instead of 2025.
# Paired with the primary specification on the same seeds.
# Usage:  Rscript R/72_tope_cesacion_2030.R <principal|tope_2030> <seed>
#         Rscript R/72_tope_cesacion_2030.R resumir
# Output: output/tables/diagnosticos/tope2030/

library(here); library(data.table)
here::i_am("R/72_tope_cesacion_2030.R")
int_dir <- here::here("data", "interim"); out_dir <- here::here("output", "tables")
dir_sal <- here::here("output", "tables", "diagnosticos", "tope2030")
dir.create(dir_sal, recursive = TRUE, showWarnings = FALSE)
ARGS <- commandArgs(trailingOnly = TRUE)

if (length(ARGS) >= 1 && ARGS[1] == "resumir") {
  d <- rbindlist(lapply(list.files(dir_sal, "^(principal|tope_2030)_[0-9]+[.]csv$", full.names = TRUE), fread))
  w <- d[, .(prev_base = 100 * prevalencia[rama == "base"],
             brecha_base = 100 * (prev_T1 - prev_T3)[rama == "base"],
             brecha_esc = 100 * (prev_T1 - prev_T3)[rama == "esc"],
             cons_pct = 100 * (consumo_mill[rama == "esc"] / consumo_mill[rama == "base"] - 1),
             prev_pp = 100 * (prevalencia[rama == "esc"] - prevalencia[rama == "base"]),
             muertes = muertes_acum[rama == "base"] - muertes_acum[rama == "esc"]),
         by = .(variante, semilla, anio)]
  t25 <- fread(file.path(out_dir, "59_espacio_escenarios_tercil.csv"))[escenario_impuesto == "base" & anio == 2025]
  b25 <- 100 * (t25[tercil == 1]$prevalencia - t25[tercil == 3]$prevalencia)
  w[, pct_ensanch := 100 * (brecha_base - brecha_esc) / (brecha_base - b25)]
  m <- w[, lapply(.SD, mean), by = .(variante, anio), .SDcols = -"semilla"]
  print(m[order(anio, variante)], digits = 4)
  p <- merge(w[variante == "tope_2030"], w[variante == "principal"], by = c("semilla", "anio"))
  cat("\nDiferencias pareadas tope_2030 - principal (media, EE):\n")
  for (v in c("prev_base", "cons_pct", "prev_pp", "brecha_base", "pct_ensanch", "muertes")) {
    x <- p[[paste0(v, ".x")]] - p[[paste0(v, ".y")]]
    r <- p[, .(m = mean(x[.I]), ee = sd(x[.I]) / sqrt(.N)), by = anio]
    cat(v, ":", paste(sprintf("%d %.3f (%.3f)", r$anio, r$m, r$ee), collapse = " | "), "\n")
  }
  fwrite(m, file.path(dir_sal, "resumen_tope2030.csv"))
  quit(save = "no")
}

VARIANTES <- c("principal", "tope_2030")
ANIOS_SNAP <- c(2030L, 2035L, 2045L, 2060L)
ANIO_INICIO_MUERTES <- 2027L
ESC_PRINCIPAL <- "conv75_gradual"
VARIANTE <- ARGS[1]; SEMILLA <- as.numeric(ARGS[2])
stopifnot(VARIANTE %in% VARIANTES, !is.na(SEMILLA))
log_con <- file(here::here("logs", sprintf("72_tope_cesacion_%s_%d.log", VARIANTE, SEMILLA)), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { m <- sprintf(...); cat(m, "\n"); cat(m, "\n", file = log_con) }
log_msg("=== 72_tope_cesacion_2030.R %s %d -- %s ===", VARIANTE, SEMILLA, format(Sys.time()))
source(here::here("R", "24_motor_transiciones.R"))
if (VARIANTE == "tope_2030") {
  txt <- paste(deparse(aplicar_cesacion, width.cutoff = 500L), collapse = "\n")
  n <- lengths(regmatches(txt, gregexpr("ANIO_TOPE_TENDENCIA", txt, fixed = TRUE)))
  stopifnot(n == 1)
  txt <- sub("ANIO_TOPE_TENDENCIA", "2030L", txt, fixed = TRUE)
  aplicar_cesacion <- eval(parse(text = txt))
  stopifnot(grepl("2030L", paste(deparse(aplicar_cesacion), collapse = "")))
  log_msg("aplicar_cesacion parcheada: tope 2030")
}

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params_escalada <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor)
param_downshift <- params_cal[parametro == "raw_par3"]$valor

insumos <- cargar_insumos_motor(regla_precio = "mantenimiento_politica", anio_max_precio = 2070L)
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
               prev_T1 = mean(cig[dt$tercil == 1] > 0), prev_T2 = mean(cig[dt$tercil == 2] > 0),
               prev_T3 = mean(cig[dt$tercil == 3] > 0),
               muertes_acum = m[anio >= ANIO_INICIO_MUERTES & anio < a, sum(muertes)])
  }))
  log_msg("Rama %s: %.1f min", rama, as.numeric(difftime(Sys.time(), t0, units = "mins")))
}
res <- rbindlist(out)
log_msg("%s", paste(capture.output(print(res)), collapse = "\n"))
fwrite(res, file.path(dir_sal, sprintf("%s_%d.csv", VARIANTE, SEMILLA)))
close(log_con)
