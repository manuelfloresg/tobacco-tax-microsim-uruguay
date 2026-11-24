# 73_recaida_cota_inferior.R
#
# Sensitivity analysis: relapse set at the lower bound of the ECH flow balance, from age 65
# (cota_65) or from age 45 (cota_45). Saves the projection and the retrospective run by age group.
# Paired with the primary specification on the same seeds.
# Usage:  Rscript R/73_recaida_cota_inferior.R <principal|cota_65|cota_45> <seed>
#         Rscript R/73_recaida_cota_inferior.R resumir
# Output: output/tables/diagnosticos/recaida_cota/

library(here); library(data.table)
here::i_am("R/73_recaida_cota_inferior.R")
int_dir <- here::here("data", "interim"); out_dir <- here::here("output", "tables")
dir_sal <- here::here("output", "tables", "diagnosticos", "recaida_cota")
dir.create(dir_sal, recursive = TRUE, showWarnings = FALSE)
ARGS <- commandArgs(trailingOnly = TRUE)

if (length(ARGS) >= 1 && ARGS[1] == "resumir") {
  P <- rbindlist(lapply(list.files(dir_sal, "^(principal|cota_65|cota_45)_[0-9]+[.]csv$", full.names = TRUE), fread))
  B <- rbindlist(lapply(list.files(dir_sal, "^back_.*[.]csv$", full.names = TRUE), fread))
  T0f <- list.files(here::here("output", "tables", "diagnosticos", "tope2030"), "^principal_[0-9]+[.]csv$", full.names = TRUE)
  if (length(T0f)) {
    g <- merge(P[variante == "principal"], rbindlist(lapply(T0f, fread)), by = c("semilla", "rama", "anio"), suffixes = c("", ".t"))
    cat("Guardarrail contra R/72 principal: filas", nrow(g), " max |dif prev|", max(abs(g$prevalencia - g$prevalencia.t)),
        " max |dif cons|", max(abs(g$consumo_mill - g$consumo_mill.t)), "\n\n")
  }
  w <- dcast(P, variante + semilla + anio ~ rama, value.var = c("prevalencia", "consumo_mill", "prev_T1", "prev_T3", "muertes_acum"))
  w[, `:=`(cons_pct = 100 * (consumo_mill_esc / consumo_mill_base - 1),
           prev_pp = 100 * (prevalencia_esc - prevalencia_base),
           prev_base = 100 * prevalencia_base, cons_base = consumo_mill_base,
           brecha_base = 100 * (prev_T1_base - prev_T3_base),
           cambio_brecha = 100 * ((prev_T1_esc - prev_T3_esc) - (prev_T1_base - prev_T3_base)),
           muertes = muertes_acum_base - muertes_acum_esc)]
  b25 <- w[anio == 2025, .(semilla, variante, b25 = brecha_base)]
  w <- merge(w, b25, by = c("semilla", "variante"))
  w[, pct_remueve := 100 * (-cambio_brecha) / (brecha_base - b25)]
  med <- w[, lapply(.SD, mean), by = .(variante, anio),
           .SDcols = c("prev_base", "cons_base", "cons_pct", "prev_pp", "brecha_base", "pct_remueve", "muertes")]
  print(med[order(anio, variante)], digits = 4)
  cat("\nDiferencias pareadas contra la principal (media, EE de las semillas):\n")
  pr <- w[variante == "principal"]
  for (v in c("cota_65", "cota_45")) {
    d <- merge(w[variante == v], pr, by = c("semilla", "anio"), suffixes = c("", ".p"))
    for (col in c("prev_base", "cons_base", "cons_pct", "prev_pp", "pct_remueve", "muertes")) {
      d[, dd := get(col) - get(paste0(col, ".p"))]
      r <- d[, .(m = mean(dd), ee = sd(dd) / sqrt(.N)), by = anio][anio %in% c(2030, 2045, 2060)]
      cat(sprintf("%-8s %-12s %s\n", v, col, paste(sprintf("%d: %+.3f (%.3f)", r$anio, r$m, r$ee), collapse = "  ")))
    }
  }
  obs <- fread(file.path(out_dir, "43_momentos_observados_ech.csv"))
  obs[, prev_obs := prop_ocasional + prop_diario]
  bb <- merge(B[, .(prev = mean(prev)), by = .(variante, anio, grupo_etario)], obs[, .(anio, grupo_etario, prev_obs)],
              by = c("anio", "grupo_etario"))
  eam <- bb[, .(err = round(100 * mean(prev - prev_obs), 2), eam = round(100 * mean(abs(prev - prev_obs)), 2)),
            by = .(variante, grupo_etario)]
  cat("\nBackcast, error medio (sim - obs) y EAM en pp, nueve anios:\n")
  print(dcast(eam, grupo_etario ~ variante, value.var = c("err", "eam")))
  fwrite(med, file.path(dir_sal, "resumen_recaida_cota.csv"))
  fwrite(eam, file.path(dir_sal, "resumen_recaida_cota_backcast.csv"))
  quit(save = "no")
}

VARIANTES <- c(principal = NA, cota_65 = 65, cota_45 = 45)
FACTOR_RECAIDA <- 0.012 / 0.021
ANIOS_BACK <- c(2014L, 2017L, 2018L, 2019L, 2021L, 2022L, 2023L, 2024L, 2025L)
ANIOS_SNAP <- c(ANIOS_BACK, 2030L, 2035L, 2045L, 2060L)
ANIO_INICIO_MUERTES <- 2027L
ESC_PRINCIPAL <- "conv75_gradual"
VARIANTE <- ARGS[1]; SEMILLA <- as.numeric(ARGS[2])
stopifnot(VARIANTE %in% names(VARIANTES), !is.na(SEMILLA))
log_con <- file(here::here("logs", sprintf("73_recaida_cota_%s_%d.log", VARIANTE, SEMILLA)), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { m <- sprintf(...); cat(m, "\n"); cat(m, "\n", file = log_con) }
log_msg("=== 73_recaida_cota_inferior.R %s %d -- %s ===", VARIANTE, SEMILLA, format(Sys.time()))
source(here::here("R", "24_motor_transiciones.R"))

EDAD_CORTE <- VARIANTES[[VARIANTE]]
if (!is.na(EDAD_CORTE)) {
  aplicar_recaida <- function(pop, modelo_recaida) {
    expuesto <- pop$estado %in% c(4, 5)
    if (!RECAIDA_EN_CICLO_DE_CESACION) expuesto <- expuesto & pop$anios_desde_dejar > 0
    idx <- which(expuesto)
    if (length(idx) == 0) return(pop)
    sub <- pop[idx]
    usa_edad <- recaida_usa_edad(modelo_recaida)
    nd <- if (usa_edad) {
      sub[, .(punto_medio = pmax(anios_desde_dejar, 0.05), sexo, grupo_edad = GRUPOS_EDAD_RECAIDA(edad))]
    } else {
      sub[, .(punto_medio = pmax(anios_desde_dejar, 0.05), sexo)]
    }
    haz <- predict(modelo_recaida, newdata = nd, type = "response")
    haz <- haz * ifelse(sub$edad >= EDAD_CORTE, FACTOR_RECAIDA, 1)
    evento <- runif(nrow(sub)) < pmin(1, pmax(0, haz))
    idx_evento <- idx[evento]
    pop[idx_evento, estado := fifelse(patron_antes_de_dejar == "diario", 3, 2)]
    pop[idx_evento, `:=`(anios_desde_dejar = NA_real_, patron_antes_de_dejar = NA_character_)]
    pop
  }
  log_msg("aplicar_recaida parcheada: factor %.4f desde los %d", FACTOR_RECAIDA, EDAD_CORTE)
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
  pop <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"))
  pop[, sexo := factor(sexo, levels = c("varon", "mujer"))]
  pop[, tercil := factor(tercil, levels = c(1, 2, 3))]
  pop <- asignar_u_intensidad(pop)
  snaps <- list(); a <- 2011L
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
    snaps[[as.character(obj)]] <- list(
      proy = if (obj >= 2025L) {
        cig <- calcular_cig_dia(pop, obj, insumos$intensidad, precio_lookup)
        data.table(anio = obj, prevalencia = mean(cig > 0), consumo_mill = sum(cig) * 365 / 1e6,
                   prev_T1 = mean(cig[pop$tercil == 1] > 0), prev_T2 = mean(cig[pop$tercil == 2] > 0),
                   prev_T3 = mean(cig[pop$tercil == 3] > 0))
      },
      back = if (obj %in% ANIOS_BACK) {
        pop[, .(prev = mean(estado %in% c(2L, 3L))),
            by = .(grupo_etario = fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+"))
            ][, anio := obj]
      })
  }
  snaps
}

out <- list(); back <- NULL
for (rama in c("base", "esc")) {
  pl <- if (rama == "base") precio_base else precio_esc
  t0 <- Sys.time()
  activar_registro_muertes()
  snaps <- correr(pl, if (rama == "base") NULL else precio_base)
  m <- muertes_registradas(); desactivar_registro_muertes()
  out[[rama]] <- rbindlist(lapply(snaps, `[[`, "proy"))[
    , `:=`(variante = VARIANTE, semilla = SEMILLA, rama = rama)][
    , muertes_acum := sapply(anio, function(a) m[anio >= ANIO_INICIO_MUERTES & anio < a, sum(muertes)])]
  if (rama == "base")
    back <- rbindlist(lapply(snaps, `[[`, "back"))[, `:=`(variante = VARIANTE, semilla = SEMILLA)]
  log_msg("Rama %s: %.1f min", rama, as.numeric(difftime(Sys.time(), t0, units = "mins")))
}
res <- rbindlist(out)
log_msg("%s", paste(capture.output(print(res)), collapse = "\n"))
fwrite(res, file.path(dir_sal, sprintf("%s_%d.csv", VARIANTE, SEMILLA)))
fwrite(back, file.path(dir_sal, sprintf("back_%s_%d.csv", VARIANTE, SEMILLA)))
close(log_con)
