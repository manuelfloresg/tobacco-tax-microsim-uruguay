# 45_sensibilidad_elasticidad_t1.R
#
# Envelope over the income gradient of the price elasticity: the primary scenario re-run with
# T1/T3 elasticity ratios from 0.2 to 3.0 at a constant aggregate elasticity, and with the
# alternative aggregate level (-0.774).
# Usage:  Rscript R/45_sensibilidad_elasticidad_t1.R C   (the variants reported; about 3 hours)
# Input:  synthetic population 2011, output/tables/43_*, 59_*, 74_*
# Output: output/tables/72_*.csv

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/45_sensibilidad_elasticidad_t1.R")

log_con <- file(here::here("logs", "45_sensibilidad_elasticidad_t1.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 45_sensibilidad_elasticidad_t1.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

source(here::here("R", "24_motor_transiciones.R"))

ANIOS_SNAP <- c(2025L, seq(2030L, 2060L, by = 5L))
ANIO_MAX_PRECIO <- 2070L
ANIO_INICIO_ESCENARIO <- 2027L
HORIZONTE_GRADUAL <- 5L
SHARE_OMS <- 0.75

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params_escalada <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor)
param_downshift <- params_cal[parametro == "raw_par3"]$valor

ELAST_BASE <- c(`1` = -1.391, `2` = -0.639, `3` = -0.639)

log_msg("\n--- 1) Variantes del gradiente por tercil ---")

arch_pesos <- file.path(out_dir, "74_pesos_consumo_tercil.csv")
if (!file.exists(arch_pesos)) stop("Falta ", arch_pesos, " -- correr R/47_especificacion_elasticidad.R primero.")
pesos_dt <- fread(arch_pesos)[order(tercil)]
w <- setNames(pesos_dt$share_consumo, as.character(pesos_dt$tercil))
stopifnot(abs(sum(w) - 1) < 1e-8)
elast_agregada <- sum(w * ELAST_BASE[names(w)])
log_msg("Participacion en el consumo (rama base 2025): T1=%.4f T2=%.4f T3=%.4f", w["1"], w["2"], w["3"])
log_msg("Elasticidad agregada ponderada por consumo de la variante base: %.4f", elast_agregada)

gradiente <- function(forma) {
  e <- forma / sum(w * forma) * elast_agregada
  names(e) <- names(w)
  e
}
VARIANTES <- list(
  base       = ELAST_BASE,
  homogenea  = gradiente(c(`1` = 1, `2` = 1, `3` = 1)),
  iarc       = NULL,
  invertida  = gradiente(c(`1` = 1, `2` = 1.391 / 0.639, `3` = 1.391 / 0.639)),
  agregado_639 = c(`1` = -0.639, `2` = -0.639, `3` = -0.639),
  ferre_774    = c(`1` = -0.774, `2` = -0.774, `3` = -0.774)
)
GRUPO <- c(base = "A", homogenea = "A", iarc = "A", invertida = "A",
           agregado_639 = "B", ferre_774 = "B")
x <- (elast_agregada - w["1"] * (-1.0)) / (w["2"] + w["3"])
VARIANTES$iarc <- c(`1` = -1.0, `2` = as.numeric(x), `3` = as.numeric(x))

arch_espec <- file.path(out_dir, "74_especificacion_elasticidad.csv")
if (!file.exists(arch_espec)) stop("Falta ", arch_espec, " -- correr R/47_especificacion_elasticidad.R primero.")
espec <- fread(arch_espec)
for (i in seq_len(nrow(espec))) {
  nm <- paste0("C_", espec$caso[i])
  VARIANTES[[nm]] <- c(`1` = espec$T1[i], `2` = espec$T2[i], `3` = espec$T3[i])
  GRUPO[nm] <- "C"
}

args_cli <- commandArgs(trailingOnly = TRUE)

arg_solo   <- grep("^solo=", args_cli, value = TRUE)
args_grupo <- setdiff(args_cli, arg_solo)
GRUPOS_A_CORRER <- if (length(args_grupo) > 0) toupper(args_grupo) else c("A", "B", "C")
VARIANTES_A_CORRER <- if (length(arg_solo) > 0) {
  trimws(unlist(strsplit(sub("^solo=", "", arg_solo), ",")))
} else NULL
if (!is.null(VARIANTES_A_CORRER)) {
  faltan <- setdiff(VARIANTES_A_CORRER, names(VARIANTES))
  if (length(faltan) > 0) {
    stop("solo= nombra variantes inexistentes: ", paste(faltan, collapse = ", "),
         "
Disponibles: ", paste(names(VARIANTES), collapse = ", "))
  }
}
log_msg("Grupos a correr: %s", paste(GRUPOS_A_CORRER, collapse = ", "))
if (!is.null(VARIANTES_A_CORRER)) {
  log_msg("Filtro por variante: %s (las demas conservan sus filas previas)",
          paste(VARIANTES_A_CORRER, collapse = ", "))
}

tab_var <- rbindlist(lapply(names(VARIANTES), function(nm) {
  e <- VARIANTES[[nm]]
  data.table(variante = nm, grupo = GRUPO[[nm]], T1 = e["1"], T2 = e["2"], T3 = e["3"],
             agregada = sum(w * e[names(w)]), gradiente_T1_sobre_T3 = e["1"] / e["3"])
}))
fwrite(tab_var, file.path(out_dir, "72_variantes_elasticidad.csv"))
log_msg("Variantes (todas con la misma elasticidad agregada):\n%s",
        paste(capture.output(print(tab_var, digits = 4)), collapse = "\n"))

log_msg("\n--- 2) Trayectoria de referencia (conv75_gradual) ---")

insumos <- cargar_insumos_motor(regla_precio = "mantenimiento_politica", anio_max_precio = ANIO_MAX_PRECIO)
precio_base <- insumos$precio_lookup
ultimo_anio_dato <- attr(precio_base, "ultimo_anio_dato")
tray_tc <- fread(file.path(out_dir, "59_trayectorias_impuesto.csv"))[escenario_impuesto == "conv75_gradual"]
stopifnot(nrow(tray_tc) > 0, "factor_imesi" %in% names(tray_tc))
tray_imp <- tray_tc[, .(anio, factor_impuesto_real)]
factor_obj <- max(tray_tc$factor_imesi)
log_msg("factor objetivo sobre tau_ancla = %.4f (+%.1f%% real), rampa %d-%d",
        factor_obj, 100 * (factor_obj - 1), ANIO_INICIO_ESCENARIO,
        ANIO_INICIO_ESCENARIO + HORIZONTE_GRADUAL - 1L)

insumos_esc <- cargar_insumos_motor(regla_precio = "mantenimiento_politica",
                                     anio_max_precio = ANIO_MAX_PRECIO,
                                     trayectoria_impuesto = tray_imp)
precio_esc <- insumos_esc$precio_lookup

log_msg("\n--- 3) Corridas ---")

correr <- function(precio_lookup, precio_ref = NULL, semilla = 20260809) {
  set.seed(semilla)
  pop <- cargar_poblacion_proyeccion()
  snaps <- list(); a <- ANIO_POB_PROYECCION
  for (obj in ANIOS_SNAP) {
    while (a < obj) {
      ov <- if (is.null(precio_ref)) NULL else {
        r <- precio_lookup[anio == a]$p_reales / precio_ref[anio == a]$p_reales
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

resumir_tercil <- function(snaps, precio_lookup, etiqueta, variante) {
  rbindlist(lapply(snaps, function(dt) {
    a <- dt$anio_snapshot[1]
    d <- copy(dt)
    d[, cig := calcular_cig_dia(d, a, insumos$intensidad, precio_lookup)]
    tot <- sum(d$cig)
    out <- d[cig > 0, .(N_fumadores = .N, prevalencia_tercil = .N / d[tercil == .BY$tercil, .N],
                        consumo_mill = sum(cig) * 365 / 1e6, share_consumo = sum(cig) / tot), by = tercil]
    out[, `:=`(escenario = etiqueta, variante = variante, anio = a)]
    out
  }))
}

resultados <- list()
for (nm in names(VARIANTES)) {
  if (!GRUPO[[nm]] %in% GRUPOS_A_CORRER) next
  if (!is.null(VARIANTES_A_CORRER) && !nm %in% VARIANTES_A_CORRER) next
  e <- VARIANTES[[nm]]
  assign("ELASTICIDAD_TOTAL_TERCIL", e, envir = .GlobalEnv)
  assign("ELASTICIDAD_EXTENSIVA_TERCIL", e * SHARE_EXTENSIVO_TERCIL[names(e)], envir = .GlobalEnv)
  assign("ELASTICIDAD_INTENSIVA_TERCIL", e * (1 - SHARE_EXTENSIVO_TERCIL[names(e)]), envir = .GlobalEnv)
  t0 <- Sys.time()
  s_base <- correr(precio_base, NULL)
  s_esc  <- correr(precio_esc, precio_base)
  resultados[[nm]] <- rbind(resumir_tercil(s_base, precio_base, "base", nm),
                            resumir_tercil(s_esc, precio_esc, "conv75_gradual", nm))
  log_msg("Variante '%s' (T1=%.3f, T2=%.3f, T3=%.3f) corrida en %.1f min", nm,
          e["1"], e["2"], e["3"],
          as.numeric(difftime(Sys.time(), t0, units = "mins")))
}
R <- rbindlist(resultados)

fundir_con_previo <- function(nuevo, archivo, col_variante = "variante") {
  ruta <- file.path(out_dir, archivo)
  if (file.exists(ruta) && !identical(sort(GRUPOS_A_CORRER), c("A", "B", "C"))) {
    previo <- fread(ruta)
    if (col_variante %in% names(previo)) {
      previo <- previo[!get(col_variante) %in% unique(nuevo[[col_variante]])]
      if (nrow(previo) > 0) {
        log_msg("Conservadas %d filas de variantes no corridas en %s.", nrow(previo), archivo)
        nuevo <- rbind(previo, nuevo, fill = TRUE)
      }
    }
  }
  nuevo
}

R <- fundir_con_previo(R, "72_sensibilidad_t1_tercil.csv")
fwrite(R, file.path(out_dir, "72_sensibilidad_t1_tercil.csv"))

log_msg("\n--- 4) Progresividad: participacion de T1 en el consumo, escenario contra base ---")

prog <- dcast(R[, .(variante, escenario, anio, tercil, share_consumo)],
              variante + anio + tercil ~ escenario, value.var = "share_consumo")
setnames(prog, c("base", "conv75_gradual"), c("share_base", "share_esc"))
prog[, dif_pp := 100 * (share_esc - share_base)]
setorder(prog, variante, tercil, anio)
fwrite(prog, file.path(out_dir, "72_sensibilidad_t1_progresividad.csv"))

tabla <- dcast(prog[anio %in% c(2030, 2045, 2060) & tercil == 1], variante ~ anio, value.var = "dif_pp")
log_msg("Cambio en la participacion de T1 en el consumo (escenario - base, pp):\n%s",
        paste(capture.output(print(tabla, digits = 3)), collapse = "\n"))
log_msg("Negativo = el escenario reduce la participacion de T1 en el consumo respecto del")
log_msg("contrafactico, que es el resultado de 'progresividad dinamica'.")

consumo <- dcast(R[, .(variante, escenario, anio, tercil, consumo_mill)],
                 variante + anio + tercil ~ escenario, value.var = "consumo_mill")
setnames(consumo, c("base", "conv75_gradual"), c("consumo_base", "consumo_esc"))
consumo[, caida_pct := 100 * (consumo_esc / consumo_base - 1)]
fwrite(consumo, file.path(out_dir, "72_sensibilidad_t1_consumo.csv"))
log_msg("\nCaida del consumo a 2060 por tercil y variante (%% contra la base del mismo anio):\n%s",
        paste(capture.output(print(dcast(consumo[anio == 2060], variante ~ tercil, value.var = "caida_pct"), digits = 3)), collapse = "\n"))

log_msg("\n=== outputs: 72_variantes_elasticidad.csv, 72_sensibilidad_t1_tercil.csv,")
log_msg("    72_sensibilidad_t1_progresividad.csv, 72_sensibilidad_t1_consumo.csv ===")
close(log_con)
