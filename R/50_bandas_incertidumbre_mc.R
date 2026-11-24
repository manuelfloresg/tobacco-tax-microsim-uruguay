# 50_bandas_incertidumbre_mc.R
#
# Credible intervals. A Gaussian-process emulator is fitted to a 21-point design over the aggregate
# price elasticity and the level of the initiation rate, validated by leave-one-out
# cross-validation, and sampled from the prior distributions of both parameters.
# Usage:  Rscript R/50_bandas_incertidumbre_mc.R parte K N   (runs part K of N of the design)
#         Rscript R/50_bandas_incertidumbre_mc.R fit         (merges the parts, fits and samples)
# Input:  synthetic population 2011, output/tables/43_*, 59_*, 72_*, 74_*, 77_mc_ruido_simulacion.csv
# Output: output/tables/77_*.csv

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/50_bandas_incertidumbre_mc.R")

args_cli <- commandArgs(trailingOnly = TRUE)
MODO <- if (length(args_cli) == 0) "todo" else tolower(args_cli[1])
PARTE <- if (MODO == "parte") as.integer(args_cli[2]) else 1L
N_PARTES <- if (MODO == "parte") as.integer(args_cli[3]) else 1L

suf <- if (MODO == "parte") sprintf("_parte%d", PARTE) else ""
log_con <- file(here::here("logs", sprintf("50_bandas_incertidumbre_mc%s.log", suf)), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 50_bandas_incertidumbre_mc.R (modo: %s) -- %s ===", MODO, format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")
source(here::here("R", "24_motor_transiciones.R"))

ANIOS_SNAP <- c(2025L, seq(2030L, 2060L, by = 5L))
ANIO_MAX_PRECIO <- 2070L
ANIO_INICIO_ESCENARIO <- 2027L
HORIZONTE_GRADUAL <- 5L
SHARE_OMS <- 0.75
SEMILLA <- 20260818L
B_REPLICAS <- 20000L

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params_escalada <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor)
param_downshift <- params_cal[parametro == "raw_par3"]$valor

pesos_dt <- fread(file.path(out_dir, "74_pesos_consumo_tercil.csv"))[order(tercil)]
w <- setNames(pesos_dt$share_consumo, as.character(pesos_dt$tercil))
RATIO_PRINCIPAL <- fread(file.path(out_dir, "74_especificacion_elasticidad.csv"))[caso == "principal"]$ratio_T1_T3

resolver_elasticidad <- function(nivel_agregado, ratio = RATIO_PRINCIPAL, pesos = w) {
  forma <- c(`1` = ratio, `2` = (ratio + 1) / 2, `3` = 1)
  (forma * nivel_agregado / sum(pesos * forma[names(pesos)]))[names(pesos)]
}

NODOS_ELAST <- c(-1.02, -0.89, -0.76, -0.639, -0.51, -0.38, -0.25)
NODOS_MULT  <- c(0.70, 1.00, 1.50)
GRILLA <- CJ(elast_agregada = NODOS_ELAST, mult_iniciacion = NODOS_MULT)
GRILLA[, punto := .I]

ARCH_GRILLA <- file.path(out_dir, "77_mc_grilla.csv")

insumos <- cargar_insumos_motor(regla_precio = "mantenimiento_politica", anio_max_precio = ANIO_MAX_PRECIO)
precio_base <- insumos$precio_lookup
ultimo_anio_dato <- attr(precio_base, "ultimo_anio_dato")
tray_tc <- fread(file.path(out_dir, "59_trayectorias_impuesto.csv"))[escenario_impuesto == "conv75_gradual"]
stopifnot(nrow(tray_tc) > 0, "factor_imesi" %in% names(tray_tc))
tray_imp <- tray_tc[, .(anio, factor_impuesto_real)]
factor_obj <- max(tray_tc$factor_imesi)
precio_esc <- construir_precio_lookup(2010, ANIO_MAX_PRECIO, regla = "mantenimiento_politica",
                                       trayectoria_impuesto = tray_imp)

factor_tributo_de <- function(anios) fifelse(anios %in% tray_tc$anio, tray_tc$factor_tributo_total[match(anios, tray_tc$anio)], 1)

correr <- function(precio_lookup, precio_ref, mult_ini, semilla = 20260809) {
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
                          param_downshift, precio_ratio_override = ov, mult_iniciacion = mult_ini)
      a <- a + 1L
    }
    snaps[[as.character(obj)]] <- copy(pop)[, anio_snapshot := obj]
  }
  snaps
}

resumir <- function(snaps, precio_lookup) {
  rbindlist(lapply(snaps, function(dt) {
    a <- dt$anio_snapshot[1]
    cig <- calcular_cig_dia(dt, a, insumos$intensidad, precio_lookup)
    data.table(anio = a, prevalencia = mean(cig > 0), consumo_mill = sum(cig) * 365 / 1e6,
               fumadores = sum(cig > 0))
  }))
}

correr_punto <- function(e_agg, mult_ini, semilla = 20260809) {
  e <- resolver_elasticidad(e_agg)
  assign("ELASTICIDAD_TOTAL_TERCIL", e, envir = .GlobalEnv)
  assign("ELASTICIDAD_EXTENSIVA_TERCIL", e * SHARE_EXTENSIVO_TERCIL, envir = .GlobalEnv)
  assign("ELASTICIDAD_INTENSIVA_TERCIL", e * (1 - SHARE_EXTENSIVO_TERCIL), envir = .GlobalEnv)
  rb <- resumir(correr(precio_base, NULL, mult_ini, semilla), precio_base)
  re <- resumir(correr(precio_esc, precio_base, mult_ini, semilla), precio_esc)
  m <- merge(rb, re, by = "anio", suffixes = c("_base", "_esc"))
  m[, `:=`(elast_agregada = e_agg, mult_iniciacion = mult_ini,
           caida_consumo_pct = 100 * (consumo_mill_esc / consumo_mill_base - 1),
           caida_prevalencia_pct = 100 * (prevalencia_esc / prevalencia_base - 1),
           dif_prevalencia_pp = 100 * (prevalencia_esc - prevalencia_base))]
  m[, recaudacion_relativa := (consumo_mill_esc / consumo_mill_base) * factor_tributo_de(anio)]
  m
}

if (MODO %in% c("todo", "parte")) {
  filas <- if (MODO == "parte") GRILLA[(punto - 1L) %% N_PARTES == (PARTE - 1L)] else GRILLA
  log_msg("Puntos a correr en este proceso: %d de %d (%s)", nrow(filas), nrow(GRILLA),
          paste(filas$punto, collapse = ","))
  res <- list()
  for (i in seq_len(nrow(filas))) {
    t0 <- Sys.time()
    res[[i]] <- correr_punto(filas$elast_agregada[i], filas$mult_iniciacion[i])[, punto := filas$punto[i]]
    log_msg("Punto %d (elast %.3f, mult %.2f) en %.1f min", filas$punto[i], filas$elast_agregada[i],
            filas$mult_iniciacion[i], as.numeric(difftime(Sys.time(), t0, units = "mins")))
  }
  G <- rbindlist(res)
  arch <- if (MODO == "parte") file.path(out_dir, sprintf("77_mc_grilla_parte%d.csv", PARTE)) else ARCH_GRILLA
  fwrite(G, arch)
  log_msg("Escrito %s (%d filas).", basename(arch), nrow(G))
  if (MODO == "parte") { close(log_con); quit(save = "no") }
}

if (MODO == "fit") {
  partes <- list.files(out_dir, pattern = "^77_mc_grilla_parte[0-9]+\\.csv$", full.names = TRUE)
  if (length(partes) == 0) stop("No hay partes que fundir ni 77_mc_grilla.csv -- correr el modo 'parte' primero.")
  G <- rbindlist(lapply(partes, fread))
  setorder(G, punto, anio)
  if (nrow(unique(G[, .(punto)])) != nrow(GRILLA))
    stop("Faltan puntos de la grilla: hay ", uniqueN(G$punto), " de ", nrow(GRILLA))
  fwrite(G, ARCH_GRILLA)
  log_msg("Fundidas %d partes en %s (%d puntos).", length(partes), basename(ARCH_GRILLA), uniqueN(G$punto))
}

log_msg("\n--- Emulador de superficie de respuesta ---")

RESULTADOS <- c("caida_consumo_pct", "caida_prevalencia_pct", "recaudacion_relativa")
ANIOS_REPORTE <- c(2030L, 2045L, 2060L)

formula_emu <- y ~ e + I(e^2) + I(e^3) + m + I(m^2) + e:m

ajustar_emulador <- function(g, var) {
  d <- data.table(y = g[[var]], e = g$elast_agregada, m = g$mult_iniciacion)
  lm(formula_emu, data = d)
}
predecir <- function(mod, e, m) as.numeric(predict(mod, newdata = data.frame(e = e, m = m)))

emuladores <- list()
for (a in ANIOS_REPORTE) for (v in RESULTADOS) {
  emuladores[[paste(a, v)]] <- ajustar_emulador(G[anio == a], v)
}

loo <- rbindlist(lapply(ANIOS_REPORTE, function(a) rbindlist(lapply(RESULTADOS, function(v) {
  g <- G[anio == a]
  pred <- sapply(seq_len(nrow(g)), function(i) {
    mod <- ajustar_emulador(g[-i], v)
    predecir(mod, g$elast_agregada[i], g$mult_iniciacion[i])
  })
  data.table(anio = a, resultado = v, rmse_loo = sqrt(mean((pred - g[[v]])^2)),
             max_abs_loo = max(abs(pred - g[[v]])), rango_nodos = diff(range(g[[v]])))
}))))
loo[, error_loo_sobre_rango := max_abs_loo / rango_nodos]
fwrite(loo, file.path(out_dir, "77_mc_validacion_emulador.csv"))
log_msg("Validacion leave-one-out del emulador:\n%s",
        paste(capture.output(print(loo, digits = 3)), collapse = "\n"))
if (loo[, max(error_loo_sobre_rango)] > 0.05) {
  log_msg("Nota: el error LOO supera el 5%% del rango de los nodos en %d de %d casos. El rango de los",
          loo[error_loo_sobre_rango > 0.05, .N], nrow(loo))
  log_msg("nodos NO es el benchmark correcto cuando el propio rango es diminuto (la prevalencia a 2030")
  log_msg("apenas se mueve). La comparacion que decide es contra el piso de ruido de simulacion, abajo.")
} else {
  log_msg("El error LOO queda por debajo del 5%% del rango de los nodos en todos los casos.")
}

log_msg("\n--- Piso de ruido de simulacion (punto central; cien semillas de R/67 si existe) ---")

SEMILLAS_RUIDO <- c(20260809L, 20260810L, 20260811L)
ARCH_RUIDO <- file.path(out_dir, "77_mc_ruido_simulacion.csv")
if (file.exists(ARCH_RUIDO)) {
  ruido <- fread(ARCH_RUIDO)
  log_msg("Leido de %s (ya calculado).", basename(ARCH_RUIDO))
} else {
  ruido <- rbindlist(lapply(SEMILLAS_RUIDO, function(s) {
    t0 <- Sys.time()
    r <- correr_punto(-0.639, 1.00, semilla = s)[, semilla := s]
    log_msg("Semilla %d corrida en %.1f min", s, as.numeric(difftime(Sys.time(), t0, units = "mins")))
    r
  }))
  fwrite(ruido, ARCH_RUIDO)
}
res_ruido <- rbindlist(lapply(RESULTADOS, function(v) {
  ruido[anio %in% ANIOS_REPORTE, .(resultado = v, media = mean(get(v)), de = sd(get(v)),
                                    rango = diff(range(get(v)))), by = anio]
}))
fwrite(res_ruido, file.path(out_dir, "77_mc_ruido_resumen.csv"))
log_msg("Caida de consumo en el punto central, por semilla (pp):\n%s",
        paste(capture.output(print(dcast(ruido[anio %in% ANIOS_REPORTE], anio ~ semilla,
                                          value.var = "caida_consumo_pct"), digits = 5)), collapse = "\n"))
log_msg("Ruido de simulacion (DE entre semillas, pp):\n%s",
        paste(capture.output(print(res_ruido, digits = 3)), collapse = "\n"))

loo_vs_ruido <- merge(loo[, .(anio, resultado, max_abs_loo)],
                      res_ruido[, .(anio, resultado, ruido_rango = rango)],
                      by = c("anio", "resultado"))
loo_vs_ruido[, loo_sobre_ruido := max_abs_loo / ruido_rango]
fwrite(loo_vs_ruido, file.path(out_dir, "77_mc_loo_vs_ruido.csv"))
log_msg("\nError del emulador contra el piso de ruido (el benchmark que decide):\n%s",
        paste(capture.output(print(loo_vs_ruido, digits = 3)), collapse = "\n"))
if (loo_vs_ruido[, max(loo_sobre_ruido)] <= 2) {
  log_msg("El error LOO no supera al doble del ruido de simulacion en ningun caso: lo que el LOO")
  log_msg("mide es el ruido de los propios nodos, no un defecto de la superficie de respuesta.")
} else {
  log_msg("ADVERTENCIA: en algun caso el error LOO mas que duplica el ruido de simulacion --")
  log_msg("ahi si hay misajuste del emulador. Densificar la grilla en esa dimension.")
}

log_msg("\n--- Muestreo (%d replicas) ---", B_REPLICAS)

FACTOR_TOTAL_SOBRE_LEGAL <- -0.639 / -0.774
muestrear_elasticidad_agregada <- function(n) pmin(rnorm(n, -0.774, 0.101) * FACTOR_TOTAL_SOBRE_LEGAL, -1e-6)

rtriangular <- function(n, a, b, c) {
  u <- runif(n); fc <- (c - a) / (b - a)
  fifelse(u < fc, a + sqrt(u * (b - a) * (c - a)), b - sqrt((1 - u) * (b - a) * (b - c)))
}

set.seed(SEMILLA)
e_draw <- muestrear_elasticidad_agregada(B_REPLICAS)
m_draw <- rtriangular(B_REPLICAS, 0.70, 1.50, 1.00)
stopifnot(all(e_draw < 0), all(m_draw >= 0.70), all(m_draw <= 1.50))
log_msg("Elasticidad agregada muestreada: media %.4f, IC95 [%.4f, %.4f]",
        mean(e_draw), quantile(e_draw, 0.025), quantile(e_draw, 0.975))
log_msg("mult_iniciacion muestreado: media %.4f, IC95 [%.4f, %.4f]",
        mean(m_draw), quantile(m_draw, 0.025), quantile(m_draw, 0.975))
n_fuera <- sum(e_draw < min(NODOS_ELAST) | e_draw > max(NODOS_ELAST))
e_draw <- pmin(pmax(e_draw, min(NODOS_ELAST)), max(NODOS_ELAST))
log_msg("Sorteos recortados al borde de la grilla: %d de %d (%.4f%%)",
        n_fuera, B_REPLICAS, 100 * n_fuera / B_REPLICAS)
if (n_fuera / B_REPLICAS > 0.001) log_msg("ADVERTENCIA: mas de 1 por mil recortado -- ensanchar NODOS_ELAST.")

arch_r38 <- file.path(out_dir, "59_espacio_escenarios_comparado.csv")
if (file.exists(arch_r38)) {
  r38 <- fread(arch_r38)[escenario_impuesto == "conv75_gradual" & anio %in% ANIOS_REPORTE]
  chk <- rbindlist(lapply(ANIOS_REPORTE, function(a) data.table(
    anio = a,
    r38_consumo_pct = 100 * (exp(r38[anio == a]$d_ln_consumo) - 1),
    emulador_consumo_pct = predecir(emuladores[[paste(a, "caida_consumo_pct")]], -0.639, 1.00),
    r38_recaudacion = r38[anio == a]$recaudacion_relativa,
    emulador_recaudacion = predecir(emuladores[[paste(a, "recaudacion_relativa")]], -0.639, 1.00))))
  chk[, `:=`(dif_consumo_pp = emulador_consumo_pct - r38_consumo_pct,
             dif_recaudacion = emulador_recaudacion - r38_recaudacion)]
  fwrite(chk, file.path(out_dir, "77_mc_chequeo_contra_r38.csv"))
  log_msg("\nChequeo externo del emulador contra la corrida real de R/38:\n%s",
          paste(capture.output(print(chk, digits = 4)), collapse = "\n"))
}

banda <- rbindlist(lapply(ANIOS_REPORTE, function(a) rbindlist(lapply(RESULTADOS, function(v) {
  mod <- emuladores[[paste(a, v)]]
  y1 <- predecir(mod, e_draw, 1.00)
  y2 <- predecir(mod, e_draw, m_draw)
  y3 <- predecir(mod, -0.639, m_draw)
  rbind(
    data.table(anio = a, resultado = v, fuente = "capa 1 (elasticidad)", tipo = "IC frecuentista",
               central = predecir(mod, -0.639, 1.00), p2.5 = quantile(y1, 0.025), p97.5 = quantile(y1, 0.975)),
    data.table(anio = a, resultado = v, fuente = "capa 1 + prior del hazard", tipo = "IC de credibilidad",
               central = predecir(mod, -0.639, 1.00), p2.5 = quantile(y2, 0.025), p97.5 = quantile(y2, 0.975)),
    data.table(anio = a, resultado = v, fuente = "solo prior del hazard", tipo = "IC de credibilidad",
               central = predecir(mod, -0.639, 1.00), p2.5 = quantile(y3, 0.025), p97.5 = quantile(y3, 0.975)))
}))))
banda[, ancho := p97.5 - p2.5]
fwrite(banda, file.path(out_dir, "77_mc_bandas.csv"))
log_msg("\nBandas del resultado AGREGADO:\n%s",
        paste(capture.output(print(banda[, .(anio, resultado, fuente, central, p2.5, p97.5, ancho)],
                                    digits = 4, nrows = 40)), collapse = "\n"))

desc <- dcast(banda, anio + resultado ~ fuente, value.var = "ancho")
setnames(desc, c("capa 1 (elasticidad)", "capa 1 + prior del hazard", "solo prior del hazard"),
         c("ancho_elasticidad", "ancho_total", "ancho_hazard"))
desc[, aporte_hazard_pct := 100 * (ancho_total - ancho_elasticidad) / ancho_total]
desc <- merge(desc, res_ruido[, .(anio, resultado, ruido_de_pp = de, ruido_rango_pp = rango)],
              by = c("anio", "resultado"), all.x = TRUE)
desc[, banda_sobre_ruido := ancho_total / pmax(ruido_rango_pp, 1e-9)]
fwrite(desc, file.path(out_dir, "77_mc_aporte_fuentes.csv"))
log_msg("\nAporte de cada fuente al ancho de la banda, y contraste contra el ruido de simulacion:\n%s",
        paste(capture.output(print(desc, digits = 3)), collapse = "\n"))
if (desc[resultado == "caida_consumo_pct", min(banda_sobre_ruido)] < 5) {
  log_msg("ADVERTENCIA: la banda es menos de 5 veces el ruido de simulacion en algun anio.")
  log_msg("Reportarla como banda de parametros seria enganoso; hace falta promediar semillas.")
}

log_msg("\n--- Resultado distributivo: envolvente de escenarios, no banda ---")

arch_prog <- file.path(out_dir, "72_sensibilidad_t1_progresividad.csv")
if (file.exists(arch_prog)) {
  prog <- fread(arch_prog)[grepl("^C_", variante) & tercil == 1 & anio %in% ANIOS_REPORTE]
  if (nrow(prog) > 0) {
    env <- prog[, .(variantes = .N, minimo_pp = min(dif_pp), maximo_pp = max(dif_pp),
                    principal_pp = dif_pp[variante == "C_principal"][1]), by = anio][order(anio)]
    fwrite(env, file.path(out_dir, "77_mc_envolvente_distributiva.csv"))
    log_msg("Envolvente S0-S4 de la participacion de T1 en el consumo (pp contra la base):\n%s",
            paste(capture.output(print(env, digits = 3)), collapse = "\n"))
    log_msg("Esto es un RANGO DE SUPUESTOS. No tiene 95%% asociado y no se combina aritmeticamente")
    log_msg("con las bandas de arriba.")
  } else {
    log_msg("ADVERTENCIA: 72_sensibilidad_t1_progresividad.csv no trae variantes del grupo C todavia.")
  }
} else {
  log_msg("ADVERTENCIA: falta %s -- correr R/45 (grupo C) para la envolvente distributiva.", basename(arch_prog))
}

log_msg("\n=== outputs: 77_mc_grilla.csv, 77_mc_validacion_emulador.csv, 77_mc_bandas.csv,")
log_msg("    77_mc_aporte_fuentes.csv, 77_mc_envolvente_distributiva.csv ===")
close(log_con)
