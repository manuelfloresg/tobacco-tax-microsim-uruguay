# 78_sensibilidades_tc.R
#
# Sensitivity analyses of the article, annual 2025-2060: abrupt convergence, delayed start,
# transient cessation response, initiation elasticity halved and doubled, partial intensive
# adjustment, equal price response across tertiles, 80% pass-through with illicit diversion,
# roll-your-own excise raised in proportion, graded former-smoker mortality risk, and starts from
# the 2025 survey population. Revenue is excise plus value added tax. 100 seeds for the primary,
# abrupt and graded-risk variants, 20 for the others. Resumable.
# Usage:  Rscript R/78_sensibilidades_tc.R parte K N   (part K of N)
#         Rscript R/78_sensibilidades_tc.R fit         (merges and summarises)
# Output: output/tables/95_tc_*.csv

library(here)
library(data.table)
library(haven)
library(splines)
here::i_am("R/78_sensibilidades_tc.R")

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")
dir_r78 <- file.path(int_dir, "r78")
dir.create(dir_r78, showWarnings = FALSE)

ARGS <- commandArgs(trailingOnly = TRUE)
MODO <- if (length(ARGS) >= 1L) ARGS[1] else "parte"
stopifnot(MODO %in% c("parte", "fit", "prueba"))

SEMILLA_0  <- 20260809L
SEM_100    <- SEMILLA_0 + 0:99
SEM_20     <- SEMILLA_0 + 0:19
ANIO_INICIO_POLITICA <- 2027L
ANIO_FIN   <- 2060L
LAMBDA_INTENSIVO <- 0.5
ANIO_FIN_CES_TRANSITORIA <- 2033L
ILICITO_0 <- 0.12; ILICITO_1 <- 0.21
DESVIO <- (ILICITO_1 - ILICITO_0) / (1 - ILICITO_0)

VARIANTES <- list(
  principal        = list(semillas = SEM_100, ramas = c("base", "esc")),
  abrupto          = list(semillas = SEM_100, ramas = "esc"),
  ces_transitoria  = list(semillas = SEM_20,  ramas = "esc"),
  ini_mitad        = list(semillas = SEM_20,  ramas = "esc"),
  ini_doble        = list(semillas = SEM_20,  ramas = "esc"),
  resp_igual       = list(semillas = SEM_20,  ramas = "esc"),
  traslado_desvio  = list(semillas = SEM_20,  ramas = "esc"),
  ryo_proporcional = list(semillas = SEM_20,  ramas = "esc"),
  inicio_2032      = list(semillas = SEM_20,  ramas = "esc"),
  inicio_2025_obs  = list(semillas = SEM_20,  ramas = c("base", "esc")),
  inicio_2025_corr = list(semillas = SEM_20,  ramas = c("base", "esc")),
  rr_graduado      = list(semillas = SEM_100, ramas = c("base", "esc"))
)
TRABAJOS <- rbindlist(lapply(names(VARIANTES), function(v)
  CJ(variante = v, semilla = VARIANTES[[v]]$semillas, rama = VARIANTES[[v]]$ramas)))
setorder(TRABAJOS, semilla, variante, rama)
TRABAJOS[, id := .I]
archivo_trabajo <- function(v, s, r, tipo) file.path(dir_r78, sprintf("%s_%s_%d_%s.csv", tipo, v, s, r))

if (MODO == "fit") {
  log_con <- file(here::here("logs", "78_sensibilidades_tc_fit.log"), open = "wt", encoding = "UTF-8")
  log_msg <- function(...) { m <- sprintf(...); cat(m, "\n"); cat(m, "\n", file = log_con) }
  leer <- function(tipo) rbindlist(lapply(list.files(dir_r78, sprintf("^%s_.*[.]csv$", tipo), full.names = TRUE), fread))
  A <- leer("agg"); D <- leer("muertes"); PY <- leer("py")
  log_msg("Archivos: %d agregados, %d de muertes, %d de anios-persona", uniqueN(A[, .(variante, semilla, rama)]),
          uniqueN(D[, .(variante, semilla, rama)]), uniqueN(PY[, .(variante, semilla)]))
  base_pr <- A[variante == "principal" & rama == "base"]
  solo_esc <- names(VARIANTES)[sapply(VARIANTES, function(x) identical(x$ramas, "esc"))]
  A <- rbind(A, rbindlist(lapply(solo_esc, function(v) {
    s <- unique(A[variante == v]$semilla); copy(base_pr[semilla %in% s])[, variante := v] })))
  D <- rbind(D, rbindlist(lapply(solo_esc, function(v) {
    s <- unique(D[variante == v]$semilla)
    copy(D[variante == "principal" & rama == "base" & semilla %in% s])[, variante := v] })))
  fwrite(A, file.path(out_dir, "95_tc_anual_por_semilla.csv"))

  w <- dcast(A, variante + semilla + anio ~ rama,
             value.var = c("prevalencia", "consumo_mill", "consumo_int_mill", "fumadores", "N",
                           "prev_T1", "prev_T2", "prev_T3", "recaudacion_idx", "recaudacion_imesi_idx"))
  w[, `:=`(cons_pct = 100 * (consumo_mill_esc / consumo_mill_base - 1),
           cons_pct_int_parcial = 100 * (consumo_int_mill_esc / consumo_mill_base - 1),
           prev_pp = 100 * (prevalencia_esc - prevalencia_base),
           prev_base = 100 * prevalencia_base,
           brecha_base = 100 * (prev_T1_base - prev_T3_base),
           brecha_esc = 100 * (prev_T1_esc - prev_T3_esc),
           razon_base = prev_T1_base / prev_T3_base, razon_esc = prev_T1_esc / prev_T3_esc,
           d_T1 = 100 * (prev_T1_esc - prev_T1_base), d_T3 = 100 * (prev_T3_esc - prev_T3_base),
           recaudacion_relativa = recaudacion_idx_esc / recaudacion_idx_base,
           recaudacion_imesi_relativa = recaudacion_imesi_idx_esc / recaudacion_imesi_idx_base,
           recaudacion_imesi_extra_idx = recaudacion_imesi_idx_esc - recaudacion_imesi_idx_base,
           cig_evitados_mill = consumo_mill_base - consumo_mill_esc,
           fumadores_evitados = fumadores_base - fumadores_esc,
           recaudacion_extra_idx = recaudacion_idx_esc - recaudacion_idx_base)]
  b25 <- w[anio == 2025L, .(variante, semilla, b25 = brecha_base, r25 = razon_base)]
  w <- merge(w, b25, by = c("variante", "semilla"))
  w[, `:=`(pct_ensanche_removido = 100 * (brecha_base - brecha_esc) / (brecha_base - b25),
           pct_ensanche_relativo_removido = 100 * (razon_base - razon_esc) / (razon_base - r25))]
  setorder(w, variante, semilla, anio)
  w[, `:=`(cum_cig_evitados_mill = cumsum(fifelse(anio >= ANIO_INICIO_POLITICA, cig_evitados_mill, 0)),
           cum_anios_fumando_evitados = cumsum(fifelse(anio >= ANIO_INICIO_POLITICA, fumadores_evitados, 0)),
           cum_rec_extra_pct = 100 * cumsum(fifelse(anio >= ANIO_INICIO_POLITICA, recaudacion_extra_idx, 0)) /
             cumsum(fifelse(anio >= ANIO_INICIO_POLITICA, recaudacion_idx_base, 0)),
           cum_rec_imesi_extra_pct = 100 * cumsum(fifelse(anio >= ANIO_INICIO_POLITICA, recaudacion_imesi_extra_idx, 0)) /
             cumsum(fifelse(anio >= ANIO_INICIO_POLITICA, recaudacion_imesi_idx_base, 0))),
    by = .(variante, semilla)]
  dm <- D[anio >= ANIO_INICIO_POLITICA, .(muertes = sum(muertes)), by = .(variante, semilla, rama, anio)]
  dm <- dcast(dm, variante + semilla + anio ~ rama, value.var = "muertes")
  setorder(dm, variante, semilla, anio)
  dm[, muertes_evitadas_cum := cumsum(base - esc), by = .(variante, semilla)]
  dm[, anio := anio + 1L]
  w <- merge(w, dm[, .(variante, semilla, anio, muertes_evitadas_cum)], by = c("variante", "semilla", "anio"), all.x = TRUE)
  w[is.na(muertes_evitadas_cum), muertes_evitadas_cum := 0]
  fwrite(w, file.path(out_dir, "95_tc_efectos_por_semilla.csv"))

  cols <- c("prev_base", "cons_pct", "cons_pct_int_parcial", "prev_pp", "brecha_base", "brecha_esc", "d_T1", "d_T3",
            "pct_ensanche_removido", "razon_base", "razon_esc", "pct_ensanche_relativo_removido",
            "recaudacion_relativa", "recaudacion_imesi_relativa", "cum_cig_evitados_mill", "cum_anios_fumando_evitados",
            "cum_rec_extra_pct", "cum_rec_imesi_extra_pct",
            "muertes_evitadas_cum")
  med <- w[, c(list(n_semillas = .N), lapply(.SD, mean), setNames(lapply(.SD, function(x) sd(x) / sqrt(.N)), paste0(cols, "_ee"))),
           by = .(variante, anio), .SDcols = cols]
  setorder(med, variante, anio)
  fwrite(med, file.path(out_dir, "95_tc_efectos_resumen.csv"))
  pr <- w[variante == "principal", c("semilla", "anio", cols), with = FALSE]
  dif <- merge(w[variante != "principal", c("variante", "semilla", "anio", cols), with = FALSE], pr,
               by = c("semilla", "anio"), suffixes = c("", ".p"))
  difr <- dif[, c(list(n = .N), setNames(lapply(cols, function(cc) mean(get(cc) - get(paste0(cc, ".p")))), paste0("d_", cols)),
                  setNames(lapply(cols, function(cc) sd(get(cc) - get(paste0(cc, ".p"))) / sqrt(.N)), paste0("d_", cols, "_ee"))),
              by = .(variante, anio)]
  fwrite(difr, file.path(out_dir, "95_tc_diferencias_pareadas.csv"))

  mt <- D[anio >= ANIO_INICIO_POLITICA & anio < ANIO_FIN & variante %in% c("principal", "rr_graduado"),
          .(muertes = sum(muertes)), by = .(variante, semilla, rama, tercil, grupo_edad)]
  mt <- dcast(mt, variante + semilla + tercil + grupo_edad ~ rama, value.var = "muertes", fill = 0)
  mt[, evitadas := base - esc]
  py <- PY[anio >= ANIO_INICIO_POLITICA & anio < ANIO_FIN, .(py = sum(N)), by = .(variante, semilla, tercil, grupo_edad)]
  mt <- merge(mt, py, by = c("variante", "semilla", "tercil", "grupo_edad"))
  est <- mt[, .(py_tot = sum(py)), by = .(variante, semilla, grupo_edad)][, peso := py_tot / sum(py_tot), by = .(variante, semilla)]
  mt <- merge(mt, est[, .(variante, semilla, grupo_edad, peso)], by = c("variante", "semilla", "grupo_edad"))
  porter <- mt[, .(evitadas = sum(evitadas), py = sum(py),
                   tasa_bruta = 1e5 * sum(evitadas) / sum(py),
                   tasa_estandarizada = 1e5 * sum(peso * evitadas / py)), by = .(variante, semilla, tercil)]
  porter[, pct := 100 * evitadas / sum(evitadas), by = .(variante, semilla)]
  rt <- porter[, .(n = .N, evitadas = mean(evitadas), evitadas_ee = sd(evitadas) / sqrt(.N), pct = mean(pct),
                   tasa_bruta = mean(tasa_bruta), tasa_estandarizada = mean(tasa_estandarizada),
                   tasa_estandarizada_ee = sd(tasa_estandarizada) / sqrt(.N)), by = .(variante, tercil)]
  setorder(rt, variante, tercil)
  fwrite(rt, file.path(out_dir, "95_tc_muertes_tercil.csv"))

  log_msg("Resumen a 2030, 2045, 2060:\n%s", paste(capture.output(print(
    med[anio %in% c(2030L, 2045L, 2060L), .(variante, anio, n_semillas, prev_base = round(prev_base, 2),
        cons_pct = round(cons_pct, 2), prev_pp = round(prev_pp, 3), pct_rem = round(pct_ensanche_removido, 1),
        pct_rem_rel = round(pct_ensanche_relativo_removido, 1), rec = round(recaudacion_relativa, 3),
        muertes = round(muertes_evitadas_cum))])), collapse = "\n"))
  log_msg("Muertes por tercil:\n%s", paste(capture.output(print(rt, digits = 4)), collapse = "\n"))
  log_msg("=== outputs: 95_tc_anual_por_semilla, 95_tc_efectos_por_semilla, 95_tc_efectos_resumen, 95_tc_diferencias_pareadas, 95_tc_muertes_tercil ===")
  close(log_con)
  quit(save = "no")
}

PARTE <- if (MODO == "parte") as.integer(ARGS[2]) else 1L
N_PAR <- if (MODO == "parte") as.integer(ARGS[3]) else 1L
log_con <- file(here::here("logs", sprintf("78_sensibilidades_tc_%s%d.log", MODO, PARTE)), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { m <- sprintf(...); cat(m, "\n"); cat(m, "\n", file = log_con) }
log_msg("=== 78_sensibilidades_tc.R %s %d/%d -- %s ===", MODO, PARTE, N_PAR, format(Sys.time()))
source(here::here("R", "24_motor_transiciones.R"))

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params_escalada <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor)
param_downshift <- params_cal[parametro == "raw_par3"]$valor

insumos <- cargar_insumos_motor(regla_precio = "mantenimiento_politica", anio_max_precio = 2070L)
tabla_principal <- insumos$tabla_superv
tabla_graduada <- NULL
precio_base <- insumos$precio_lookup
ELAST_TOT_0 <- ELASTICIDAD_TOTAL_TERCIL; SHARE_0 <- SHARE_EXTENSIVO_TERCIL

trayectoria_variante <- function(v) {
  if (v == "abrupto") return(trayectoria_fiscal(precio_base, 0.75, ANIO_INICIO_POLITICA, "abrupto"))
  if (v == "inicio_2032") return(trayectoria_fiscal(precio_base, 0.75, 2032L, "gradual"))
  if (v == "ryo_proporcional") return(trayectoria_fiscal(precio_base, 0.75, ANIO_INICIO_POLITICA, "gradual", ryo = "proporcional"))
  if (v == "traslado_desvio") return(trayectoria_fiscal(precio_base, 0.75, ANIO_INICIO_POLITICA, "gradual",
                                                         traslado = 0.8, desvio = DESVIO, desvio_gradual = TRUE))
  trayectoria_fiscal(precio_base, 0.75, ANIO_INICIO_POLITICA, "gradual")
}

precio_intensivo_parcial <- function(pl) {
  out <- copy(pl); setorder(out, anio)
  pb <- precio_base[match(out$anio, precio_base$anio)]$p_reales
  lr <- log(out$p_reales / pb); le <- numeric(length(lr))
  for (i in seq_along(lr)) le[i] <- if (i == 1L) 0 else le[i - 1L] + LAMBDA_INTENSIVO * (lr[i] - le[i - 1L])
  out[, p_reales := pb * exp(le)]
  out
}

correr_trabajo <- function(v, s, rama) {
  if (v == "resp_igual") {
    e <- c(`1` = -0.639, `2` = -0.639, `3` = -0.639); sh <- c(`1` = 0.5, `2` = 0.5, `3` = 0.5)
  } else { e <- ELAST_TOT_0; sh <- SHARE_0 }
  assign("ELASTICIDAD_TOTAL_TERCIL", e, envir = .GlobalEnv)
  assign("ELASTICIDAD_EXTENSIVA_TERCIL", e * sh, envir = .GlobalEnv)
  assign("ELASTICIDAD_INTENSIVA_TERCIL", e * (1 - sh), envir = .GlobalEnv)
  if (v == "rr_graduado") {
    if (is.null(tabla_graduada)) tabla_graduada <<- fread(file.path(out_dir, "42_supervivencia_por_estado_graduado.csv"))
    tabla <- tabla_graduada
  } else tabla <- tabla_principal
  inicio <- if (startsWith(v, "inicio_2025")) 2025L else ANIO_POB_PROYECCION
  sufijo_pop <- if (v == "inicio_2025_corr") "_corr" else ""
  tr <- trayectoria_variante(v)
  pl <- if (rama == "base") precio_base else
    construir_precio_lookup(2010, 2070L, regla = "mantenimiento_politica", trayectoria_impuesto = tr[, .(anio, factor_impuesto_real)])
  pl_int <- if (rama == "esc" && v == "principal") precio_intensivo_parcial(pl) else pl
  fac <- function(a) if (rama == "base" || !a %in% tr$anio) 1 else tr$factor_tributo_total[tr$anio == a]
  fac_imesi <- function(a) if (rama == "base" || !a %in% tr$anio) 1 else tr$factor_imesi[tr$anio == a]
  dsv <- function(a) if (rama == "base" || !a %in% tr$anio) 0 else tr$desvio[tr$anio == a]

  set.seed(s)
  pop <- cargar_poblacion_proyeccion(inicio, sufijo_pop)
  activar_registro_muertes()
  filas <- list(); pys <- list(); a <- inicio
  while (a <= ANIO_FIN) {
    if (a >= 2025L) {
      cig <- calcular_cig_dia(pop, a, insumos$intensidad, pl)
      cig_int <- if (identical(pl_int, pl)) cig else calcular_cig_dia(pop, a, insumos$intensidad, pl_int)
      fuma <- cig > 0; tt <- as.integer(pop$tercil)
      legal <- sum(cig) * 365 / 1e6 * (1 - ILICITO_0 - dsv(a) * (1 - ILICITO_0))
      filas[[length(filas) + 1L]] <- data.table(
        variante = v, semilla = s, rama = rama, anio = a, N = nrow(pop),
        prevalencia = mean(fuma), fumadores = sum(fuma),
        consumo_mill = sum(cig) * 365 / 1e6, consumo_int_mill = sum(cig_int) * 365 / 1e6,
        prev_T1 = mean(fuma[tt == 1L]), prev_T2 = mean(fuma[tt == 2L]), prev_T3 = mean(fuma[tt == 3L]),
        cons_T1 = sum(cig[tt == 1L]) * 365 / 1e6, cons_T3 = sum(cig[tt == 3L]) * 365 / 1e6,
        recaudacion_idx = legal * fac(a), recaudacion_imesi_idx = legal * fac_imesi(a))
      if (rama == "base" && v %in% c("principal", "rr_graduado"))
        pys[[length(pys) + 1L]] <- pop[, .(N = .N), by = .(tercil, grupo_edad = grupo_etario_intensidad(edad))][
          , `:=`(variante = v, semilla = s, anio = a)]
    }
    if (a == ANIO_FIN) break
    r <- pl[anio == a]$p_reales / precio_base[anio == a]$p_reales
    ov <- if (isTRUE(all.equal(r, 1, tolerance = 1e-12))) NULL else r
    rc <- ri <- NULL
    if (!is.null(ov)) {
      if (v == "ces_transitoria" && a > ANIO_FIN_CES_TRANSITORIA) rc <- 1
      if (v == "ini_mitad") ri <- ov^0.5
      if (v == "ini_doble") ri <- ov^2
    }
    pop <- ciclo_anual(pop, a, insumos$modelos, tabla, pl, insumos$pob_ine, insumos$entrada_15_por_anio,
                       params_escalada, param_downshift, precio_ratio_override = ov,
                       precio_ratio_cesacion = rc, precio_ratio_iniciacion = ri)
    a <- a + 1L
  }
  m <- muertes_registradas(); desactivar_registro_muertes()
  m <- m[, .(muertes = sum(muertes)), by = .(anio, tercil, grupo_edad)][, `:=`(variante = v, semilla = s, rama = rama)]
  fwrite(rbindlist(filas), archivo_trabajo(v, s, rama, "agg"))
  fwrite(m, archivo_trabajo(v, s, rama, "muertes"))
  if (length(pys)) fwrite(rbindlist(pys), archivo_trabajo(v, s, rama, "py"))
  rbindlist(filas)
}

if (MODO == "prueba") {
  ANIO_FIN <- 2028L
  t0 <- Sys.time(); r <- correr_trabajo("principal", SEMILLA_0, "base")
  log_msg("prueba base 2025-2028: %.1f min\n%s", as.numeric(difftime(Sys.time(), t0, units = "mins")),
          paste(capture.output(print(r)), collapse = "\n"))
  t0 <- Sys.time(); r <- correr_trabajo("inicio_2025_corr", SEMILLA_0, "esc")
  log_msg("prueba inicio_2025_corr esc 2025-2028: %.1f min\n%s", as.numeric(difftime(Sys.time(), t0, units = "mins")),
          paste(capture.output(print(r)), collapse = "\n"))
  unlink(list.files(dir_r78, full.names = TRUE))
  close(log_con); quit(save = "no")
}

mios <- TRABAJOS[(id - 1L) %% N_PAR == (PARTE - 1L)]
log_msg("Trabajos de esta parte: %d de %d", nrow(mios), nrow(TRABAJOS))
for (i in seq_len(nrow(mios))) {
  v <- mios$variante[i]; s <- mios$semilla[i]; rama <- mios$rama[i]
  if (file.exists(archivo_trabajo(v, s, rama, "muertes"))) next
  t0 <- Sys.time()
  correr_trabajo(v, s, rama)
  log_msg("[%s] %s %d %s: %.1f min", format(Sys.time(), "%H:%M"), v, s, rama,
          as.numeric(difftime(Sys.time(), t0, units = "mins")))
  gc(verbose = FALSE)
}
log_msg("=== parte %d terminada ===", PARTE)
close(log_con)
