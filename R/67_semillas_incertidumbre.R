# 67_semillas_incertidumbre.R
#
# Simulation noise: every reported quantity re-run with 100 seeds on six branches. Reported numbers
# come from the run with seed 20260809; the seeds give simulation standard errors, and deaths
# averted are reported as the mean over seeds. Aborts unless seed 20260809 reproduces R/38.
# Usage:  Rscript R/67_semillas_incertidumbre.R parte K N   (part K of N)
#         Rscript R/67_semillas_incertidumbre.R fusion      (merges the parts)
# Input:  synthetic population 2011, output/tables/43_*, 59_*, 88_*
# Output: output/tables/89_*.csv, 90_*.csv, 91_semillas_resumen.csv,
#         informe/tablas/muertes_evitadas_tercil.csv

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/67_semillas_incertidumbre.R")

ARGS  <- commandArgs(trailingOnly = TRUE)
MODO  <- if (length(ARGS) >= 1L) ARGS[1] else "todo"
PARTE <- if (MODO == "parte") as.integer(ARGS[2]) else 1L
N_PAR <- if (MODO == "parte") as.integer(ARGS[3]) else 1L
if (!MODO %in% c("todo", "parte", "fusion", "prueba")) stop("modo desconocido: ", MODO)

nombre_log <- if (MODO == "parte") sprintf("67_parte%d.log", PARTE) else
  sprintf("67_semillas_incertidumbre%s.log", if (MODO == "prueba") "_prueba" else "")
log_con <- file(here::here("logs", nombre_log), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 67_semillas_incertidumbre.R -- %s ===", format(Sys.time()))
log_msg("modo = %s%s", MODO, if (MODO == "parte") sprintf(" (%d de %d)", PARTE, N_PAR) else "")

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")
source(here::here("R", "24_motor_transiciones.R"))

ANIOS_SNAP      <- c(2025L, seq(2030L, 2060L, by = 5L))
ANIO_MAX_PRECIO <- 2070L
ANIO_INICIO     <- 2027L
SEMILLA_PRINCIPAL <- 20260809L
N_SEMILLAS      <- 100L
SEMILLAS        <- SEMILLA_PRINCIPAL + seq_len(N_SEMILLAS) - 1L

RAMAS <- list(
  base            = list(p = "base",           ces = TRUE,  ini = TRUE),
  conv75_gradual  = list(p = "conv75_gradual", ces = TRUE,  ini = TRUE),
  solo_cesacion   = list(p = "conv75_gradual", ces = TRUE,  ini = FALSE),
  solo_iniciacion = list(p = "conv75_gradual", ces = FALSE, ini = TRUE),
  conv75_abrupto  = list(p = "conv75_abrupto", ces = TRUE,  ini = TRUE),
  convEU_gradual  = list(p = "convEU_gradual", ces = TRUE,  ini = TRUE)
)
ESCENARIOS <- c("conv75_gradual", "conv75_abrupto", "convEU_gradual")

if (MODO %in% c("todo", "parte", "prueba")) {

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor,
            params_cal[parametro == "raw_par3"]$valor)
params_escalada <- params[1:2]; param_downshift <- params[3]

correr <- function(anios_snapshot, insumos, precio_lookup, precio_base = NULL,
                   semilla, resp_ces = TRUE, resp_ini = TRUE) {
  set.seed(semilla)
  pop <- cargar_poblacion_proyeccion()
  snaps <- list(); a <- ANIO_POB_PROYECCION
  for (obj in anios_snapshot) {
    while (a < obj) {
      ov <- if (is.null(precio_base)) NULL else {
        r <- precio_lookup[anio == a]$p_reales / precio_base[anio == a]$p_reales
        if (isTRUE(all.equal(r, 1, tolerance = 1e-12))) NULL else r
      }
      pop <- ciclo_anual(pop, a, insumos$modelos, insumos$tabla_superv, precio_lookup,
                          insumos$pob_ine, insumos$entrada_15_por_anio, params_escalada,
                          param_downshift, precio_ratio_override = ov,
                          resp_precio_cesacion = resp_ces, resp_precio_iniciacion = resp_ini)
      a <- a + 1L
    }
    snaps[[as.character(obj)]] <- copy(pop)[, anio_snapshot := obj]
  }
  snaps
}

resumir <- function(sn, ins, pl, rama, sem) {
  agg <- list(); ter <- list(); sx <- list()
  for (dt in sn) {
    a <- dt$anio_snapshot[1]
    d <- dt[, .(tercil, sexo)]
    d[, cig := calcular_cig_dia(dt, a, ins$intensidad, pl)]
    agg[[length(agg) + 1L]] <- data.table(
      rama = rama, semilla = sem, anio = a, N = nrow(d), N_fumadores = sum(d$cig > 0),
      prevalencia = mean(d$cig > 0), intensidad_media = sum(d$cig) / max(1, sum(d$cig > 0)),
      consumo_mill = sum(d$cig) * 365 / 1e6)
    t <- d[, .(N_grupo = .N, N_fumadores = sum(cig > 0), consumo_mill = sum(cig) * 365 / 1e6),
           by = tercil]
    t[, `:=`(rama = rama, semilla = sem, anio = a)]
    ter[[length(ter) + 1L]] <- t
    s <- d[, .(N_grupo = .N, N_fumadores = sum(cig > 0), consumo_mill = sum(cig) * 365 / 1e6),
           by = sexo]
    s[, `:=`(rama = rama, semilla = sem, anio = a)]
    sx[[length(sx) + 1L]] <- s
  }
  list(agg = rbindlist(agg), ter = rbindlist(ter), sx = rbindlist(sx))
}

insumos <- cargar_insumos_motor(regla_precio = "mantenimiento_politica",
                                anio_max_precio = ANIO_MAX_PRECIO)
precio_base <- insumos$precio_lookup
tray_all <- fread(file.path(out_dir, "59_trayectorias_impuesto.csv"))
precios <- list(base = precio_base)
for (e in ESCENARIOS) {
  precios[[e]] <- construir_precio_lookup(
    2010, ANIO_MAX_PRECIO, regla = "mantenimiento_politica",
    trayectoria_impuesto = tray_all[escenario_impuesto == e, .(anio, factor_impuesto_real)])
}

SEM_ESTA <- switch(MODO,
  parte  = SEMILLAS[(seq_along(SEMILLAS) - 1L) %% N_PAR == (PARTE - 1L)],
  prueba = SEMILLA_PRINCIPAL,
  SEMILLAS)
RAMAS_ESTA <- if (MODO == "prueba") RAMAS[c("base", "conv75_gradual")] else RAMAS

A <- list(); TT <- list(); SX <- list(); M <- list()
for (sem in SEM_ESTA) {
  for (nm in names(RAMAS_ESTA)) {
    cfg <- RAMAS_ESTA[[nm]]
    pl <- precios[[cfg$p]]
    pb <- if (cfg$p == "base") NULL else precio_base
    t0 <- Sys.time()
    activar_registro_muertes()
    sn <- correr(ANIOS_SNAP, insumos, pl, pb, semilla = sem,
                 resp_ces = cfg$ces, resp_ini = cfg$ini)
    m <- muertes_registradas()
    desactivar_registro_muertes()
    r <- resumir(sn, insumos, pl, nm, sem); rm(sn); gc(verbose = FALSE)
    A[[length(A) + 1L]] <- r$agg; TT[[length(TT) + 1L]] <- r$ter; SX[[length(SX) + 1L]] <- r$sx
    m <- m[anio >= ANIO_INICIO, .(muertes = sum(muertes)), by = tercil]
    m[, `:=`(rama = nm, semilla = sem)]
    M[[length(M) + 1L]] <- m
    log_msg("semilla %d, rama %-16s  (%.1f min)", sem, nm,
            as.numeric(difftime(Sys.time(), t0, units = "mins")))
  }
  suf <- switch(MODO, parte = sprintf("_parte%d", PARTE), prueba = "_prueba", "")
  fwrite(rbindlist(A),  file.path(out_dir, sprintf("91_semillas_agregado%s.csv", suf)))
  fwrite(rbindlist(TT), file.path(out_dir, sprintf("91_semillas_tercil%s.csv", suf)))
  fwrite(rbindlist(SX), file.path(out_dir, sprintf("91_semillas_sexo%s.csv", suf)))
  fwrite(rbindlist(M),  file.path(out_dir, sprintf("91_semillas_muertes%s.csv", suf)))
}

if (MODO == "prueba") {
  ref <- fread(file.path(out_dir, "59_espacio_escenarios_agregado.csv"))
  a <- rbindlist(A)
  cmp <- merge(a[, .(escenario = rama, anio, prev67 = prevalencia, cons67 = consumo_mill)],
               ref[, .(escenario, anio, prevalencia, consumo_mill)], by = c("escenario", "anio"))
  cmp[, `:=`(d_prev = prev67 - prevalencia, d_cons = cons67 - consumo_mill)]
  log_msg("Prueba, semilla principal contra 59_espacio_escenarios_agregado.csv:\n%s",
          paste(capture.output(print(cmp)), collapse = "\n"))
  log_msg("max |d_prev| = %.3g, max |d_cons| = %.3g", max(abs(cmp$d_prev)), max(abs(cmp$d_cons)))
}
log_msg("escrito: %d semillas x %d ramas", length(SEM_ESTA), length(RAMAS_ESTA))
if (MODO %in% c("parte", "prueba")) { close(log_con); quit(save = "no") }
}

juntar <- function(base) {
  arch <- list.files(out_dir, pattern = sprintf("^%s_parte[0-9]+\\.csv$", base), full.names = TRUE)
  if (MODO == "todo") arch <- file.path(out_dir, paste0(base, ".csv"))
  if (!length(arch)) stop("no hay partes de ", base, ": correr primero en modo parte.")
  d <- rbindlist(lapply(arch, fread))
  fwrite(d, file.path(out_dir, paste0(base, ".csv")))
  d
}
A  <- juntar("91_semillas_agregado")
TT <- juntar("91_semillas_tercil")
SX <- juntar("91_semillas_sexo")
M  <- juntar("91_semillas_muertes")

completas <- A[, .(n = uniqueN(rama)), by = semilla][n == length(RAMAS)]$semilla
A <- A[semilla %in% completas]; TT <- TT[semilla %in% completas]
SX <- SX[semilla %in% completas]; M <- M[semilla %in% completas]
nsem <- length(completas)
log_msg("fusion: %d semillas completas (de %d con alguna rama)", nsem, uniqueN(A$semilla))
if (!SEMILLA_PRINCIPAL %in% completas) stop("falta la semilla principal: no hay guardarrail posible")

ref <- fread(file.path(out_dir, "59_espacio_escenarios_agregado.csv"))
g1 <- merge(A[semilla == SEMILLA_PRINCIPAL & rama %in% c("base", ESCENARIOS),
              .(escenario = rama, anio, prevalencia, consumo_mill)],
            ref[, .(escenario, anio, prev_ref = prevalencia, cons_ref = consumo_mill)],
            by = c("escenario", "anio"))
d1 <- max(abs(c(g1$prevalencia - g1$prev_ref, g1$consumo_mill - g1$cons_ref)))
desc_ref <- fread(file.path(out_dir, "88_descomposicion_margen_extensivo.csv"))
g2 <- dcast(A[semilla == SEMILLA_PRINCIPAL & rama %in% c("solo_cesacion", "solo_iniciacion")],
            anio ~ rama, value.var = "prevalencia")
g2 <- merge(g2, desc_ref[, .(anio, prev_solo_cesacion, prev_solo_iniciacion)], by = "anio")
d2 <- max(abs(c(g2$solo_cesacion - g2$prev_solo_cesacion, g2$solo_iniciacion - g2$prev_solo_iniciacion)))
log_msg("Guardarrail semilla %d: max desvio contra R/38 = %.3g; contra R/64 = %.3g",
        SEMILLA_PRINCIPAL, d1, d2)
if (nrow(g1) != 4L * length(ANIOS_SNAP) || d1 > 1e-9 || d2 > 1e-9)
  stop("la semilla principal NO reproduce las corridas publicadas: no se funde nada")
arch89 <- file.path(out_dir, "89_ruido_muertes_por_semilla.csv")
if (file.exists(arch89)) {
  m89 <- fread(arch89)[semilla == SEMILLA_PRINCIPAL]
  m89[, rama := fifelse(rama == "completa", "conv75_gradual", rama)]
  g3 <- merge(M[semilla == SEMILLA_PRINCIPAL], m89, by = c("rama", "tercil"), suffixes = c("", "_ref"))
  log_msg("Guardarrail muertes contra R/65: %d celdas, max desvio %d",
          nrow(g3), as.integer(max(abs(g3$muertes - g3$muertes_ref))))
  if (nrow(g3) && any(g3$muertes != g3$muertes_ref)) stop("las muertes de la semilla principal no reproducen R/65")
}

tau <- fread(file.path(out_dir, "59_espacio_escenarios_comparado.csv"))[
  , .(escenario, anio, ratio_tau = factor_tributo_total)]

base_a <- A[rama == "base", .(semilla, anio, prev_b = prevalencia, int_b = intensidad_media,
                               cons_b = consumo_mill, fum_b = N_fumadores)]
filas <- list()
agregar <- function(dt, magnitud, escenario) {
  filas[[length(filas) + 1L]] <<- dt[, .(semilla, anio, valor, magnitud = magnitud, escenario = escenario)]
}
for (e in ESCENARIOS) {
  x <- merge(A[rama == e], base_a, by = c("semilla", "anio"))
  x <- merge(x, tau[escenario == e, .(anio, ratio_tau)], by = "anio", all.x = TRUE)
  x[is.na(ratio_tau), ratio_tau := 1]
  agregar(x[, .(semilla, anio, valor = 100 * (consumo_mill / cons_b - 1))], "consumo_pct", e)
  agregar(x[, .(semilla, anio, valor = 100 * (prevalencia - prev_b))], "prevalencia_pp", e)
  agregar(x[, .(semilla, anio, valor = 100 * (prevalencia / prev_b - 1))], "prevalencia_rel_pct", e)
  agregar(x[, .(semilla, anio, valor = 100 * (N_fumadores / fum_b - 1))], "fumadores_pct", e)
  agregar(x[, .(semilla, anio, valor = ratio_tau * consumo_mill / cons_b)], "recaudacion_relativa", e)
  agregar(x[anio > 2025L, .(semilla, anio,
             valor = 100 * log(prevalencia / prev_b) / log(consumo_mill / cons_b))],
          "share_extensivo_pct", e)
}
ab <- merge(A[rama == "conv75_abrupto", .(semilla, anio, c_ab = consumo_mill)],
            A[rama == "conv75_gradual", .(semilla, anio, c_gr = consumo_mill)], by = c("semilla", "anio"))
ab <- merge(ab, base_a, by = c("semilla", "anio"))
agregar(ab[, .(semilla, anio, valor = 100 * (c_ab - c_gr) / cons_b)], "abrupto_menos_gradual_consumo_pp", "conv75_abrupto")
agregar(base_a[, .(semilla, anio, valor = 100 * prev_b)], "prevalencia_nivel_pct", "base")
agregar(base_a[, .(semilla, anio, valor = cons_b)], "consumo_nivel_mill", "base")

dsc <- dcast(A[rama %in% c("base", "solo_cesacion", "solo_iniciacion")], semilla + anio ~ rama,
             value.var = "prevalencia")
dsc <- dsc[anio > 2025L]
dsc[, `:=`(ext_ces = log(solo_cesacion / base), ext_ini = log(solo_iniciacion / base))]
agregar(dsc[, .(semilla, anio, valor = 100 * ext_ces / (ext_ces + ext_ini))], "pct_cesacion_extensivo", "conv75_gradual")

TT[, prevalencia := N_fumadores / N_grupo]
tb <- dcast(TT[rama %in% c("base", "conv75_gradual")], semilla + anio + tercil ~ rama,
            value.var = "prevalencia")
for (k in 1:3) {
  agregar(tb[tercil == k, .(semilla, anio, valor = 100 * (conv75_gradual - base))],
          sprintf("efecto_pp_T%d", k), "conv75_gradual")
  agregar(tb[tercil == k, .(semilla, anio, valor = 100 * base)], sprintf("prevalencia_T%d_pct", k), "base")
}
br <- dcast(tb, semilla + anio ~ tercil, value.var = c("base", "conv75_gradual"))
br[, `:=`(brecha_base = 100 * (base_1 - base_3), brecha_esc = 100 * (conv75_gradual_1 - conv75_gradual_3))]
agregar(br[, .(semilla, anio, valor = brecha_base)], "brecha_pp", "base")
agregar(br[, .(semilla, anio, valor = brecha_esc - brecha_base)], "cambio_brecha_pp", "conv75_gradual")
b25 <- br[anio == 2025L, .(semilla, brecha_2025 = brecha_base)]
br <- merge(br, b25, by = "semilla")
agregar(br[anio > 2025L, .(semilla, anio, valor = 100 * (brecha_base - brecha_esc) / (brecha_base - brecha_2025))],
        "pct_ensanchamiento_removido", "conv75_gradual")

SX[, prevalencia := N_fumadores / N_grupo]
sb <- dcast(SX[rama %in% c("base", "conv75_gradual")], semilla + anio + sexo ~ rama,
            value.var = c("prevalencia", "consumo_mill", "N_fumadores"))
for (s in c("varon", "mujer")) {
  agregar(sb[sexo == s, .(semilla, anio, valor = 100 * (prevalencia_conv75_gradual - prevalencia_base))],
          paste0("efecto_pp_", s), "conv75_gradual")
  agregar(sb[sexo == s, .(semilla, anio, valor = 100 * (consumo_mill_conv75_gradual / consumo_mill_base - 1))],
          paste0("consumo_pct_", s), "conv75_gradual")
  agregar(sb[sexo == s, .(semilla, anio, valor = 100 * (N_fumadores_conv75_gradual / N_fumadores_base - 1))],
          paste0("fumadores_pct_", s), "conv75_gradual")
  agregar(sb[sexo == s, .(semilla, anio, valor = 100 * prevalencia_base)], paste0("prevalencia_", s, "_pct"), "base")
}

mw <- dcast(M, semilla + tercil ~ rama, value.var = "muertes")
for (e in ESCENARIOS) {
  mw[, ev := base - get(e)]
  agregar(mw[, .(valor = sum(ev)), by = semilla][, anio := 2060L], "muertes_evitadas_total", e)
  for (k in 1:3) agregar(mw[tercil == k, .(semilla, valor = ev)][, anio := 2060L],
                         sprintf("muertes_evitadas_T%d", k), e)
}
agregar(M[rama == "base", .(valor = sum(muertes)), by = semilla][, anio := 2060L], "muertes_total_base", "base")

S <- rbindlist(filas, use.names = TRUE)
res <- S[, .(n = .N,
             corrida_principal = valor[semilla == SEMILLA_PRINCIPAL],
             media = mean(valor), de = sd(valor), minimo = min(valor), maximo = max(valor)),
         by = .(magnitud, escenario, anio)]
res[, `:=`(ee_media = de / sqrt(n))]
res[, `:=`(desvios_una_corrida = abs(media) / de,
           errores_estandar_media = abs(media) / ee_media,
           z_principal = (corrida_principal - media) / de)]
setorder(res, magnitud, escenario, anio)
fwrite(res, file.path(out_dir, "91_semillas_resumen.csv"))
log_msg("\n--- Resumen (anios 2030, 2045, 2060) ---\n%s",
        paste(capture.output(print(res[anio %in% c(2030L, 2045L, 2060L)], digits = 4, nrows = 500)),
              collapse = "\n"))

pc <- merge(A[rama == "conv75_gradual", .(semilla, anio, prevalencia_esc = prevalencia,
                                          consumo_mill_esc = consumo_mill, fumadores_esc = N_fumadores)],
            base_a[, .(semilla, anio, prevalencia_base = prev_b, consumo_mill_base = cons_b,
                       fumadores_base = fum_b)], by = c("semilla", "anio"))
pc <- merge(pc, tau[escenario == "conv75_gradual", .(anio, ratio_tau)], by = "anio", all.x = TRUE)
pc[is.na(ratio_tau), ratio_tau := 1]
pc[, `:=`(elast_agregada = -0.639, mult_iniciacion = 1,
          caida_consumo_pct = 100 * (consumo_mill_esc / consumo_mill_base - 1),
          caida_prevalencia_pct = 100 * (prevalencia_esc / prevalencia_base - 1),
          dif_prevalencia_pp = 100 * (prevalencia_esc - prevalencia_base),
          recaudacion_relativa = ratio_tau * consumo_mill_esc / consumo_mill_base)]
fwrite(pc[, .(anio, prevalencia_base, consumo_mill_base, fumadores_base, prevalencia_esc,
              consumo_mill_esc, fumadores_esc, elast_agregada, mult_iniciacion, caida_consumo_pct,
              caida_prevalencia_pct, dif_prevalencia_pp, recaudacion_relativa, semilla)],
       file.path(out_dir, "77_mc_ruido_simulacion.csv"))

fila_res <- function(mag, nivel) {
  r <- res[magnitud == mag & escenario == "conv75_gradual" & anio == 2060L]
  data.table(nivel = nivel, media = r$media, de = r$de, minimo = r$minimo, maximo = r$maximo,
             n = r$n, ee_media = r$ee_media, desvios_una_corrida = r$desvios_una_corrida,
             errores_estandar_media = r$errores_estandar_media)
}
fwrite(rbind(fila_res("muertes_evitadas_total", "total"),
             fila_res("muertes_evitadas_T1", "tercil 1"),
             fila_res("muertes_evitadas_T2", "tercil 2"),
             fila_res("muertes_evitadas_T3", "tercil 3")),
       file.path(out_dir, "89_ruido_muertes_resumen.csv"))

fmt66 <- function(mags, anios = NULL) {
  r <- res[magnitud %in% mags & escenario %in% c("conv75_gradual", "base")]
  if (!is.null(anios)) r <- r[anio %in% anios]
  r[, .(anio, media, de, minimo, maximo, magnitud, n, ee_media, desvios_una_corrida, errores_estandar_media)]
}
fwrite(rbind(fmt66("pct_cesacion_extensivo")[, magnitud := "pct_cesacion"],
             res[magnitud == "share_extensivo_pct" & escenario == "conv75_gradual",
                 .(anio, media, de, minimo, maximo, magnitud = "share_extensivo", n, ee_media,
                   desvios_una_corrida, errores_estandar_media)]),
       file.path(out_dir, "90_ruido_descomposicion.csv"))
ter_res <- rbind(
  res[magnitud %like% "^efecto_pp_T" & anio == 2060L, .(magnitud, media, de, minimo, maximo, n, ee_media, desvios_una_corrida, errores_estandar_media)],
  res[magnitud == "brecha_pp" & anio %in% c(2025L, 2060L), .(magnitud = paste0("brecha_base_", anio), media, de, minimo, maximo, n, ee_media, desvios_una_corrida, errores_estandar_media)],
  res[magnitud == "cambio_brecha_pp" & anio == 2060L, .(magnitud = "cambio_brecha_2060", media, de, minimo, maximo, n, ee_media, desvios_una_corrida, errores_estandar_media)])
fwrite(ter_res, file.path(out_dir, "90_ruido_terciles_resumen.csv"))

num_es <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
tm <- rbindlist(lapply(1:3, function(k) {
  r <- res[magnitud == sprintf("muertes_evitadas_T%d", k) & escenario == "conv75_gradual" & anio == 2060L]
  data.table(k = k, media = r$media, ee = r$ee_media)
}))
tot <- res[magnitud == "muertes_evitadas_total" & escenario == "conv75_gradual" & anio == 2060L]
tabla3 <- data.table(
  `Tercil de ingreso` = c("T1 (menores ingresos)", "T2", "T3 (mayores ingresos)"),
  `Muertes evitadas` = num_es(tm$media), `Error estándar` = num_es(tm$ee),
  `% del total` = num_es(100 * tm$media / tot$media, 1))
fwrite(tabla3, here::here("informe", "tablas", "muertes_evitadas_tercil.csv"))
log_msg("\nTabla 3 del DT:\n%s", paste(capture.output(print(tabla3)), collapse = "\n"))

log_msg("\n=== outputs: 91_semillas_*.csv, 77_mc_ruido_simulacion.csv, 89_ruido_muertes_resumen.csv,")
log_msg("    90_ruido_descomposicion.csv, 90_ruido_terciles_resumen.csv, informe/tablas/muertes_evitadas_tercil.csv ===")
log_msg("Siguiente: Rscript R/50_bandas_incertidumbre_mc.R fit  (recalcula el piso de ruido de las bandas)")
close(log_con)
