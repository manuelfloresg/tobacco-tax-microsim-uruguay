# 64_margen_extensivo_y_muertes.R
#
# Decomposition of the extensive margin into cessation and avoided initiation, and deaths averted
# between the status quo and the primary scenario, by income tertile.
# Input:  synthetic population 2011, output/tables/43_*, 59_*
# Output: output/tables/88_*.csv

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/64_margen_extensivo_y_muertes.R")

log_con <- file(here::here("logs", "64_margen_extensivo_y_muertes.log"),
                open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 64_margen_extensivo_y_muertes.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")
source(here::here("R", "24_motor_transiciones.R"))

ANIOS_SNAP      <- c(2025L, seq(2030L, 2060L, by = 5L))
ANIO_MAX_PRECIO <- 2070L
ESC_PRINCIPAL   <- "conv75_gradual"
SEMILLA         <- 20260809

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor,
            params_cal[parametro == "raw_par3"]$valor)
params_escalada <- params[1:2]; param_downshift <- params[3]

correr <- function(anios_snapshot, insumos, precio_lookup, precio_base = NULL,
                   semilla = SEMILLA, resp_ces = TRUE, resp_ini = TRUE) {
  set.seed(semilla)
  pop <- cargar_poblacion_proyeccion()
  snaps <- list()
  a <- ANIO_POB_PROYECCION
  for (obj in anios_snapshot) {
    while (a < obj) {
      ov <- if (is.null(precio_base)) NULL else {
        r <- precio_lookup[anio == a]$p_reales / precio_base[anio == a]$p_reales
        if (isTRUE(all.equal(r, 1, tolerance = 1e-12))) NULL else r
      }
      pop <- ciclo_anual(pop, a, insumos$modelos, insumos$tabla_superv, precio_lookup,
                          insumos$pob_ine, insumos$entrada_15_por_anio, params_escalada,
                          param_downshift, precio_ratio_override = ov,
                          resp_precio_cesacion = resp_ces,
                          resp_precio_iniciacion = resp_ini)
      a <- a + 1L
    }
    snaps[[as.character(obj)]] <- copy(pop)[, anio_snapshot := obj]
  }
  snaps
}

resumir <- function(snaps_x, insumos_x, precio_lookup, etiqueta) {
  rbindlist(lapply(snaps_x, function(dt) {
    a <- dt$anio_snapshot[1]
    cig <- calcular_cig_dia(dt, a, insumos_x$intensidad, precio_lookup)
    data.table(rama = etiqueta, anio = a, N = nrow(dt), prevalencia = mean(cig > 0),
               intensidad_media = sum(cig) / max(1, sum(cig > 0)),
               consumo_mill = sum(cig) * 365 / 1e6,
               p_reales = precio_lookup[anio == a]$p_reales)
  }))
}

log_msg("\n--- Insumos ---")
insumos <- cargar_insumos_motor(regla_precio = "mantenimiento_politica",
                                anio_max_precio = ANIO_MAX_PRECIO)
precio_base <- insumos$precio_lookup

tray <- fread(file.path(out_dir, "59_trayectorias_impuesto.csv"))[
  escenario_impuesto == ESC_PRINCIPAL, .(anio, factor_impuesto_real)]
if (nrow(tray) == 0L)
  stop("59_trayectorias_impuesto.csv no trae la trayectoria ", ESC_PRINCIPAL,
       ": correr R/38_espacio_escenarios.R antes que este script.")
log_msg("Trayectoria %s leida de 59_trayectorias_impuesto.csv: %d anios, factor en regimen %.4f",
        ESC_PRINCIPAL, nrow(tray), max(tray$factor_impuesto_real))

precio_esc <- construir_precio_lookup(2010, ANIO_MAX_PRECIO, regla = "mantenimiento_politica",
                                      trayectoria_impuesto = tray)

RAMAS <- list(
  base            = list(precio = "base", resp_ces = TRUE,  resp_ini = TRUE),
  completa        = list(precio = "esc",  resp_ces = TRUE,  resp_ini = TRUE),
  solo_cesacion   = list(precio = "esc",  resp_ces = TRUE,  resp_ini = FALSE),
  solo_iniciacion = list(precio = "esc",  resp_ces = FALSE, resp_ini = TRUE)
)

res <- list(); muertes <- list()

for (nm in names(RAMAS)) {
  cfg <- RAMAS[[nm]]
  pl  <- if (cfg$precio == "base") precio_base else precio_esc
  pb  <- if (cfg$precio == "base") NULL else precio_base

  t0 <- Sys.time()
  activar_registro_muertes()
  snaps <- correr(ANIOS_SNAP, insumos, precio_lookup = pl, precio_base = pb,
                  resp_ces = cfg$resp_ces, resp_ini = cfg$resp_ini)
  m <- muertes_registradas()
  desactivar_registro_muertes()

  res[[nm]] <- resumir(snaps, insumos, pl, nm)
  m[, rama := nm]
  muertes[[nm]] <- m
  log_msg("Rama %-16s corrida en %5.1f min  (cesacion=%s iniciacion=%s, %d muertes registradas)",
          nm, as.numeric(difftime(Sys.time(), t0, units = "mins")),
          cfg$resp_ces, cfg$resp_ini, sum(m$muertes))
}

resultados <- rbindlist(res)

log_msg("\n--- Guardarrail contra R/38 ---")
ref <- fread(file.path(out_dir, "59_espacio_escenarios_agregado.csv"))[escenario == "base"]
chk <- merge(resultados[rama == "base", .(anio, prevalencia, consumo_mill)],
             ref[, .(anio, prev_ref = prevalencia, cons_ref = consumo_mill)], by = "anio")
chk[, `:=`(d_prev = abs(prevalencia - prev_ref), d_cons = abs(consumo_mill - cons_ref))]
log_msg("Maxima diferencia contra 59_espacio_escenarios_agregado.csv: prevalencia %.3e, consumo %.3e",
        max(chk$d_prev), max(chk$d_cons))
if (max(chk$d_prev) > 1e-12 || max(chk$d_cons) > 1e-9)
  stop("la rama base no reproduce la de R/38: correr() o resumir() se separaron del original.\n",
       "  No usar estos resultados hasta entender la diferencia.")
log_msg("OK: la rama base es identica a la de R/38.")

log_msg("\n--- A) Descomposicion del margen extensivo ---")

w <- dcast(resultados[, .(rama, anio, prevalencia)], anio ~ rama, value.var = "prevalencia")
w[, `:=`(
  ext_total       = log(completa / base),
  ext_cesacion    = log(solo_cesacion / base),
  ext_iniciacion  = log(solo_iniciacion / base)
)]
w[, interaccion := ext_total - ext_cesacion - ext_iniciacion]
w[, `:=`(
  pct_cesacion   = 100 * ext_cesacion / (ext_cesacion + ext_iniciacion),
  pct_iniciacion = 100 * ext_iniciacion / (ext_cesacion + ext_iniciacion),
  pct_interaccion_sobre_total = 100 * interaccion / ext_total
)]
desc <- w[anio > 2025L, .(anio, prev_base = base, prev_completa = completa,
                          prev_solo_cesacion = solo_cesacion,
                          prev_solo_iniciacion = solo_iniciacion,
                          ext_total, ext_cesacion, ext_iniciacion, interaccion,
                          pct_cesacion, pct_iniciacion, pct_interaccion_sobre_total)]
fwrite(desc, file.path(out_dir, "88_descomposicion_margen_extensivo.csv"))
log_msg("%s", paste(capture.output(print(desc[, .(anio, ext_total, ext_cesacion, ext_iniciacion,
                                                  interaccion, pct_cesacion, pct_iniciacion)],
                                         digits = 4)), collapse = "\n"))

log_msg("\n--- B) Muertes evitadas ---")

mm <- rbindlist(muertes)
fwrite(mm, file.path(out_dir, "88_muertes_por_rama.csv"))

ANIO_INICIO <- 2027L
tot <- mm[anio >= ANIO_INICIO, .(muertes = sum(muertes)), by = .(rama, anio)]
tw <- dcast(tot, anio ~ rama, value.var = "muertes")
tw[, evitadas := base - completa]
setorder(tw, anio)
tw[, evitadas_acum := cumsum(evitadas)]

por_ter <- mm[anio >= ANIO_INICIO & rama %in% c("base", "completa"),
              .(muertes = sum(muertes)), by = .(rama, tercil)]
pt <- dcast(por_ter, tercil ~ rama, value.var = "muertes")
pt[, `:=`(evitadas = base - completa, pct = 100 * (base - completa) / base)]

fwrite(tw, file.path(out_dir, "88_muertes_evitadas.csv"))
log_msg("Muertes acumuladas %d-2060, base %d contra escenario %d: %d evitadas (%.2f%%)",
        ANIO_INICIO, sum(tw$base), sum(tw$completa), sum(tw$evitadas),
        100 * sum(tw$evitadas) / sum(tw$base))
log_msg("Por tercil:\n%s", paste(capture.output(print(pt)), collapse = "\n"))
fwrite(pt, file.path(out_dir, "88_muertes_evitadas_por_tercil.csv"))

log_msg("\nADVERTENCIA: el nivel de muertes evitadas hereda la sobre-prediccion del stock")
log_msg("de fumadores en 45-64 y 65+, que es donde se concentra la mortalidad atribuible.")
log_msg("La comparacion entre ramas es mas robusta que el nivel: comparten poblacion.")

log_msg("\n=== outputs: 86_descomposicion_margen_extensivo.csv, 86_muertes_por_rama.csv,")
log_msg("    86_muertes_evitadas.csv, 86_muertes_evitadas_por_tercil.csv ===")
close(log_con)
