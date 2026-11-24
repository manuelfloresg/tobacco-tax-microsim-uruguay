# 38_espacio_escenarios.R
#
# Status quo and the three tax trajectories (gradual and abrupt convergence to a 75% tax share,
# gradual convergence to 81.55%), 2025-2060, with results by income tertile and sex and the
# decomposition of the consumption change into extensive and intensive margins. One run with seed
# 20260809 for every branch.
# Input:  synthetic population 2011, output/tables/43_parametros_calibrados.csv
# Output: output/tables/59_*.csv

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/38_espacio_escenarios.R")

log_con <- file(here::here("logs", "38_espacio_escenarios.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 38_espacio_escenarios.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")
source(here::here("R", "24_motor_transiciones.R"))

ANIO_INICIO_ESCENARIO <- 2027L
HORIZONTE_GRADUAL <- 5L
ANIOS_SNAP <- c(2025L, seq(2030L, 2060L, by = 5L))
ANIO_MAX_PRECIO <- 2070L

SHARE_OMS <- 0.75
SHARE_EU_PROMEDIO <- 0.8155

S_ILICITO <- 0.12

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor,
            params_cal[parametro == "raw_par3"]$valor)
params_escalada <- params[1:2]; param_downshift <- params[3]

compute_factor_convergencia <- function(precio_lookup, share_objetivo) {
  tau_ancla <- attr(precio_lookup, "tau_ancla")
  ultimo_anio_dato <- attr(precio_lookup, "ultimo_anio_dato")
  if (is.null(tau_ancla) || is.null(ultimo_anio_dato)) {
    stop("compute_factor_convergencia(): precio_lookup no trae tau_ancla/ultimo_anio_dato -- ",
         "¿se construyo con regla distinta de mantenimiento_politica/impuesto_real_constante?")
  }
  p_ult_obs <- precio_lookup[anio == ultimo_anio_dato]$p_reales
  neto_efectivo <- p_ult_obs - tau_ancla
  (share_objetivo / (1 - share_objetivo)) * neto_efectivo / tau_ancla
}

construir_trayectoria_convergencia <- function(precio_lookup, share_objetivo, anio_inicio,
                                                modo = c("abrupto", "gradual"),
                                                horizonte_gradual = 5L, anio_max = 2070L) {
  modo <- match.arg(modo)
  factor_obj <- compute_factor_convergencia(precio_lookup, share_objetivo)
  anios <- anio_inicio:anio_max
  if (modo == "abrupto") {
    factores <- rep(factor_obj, length(anios))
  } else {
    rampa <- seq(1, factor_obj, length.out = horizonte_gradual + 1L)[-1]
    factores <- c(rampa, rep(factor_obj, max(0L, length(anios) - length(rampa))))[seq_along(anios)]
  }
  data.table(anio = anios, factor_impuesto_real = factores)
}

calcular_tau_stat <- function(precio_lookup, anios) {
  tau_ancla <- attr(precio_lookup, "tau_ancla")
  ultimo_anio_dato <- attr(precio_lookup, "ultimo_anio_dato")
  p_ult_obs <- precio_lookup[anio == ultimo_anio_dato]$p_reales
  neto_efectivo <- p_ult_obs - tau_ancla
  p <- precio_lookup[match(anios, anio)]$p_reales
  fifelse(anios > ultimo_anio_dato, p - neto_efectivo, NA_real_)
}

correr <- function(anios_snapshot, insumos, precio_lookup, precio_base = NULL, semilla = 20260809) {
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
                          param_downshift, precio_ratio_override = ov)
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
    data.table(escenario = etiqueta, anio = a, N = nrow(dt), prevalencia = mean(cig > 0),
               intensidad_media = sum(cig) / max(1, sum(cig > 0)), consumo_mill = sum(cig) * 365 / 1e6,
               p_reales = precio_lookup[anio == a]$p_reales)
  }))
}

resumir_tercil <- function(snaps_x, insumos_x, precio_lookup, etiqueta) {
  rbindlist(lapply(snaps_x, function(dt) {
    a <- dt$anio_snapshot[1]
    d <- copy(dt)
    d[, cig := calcular_cig_dia(d, a, insumos_x$intensidad, precio_lookup)]
    tot <- sum(d$cig)
    out <- d[cig > 0, .(N_fumadores = .N, consumo_mill = sum(cig) * 365 / 1e6,
                         share_consumo = sum(cig) / tot), by = tercil]
    n_tercil <- d[, .(N_tercil = .N), by = tercil]
    out <- merge(out, n_tercil, by = "tercil")
    out[, prevalencia := N_fumadores / N_tercil]
    out[, `:=`(escenario = etiqueta, anio = a)]
    out
  }))
}

resumir_sexo <- function(snaps_x, insumos_x, precio_lookup, etiqueta) {
  rbindlist(lapply(snaps_x, function(dt) {
    a <- dt$anio_snapshot[1]
    d <- copy(dt)
    d[, cig := calcular_cig_dia(d, a, insumos_x$intensidad, precio_lookup)]
    tot <- sum(d$cig)
    out <- d[cig > 0, .(N_fumadores = .N, consumo_mill = sum(cig) * 365 / 1e6,
                         share_consumo = sum(cig) / tot), by = sexo]
    n_sexo <- d[, .(N_sexo = .N), by = sexo]
    out <- merge(out, n_sexo, by = "sexo")
    out[, prevalencia := N_fumadores / N_sexo]
    out[, `:=`(escenario = etiqueta, anio = a)]
    out
  }))
}

log_msg("\n--- 1) Insumos compartidos y rama base ---")
insumos <- cargar_insumos_motor(regla_precio = "mantenimiento_politica", anio_max_precio = ANIO_MAX_PRECIO)
precio_base <- insumos$precio_lookup
ultimo_anio_dato <- attr(precio_base, "ultimo_anio_dato")
log_msg("tau_ancla=%.4f neto_ancla=%.4f anio_ancla=%d ultimo_anio_dato=%d",
        attr(precio_base, "tau_ancla"), attr(precio_base, "neto_ancla"),
        attr(precio_base, "anio_ancla"), ultimo_anio_dato)
log_msg("Participacion ilicita constante usada para recaudacion: %.0f%% (Ramos & Abascal) -- ", 100 * S_ILICITO)
log_msg("se cancela algebraicamente en recaudacion_relativa por ser igual en todas las ramas.")

t0 <- Sys.time()
snaps_base <- correr(ANIOS_SNAP, insumos, precio_lookup = precio_base, precio_base = NULL)
res_base <- resumir(snaps_base, insumos, precio_base, "base")
tercil_base <- resumir_tercil(snaps_base, insumos, precio_base, "base")
sexo_base   <- resumir_sexo(snaps_base, insumos, precio_base, "base")
log_msg("Rama base corrida en %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))

trayectorias <- list(
  conv75_abrupto = list(share = SHARE_OMS, modo = "abrupto"),
  conv75_gradual = list(share = SHARE_OMS, modo = "gradual"),
  convEU_gradual = list(share = SHARE_EU_PROMEDIO, modo = "gradual")
)

log_msg("\n--- 2) Factores de convergencia ---")
for (nm in names(trayectorias)) {
  tf <- trayectoria_fiscal(precio_base, trayectorias[[nm]]$share, ANIO_INICIO_ESCENARIO, trayectorias[[nm]]$modo,
                           HORIZONTE_GRADUAL, ANIO_MAX_PRECIO)
  log_msg("%s: carga total objetivo %.2f%% -> IMESI real x%.4f (+%.1f%%), precio x%.4f (+%.1f%%)",
          nm, 100 * trayectorias[[nm]]$share, max(tf$factor_imesi), 100 * (max(tf$factor_imesi) - 1),
          max(tf$ratio_precio), 100 * (max(tf$ratio_precio) - 1))
}

trayectorias_dt <- list(); precios <- list()
resultados <- list(); resultados_tercil <- list(); resultados_sexo <- list()

log_msg("\n--- 3) Corridas de escenario (%d trayectorias, pass-through 100%%) ---", length(trayectorias))

for (nombre_tray in names(trayectorias)) {
  cfg <- trayectorias[[nombre_tray]]
  traj_dt <- trayectoria_fiscal(precio_base, cfg$share, ANIO_INICIO_ESCENARIO, cfg$modo,
                                HORIZONTE_GRADUAL, ANIO_MAX_PRECIO)
  trayectorias_dt[[nombre_tray]] <- copy(traj_dt)[, escenario_impuesto := nombre_tray]
  precio_esc <- construir_precio_lookup(2010, ANIO_MAX_PRECIO, regla = "mantenimiento_politica",
                                         trayectoria_impuesto = traj_dt[, .(anio, factor_impuesto_real)])
  stopifnot(max(abs(precio_esc[anio %in% traj_dt$anio]$p_reales /
                      precio_base[anio %in% traj_dt$anio]$p_reales - traj_dt$ratio_precio)) < 1e-9)
  precios[[nombre_tray]] <- precio_esc

  t0 <- Sys.time()
  snaps <- correr(ANIOS_SNAP, insumos, precio_lookup = precio_esc, precio_base = precio_base)
  res <- resumir(snaps, insumos, precio_esc, nombre_tray)
  res[, escenario_impuesto := nombre_tray]
  resultados[[nombre_tray]] <- res
  tercil <- resumir_tercil(snaps, insumos, precio_esc, nombre_tray)
  tercil[, escenario_impuesto := nombre_tray]
  resultados_tercil[[nombre_tray]] <- tercil
  sx <- resumir_sexo(snaps, insumos, precio_esc, nombre_tray)
  sx[, escenario_impuesto := nombre_tray]
  resultados_sexo[[nombre_tray]] <- sx
  log_msg("%s corrida en %.1f min", nombre_tray, as.numeric(difftime(Sys.time(), t0, units = "mins")))
}

log_msg("\n--- 4) Armado de tablas de salida ---")

fwrite(rbindlist(trayectorias_dt), file.path(out_dir, "59_trayectorias_impuesto.csv"))

agregado <- rbindlist(c(list(res_base[, escenario_impuesto := "base"]), resultados), fill = TRUE)
setorder(agregado, escenario_impuesto, anio)
fwrite(agregado, file.path(out_dir, "59_espacio_escenarios_agregado.csv"))

base_wide <- res_base[, .(anio, N_base = N, prevalencia_base = prevalencia, intensidad_base = intensidad_media,
                           consumo_base = consumo_mill, p_reales_base = p_reales)]
comp <- merge(rbindlist(resultados), base_wide, by = "anio")
comp[, `:=`(
  aporte_extensivo = log(N * prevalencia / (N_base * prevalencia_base)),
  aporte_intensivo = log(intensidad_media / intensidad_base),
  d_ln_consumo     = log(consumo_mill / consumo_base)
)]

fac_tbl <- rbindlist(lapply(names(trayectorias_dt), function(nm) {
  tr <- trayectorias_dt[[nm]]
  data.table(escenario_impuesto = nm, anio = ANIOS_SNAP,
             factor_imesi = fifelse(ANIOS_SNAP %in% tr$anio, tr$factor_imesi[match(ANIOS_SNAP, tr$anio)], 1),
             factor_tributo_total = fifelse(ANIOS_SNAP %in% tr$anio, tr$factor_tributo_total[match(ANIOS_SNAP, tr$anio)], 1),
             ratio_precio = fifelse(ANIOS_SNAP %in% tr$anio, tr$ratio_precio[match(ANIOS_SNAP, tr$anio)], 1))
}))
comp <- merge(comp, fac_tbl, by = c("escenario_impuesto", "anio"))
comp[, `:=`(legal_mill = consumo_mill * (1 - S_ILICITO), legal_mill_base = consumo_base * (1 - S_ILICITO))]
comp[, recaudacion_relativa := (consumo_mill / consumo_base) * factor_tributo_total]
comp[, recaudacion_imesi_relativa := (consumo_mill / consumo_base) * factor_imesi]
comp[, recaudacion_iva_relativa := (consumo_mill / consumo_base) * ratio_precio]
comp[, N_base := NULL]
setorder(comp, escenario_impuesto, anio)
fwrite(comp, file.path(out_dir, "59_espacio_escenarios_comparado.csv"))

tercil_todo <- rbindlist(c(list(tercil_base[, escenario_impuesto := "base"]), resultados_tercil), fill = TRUE)
setorder(tercil_todo, escenario_impuesto, anio, tercil)
fwrite(tercil_todo, file.path(out_dir, "59_espacio_escenarios_tercil.csv"))

sexo_todo <- rbindlist(c(list(sexo_base[, escenario_impuesto := "base"]), resultados_sexo), fill = TRUE)
setorder(sexo_todo, escenario_impuesto, anio, sexo)
fwrite(sexo_todo, file.path(out_dir, "59_espacio_escenarios_sexo.csv"))

log_msg("\n--- 5) Chequeos ---")

chk <- merge(agregado[escenario_impuesto == "base", .(anio, prevalencia, consumo_mill)],
             agregado[escenario_impuesto == "conv75_abrupto" & anio < ANIO_INICIO_ESCENARIO, .(anio, prevalencia, consumo_mill)],
             by = "anio", suffixes = c("_base", "_esc"))
if (nrow(chk) > 0) {
  log_msg("Regresion (base vs escenario antes de %d, deben coincidir): max desvio prevalencia %.2e | consumo %.2e",
          ANIO_INICIO_ESCENARIO, max(abs(chk$prevalencia_base - chk$prevalencia_esc)),
          max(abs(chk$consumo_mill_base - chk$consumo_mill_esc)))
} else {
  log_msg("ADVERTENCIA: no hay años de snapshot antes de %d para chequear regresion.", ANIO_INICIO_ESCENARIO)
}

log_msg("\nResumen comparado (prevalencia/consumo/recaudacion relativa a la base, mismo año):")
log_msg("%s", paste(capture.output(print(comp[, .(escenario_impuesto, anio,
        extensivo_pct = round(100 * (exp(aporte_extensivo) - 1), 2),
        intensivo_pct = round(100 * (exp(aporte_intensivo) - 1), 2),
        consumo_pct = round(100 * (exp(d_ln_consumo) - 1), 2),
        recaudacion_relativa = round(recaudacion_relativa, 3))])), collapse = "\n"))

log_msg("\n=== outputs: 59_trayectorias_impuesto.csv, 59_espacio_escenarios_agregado.csv, ===")
log_msg("=== 59_espacio_escenarios_comparado.csv, 59_espacio_escenarios_tercil.csv ===")
close(log_con)
