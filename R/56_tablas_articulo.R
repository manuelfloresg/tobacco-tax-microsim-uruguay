# 56_tablas_articulo.R
#
# Tables of the article (English) and of the working paper (Spanish), and the values cited in the
# text. Does not run the model.
# Usage:  Rscript R/56_tablas_articulo.R      (article: paper/TC/tables/)
#         Rscript R/56_tablas_articulo.R es   (working paper: informe/tablas/)
# Input:  results in output/tables/
# Output: table1.md, table2.md, the CSV behind them, numeros_del_texto.csv

library(here)
library(data.table)

here::i_am("R/56_tablas_articulo.R")

ARGS   <- commandArgs(trailingOnly = TRUE)
IDIOMA <- if (length(ARGS) >= 1L && ARGS[1] %in% c("en", "es")) ARGS[1] else "en"

log_con <- file(here::here("logs", sprintf("56_tablas_articulo_%s.log", IDIOMA)),
                open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 56_tablas_articulo.R (idioma = %s) -- %s ===", IDIOMA, format(Sys.time()))

tab_dir  <- here::here("output", "tables")
out_dir  <- if (IDIOMA == "en") here::here("paper", "TC", "tables") else here::here("informe", "tablas")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

es <- IDIOMA == "es"
DEC <- if (es) "," else "."
L2 <- function(en, es_) if (es) es_ else en

ANIOS_TABLA   <- c(2030L, 2045L, 2060L)
ESC_PRINCIPAL <- "conv75_gradual"

ETIQ_ESC <- c(
  conv75_gradual = L2("WHO 75% tax share, gradual (primary scenario)",
                      "OMS 75%, gradual (principal)"),
  conv75_abrupto = L2("WHO 75% tax share, abrupt",
                      "OMS 75%, abrupto"),
  convEU_gradual = L2("EU average 81.55% tax share, gradual",
                      "Promedio UE 81,55%, gradual")
)
ORDEN_ESC <- names(ETIQ_ESC)

ETIQ_VAR <- c(
  C_S0_sin_gradiente        = L2("No gradient",            "Sin gradiente"),
  C_S6_gradiente_mediano    = L2("Median of the evidence", "Mediana de la evidencia"),
  C_principal               = L2("Primary",                "Principal"),
  C_S5_gradiente_muy_fuerte = L2("Upper bound",            "Cota superior"),
  C_S4_nivel_alternativo    = L2("Alternative level",      "Nivel alternativo")
)
ORDEN_VAR <- names(ETIQ_VAR)

ETIQ_REP <- c(
  keeler_acotado = L2("Bounded (primary)", "Acotado (principal)"),
  uniforme_50    = L2("Uniform 50%",       "Uniforme 50%"),
  keeler_literal = L2("Literal",           "Literal")
)
ORDEN_REP <- names(ETIQ_REP)
ETIQ_REP_SHARES <- gsub(".", DEC, c(
  keeler_acotado = "0.63 / 0.46 / 0.34",
  uniforme_50    = "0.50 / 0.50 / 0.50",
  keeler_literal = "1.00 / 0.42 / 0.00"
), fixed = TRUE)

leer <- function(f) {
  p <- file.path(tab_dir, f)
  if (!file.exists(p)) stop("falta ", f, ": correr el script que lo produce antes que este.")
  fread(p)
}

registro <- list()
anotar <- function(clave, valor, unidad, fuente) {
  registro[[length(registro) + 1L]] <<- data.table(
    clave = clave, valor = valor, unidad = unidad, fuente = fuente)
  invisible(valor)
}

log_msg("\n--- 1) Table 1: efecto agregado por trayectoria ---")

agg <- leer("59_espacio_escenarios_agregado.csv")
cmp <- leer("59_espacio_escenarios_comparado.csv")

base <- agg[escenario == "base", .(anio, N_base = N)]
d <- merge(cmp, base, by = "anio")
d[, `:=`(
  fumadores_esc  = N * prevalencia,
  fumadores_base = N_base * prevalencia_base
)]
d[, `:=`(
  consumo_pct    = 100 * (consumo_mill / consumo_base - 1),
  prev_pp        = 100 * (prevalencia - prevalencia_base),
  prev_pct       = 100 * (prevalencia / prevalencia_base - 1),
  fumadores_pct  = 100 * (fumadores_esc / fumadores_base - 1)
)]

bandas <- leer("77_mc_bandas.csv")
bd <- bandas[fuente == "capa 1 + prior del hazard",
             .(anio, resultado, central_emulador = central, lo = p2.5, hi = p97.5)]

fmt_num <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = DEC)
fmt_ic  <- function(lo, hi, d = 1)
  sprintf("(%s %s %s)", fmt_num(lo, d), L2("to", "a"), fmt_num(hi, d))

prev_base_pct <- setNames(100 * agg[escenario == "base"][match(ANIOS_TABLA, anio), prevalencia],
                          ANIOS_TABLA)

celda <- function(esc, a_, campo) {
  v <- d[escenario == esc & anio == a_][[campo]]
  res <- switch(campo,
    consumo_pct   = fmt_num(v, 1),
    prev_pp       = fmt_num(v, 2),
    fumadores_pct = fmt_num(v, 1),
    recaudacion_relativa = fmt_num(v, 3),
    fmt_num(v, 2))
  if (esc != ESC_PRINCIPAL) return(res)
  b <- switch(campo,
    consumo_pct          = bd[anio == a_ & resultado == "caida_consumo_pct"],
    prev_pp              = bd[anio == a_ & resultado == "caida_prevalencia_pct"],
    recaudacion_relativa = bd[anio == a_ & resultado == "recaudacion_relativa"],
    NULL)
  if (is.null(b) || !nrow(b)) return(res)
  if (campo == "prev_pp") {
    f <- prev_base_pct[[as.character(a_)]] / 100
    return(paste(res, fmt_ic(b$lo * f, b$hi * f, 2)))
  }
  paste(res, fmt_ic(b$lo, b$hi, if (campo == "recaudacion_relativa") 3 else 1))
}

sem_res <- fread(here::here("output", "tables", "91_semillas_resumen.csv"))
efs_r78 <- fread(here::here("output", "tables", "95_tc_efectos_por_semilla.csv"))
VARIANTE_R78 <- c(conv75_gradual = "principal", conv75_abrupto = "abrupto")
fila_muertes <- function(esc) {
  if (IDIOMA != "en") return(NULL)
  m <- efs_r78[variante == VARIANTE_R78[[esc]] & anio %in% ANIOS_TABLA,
               .(n = .N, media = mean(muertes_evitadas_cum),
                 ee = sd(muertes_evitadas_cum) / sqrt(.N)), keyby = anio]
  if (nrow(m) != length(ANIOS_TABLA) || any(m$n != 100L))
    stop("muertes de R/78 incompletas para ", esc, ": se esperan 100 semillas por anio")
  r67 <- sem_res[magnitud == "muertes_evitadas_total" & escenario == esc & anio == 2060L]
  if (nrow(r67) != 1L || abs(r67$media - m[anio == 2060L, media]) > 1e-6)
    stop("las muertes a 2060 de R/78 no reproducen las de R/67 para ", esc)
  data.table(indicador = "Deaths averted since 2027 (SE)",
             v = sprintf("%s (%s)", round(m$media), round(m$ee)))
}

filas_esc <- function(esc) {
  rbindlist(list(
    data.table(indicador = L2("Consumption, % change", "Consumo, % de cambio"),
               v = vapply(ANIOS_TABLA, function(a) celda(esc, a, "consumo_pct"), "")),
    data.table(indicador = L2("Prevalence, pp change", "Prevalencia, cambio pp"),
               v = vapply(ANIOS_TABLA, function(a) celda(esc, a, "prev_pp"), "")),
    data.table(indicador = L2("People who smoke, % change", "Fumadores, % de cambio"),
               v = vapply(ANIOS_TABLA, function(a) celda(esc, a, "fumadores_pct"), "")),
    data.table(indicador = L2("Revenue, ratio to status quo", "Recaudación, cociente"),
               v = vapply(ANIOS_TABLA, function(a) celda(esc, a, "recaudacion_relativa"), "")),
    fila_muertes(esc)
  ))[, .(bloque = ETIQ_ESC[[esc]], indicador,
         anio = rep(ANIOS_TABLA, length.out = .N), valor = v)]
}

b0 <- agg[escenario == "base" & anio %in% ANIOS_TABLA]
b0[, fumadores_miles := N * prevalencia / 1000]
niveles <- rbindlist(list(
  data.table(indicador = L2("Consumption, million sticks", "Consumo, millones"),
             anio = b0$anio, valor = fmt_num(b0$consumo_mill, 0)),
  data.table(indicador = L2("Prevalence, %", "Prevalencia, %"),
             anio = b0$anio, valor = fmt_num(100 * b0$prevalencia, 2)),
  data.table(indicador = L2("People who smoke, thousands", "Fumadores, miles"),
             anio = b0$anio, valor = fmt_num(b0$fumadores_miles, 0))
))[, .(bloque = L2("Status quo (levels)", "Statu quo"), indicador, anio, valor)]

ORDEN_T1 <- if (IDIOMA == "en") setdiff(ORDEN_ESC, "convEU_gradual") else ORDEN_ESC
t1_largo <- rbindlist(c(list(niveles), lapply(ORDEN_T1, filas_esc)))
t1 <- dcast(t1_largo, bloque + indicador ~ anio, value.var = "valor")
orden_bloque <- c(L2("Status quo (levels)", "Statu quo"), unname(ETIQ_ESC[ORDEN_T1]))
t1[, bloque := factor(bloque, levels = orden_bloque)]
orden_ind <- unique(t1_largo$indicador)
t1[, indicador := factor(indicador, levels = orden_ind)]
setorder(t1, bloque, indicador)
t1[, `:=`(bloque = as.character(bloque), indicador = as.character(indicador))]

fwrite(t1, file.path(out_dir, "table1_main.csv"))
log_msg("Table 1 (%d filas):\n%s", nrow(t1),
        paste(capture.output(print(t1)), collapse = "\n"))

for (a in ANIOS_TABLA) {
  anotar(sprintf("t1_consumo_pct_gradual_%d", a),
         d[escenario == ESC_PRINCIPAL & anio == a, consumo_pct], "%", "59_espacio_escenarios_comparado.csv")
  anotar(sprintf("t1_recaudacion_gradual_%d", a),
         d[escenario == ESC_PRINCIPAL & anio == a, recaudacion_relativa], "ratio", "59_espacio_escenarios_comparado.csv")
}
for (e in ORDEN_ESC)
  anotar(sprintf("t1_consumo_pct_%s_2060", e),
         d[escenario == e & anio == 2060L, consumo_pct], "%", "59_espacio_escenarios_comparado.csv")
for (r in c("caida_consumo_pct", "caida_prevalencia_pct", "recaudacion_relativa")) {
  b <- bd[anio == 2060L & resultado == r]
  anotar(paste0("ic95_lo_", r, "_2060"), b$lo, "-", "77_mc_bandas.csv (capa 1 + prior)")
  anotar(paste0("ic95_hi_", r, "_2060"), b$hi, "-", "77_mc_bandas.csv (capa 1 + prior)")
}
anotar("prev_pct_gradual_2060", d[escenario == ESC_PRINCIPAL & anio == 2060L, prev_pct], "%",
       "59_espacio_escenarios_comparado.csv")
anotar("fumadores_pct_gradual_2060", d[escenario == ESC_PRINCIPAL & anio == 2060L, fumadores_pct], "%",
       "59_espacio_escenarios_comparado.csv")

log_msg("\n--- 2) Table 2: sensibilidad ---")

var_el <- leer("72_variantes_elasticidad.csv")[grupo == "C"]
cons_v <- leer("72_sensibilidad_t1_consumo.csv")[variante %in% ORDEN_VAR & anio == 2060L]
cons_v <- cons_v[, .(consumo_pct = 100 * (sum(consumo_esc) / sum(consumo_base) - 1)), by = variante]
raz_v  <- leer("79_razon_efecto_politica_envolvente.csv")[anio == 2060L]

env_prev <- leer("79_razon_prevalencia_envolvente.csv")[anio == 2060L]
env_w <- dcast(env_prev, variante ~ escenario, value.var = c("prev_T1", "prev_T3"))
esc_col <- function(pref) {
  nm <- grep(paste0("^", pref, "_"), names(env_w), value = TRUE)
  nm[nm != paste0(pref, "_base")]
}
env_w[, `:=`(
  dpp_T1 = 100 * (get(esc_col("prev_T1")) - prev_T1_base),
  dpp_T3 = 100 * (get(esc_col("prev_T3")) - prev_T3_base)
)]
env_w[, brecha_pp := dpp_T1 - dpp_T3]

ram_ens <- leer("79_razon_prevalencia_ramas.csv")[escenario == "base"]
ENSANCHAMIENTO_PP <- ram_ens[anio == 2060L, brecha_T1_T3_pp] - ram_ens[anio == 2025L, brecha_T1_T3_pp]
stopifnot(length(ENSANCHAMIENTO_PP) == 1L, ENSANCHAMIENTO_PP > 0)
log_msg("Ensanchamiento de la brecha bajo statu quo 2025-2060: %.3fpp (denominador de la columna nueva).",
        ENSANCHAMIENTO_PP)

panelA <- Reduce(function(x, y) merge(x, y, by = "variante"),
                 list(var_el[, .(variante, gradiente_T1_sobre_T3, agregada)],
                      cons_v,
                      raz_v[, .(variante, razon_base, razon_esc, efecto)],
                      env_w[, .(variante, dpp_T1, dpp_T3, brecha_pp)]))
panelA[, variante := factor(variante, levels = ORDEN_VAR)]
setorder(panelA, variante)
panelA[, pct_ensanchamiento := 100 * (-brecha_pp) / ENSANCHAMIENTO_PP]
panelA_out <- panelA[, .(
  ETIQ_VAR[as.character(variante)],
  fmt_num(gradiente_T1_sobre_T3, 2),
  fmt_num(agregada, 3),
  fmt_num(consumo_pct, 1),
  sprintf("%s / %s", fmt_num(dpp_T1, 2), fmt_num(dpp_T3, 2)),
  fmt_num(brecha_pp, 2),
  fmt_num(pct_ensanchamiento, 0)
)]
setnames(panelA_out, c(
  L2("Variant", "Variante"),
  L2("T1/T3 elasticity ratio", "Razón de elasticidades T1/T3"),
  L2("Aggregate elasticity", "Elasticidad agregada"),
  L2("Consumption, % change", "Consumo, % de cambio"),
  L2("Prevalence change, T1 / T3, pp", "Cambio de prevalencia, T1 / T3, pp"),
  L2("Change in gap, pp", "Cambio en la brecha, pp"),
  L2("% of projected widening", "% del ensanchamiento proyectado")))
fwrite(panelA_out, file.path(out_dir, "table2_panelA_elasticity.csv"))
log_msg("Panel A:\n%s", paste(capture.output(print(panelA_out)), collapse = "\n"))
log_msg("Razon T1/T3 en el statu quo, rango entre variantes: %.3f a %.3f (va a la nota, no a una columna).",
        min(panelA$razon_base), max(panelA$razon_base))

n_var <- nrow(panelA)
n_neg <- panelA[efecto < 0, .N]
anotar("panelA_variantes_efecto_negativo", n_neg, sprintf("de %d", n_var), "79_razon_efecto_politica_envolvente.csv")
n_t1  <- panelA[dpp_T1 < dpp_T3, .N]
anotar("panelA_variantes_T1_cae_mas_pp", n_t1, sprintf("de %d", n_var), "79_razon_prevalencia_envolvente.csv")
anotar("panelA_pct_ensanchamiento_min", min(panelA$pct_ensanchamiento), "%", "79_razon_prevalencia_envolvente.csv")
anotar("panelA_pct_ensanchamiento_max", max(panelA$pct_ensanchamiento), "%", "79_razon_prevalencia_envolvente.csv")
log_msg("Efecto sobre la razon negativo en %d de %d variantes (diagnostico; la razon ya no va al cuerpo).", n_neg, n_var)
log_msg("Caida ABSOLUTA de prevalencia mayor en T1 que en T3 en %d de %d variantes.", n_t1, n_var)

rep_def <- leer("75_reparto_margen.csv")
rep_agg <- leer("75_margen_agregado.csv")[anio == 2060L]
rep_pro <- leer("75_margen_progresividad.csv")[anio == 2060L & metrica == "prevalencia"]
rep_pp  <- dcast(rep_pro, reparto ~ tercil, value.var = "dif_pp")
setnames(rep_pp, c("1", "2", "3"), c("dpp_T1", "dpp_T2", "dpp_T3"), skip_absent = TRUE)
rep_pp[, brecha_pp := dpp_T1 - dpp_T3]

panelB <- Reduce(function(x, y) merge(x, y, by = "reparto"),
                 list(rep_def[, .(reparto, share_ext_T1, share_ext_T2, share_ext_T3)],
                      rep_agg[, .(reparto, caida_consumo_pct, caida_fumadores_pct)],
                      rep_pp[, .(reparto, dpp_T1, dpp_T3, brecha_pp)]))
panelB[, reparto := factor(reparto, levels = ORDEN_REP)]
setorder(panelB, reparto)
panelB_out <- panelB[, .(
  ETIQ_REP[as.character(reparto)],
  ETIQ_REP_SHARES[as.character(reparto)],
  fmt_num(caida_consumo_pct, 1),
  fmt_num(caida_fumadores_pct, 2),
  sprintf("%s / %s", fmt_num(dpp_T1, 2), fmt_num(dpp_T3, 2))
)]
setnames(panelB_out, c(
  L2("Split", "Reparto"),
  L2("Extensive share, T1/T2/T3", "Proporción extensiva, T1/T2/T3"),
  L2("Consumption, % change", "Consumo, % de cambio"),
  L2("People who smoke, % change", "Fumadores, % de cambio"),
  L2("Prevalence change, T1 / T3, pp", "Cambio de prevalencia, T1 / T3, pp")))
fwrite(panelB_out, file.path(out_dir, "table2_panelB_margin.csv"))
log_msg("Panel B:\n%s", paste(capture.output(print(panelB_out)), collapse = "\n"))

for (r in ORDEN_REP)
  anotar(sprintf("panelB_fumadores_pct_%s_2060", r),
         panelB[reparto == r, caida_fumadores_pct], "%", "75_margen_agregado.csv")

prog_c <- leer("72_sensibilidad_t1_progresividad.csv")[
  variante %in% ORDEN_VAR & tercil == 1L & anio %in% c(2030L, 2045L, 2060L)]
envd <- prog_c[, .(variantes = .N, minimo_pp = min(dif_pp), maximo_pp = max(dif_pp),
                   principal_pp = dif_pp[variante == "C_principal"][1]), by = anio][order(anio)]
stopifnot(nrow(envd) == 3L, all(envd$variantes == length(ORDEN_VAR)))
panelC_out <- envd[, .(
  anio,
  fmt_num(principal_pp, 2),
  sprintf("%s %s %s", fmt_num(minimo_pp, 2), L2("to", "a"), fmt_num(maximo_pp, 2)),
  fifelse(minimo_pp * maximo_pp > 0, L2("yes", "sí"), "no")
)]
setnames(panelC_out, c(
  L2("Year", "Año"),
  L2("Central estimate, pp", "Estimación central, pp"),
  L2("Envelope across panel A, pp", "Envolvente del panel A, pp"),
  L2("Sign determined", "Signo determinado")))
fwrite(panelC_out, file.path(out_dir, "table2_panelC_envelope.csv"))
log_msg("Panel C:\n%s", paste(capture.output(print(panelC_out)), collapse = "\n"))

anotar("panelC_envolvente_min_2060", envd[anio == 2060L, minimo_pp], "pp", "72_sensibilidad_t1_progresividad.csv")
anotar("panelC_envolvente_max_2060", envd[anio == 2060L, maximo_pp], "pp", "72_sensibilidad_t1_progresividad.csv")
anotar("panelC_central_2060",        envd[anio == 2060L, principal_pp], "pp", "72_sensibilidad_t1_progresividad.csv")

log_msg("\n--- 3) Otras cifras citadas en Results y Methods ---")

ram <- leer("79_razon_prevalencia_ramas.csv")
for (a in c(2025L, 2060L)) {
  anotar(sprintf("razon_T1_T3_base_%d", a), ram[escenario == "base" & anio == a, razon_T1_T3], "-", "79_razon_prevalencia_ramas.csv")
  for (tt in c("prev_T1", "prev_T2", "prev_T3"))
    anotar(sprintf("%s_base_%d", tt, a), 100 * ram[escenario == "base" & anio == a][[tt]], "%", "79_razon_prevalencia_ramas.csv")
}
anotar("razon_T1_T3_gradual_2060", ram[escenario == ESC_PRINCIPAL & anio == 2060L, razon_T1_T3], "-", "79_razon_prevalencia_ramas.csv")
anotar("brecha_pp_base_2025", ram[escenario == "base" & anio == 2025L, brecha_T1_T3_pp], "pp", "79_razon_prevalencia_ramas.csv")
anotar("brecha_pp_base_2060", ram[escenario == "base" & anio == 2060L, brecha_T1_T3_pp], "pp", "79_razon_prevalencia_ramas.csv")

mar <- d[escenario == ESC_PRINCIPAL, .(anio, aporte_extensivo, aporte_intensivo, d_ln_consumo)]
mar[, share_ext_pct := 100 * aporte_extensivo / d_ln_consumo]
log_msg("Aporte del margen extensivo al cambio de log consumo (%s):\n%s", ESC_PRINCIPAL,
        paste(capture.output(print(mar, digits = 4)), collapse = "\n"))
for (a in ANIOS_TABLA)
  anotar(sprintf("share_extensivo_pct_%d", a), mar[anio == a, share_ext_pct], "%",
         "59_espacio_escenarios_comparado.csv")

tray <- leer("59_trayectorias_impuesto.csv")
reg <- tray[, .(factor_regimen = max(factor_impuesto_real)), by = escenario_impuesto]
log_msg("Factor de impuesto real en regimen:\n%s",
        paste(capture.output(print(reg, digits = 6)), collapse = "\n"))
for (i in seq_len(nrow(reg)))
  anotar(sprintf("factor_impuesto_%s", reg$escenario_impuesto[i]),
         100 * (reg$factor_regimen[i] - 1), "% sobre el impuesto real", "59_trayectorias_impuesto.csv")
reg3 <- tray[, .(imesi = max(factor_imesi), tributo = max(factor_tributo_total), precio = max(ratio_precio)),
             by = escenario_impuesto]
for (i in seq_len(nrow(reg3))) {
  e <- reg3$escenario_impuesto[i]
  anotar(sprintf("suba_imesi_%s", e), 100 * (reg3$imesi[i] - 1), "% sobre el IMESI real", "59_trayectorias_impuesto.csv")
  anotar(sprintf("suba_tributo_total_%s", e), 100 * (reg3$tributo[i] - 1), "% sobre IMESI + IVA por cajilla", "59_trayectorias_impuesto.csv")
  anotar(sprintf("suba_precio_%s", e), 100 * (reg3$precio[i] - 1), "% sobre el precio del statu quo", "59_trayectorias_impuesto.csv")
}

brk <- leer("80_resumen_dentro_bracket.csv")
log_msg("Posicion del backcast contra el bracket ampliado:\n%s",
        paste(capture.output(print(brk)), collapse = "\n"))
for (i in seq_len(nrow(brk))) {
  anotar(sprintf("bracket_dentro_%s", brk$grupo_etario[i]),
         brk$dentro_ancho[i], sprintf("de %d anios", brk$anios[i]), "80_resumen_dentro_bracket.csv")
  anotar(sprintf("bracket_exceso_max_%s", brk$grupo_etario[i]),
         brk$exceso_max_ancho_pp[i], "pp", "80_resumen_dentro_bracket.csv")
}
deff <- leer("80_deff_medido_resumen.csv")
anotar("deff_mediana", median(deff$deff_mediana), "-", "80_deff_medido_resumen.csv")

ruido <- leer("77_mc_ruido_resumen.csv")[anio == 2060L]
for (i in seq_len(nrow(ruido)))
  anotar(sprintf("ruido_de_%s_2060", ruido$resultado[i]), ruido$de[i], "pp", "77_mc_ruido_resumen.csv")

ap <- leer("77_mc_aporte_fuentes.csv")[anio == 2060L]
for (i in seq_len(nrow(ap)))
  anotar(sprintf("aporte_hazard_pct_%s_2060", ap$resultado[i]), ap$aporte_hazard_pct[i], "% del ancho",
         "77_mc_aporte_fuentes.csv")

log_msg("\n--- 4) Tablas del suplemento ---")

sup <- list(
  S3_elasticity_specification = leer("72_variantes_elasticidad.csv"),
  S4_margin_split             = leer("75_reparto_margen.csv"),
  S6_backcast_by_year         = leer("44_validacion_prevalencia_resumen_anual.csv"),
  S6_bracket_position         = leer("80_posicion_motor_bracket_ampliado.csv"),
  S6_design_effect            = leer("80_deff_medido_resumen.csv"),
  S7_bands_full               = leer("77_mc_bandas.csv"),
  S7_band_sources             = leer("77_mc_aporte_fuentes.csv"),
  S7_emulator_validation      = leer("77_mc_validacion_emulador.csv"),
  S8_scenarios_aggregate      = leer("59_espacio_escenarios_comparado.csv"),
  S8_scenarios_by_tertile     = leer("59_espacio_escenarios_tercil.csv")
)
sup$S8_envelope_full          <- leer("79_razon_prevalencia_envolvente.csv")
sup$S8_margin_variants        <- leer("75_margen_progresividad.csv")
sup$S9_historical_validation  <- leer("60_validacion_historica_fctc_episodio.csv")
sup$S1_tax_trajectories       <- leer("59_trayectorias_impuesto.csv")

for (nm in names(sup)) {
  fwrite(sup[[nm]], file.path(out_dir, paste0("supp_", nm, ".csv")))
  log_msg("  supp_%s.csv (%d filas)", nm, nrow(sup[[nm]]))
}

reg_dt <- rbindlist(registro)
reg_dt[, valor := round(valor, 6)]
fwrite(reg_dt, file.path(out_dir, "numeros_del_texto.csv"))
if (IDIOMA == "en") {
  fwrite(reg_dt, file.path(tab_dir, "82_numeros_del_texto.csv"))
  fwrite(t1,     file.path(tab_dir, "82_table1_main.csv"))
}
log_msg("\nVolcado de cifras (%d entradas):\n%s", nrow(reg_dt),
        paste(capture.output(print(reg_dt, nrows = 200)), collapse = "\n"))

log_msg("\n=== outputs: paper/tables/table1_main.csv, table2_panel{A,B,C}_*.csv,")
log_msg("    supp_*.csv, numeros_del_texto.csv; output/tables/82_* ===")

log_msg("\n--- 6) Fragmentos markdown ---")

esc_md <- function(x) as.character(x)

md_tabla <- function(d, alinear_izq = 1L, anchos = NULL) {
  enc <- names(d)
  if (is.null(anchos)) anchos <- rep(3L, length(enc))
  guiones <- strrep("-", pmax(3L, anchos))
  sep <- ifelse(seq_along(enc) %in% alinear_izq, paste0(":", guiones), paste0(guiones, ":"))
  filas <- apply(d, 1L, function(r) paste0("| ", paste(esc_md(r), collapse = " | "), " |"))
  c(paste0("| ", paste(esc_md(enc), collapse = " | "), " |"),
    paste0("|", paste(sep, collapse = "|"), "|"),
    filas)
}

EN <- IDIOMA == "en"
ANCHO_T1  <- if (EN) c(36L, 21L, 21L, 22L) else rep(3L, 4L)
ANCHO_T2A <- if (EN) c(23L, 10L, 11L, 14L, 15L, 12L, 13L) else NULL
ANCHO_T2B <- if (EN) c(22L, 22L, 17L, 19L, 20L) else NULL
titulo_panel <- function(txt) if (EN) c("::: {custom-style=\"TablePanel\"}", txt, ":::") else txt

sep1 <- md_tabla(data.frame(a = 1, b = 1, c = 1, d = 1), anchos = ANCHO_T1)[2]
lin1 <- c("|   | 2030 | 2045 | 2060 |", if (EN) sep1 else "|:---|---:|---:|---:|")
for (b in orden_bloque) {
  lin1 <- c(lin1, sprintf("| **%s** |   |   |   |", esc_md(b)))
  sub <- t1[bloque == b]
  for (i in seq_len(nrow(sub)))
    lin1 <- c(lin1, sprintf("| %s | %s | %s | %s |", esc_md(sub$indicador[i]),
                            esc_md(sub[["2030"]][i]), esc_md(sub[["2045"]][i]),
                            esc_md(sub[["2060"]][i])))
}
writeLines(lin1, file.path(out_dir, L2("table1.md", "tabla1.md")))

lin2 <- c(titulo_panel(L2("**Panel A. Elasticity gradient across income tertiles**",
             "**Panel A. Gradiente de elasticidad entre terciles de ingreso**")), "",
          md_tabla(panelA_out, anchos = ANCHO_T2A), "",
          titulo_panel(L2("**Panel B. Split of the price response between the extensive margin (quitting and not starting) and the intensive margin**",
             "**Panel B. Reparto de la respuesta al precio entre el margen extensivo (cesación y no inicio) y el intensivo**")), "",
          md_tabla(panelB_out, anchos = ANCHO_T2B),
          if (IDIOMA == "en") character(0) else c("",
          L2("**Panel C. Change in the lowest tertile's share of total consumption**",
             "**Panel C. Cambio en la participación del tercil inferior en el consumo total**"), "",
          md_tabla(panelC_out, alinear_izq = 1L)))
writeLines(lin2, file.path(out_dir, L2("table2.md", "tabla2.md")))
log_msg("  %s y %s (%d y %d lineas)", L2("table1.md", "tabla1.md"),
        L2("table2.md", "tabla2.md"), length(lin1), length(lin2))

log_msg("=== outputs: paper/tables/*.csv y *.md; output/tables/82_*, 83_* ===")
close(log_con)
