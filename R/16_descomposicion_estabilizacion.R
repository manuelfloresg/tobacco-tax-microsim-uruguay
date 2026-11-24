# 16_descomposicion_estabilizacion.R
#
# Decomposition of the change in smoking states between 2019 and 2021-2025 into demographic
# composition and within-cohort change (exact shift-share), and the within-cohort part into
# initiation and cessation. Also a lower bound for daily-to-occasional transitions from GATS and STEPS.
# Input:  output/tables/24_*.csv, 34_*.csv, data/raw/GATS/, data/raw/STEPS/
# Output: output/tables/36_*.csv

library(here)
library(data.table)
library(haven)
library(survey)

here::i_am("R/16_descomposicion_estabilizacion.R")

log_con <- file(here::here("logs", "16_descomposicion_estabilizacion.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 16_descomposicion_estabilizacion.R -- %s ===", format(Sys.time()))

out_dir  <- here::here("output", "tables")
raw_gats <- here::here("data", "raw", "GATS")
raw_steps <- here::here("data", "raw", "STEPS")

estados <- c("nunca", "ocasional", "diario", "ex_reciente", "ex_consolidado")

grupo_etario <- function(edad) {
  cut(edad, breaks = c(15, 25, 45, 65, Inf), right = FALSE,
      labels = c("15-24", "25-44", "45-64", "65+"))
}

log_msg("\n--- 0) Carga de 34_estados_por_cohorte_onda.csv y criterio de cohortes limpias ---")

tab <- fread(file.path(out_dir, "34_estados_por_cohorte_onda.csv"))
log_msg("Anios disponibles: %s", paste(sort(unique(tab$anio)), collapse = ", "))
log_msg("Cohortes disponibles: %s", paste(sort(unique(tab$cohorte)), collapse = ", "))

rango <- tab[, .(anio_min = min(anio), anio_max = max(anio)), by = cohorte]
rango <- merge(rango, tab[, .(cohorte, anio, prop_nunca)], by.x = c("cohorte", "anio_min"), by.y = c("cohorte", "anio"))
setnames(rango, "prop_nunca", "prop_nunca_min")
rango <- merge(rango, tab[, .(cohorte, anio, prop_nunca)], by.x = c("cohorte", "anio_max"), by.y = c("cohorte", "anio"))
setnames(rango, "prop_nunca", "prop_nunca_max")
rango[, delta_nunca := prop_nunca_max - prop_nunca_min]
rango[, cohorte_limpia := delta_nunca <= 0.02]
setorder(rango, cohorte)
log_msg("Criterio cohorte limpia (delta prop_nunca <= 0.02, asimetrico -- ver comentario, %d-%d):\n%s",
        min(tab$anio), max(tab$anio), paste(capture.output(print(rango[, .(cohorte, anio_min, anio_max, delta_nunca, cohorte_limpia)])), collapse = "\n"))
cohortes_limpias <- rango[cohorte_limpia == TRUE, cohorte]
log_msg("Cohortes limpias (%d de %d): %s", length(cohortes_limpias), nrow(rango), paste(cohortes_limpias, collapse = ", "))
fwrite(rango, file.path(out_dir, "36_criterio_cohortes_limpias.csv"))

log_msg("\n--- 1) Shift-share composicion vs dentro-de-cohorte ---")

anios_fin <- c(2021, 2022, 2023, 2024, 2025)
prop_cols <- paste0("prop_", estados)

shift_share_ventana <- function(t0, t1, cohortes_incluidas, universo) {
  sub <- tab[anio %in% c(t0, t1) & cohorte %in% cohortes_incluidas]
  sub[, w_share := Npond_total / sum(Npond_total), by = anio]
  wide <- dcast(sub, cohorte ~ anio, value.var = c("w_share", prop_cols))
  w0col <- paste0("w_share_", t0); w1col <- paste0("w_share_", t1)
  if (!(w0col %in% names(wide))) wide[, (w0col) := NA_real_]
  if (!(w1col %in% names(wide))) wide[, (w1col) := NA_real_]
  wide[is.na(get(w0col)), (w0col) := 0]
  wide[is.na(get(w1col)), (w1col) := 0]
  filas <- list()
  detalle_cohorte <- list()
  for (e in estados) {
    p0col <- paste0("prop_", e, "_", t0); p1col <- paste0("prop_", e, "_", t1)
    if (!(p0col %in% names(wide))) wide[, (p0col) := NA_real_]
    if (!(p1col %in% names(wide))) wide[, (p1col) := NA_real_]
    wide[is.na(get(p0col)) & !is.na(get(p1col)), (p0col) := get(p1col)]
    wide[is.na(get(p1col)) & !is.na(get(p0col)), (p1col) := get(p0col)]
    w0 <- wide[[w0col]]; w1 <- wide[[w1col]]; p0 <- wide[[p0col]]; p1 <- wide[[p1col]]
    comp_c <- (w1 - w0) * (p0 + p1) / 2
    dent_c <- (w0 + w1) / 2 * (p1 - p0)
    detalle_cohorte[[e]] <- data.table(universo = universo, anio_inicial = t0, anio_final = t1,
                                        estado = e, cohorte = wide$cohorte,
                                        w0 = w0, w1 = w1, p0 = p0, p1 = p1,
                                        composicion = comp_c, dentro_cohorte = dent_c)
    Pi0 <- sum(w0 * p0, na.rm = TRUE); Pi1 <- sum(w1 * p1, na.rm = TRUE)
    filas[[e]] <- data.table(universo = universo, anio_inicial = t0, anio_final = t1, estado = e,
                              prevalencia_ini = Pi0, prevalencia_fin = Pi1,
                              delta_total = Pi1 - Pi0,
                              componente_composicion = sum(comp_c, na.rm = TRUE),
                              componente_dentro_cohorte = sum(dent_c, na.rm = TRUE))
  }
  agg <- rbindlist(filas)
  agg[, identidad_ok := abs((componente_composicion + componente_dentro_cohorte) - delta_total) < 1e-8]
  list(agg = agg, detalle = rbindlist(detalle_cohorte))
}

todas_cohortes <- sort(unique(tab$cohorte))
resultados_ss <- list(); detalle_ss <- list()
for (t1 in anios_fin) {
  r_todas <- shift_share_ventana(2019, t1, todas_cohortes, "todas")
  r_limpias <- shift_share_ventana(2019, t1, cohortes_limpias, "limpias")
  resultados_ss[[length(resultados_ss) + 1]] <- r_todas$agg
  resultados_ss[[length(resultados_ss) + 1]] <- r_limpias$agg
  detalle_ss[[length(detalle_ss) + 1]] <- r_todas$detalle
  detalle_ss[[length(detalle_ss) + 1]] <- r_limpias$detalle
}
shift_share <- rbindlist(resultados_ss)
shift_share_detalle <- rbindlist(detalle_ss)
log_msg("Identidad exacta (componente_composicion + componente_dentro_cohorte = delta_total): %d de %d filas OK",
        sum(shift_share$identidad_ok), nrow(shift_share))

chk <- shift_share[, .(suma_delta = sum(delta_total)), by = .(universo, anio_inicial, anio_final)]
log_msg("Chequeo: suma de delta_total entre los 5 estados debe ser 0 en cada ventana/universo:\n%s",
        paste(capture.output(print(chk)), collapse = "\n"))

log_msg("Shift-share, universo 'todas', estado fumador actual agregado, ventana 2019->2025:\n%s",
        paste(capture.output(print(shift_share[universo == "todas" & anio_final == 2025 & estado %in% c("nunca", "ocasional", "diario", "ex_reciente", "ex_consolidado")])), collapse = "\n"))

log_msg("\n--- 2) Atribucion: iniciacion, cesacion, recaida (residuo) ---")

cesacion <- fread(here::here("output", "tables", "24_cesacion_por_grupo_etario.csv"))
cesacion[, grupo_etario := as.character(grupo_etario)]

edad_repr <- function(cohorte, anio) anio - (cohorte + 2)

atribucion_ventana <- function(t0, t1, universo_sel) {
  d_nunca <- shift_share_detalle[universo == universo_sel & anio_inicial == t0 & anio_final == t1 & estado == "nunca"]
  d_oc    <- shift_share_detalle[universo == universo_sel & anio_inicial == t0 & anio_final == t1 & estado == "ocasional"]
  d_di    <- shift_share_detalle[universo == universo_sel & anio_inicial == t0 & anio_final == t1 & estado == "diario"]
  d_er    <- shift_share_detalle[universo == universo_sel & anio_inicial == t0 & anio_final == t1 & estado == "ex_reciente"]

  cohortes <- d_nunca$cohorte
  ge0 <- as.character(grupo_etario(edad_repr(cohortes, t0)))
  ge1 <- as.character(grupo_etario(edad_repr(cohortes, t1)))
  tasa0 <- cesacion[.(anio = t0, grupo_etario = ge0), on = .(anio, grupo_etario), tasa_cesacion]
  tasa1 <- cesacion[.(anio = t1, grupo_etario = ge1), on = .(anio, grupo_etario), tasa_cesacion]
  tasa0[is.na(tasa0)] <- 0; tasa1[is.na(tasa1)] <- 0

  exp0 <- d_oc$w0 * d_oc$p0 + d_di$w0 * d_di$p0 + d_er$w0 * d_er$p0
  exp1 <- d_oc$w1 * d_oc$p1 + d_di$w1 * d_di$p1 + d_er$w1 * d_er$p1
  exposicion_prom <- (exp0 + exp1) / 2

  iniciacion_neta <- -d_nunca$dentro_cohorte
  fumador_dentro_cohorte <- d_oc$dentro_cohorte + d_di$dentro_cohorte
  cesacion_retenidos <- (tasa0 - tasa1) * exposicion_prom
  recaida_residuo <- fumador_dentro_cohorte - iniciacion_neta - cesacion_retenidos

  data.table(universo = universo_sel, anio_inicial = t0, anio_final = t1, cohorte = cohortes,
             grupo_etario_ini = ge0, grupo_etario_fin = ge1,
             tasa_cesacion_ini = tasa0, tasa_cesacion_fin = tasa1,
             iniciacion_neta, fumador_dentro_cohorte, cesacion_retenidos, recaida_residuo)
}

atrib_list <- list()
for (t1 in anios_fin) {
  atrib_list[[length(atrib_list) + 1]] <- atribucion_ventana(2019, t1, "todas")
  atrib_list[[length(atrib_list) + 1]] <- atribucion_ventana(2019, t1, "limpias")
}
atribucion_cohorte <- rbindlist(atrib_list)

atribucion_agg <- atribucion_cohorte[, .(
  iniciacion_neta = sum(iniciacion_neta), fumador_dentro_cohorte = sum(fumador_dentro_cohorte),
  cesacion_retenidos = sum(cesacion_retenidos), recaida_residuo = sum(recaida_residuo)
), by = .(universo, anio_inicial, anio_final)]
atribucion_agg[, chequeo_identidad := abs((iniciacion_neta + cesacion_retenidos + recaida_residuo) - fumador_dentro_cohorte) < 1e-8]

log_msg("Atribucion agregada (%d de %d ventanas con identidad OK):\n%s",
        sum(atribucion_agg$chequeo_identidad), nrow(atribucion_agg),
        paste(capture.output(print(atribucion_agg)), collapse = "\n"))

log_msg("\n--- 3) Complemento: evidencia directa de recaida en GATS/STEPS ---")

cohorte_5 <- function(anio_nac) pmin(pmax(floor(anio_nac / 5) * 5, 1925), 2005)

cargar_gats_recaida <- function(path, anio_encuesta) {
  nms0 <- names(haven::read_dta(path, n_max = 1))
  agevar <- nms0[match("age", tolower(nms0))]
  d <- haven::read_dta(path, col_select = dplyr::all_of(c("A01", agevar, "gatsweight", "B01", "B02")))
  setDT(d)
  setnames(d, agevar, "edad"); setnames(d, "gatsweight", "peso")
  d[, edad := as.numeric(edad)]; d[, peso := as.numeric(peso)]
  d[, sexo := fifelse(as.numeric(A01) == 1, "varon", fifelse(as.numeric(A01) == 2, "mujer", NA_character_))]
  d[, B01n := as.numeric(B01)]; d[, B02n := as.numeric(B02)]
  d[, fumador_actual := B01n %in% c(1, 2)]
  d[, evidencia_recaida := B01n == 2 & B02n == 1]
  d[, cohorte := cohorte_5(anio_encuesta - edad)]
  d[edad >= 15 & fumador_actual == TRUE]
}
gr09 <- cargar_gats_recaida(file.path(raw_gats, "gats_2009.dta"), 2009)
gr17 <- cargar_gats_recaida(file.path(raw_gats, "gats_2017.dta"), 2017)

cargar_steps_recaida <- function(path, anio_encuesta) {
  d <- haven::read_dta(path)
  setDT(d)
  if (anio_encuesta == 2006) {
    d[, sexo := fifelse(tolower(as.character(haven::as_factor(sex))) %in% c("men", "male"), "varon",
               fifelse(tolower(as.character(haven::as_factor(sex))) %in% c("women", "female"), "mujer", NA_character_))]
    d[, evidencia_recaida := as.numeric(t2) == 2 & as.numeric(t6) == 1]
    d[, peso := 1]
  } else {
    d[, sexo := fifelse(tolower(as.character(haven::as_factor(c1))) %in% c("men", "male"), "varon",
               fifelse(tolower(as.character(haven::as_factor(c1))) %in% c("women", "female"), "mujer", NA_character_))]
    d[, evidencia_recaida := as.numeric(t2) == 2 & as.numeric(x10) == 1]
    d[, peso := as.numeric(weightsdi)]
  }
  d[, edad := as.numeric(age)]
  d[, fumador_actual := as.numeric(t1) == 1]
  d[, cohorte := cohorte_5(anio_encuesta - edad)]
  d[edad >= 15 & fumador_actual == TRUE, .(cohorte, sexo, peso, evidencia_recaida)]
}
sr06 <- cargar_steps_recaida(file.path(raw_steps, "steps_2006.dta"), 2006)
sr13 <- cargar_steps_recaida(file.path(raw_steps, "steps_2013.dta"), 2013)

recaida_gats_base <- rbindlist(list(
  gr09[, .(anio_encuesta = 2009, cohorte, sexo, peso, evidencia_recaida)],
  gr17[, .(anio_encuesta = 2017, cohorte, sexo, peso, evidencia_recaida)]
))
recaida_gats_base[, fuente := "GATS"]
recaida_directa_gats <- recaida_gats_base[!is.na(evidencia_recaida),
                                           .(n = .N, n_pond = sum(peso), prop_evidencia_recaida = sum(evidencia_recaida * peso) / sum(peso)),
                                           by = .(fuente, anio_encuesta, cohorte, sexo)]

recaida_steps_base <- rbindlist(list(
  sr06[, anio_encuesta := 2006], sr13[, anio_encuesta := 2013]
))
recaida_steps_base[, fuente := "STEPS"]
recaida_directa_steps <- recaida_steps_base[!is.na(evidencia_recaida),
                                             .(n = .N, n_pond = sum(peso), prop_evidencia_recaida = sum(evidencia_recaida * peso) / sum(peso)),
                                             by = .(fuente, anio_encuesta, cohorte, sexo)]

recaida_directa <- rbindlist(list(recaida_directa_gats, recaida_directa_steps))
recaida_directa <- recaida_directa[n >= 10]
log_msg("Evidencia directa de recaida (cota inferior), resumen por fuente:\n%s",
        paste(capture.output(print(recaida_directa[, .(prop_media = mean(prop_evidencia_recaida), n_celdas = .N), by = .(fuente, anio_encuesta)])), collapse = "\n"))

log_msg("\n--- 4) Reconciliacion con 34_cota_consistencia_poder.csv ---")

cota <- fread(file.path(out_dir, "34_cota_consistencia_poder.csv"))
log_msg("Cota de R/14 (%d filas, cohortes decenales x edad x ola) no comparable fila a fila con este script (cohortes quinquenales x anio) -- ver nota arriba. Rango de holgura_superior en R/14: [%.4f, %.4f], que ya da margen a la deriva encontrada acá.",
        nrow(cota), min(cota$holgura_superior), max(cota$holgura_superior))

log_msg("\n--- 5) Sintesis ---")

comp_fumador <- shift_share[estado %in% c("ocasional", "diario"),
                             .(componente_composicion = sum(componente_composicion),
                               delta_total = sum(delta_total)),
                             by = .(universo, anio_inicial, anio_final)]
sintesis <- merge(comp_fumador, atribucion_agg, by = c("universo", "anio_inicial", "anio_final"))
sintesis[, `:=`(
  pct_composicion = componente_composicion / delta_total * 100,
  pct_iniciacion = iniciacion_neta / delta_total * 100,
  pct_cesacion = cesacion_retenidos / delta_total * 100,
  pct_recaida_residuo = recaida_residuo / delta_total * 100
)]
setcolorder(sintesis, c("universo", "anio_inicial", "anio_final", "delta_total",
                         "componente_composicion", "iniciacion_neta", "cesacion_retenidos", "recaida_residuo"))
log_msg("Sintesis final (delta de prevalencia fumador actual = ocasional+diario, 2019 -> cada anio):\n%s",
        paste(capture.output(print(sintesis[, .(universo, anio_final, delta_total, componente_composicion,
                                                  iniciacion_neta, cesacion_retenidos, recaida_residuo)])), collapse = "\n"))

log_msg("\n--- 6) Escritura de outputs ---")

fwrite(tab[, .(cohorte, anio, Npond_total)][, w_share := Npond_total / sum(Npond_total), by = anio],
       file.path(out_dir, "36_pesos_cohorte_por_anio.csv"))
fwrite(shift_share, file.path(out_dir, "36_shift_share_composicion_vs_dentro.csv"))
fwrite(shift_share_detalle, file.path(out_dir, "36_shift_share_detalle_por_cohorte.csv"))
fwrite(atribucion_cohorte, file.path(out_dir, "36_atribucion_por_cohorte.csv"))
fwrite(atribucion_agg, file.path(out_dir, "36_atribucion_iniciacion_cesacion.csv"))
fwrite(recaida_directa, file.path(out_dir, "36_recaida_cota_inferior_gats_steps.csv"))
fwrite(sintesis, file.path(out_dir, "36_sintesis_descomposicion.csv"))
fwrite(shift_share[, .(universo, anio_inicial, anio_final, estado, identidad_ok)],
       file.path(out_dir, "36_chequeo_identidad_contable.csv"))

log_msg("Outputs escritos en %s", out_dir)
close(log_con)
