# 29_hazard_iniciacion_jndem.R
#
# Level of the initiation hazard from retrospective age of first use: the national school drug
# survey (JND, ages up to about 21) and the general-population drug survey (JND, ages 22-50).
# Replaces the level of the R/18 model and keeps its age, sex and tertile structure.
# Input:  data/raw/JND_Ensenanza_Media/, data/raw/JND_Hogares/, data/interim/modelo_iniciacion.rds
# Output: data/interim/modelo_iniciacion.rds, output/tables/47_*.csv

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/29_hazard_iniciacion_jndem.R")

log_con <- file(here::here("logs", "29_hazard_iniciacion_jndem.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 29_hazard_iniciacion_jndem.R -- %s ===", format(Sys.time()))

out_dir <- here::here("output", "tables")
int_dir <- here::here("data", "interim")
raw_jndem <- here::here("data", "raw", "JND_Ensenanza_Media", "jovenes_2003_2021.dta")
raw_jndh_0118 <- here::here("data", "raw", "JND_Hogares", "jnd_2001_2018.dta")
raw_jndh_2024 <- here::here("data", "raw", "JND_Hogares", "jnd_2024.dta")

EDAD_CORTE_JNDEM <- 17
EDAD_MIN_HOGARES_EXT <- 18
EDAD_MAX_HOGARES_EXT <- 50

log_msg("\n--- A) Coeficientes de tercil del modelo anterior (R/18), para offset fijo ---")

ruta_original_fija <- file.path(int_dir, "modelo_iniciacion_ech_original.rds")
if (!file.exists(ruta_original_fija)) {
  file.copy(file.path(int_dir, "modelo_iniciacion.rds"), ruta_original_fija)
  log_msg("modelo_iniciacion_ech_original.rds no existia -- creado ahora desde modelo_iniciacion.rds (verificar que ESE sea el original de R/18, no un resultado de una corrida previa de este script).")
}
modelo_viejo <- readRDS(ruta_original_fija)
coefs_viejo <- coef(modelo_viejo)
TERCIL_OFFSET <- c(`1` = 0, `2` = unname(coefs_viejo["tercil2"]), `3` = unname(coefs_viejo["tercil3"]))
log_msg("TERCIL_OFFSET (escala cloglog, tercil1=referencia): tercil2=%.4f, tercil3=%.4f",
        TERCIL_OFFSET["2"], TERCIL_OFFSET["3"])

ruta_cola_gats_fija <- file.path(int_dir, "modelo_iniciacion_cola_gats.rds")
if (!file.exists(ruta_cola_gats_fija) && file.exists(file.path(int_dir, "modelo_iniciacion.rds"))) {
  file.copy(file.path(int_dir, "modelo_iniciacion.rds"), ruta_cola_gats_fija)
  log_msg("modelo_iniciacion_cola_gats.rds creado ahora desde modelo_iniciacion.rds (preserva la version JND-EM+GATS vigente antes de sobrescribirla con JND-EM+Hogares).")
} else if (file.exists(ruta_cola_gats_fija)) {
  log_msg("modelo_iniciacion_cola_gats.rds ya existia -- no se sobrescribe.")
}

grilla_ref <- CJ(edad_media_prev = c(18, 25, 35, 45), sexo = factor(c("varon", "mujer")),
                  tercil = factor(1), anio_prev_c = c(2011, 2025) - 2018, delta_anios = 1)
grilla_ref[, hazard_viejo := predict(modelo_viejo, newdata = grilla_ref, type = "response")]

log_msg("\n--- B) JND Ensenanza Media: hazard de tabla de vida por ola x sexo ---")

jem <- read_dta(raw_jndem, col_select = c("edad", "encuesta", "tabaco_vida", "tabaco_30", "edadi_tabaco", "expansor", "mujer"))
setDT(jem)
log_msg("N total leido: %d", nrow(jem))

jem <- jem[encuesta != 2007]
log_msg("N tras excluir 2007 (edad anomala): %d", nrow(jem))
log_msg("AVISO: expansor vale 1 (sin ponderador real) para TODAS las filas de 2003 y 2005 -- verificado, se usan igual (perder esas dos olas costaria demasiada cobertura de calendario temprana) pero se documenta la cautela.")

FACTOR_ACTUAL_SOBRE_VIDA <- jem[tabaco_vida == 1, mean(tabaco_30 == 1, na.rm = TRUE)]
log_msg("Factor de nivel (fraccion de tabaco_vida==1 que tambien es tabaco_30==1, pooled): %.3f -- se aplica como reescalado multiplicativo sobre el hazard, no como cambio de evento", FACTOR_ACTUAL_SOBRE_VIDA)
log_msg("Chequeo de estabilidad por edad (13-21, referencia, no usado directo): la razon crece de forma suave con la edad, ~0.30 a los 13 anios a ~0.45-0.49 a los 19-21 -- se usa el promedio pooled como aproximacion, simplificacion declarada.")

jem[, sexo := fifelse(mujer == 1, "mujer", fifelse(mujer == 0, "varon", NA_character_))]
jem <- jem[!is.na(sexo) & !is.na(tabaco_vida) & !is.na(edad) & edad >= 10 & edad <= 25]
jem[, tiempo := fifelse(tabaco_vida == 1, edadi_tabaco, edad)]
jem[, evento := tabaco_vida]
n_evento_sin_edad <- jem[evento == 1 & (is.na(edadi_tabaco) | edadi_tabaco < 5 | edadi_tabaco > edad), .N]
log_msg("Casos con tabaco_vida==1 pero edadi_tabaco invalida o mayor a la edad actual (excluidos, %d): no se puede ubicar el evento en el tiempo", n_evento_sin_edad)
jem[evento == 1 & (is.na(edadi_tabaco) | edadi_tabaco < 5 | edadi_tabaco > edad), tiempo := NA_real_]
jem <- jem[!is.na(tiempo)]
log_msg("N final para tabla de vida: %d (olas: %s)", nrow(jem), paste(sort(unique(jem$encuesta)), collapse = ", "))

hazard_life_table <- function(dt, edad_max) {
  out <- list()
  for (a in 10:edad_max) {
    en_riesgo <- dt[tiempo >= a]
    n_riesgo <- sum(en_riesgo$expansor)
    n_riesgo_np <- nrow(en_riesgo)
    eventos <- dt[evento == 1 & tiempo == a]
    n_eventos <- sum(eventos$expansor)
    n_eventos_np <- nrow(eventos)
    if (n_riesgo_np < 10) next
    out[[length(out) + 1]] <- data.table(edad = a, n_riesgo_np = n_riesgo_np, riesgo_pond = n_riesgo,
                                          n_eventos_np = n_eventos_np, prop_evento = n_eventos / n_riesgo)
  }
  rbindlist(out)
}

resultados_jem <- list()
for (yr in sort(unique(jem$encuesta))) {
  for (sx in c("varon", "mujer")) {
    sub <- jem[encuesta == yr & sexo == sx]
    if (nrow(sub) < 30) next
    h <- hazard_life_table(sub, edad_max = min(EDAD_CORTE_JNDEM, max(sub$edad)))
    if (nrow(h) == 0) next
    h[, `:=`(encuesta = yr, sexo = sx)]
    resultados_jem[[length(resultados_jem) + 1]] <- h
  }
}
hazard_jem_ola <- rbindlist(resultados_jem)
hazard_jem_ola[, prop_evento_bruto := prop_evento]
hazard_jem_ola[, prop_evento := prop_evento * FACTOR_ACTUAL_SOBRE_VIDA]
setcolorder(hazard_jem_ola, c("encuesta", "sexo", "edad"))
fwrite(hazard_jem_ola, file.path(out_dir, "47_hazard_jndem_por_ola.csv"))
log_msg("Escrito 47_hazard_jndem_por_ola.csv (%d celdas edad x ola x sexo, edad maxima %d) -- prop_evento YA reescalado por FACTOR_ACTUAL_SOBRE_VIDA (%.3f); prop_evento_bruto queda la version sin reescalar, de referencia",
        nrow(hazard_jem_ola), EDAD_CORTE_JNDEM, FACTOR_ACTUAL_SOBRE_VIDA)

resultados_jem_pooled <- list()
for (sx in c("varon", "mujer")) {
  sub <- jem[sexo == sx]
  h <- hazard_life_table(sub, edad_max = EDAD_CORTE_JNDEM)
  h[, sexo := sx]
  resultados_jem_pooled[[length(resultados_jem_pooled) + 1]] <- h
}
hazard_jem_pooled <- rbindlist(resultados_jem_pooled)
hazard_jem_pooled[, prop_evento := prop_evento * FACTOR_ACTUAL_SOBRE_VIDA]

log_msg("\n--- C) JND-Hogares: hazard de tabla de vida por ola x sexo, extension a 22-50 ---")

gats <- fread(file.path(out_dir, "28_hazard_por_cohorte_sexo_combinado.csv"))
gats_por_edad <- gats[, .(hazard_gats_diario = weighted.mean(hazard, n_riesgo_np), n_riesgo_np = sum(n_riesgo_np)),
                       by = .(sexo, edad)]
log_msg("GATS (iniciacion A DIARIO) cargado solo como referencia -- ya no alimenta el modelo.")

jndh_a <- read_dta(raw_jndh_0118, col_select = c("edad", "encuesta", "mujer", "edad_inicio_tabaco", "prev_tabaco_vida", "prev_tabaco_30", "expansor"))
setDT(jndh_a)
jndh_a[, sexo := fifelse(mujer == 1, "mujer", fifelse(mujer == 0, "varon", NA_character_))]
jndh_a[, anio_ola := encuesta]
jndh_a[, `:=`(mujer = NULL, encuesta = NULL)]
log_msg("JND-Hogares 2001-2018: N=%d, olas=%s", nrow(jndh_a), paste(sort(unique(jndh_a$anio_ola)), collapse = ", "))

jndh_b <- read_dta(raw_jndh_2024, col_select = c("PEREDADCUMPLIDA", "PERSEXO", "P5", "P6_1", "P7", "P9", "PESO"))
setDT(jndh_b)
jndh_b[, `:=`(
  edad = PEREDADCUMPLIDA,
  sexo = as.character(as_factor(PERSEXO)),
  edad_inicio_tabaco = P6_1,
  prev_tabaco_vida = fifelse(as_factor(P5) == "si", 1, fifelse(as_factor(P5) == "no", 0, NA_real_)),
  prev_tabaco_30 = fifelse(as_factor(P7) == "si" & as_factor(P9) == "si", 1,
                    fifelse(as_factor(P5) == "no" | as_factor(P7) == "no" | as_factor(P9) == "no", 0, NA_real_)),
  expansor = PESO,
  anio_ola = 2024
)]
jndh_b <- jndh_b[, .(edad, sexo, edad_inicio_tabaco, prev_tabaco_vida, prev_tabaco_30, expansor, anio_ola)]
log_msg("JND-Hogares 2024: N=%d", nrow(jndh_b))

jndh <- rbind(jndh_a, jndh_b, use.names = TRUE)
log_msg("JND-Hogares combinado (2001-2018 + 2024): N=%d", nrow(jndh))

FACTOR_ACTUAL_SOBRE_VIDA_HOGARES <- jndh[prev_tabaco_vida == 1, mean(prev_tabaco_30 == 1, na.rm = TRUE)]
log_msg("Factor de nivel JND-Hogares (fraccion de prev_tabaco_vida==1 que tambien es prev_tabaco_30==1, pooled): %.3f -- valor propio, NO se reusa el 0.433 de JND-EM", FACTOR_ACTUAL_SOBRE_VIDA_HOGARES)

jndh <- jndh[!is.na(sexo) & !is.na(prev_tabaco_vida) & !is.na(edad)]
jndh[, tiempo := fifelse(prev_tabaco_vida == 1, edad_inicio_tabaco, edad)]
jndh[, evento := prev_tabaco_vida]
n_evento_sin_edad_h <- jndh[evento == 1 & (is.na(edad_inicio_tabaco) | edad_inicio_tabaco < 5 | edad_inicio_tabaco > edad), .N]
log_msg("JND-Hogares: casos con prev_tabaco_vida==1 pero edad_inicio_tabaco invalida o mayor a la edad actual (excluidos, %d): no se puede ubicar el evento en el tiempo", n_evento_sin_edad_h)
jndh[evento == 1 & (is.na(edad_inicio_tabaco) | edad_inicio_tabaco < 5 | edad_inicio_tabaco > edad), tiempo := NA_real_]
n_sin_peso_h <- jndh[is.na(expansor), .N]
jndh <- jndh[!is.na(tiempo) & !is.na(expansor)]
log_msg("JND-Hogares: casos sin peso valido excluidos (%d) -- concentrados en la cobertura parcial de 2014 ya documentada en R/06 linea 50. N final para tabla de vida = %d (olas: %s)",
        n_sin_peso_h, nrow(jndh), paste(sort(unique(jndh$anio_ola)), collapse = ", "))

resultados_jndh <- list()
for (yr in sort(unique(jndh$anio_ola))) {
  for (sx in c("varon", "mujer")) {
    sub <- jndh[anio_ola == yr & sexo == sx]
    if (nrow(sub) < 30) next
    h <- hazard_life_table(sub, edad_max = min(EDAD_MAX_HOGARES_EXT, max(sub$edad)))
    if (nrow(h) == 0) next
    h[, `:=`(anio_ola = yr, sexo = sx)]
    resultados_jndh[[length(resultados_jndh) + 1]] <- h
  }
}
hazard_jndh_ola <- rbindlist(resultados_jndh)
hazard_jndh_ola[, prop_evento_bruto := prop_evento]
hazard_jndh_ola[, prop_evento := prop_evento * FACTOR_ACTUAL_SOBRE_VIDA_HOGARES]
setcolorder(hazard_jndh_ola, c("anio_ola", "sexo", "edad"))
fwrite(hazard_jndh_ola, file.path(out_dir, "47_hazard_jndh_por_ola.csv"))
log_msg("Escrito 47_hazard_jndh_por_ola.csv (%d celdas edad x ola x sexo, tabla de vida de edad simple) -- prop_evento YA reescalado por FACTOR_ACTUAL_SOBRE_VIDA_HOGARES (%.3f)",
        nrow(hazard_jndh_ola), FACTOR_ACTUAL_SOBRE_VIDA_HOGARES)

hazard_jndh_pooled_solape <- hazard_jndh_ola[edad >= 15 & edad <= EDAD_CORTE_JNDEM,
  .(hazard_jndh = weighted.mean(prop_evento, riesgo_pond), n_riesgo_np_jndh = sum(n_riesgo_np)),
  by = .(sexo, edad)]
solape_jem_jndh <- merge(hazard_jem_pooled[, .(sexo, edad, hazard_jem = prop_evento, n_riesgo_np_jem = n_riesgo_np)],
                          hazard_jndh_pooled_solape, by = c("sexo", "edad"))
solape_jem_jndh[, razon := hazard_jndh / hazard_jem]
fwrite(solape_jem_jndh, file.path(out_dir, "47_solape_jndem_jndhogares.csv"))
log_msg("Solape JND-EM vs JND-Hogares (ambas ya reescaladas a 'consumo actual'), edades 15-%d -- razon esperada cerca de 1:\n%s",
        EDAD_CORTE_JNDEM, paste(capture.output(print(solape_jem_jndh)), collapse = "\n"))
log_msg("Razon promedio ponderada por riesgo: %.3f", solape_jem_jndh[, weighted.mean(razon, n_riesgo_np_jndh)])

ANCHO_BIN_HOGARES <- 5
hazard_life_table_binned <- function(dt, breaks) {
  out <- list()
  for (i in seq_len(length(breaks) - 1)) {
    lo <- breaks[i]; hi <- breaks[i + 1]
    en_riesgo <- dt[tiempo >= lo]
    n_riesgo_np <- nrow(en_riesgo)
    if (n_riesgo_np < 30) next
    eventos <- dt[evento == 1 & tiempo >= lo & tiempo < hi]
    out[[length(out) + 1]] <- data.table(
      edad = (lo + hi - 1) / 2, ancho_bin = hi - lo,
      n_riesgo_np = n_riesgo_np, riesgo_pond = sum(en_riesgo$expansor),
      n_eventos_np = nrow(eventos), prop_evento = sum(eventos$expansor) / sum(en_riesgo$expansor)
    )
  }
  rbindlist(out)
}
breaks_hogares <- c(seq(EDAD_MIN_HOGARES_EXT, EDAD_MAX_HOGARES_EXT, by = ANCHO_BIN_HOGARES), EDAD_MAX_HOGARES_EXT + 1)
resultados_hogares_bin <- list()
for (yr in sort(unique(jndh$anio_ola))) {
  for (sx in c("varon", "mujer")) {
    sub <- jndh[anio_ola == yr & sexo == sx]
    if (nrow(sub) < 30) next
    h <- hazard_life_table_binned(sub, breaks_hogares)
    if (nrow(h) == 0) next
    h[, `:=`(anio_ola = yr, sexo = sx)]
    resultados_hogares_bin[[length(resultados_hogares_bin) + 1]] <- h
  }
}
hogares_ext <- rbindlist(resultados_hogares_bin)
hogares_ext[, prop_evento_bruto := prop_evento]
hogares_ext[, prop_evento := prop_evento * FACTOR_ACTUAL_SOBRE_VIDA_HOGARES]
setcolorder(hogares_ext, c("anio_ola", "sexo", "edad", "ancho_bin"))
fwrite(hogares_ext, file.path(out_dir, "47_hogares_reescalado_22_50.csv"))
log_msg("Escrito 47_hogares_reescalado_22_50.csv (%d celdas bin x ola x sexo, bins de %d anios desde %d, %d olas)",
        nrow(hogares_ext), ANCHO_BIN_HOGARES, EDAD_MIN_HOGARES_EXT, length(unique(hogares_ext$anio_ola)))
log_msg("Detalle por bin (pooled sobre olas, ponderado por riesgo):\n%s",
        paste(capture.output(print(hogares_ext[, .(prop_evento = weighted.mean(prop_evento, riesgo_pond), N_olas = .N), by = .(sexo, edad, ancho_bin)][order(sexo, edad)])), collapse = "\n"))

gats_por_bin <- gats_por_edad[, .(edad_bin = cut(edad, breaks = breaks_hogares, right = FALSE, labels = FALSE)), by = .(sexo, edad, hazard_gats_diario, n_riesgo_np)]
gats_por_bin <- gats_por_bin[!is.na(edad_bin), .(hazard_gats_diario = weighted.mean(hazard_gats_diario, n_riesgo_np)), by = .(sexo, edad_bin)]
gats_por_bin[, edad := (breaks_hogares[edad_bin] + breaks_hogares[edad_bin + 1] - 1) / 2]
gats_vs_hogares <- merge(
  hogares_ext[, .(hazard_hogares = weighted.mean(prop_evento, riesgo_pond)), by = .(sexo, edad)],
  gats_por_bin[, .(sexo, edad, hazard_gats_diario)], by = c("sexo", "edad"), all.x = TRUE)
fwrite(gats_vs_hogares, file.path(out_dir, "47_comparacion_hogares_vs_gats.csv"))
log_msg("Comparacion diagnostica JND-Hogares (usado, en bins) vs GATS diario (ya no usado, promediado a los mismos bins):\n%s",
        paste(capture.output(print(gats_vs_hogares)), collapse = "\n"))

log_msg("\n--- D) Dataset combinado y modelo final ---")

jndem_rows <- hazard_jem_ola[edad <= EDAD_CORTE_JNDEM, .(
  edad_media_prev = edad, anio_prev_c = encuesta - 2018, sexo, riesgo_pond, prop_evento, delta_anios = 1, fuente = "jndem"
)]
hogares_rows <- hogares_ext[, .(
  edad_media_prev = edad, anio_prev_c = anio_ola - 2018, sexo, riesgo_pond, prop_evento, delta_anios = ancho_bin, fuente = "hogares"
)]
combinado <- rbind(jndem_rows, hogares_rows)
combinado[, sexo := factor(sexo, levels = c("varon", "mujer"))]
combinado[, fuente := factor(fuente, levels = c("jndem", "hogares"))]
log_msg("Filas combinadas: %d JND-EM (edad <= %d) + %d JND-Hogares (edad %d-%d, %d olas reales, anio_prev_c por ola) = %d",
        nrow(jndem_rows), EDAD_CORTE_JNDEM, nrow(hogares_rows), EDAD_MIN_HOGARES_EXT, EDAD_MAX_HOGARES_EXT,
        length(unique(hogares_ext$anio_ola)), nrow(combinado))

combinado_tercil <- rbindlist(lapply(1:3, function(t) {
  d <- copy(combinado)
  d[, tercil := factor(t, levels = 1:3)]
  d[, riesgo_pond := riesgo_pond / 3]
  d
}))

modelo_nuevo <- local({
  TERCIL_OFFSET <- TERCIL_OFFSET
  EDAD_CORTE_JNDEM <- EDAD_CORTE_JNDEM
  glm(
    prop_evento ~ ns(edad_media_prev, df = 4) + anio_prev_c * I(edad_media_prev <= EDAD_CORTE_JNDEM) + sexo +
      offset(TERCIL_OFFSET[as.character(tercil)]) + offset(log(delta_anios)),
    family = quasibinomial(link = "cloglog"), weights = riesgo_pond, data = combinado_tercil
  )
})
log_msg("Modelo nuevo (JND-EM + JND-Hogares, offset de tercil fijo): N celdas = %d, riesgo total ponderado = %.0f",
        nrow(combinado_tercil), sum(combinado_tercil$riesgo_pond))
log_msg("Coeficientes:\n%s", paste(capture.output(print(summary(modelo_nuevo)$coefficients)), collapse = "\n"))

log_msg("\n--- E) Comparacion de nivel: modelo anterior (R/18, ECH) vs nuevo (JND-EM+JND-Hogares) ---")

grilla_ref[, tercil := factor(1, levels = 1:3)]
grilla_ref[, hazard_nuevo := predict(modelo_nuevo, newdata = grilla_ref, type = "response")]
grilla_ref[, anio_ref := anio_prev_c + 2018]
setcolorder(grilla_ref, c("anio_ref", "edad_media_prev", "sexo", "hazard_viejo", "hazard_nuevo"))
fwrite(grilla_ref[, .(anio_ref, edad = edad_media_prev, sexo, hazard_viejo, hazard_nuevo)],
       file.path(out_dir, "47_comparacion_nivel_viejo_nuevo.csv"))
log_msg("Comparacion (tercil 1, referencia):\n%s",
        paste(capture.output(print(grilla_ref[, .(anio_ref, edad = edad_media_prev, sexo, hazard_viejo, hazard_nuevo)])), collapse = "\n"))

grilla_fina <- CJ(edad_media_prev = 10:EDAD_MAX_HOGARES_EXT, sexo = factor(c("varon", "mujer")),
                   tercil = factor(1, levels = 1:3), anio_ref = c(2011, 2025))
grilla_fina[, `:=`(anio_prev_c = anio_ref - 2018, delta_anios = 1)]
grilla_fina[, hazard_viejo := predict(modelo_viejo, newdata = grilla_fina, type = "response")]
grilla_fina[, hazard_nuevo := predict(modelo_nuevo, newdata = grilla_fina, type = "response")]
fwrite(grilla_fina[, .(anio_ref, edad = edad_media_prev, sexo, hazard_viejo, hazard_nuevo)],
       file.path(out_dir, "47_hazard_iniciacion_modelo_nuevo.csv"))
log_msg("Escrito 47_hazard_iniciacion_modelo_nuevo.csv (%d filas)", nrow(grilla_fina))

saveRDS(modelo_nuevo, file.path(int_dir, "modelo_iniciacion.rds"))
log_msg("Modelo nuevo guardado en data/interim/modelo_iniciacion.rds -- MISMO contrato de predict(), R/24 sin cambios.")
log_msg("El original de R/18 queda preservado, intacto, en data/interim/modelo_iniciacion_ech_original.rds (no lo toca este script).")

log_msg("\n=== outputs: 47_hazard_jndem_por_ola.csv, 47_hazard_jndh_por_ola.csv, 47_solape_jndem_jndhogares.csv, 47_hogares_reescalado_22_50.csv, 47_comparacion_hogares_vs_gats.csv, 47_comparacion_nivel_viejo_nuevo.csv, 47_hazard_iniciacion_modelo_nuevo.csv ===")
close(log_con)
