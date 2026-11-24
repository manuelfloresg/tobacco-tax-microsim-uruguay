# 06_artefacto_instrumento.R
#
# Tests whether the 2019 change in the ECH smoking question creates an artefact: national drug
# survey (JND) prevalence series 1998-2024, overlap with the ECH, the occasional-only share in
# 2017-2018, and occasional smoking in GATS 2009 and 2017.
# Input:  data/raw/JND_Hogares/, data/raw/GATS/, data/raw/ECH/, panel from R/04
# Output: output/tables/26*.csv

library(here)
library(data.table)

here::i_am("R/06_artefacto_instrumento.R")

log_con <- file(here::here("logs", "06_artefacto_instrumento.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 06_artefacto_instrumento.R -- %s ===", format(Sys.time()))

raw_ech <- here::here("data", "raw", "ECH")
raw_jnd <- here::here("data", "raw", "JND_Hogares")
raw_gats <- here::here("data", "raw", "GATS")
out_dir <- here::here("output", "tables")

log_msg("\n--- B.1.1: serie JND-Hogares ---")

jnd_series <- list()

d98 <- haven::read_dta(file.path(raw_jnd, "jnd_1998.dta"),
                        col_select = c("fumador", "sexo", "grupoeda", "tramos_e"))
setDT(d98)
log_msg("1998: tabla fumador (cruda) = %s", paste(capture.output(table(d98$fumador, useNA = "ifany")), collapse = " | "))
stopifnot(all(d98$fumador %in% c(0, 1, NA)))
jnd_series[["1998"]] <- data.table(
  anio = 1998, n = nrow(d98), n_valido = sum(!is.na(d98$fumador)),
  prev_30d_o_similar = round(mean(d98$fumador == 1, na.rm = TRUE), 4),
  variable_usada = "fumador (binaria, enunciado exacto no verificado contra formulario)",
  cobertura_edad = "solo grupos etarios, sin edad continua"
)

djnd <- haven::read_dta(file.path(raw_jnd, "jnd_2001_2018.dta"),
                         col_select = c("encuesta", "edad", "sexo", "prev_tabaco_30", "expansor"))
setDT(djnd)
setnames(djnd, "expansor", "peso")
for (yr in sort(unique(djnd$encuesta))) {
  sub <- djnd[encuesta == yr & edad >= 15]
  n_val <- sum(!is.na(sub$prev_tabaco_30))
  if (n_val == 0) {
    log_msg("%d: prev_tabaco_30 no disponible en esta base (0 de %d validos) -- se omite de la serie", yr, nrow(sub))
    next
  }
  prev_np <- mean(sub$prev_tabaco_30, na.rm = TRUE)
  ok <- !is.na(sub$prev_tabaco_30) & !is.na(sub$peso)
  prev_p  <- sum(sub$peso[ok] * sub$prev_tabaco_30[ok]) / sum(sub$peso[ok])
  jnd_series[[as.character(yr)]] <- data.table(
    anio = yr, n = nrow(sub), n_valido = n_val,
    prev_30d_o_similar = round(prev_p, 4),
    variable_usada = sprintf("prev_tabaco_30 (base_2001_2018.dta), peso=expansor (n_con_peso=%d de %d)", sum(ok), n_val),
    cobertura_edad = "edad continua"
  )
  log_msg("%d: n=%d n_valido=%d n_con_peso=%d prev_ponderada=%.1f%% prev_sin_ponderar=%.1f%%",
          yr, nrow(sub), n_val, sum(ok), 100 * prev_p, 100 * prev_np)
}

d24 <- haven::read_dta(file.path(raw_jnd, "jnd_2024.dta"),
                        col_select = c("P5", "P7", "P9", "PEREDADCUMPLIDA", "PERSEXO", "PESO"))
setDT(d24)
setnames(d24, c("PEREDADCUMPLIDA", "PERSEXO", "PESO"), c("edad", "sexo", "peso"))
log_msg("2024: rango de edad = %s (muestra JND-H 2024 tope en 65, a diferencia de la ECH sin tope)",
        paste(range(d24$edad, na.rm = TRUE), collapse = "-"))
sub24 <- d24[edad >= 15]
sub24[, fuma_30d := fcase(
  P5 == 2, 0,
  P5 == 1 & P7 == 2, 0,
  P5 == 1 & P7 == 1 & P9 == 2, 0,
  P5 == 1 & P7 == 1 & P9 == 1, 1,
  default = NA_real_
)]
prev24_p <- sum(sub24$peso * sub24$fuma_30d, na.rm = TRUE) / sum(sub24$peso[!is.na(sub24$fuma_30d)])
jnd_series[["2024"]] <- data.table(
  anio = 2024, n = nrow(sub24), n_valido = sum(!is.na(sub24$fuma_30d)),
  prev_30d_o_similar = round(prev24_p, 4),
  variable_usada = "P5/P7/P9 combinadas (jnd_2024.dta, ultimos 30 dias, reconstruye NA de salto de filtro)",
  cobertura_edad = "edad continua, TOPE EN 65 (no hay 65+ en esta ola)"
)
log_msg("2024: prev_ponderada=%.1f%% (n_valido=%d de %d)", 100 * prev24_p, sum(!is.na(sub24$fuma_30d)), nrow(sub24))

jnd_dt <- rbindlist(jnd_series)
setorder(jnd_dt, anio)
fwrite(jnd_dt, file.path(out_dir, "26_jnd_hogares_serie.csv"))

log_msg("\n--- B.1.2: ECH vs JND-Hogares ---")

Pech <- fread(here::here("data", "interim", "panel_personas_15mas_con_informante.csv.gz"))
ech_prev <- Pech[, .(prev_ech = round(sum(peso * (estado %in% c(2, 3))) / sum(peso), 4)), by = anio]
setorder(ech_prev, anio)

comp <- merge(jnd_dt[, .(anio, prev_jnd = prev_30d_o_similar)], ech_prev, by = "anio", all = TRUE)
setorder(comp, anio)
fwrite(comp, file.path(out_dir, "26_ech_vs_jnd.csv"))
log_msg("Comparacion ECH vs JND en anios solapados:\n%s", paste(capture.output(print(comp)), collapse = "\n"))

log_msg("\n--- B.1.3: ECH 2017-2018, ocasional puro y prevalencia binaria simulada ---")

sim_bin <- function(anio_k, arch, freq_vars) {
  cols <- c("e26", "e27", "e190", "pesoano", freq_vars)
  d <- haven::read_dta(file.path(raw_ech, arch), col_select = dplyr::all_of(cols))
  setDT(d)
  setnames(d, "pesoano", "peso")
  for (v in c("e27", "e190", "peso", freq_vars)) d[, (v) := as.numeric(get(v))]
  d[, diario := Reduce(`|`, lapply(freq_vars, function(v) !is.na(get(v)) & get(v) == 1))]
  d[, ocas   := Reduce(`|`, lapply(freq_vars, function(v) !is.na(get(v)) & get(v) == 2))]
  b <- d[e27 >= 15 & e190 %in% 1:4]
  n_fumadores <- b[e190 == 1, .N]
  n_ocas_puro <- b[e190 == 1 & ocas & !diario, .N]
  prop_ocas_puro <- n_ocas_puro / n_fumadores
  prev_real <- sum(b$peso * (b$e190 == 1)) / sum(b$peso)
  prev_sin_ocasional <- sum(b$peso * (b$e190 == 1 & !(b$ocas & !b$diario))) / sum(b$peso)
  data.table(anio = anio_k, n_fumadores = n_fumadores, n_ocasional_puro = n_ocas_puro,
             prop_ocasional_puro = round(prop_ocas_puro, 4),
             prevalencia_observada = round(prev_real, 4),
             prevalencia_binaria_simulada_cota_inf = round(prev_sin_ocasional, 4),
             efecto_redaccion_pp = round(100 * (prev_real - prev_sin_ocasional), 2))
}
sim17 <- sim_bin(2017, "ech17_2017.dta", c("e190_1_10", "e190_2_1", "e190_3_1"))
sim18 <- sim_bin(2018, "ech18_2018.dta", c("e190_1_0", "e190_2_1", "e190_3_1"))
sim_dt <- rbind(sim17, sim18)
fwrite(sim_dt, file.path(out_dir, "26_ech_efecto_redaccion.csv"))
log_msg("%s", paste(capture.output(print(sim_dt)), collapse = "\n"))

log_msg("\n--- B.1.4: GATS 2009 vs 2017, participacion ocasional ---")

gats_ocas <- function(path, anio_k) {
  nms0 <- names(haven::read_dta(path, n_max = 1))
  agevar <- nms0[match("age", tolower(nms0))]
  d <- haven::read_dta(path, col_select = dplyr::all_of(c("B01", agevar, "gatsweight")))
  setDT(d)
  setnames(d, c(agevar, "gatsweight"), c("edad", "peso"))
  b <- d[edad >= 15 & B01 %in% c(1, 2)]
  n_actual <- nrow(b)
  n_ocas <- b[B01 == 2, .N]
  prop_pond <- sum(b$peso * (b$B01 == 2)) / sum(b$peso)
  data.table(anio = anio_k, n_fumadores_actuales = n_actual, n_ocasional = n_ocas,
             prop_ocasional_sin_ponderar = round(n_ocas / n_actual, 4),
             prop_ocasional_ponderada = round(prop_pond, 4))
}
gats09 <- gats_ocas(file.path(raw_gats, "gats_2009.dta"), 2009)
gats17 <- gats_ocas(file.path(raw_gats, "gats_2017.dta"), 2017)
gats_dt <- rbind(gats09, gats17)
fwrite(gats_dt, file.path(out_dir, "26_gats_ocasional.csv"))
log_msg("%s", paste(capture.output(print(gats_dt)), collapse = "\n"))

log_msg("\n--- A.1: participacion ocasional/(ocasional+diario) en la ECH, todos los anios ---")

Pech2 <- fread(here::here("data", "interim", "panel_personas_15mas_con_informante.csv.gz"))
ech_ocas <- Pech2[estado %in% c(2, 3), .(N_pond = sum(peso)), by = .(anio, estado)]
w <- dcast(ech_ocas, anio ~ estado, value.var = "N_pond", fill = 0)
setnames(w, c("2", "3"), c("ocasional", "diario"), skip_absent = TRUE)
w[, prop_ocasional_entre_fumadores := round(ocasional / (ocasional + diario), 4)]
setorder(w, anio)
fwrite(w, file.path(out_dir, "26A_ech_participacion_ocasional.csv"))
log_msg("%s", paste(capture.output(print(w[, .(anio, prop_ocasional_entre_fumadores)])), collapse = "\n"))

log_msg("\n--- A.2: JND-Hogares y ECH restringidas a 15-64 ---")

jnd_1564 <- list()
d98b <- haven::read_dta(file.path(raw_jnd, "jnd_1998.dta"), col_select = c("fumador", "grupoeda"))
setDT(d98b)
log_msg("1998: grupoeda no permite recorte fino a 64 -- se deja tabla de grupoeda para inspeccion manual: %s",
        paste(capture.output(table(d98b$grupoeda, useNA = "ifany")), collapse = " | "))

djnd2 <- haven::read_dta(file.path(raw_jnd, "jnd_2001_2018.dta"),
                          col_select = c("encuesta", "edad", "prev_tabaco_30", "expansor"))
setDT(djnd2); setnames(djnd2, "expansor", "peso")
for (yr in sort(unique(djnd2$encuesta))) {
  sub <- djnd2[encuesta == yr & edad >= 15 & edad <= 64]
  ok <- !is.na(sub$prev_tabaco_30) & !is.na(sub$peso)
  if (sum(ok) == 0) next
  jnd_1564[[as.character(yr)]] <- data.table(
    anio = yr, n_1564 = nrow(sub), n_valido = sum(!is.na(sub$prev_tabaco_30)),
    prev_1564 = round(sum(sub$peso[ok] * sub$prev_tabaco_30[ok]) / sum(sub$peso[ok]), 4))
}
sub24b <- sub24[edad >= 15 & edad <= 64]
jnd_1564[["2024"]] <- data.table(
  anio = 2024, n_1564 = nrow(sub24b), n_valido = sum(!is.na(sub24b$fuma_30d)),
  prev_1564 = round(sum(sub24b$peso * sub24b$fuma_30d, na.rm = TRUE) / sum(sub24b$peso[!is.na(sub24b$fuma_30d)]), 4))
jnd_1564_dt <- rbindlist(jnd_1564)
setorder(jnd_1564_dt, anio)
fwrite(jnd_1564_dt, file.path(out_dir, "26A_jnd_hogares_15_64.csv"))
log_msg("JND-Hogares 15-64:\n%s", paste(capture.output(print(jnd_1564_dt)), collapse = "\n"))

ech_1564 <- Pech2[edad >= 15 & edad <= 64, .(
  prev_1564 = round(sum(peso * (estado %in% c(2, 3))) / sum(peso), 4)
), by = anio]
setorder(ech_1564, anio)
fwrite(ech_1564, file.path(out_dir, "26A_ech_15_64.csv"))
log_msg("ECH 15-64:\n%s", paste(capture.output(print(ech_1564)), collapse = "\n"))

comp_1564 <- merge(jnd_1564_dt[, .(anio, prev_jnd_1564 = prev_1564)],
                    ech_1564[, .(anio, prev_ech_1564 = prev_1564)], by = "anio", all = TRUE)
setorder(comp_1564, anio)
fwrite(comp_1564, file.path(out_dir, "26A_ech_vs_jnd_15_64.csv"))
log_msg("Comparacion 15-64:\n%s", paste(capture.output(print(comp_1564)), collapse = "\n"))

log_msg("\n=== outputs: 26_jnd_hogares_serie.csv, 26_ech_vs_jnd.csv, 26_ech_efecto_redaccion.csv, 26_gats_ocasional.csv, 26A_ech_participacion_ocasional.csv, 26A_jnd_hogares_15_64.csv, 26A_ech_15_64.csv, 26A_ech_vs_jnd_15_64.csv ===")
close(log_con)
