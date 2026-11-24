# 14_deriva_anual_y_cota.R
#
# Year-by-year drift of the never-smoked share by birth cohort in the ECH, and a consistency bound
# against ever-daily smoking in GATS with design-based intervals.
# Input:  panel from R/04, data/raw/GATS/
# Output: output/tables/34_*.csv

library(here)
library(data.table)
library(survey)

here::i_am("R/14_deriva_anual_y_cota.R")

log_con <- file(here::here("logs", "14_deriva_anual_y_cota.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 14_deriva_anual_y_cota.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")
raw_gats <- here::here("data", "raw", "GATS")

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))
estado_lab <- c(`1` = "nunca", `2` = "ocasional", `3` = "diario", `4` = "ex_reciente", `5` = "ex_consolidado")

log_msg("\n--- Parte A: serie por cohorte x onda, los 5 estados ---")

st <- P[, .(N_pond = sum(peso), n = .N), by = .(cohorte, anio, estado)]
w <- dcast(st, cohorte + anio ~ estado, value.var = c("N_pond", "n"), fill = 0)
for (e in 1:5) {
  npv <- paste0("N_pond_", e); nv <- paste0("n_", e)
  if (!(npv %in% names(w))) w[, (npv) := 0]
  if (!(nv %in% names(w))) w[, (nv) := 0]
}
w[, Npond_total := N_pond_1 + N_pond_2 + N_pond_3 + N_pond_4 + N_pond_5]
w[, n_total := n_1 + n_2 + n_3 + n_4 + n_5]
for (e in 1:5) {
  w[, (paste0("prop_", estado_lab[as.character(e)])) := get(paste0("N_pond_", e)) / Npond_total]
}
setnames(w, paste0("n_", 1:5), paste0("n_", estado_lab))
setorder(w, cohorte, anio)
fwrite(w, file.path(out_dir, "34_estados_por_cohorte_onda.csv"))
log_msg("Serie completa escrita en 34_estados_por_cohorte_onda.csv (%d filas)", nrow(w))

setorder(w, cohorte, anio)
cols_prop <- paste0("prop_", estado_lab)
for (cp in cols_prop) {
  w[, paste0("d_", cp) := get(cp) - shift(get(cp)), by = cohorte]
}
w[, anio_prev := shift(anio), by = cohorte]
w[, delta_anios := anio - anio_prev]

incrementos <- w[!is.na(anio_prev), c("cohorte", "anio_prev", "anio", "delta_anios",
                                       paste0("n_", estado_lab), paste0("d_", cols_prop)), with = FALSE]
for (cp in cols_prop) {
  incrementos[, paste0(cp, "_anual") := round(get(paste0("d_", cp)) / delta_anios, 5)]
}
fwrite(incrementos, file.path(out_dir, "34_incrementos_por_cohorte_onda.csv"))

coh_limpias <- seq(1960, 1990, by = 5)
log_msg("Cohortes usadas para el indicador agregado: %s (atricion media <2.5%% verificada en 25_cota_poder_por_cohorte.csv)",
        paste(coh_limpias, collapse = ", "))

agregado <- incrementos[cohorte %in% coh_limpias, .(
  n_cohortes = .N,
  d_nunca_anual_media = round(mean(prop_nunca_anual), 5),
  d_nunca_anual_de = round(sd(prop_nunca_anual), 5),
  d_ocasional_anual_media = round(mean(prop_ocasional_anual), 5),
  d_diario_anual_media = round(mean(prop_diario_anual), 5),
  d_ex_reciente_anual_media = round(mean(prop_ex_reciente_anual), 5),
  d_ex_consolidado_anual_media = round(mean(prop_ex_consolidado_anual), 5),
  n_nunca_total = sum(n_nunca)
), by = .(anio_prev, anio, delta_anios)]
setorder(agregado, anio_prev)
fwrite(agregado, file.path(out_dir, "34_indicador_agregado_1960_1990.csv"))
log_msg("Indicador agregado (cohortes 1960-1990), pp/anio, con la descomposicion por estado:\n%s",
        paste(capture.output(print(agregado)), collapse = "\n"))

log_msg("\n--- Parte B: cota diario_ECH <= alguna_vez_diario_GATS <= 1 - nunca_ECH ---")

cohorte_10 <- function(anio_nac) floor(anio_nac / 10) * 10

cargar_gats <- function(path, anio_encuesta) {
  nms0 <- names(haven::read_dta(path, n_max = 1))
  agevar <- nms0[match("age", tolower(nms0))]
  cols <- c("B01", "B02", "B03", "B04", "B08", "B11", "A01",
            "gatsstrata", "gatscluster", agevar, "gatsweight")
  d <- haven::read_dta(path, col_select = dplyr::all_of(cols))
  setDT(d)
  setnames(d, agevar, "edad"); setnames(d, "gatsweight", "peso")
  for (v in c("B01", "B02", "B03", "B04", "B08", "B11", "edad", "peso", "gatsstrata", "gatscluster")) {
    d[, (v) := as.numeric(get(v))]
  }
  d[, ever_diario := fifelse(B01 == 1, TRUE, fifelse(B01 == 2 & B02 == 1, TRUE,
                     fifelse(B01 == 3 & B03 == 1, TRUE, fifelse(is.na(B01), NA, FALSE))))]
  d[, edad_inicio_diario := fcase(!is.na(B04), B04, !is.na(B08), B08, !is.na(B11), B11, default = NA_real_)]
  d[, anio_nac := anio_encuesta - edad]
  d[!is.na(ever_diario)]
}
g09 <- cargar_gats(file.path(raw_gats, "gats_2009.dta"), 2009)
g17 <- cargar_gats(file.path(raw_gats, "gats_2017.dta"), 2017)
g09[, cohorte := cohorte_10(anio_nac)]; g17[, cohorte := cohorte_10(anio_nac)]

edades_ref <- c(20, 25, 30, 35)

gats_ever_diario_ic <- function(dt, edad_corte) {
  d2 <- dt[edad >= edad_corte]
  d2[, ever_a_esa_edad := as.numeric(ever_diario & edad_inicio_diario <= edad_corte)]
  out <- list()
  for (coh in sort(unique(d2$cohorte))) {
    sub <- d2[cohorte == coh]
    if (nrow(sub) < 30) next
    des <- tryCatch(survey::svydesign(ids = ~gatscluster, strata = ~gatsstrata, weights = ~peso, data = sub, nest = TRUE),
                     error = function(e) NULL)
    if (is.null(des)) next
    m <- tryCatch(survey::svymean(~ever_a_esa_edad, des), error = function(e) NULL)
    if (is.null(m)) next
    ci <- confint(m)
    out[[length(out) + 1]] <- data.table(cohorte = coh, edad = edad_corte, n = nrow(sub),
                                          alguna_vez_diario_gats = as.numeric(coef(m)),
                                          gats_ic_inf = ci[1], gats_ic_sup = ci[2])
  }
  rbindlist(out)
}
gats_20 <- rbindlist(lapply(edades_ref, function(a) gats_ever_diario_ic(g09, a)))
gats_17 <- rbindlist(lapply(edades_ref, function(a) gats_ever_diario_ic(g17, a)))
gats_20[, ola := 2009]; gats_17[, ola := 2017]
gats_all <- rbind(gats_20, gats_17)

Pc <- copy(P); Pc[, cohorte := cohorte_10(anio_nac)]
ech_terms <- Pc[edad %in% edades_ref, .(
  N = sum(peso), n = .N,
  diario_ech = sum(peso * (estado == 3)) / sum(peso),
  nunca_ech = sum(peso * (estado == 1)) / sum(peso)
), by = .(cohorte, edad)]
ech_terms[, alguna_vez_ech := 1 - nunca_ech]

cota <- merge(gats_all, ech_terms, by = c("cohorte", "edad"))
cota[, `:=`(
  diario_ech = round(diario_ech, 4), alguna_vez_ech = round(alguna_vez_ech, 4),
  alguna_vez_diario_gats = round(alguna_vez_diario_gats, 4),
  gats_ic_inf = round(gats_ic_inf, 4), gats_ic_sup = round(gats_ic_sup, 4)
)]
cota[, holgura_superior := round(alguna_vez_ech - alguna_vez_diario_gats, 4)]
cota[, holgura_superior_ic_inf := round(alguna_vez_ech - gats_ic_sup, 4)]
cota[, cumple_inferior := diario_ech <= gats_ic_sup]
cota[, cumple_superior_puntual := alguna_vez_diario_gats <= alguna_vez_ech]
cota[, cumple_superior_con_ic := gats_ic_inf <= alguna_vez_ech]
setcolorder(cota, c("ola", "cohorte", "edad", "n.x", "n.y", "diario_ech",
                     "alguna_vez_diario_gats", "gats_ic_inf", "gats_ic_sup", "alguna_vez_ech",
                     "holgura_superior", "holgura_superior_ic_inf",
                     "cumple_inferior", "cumple_superior_puntual", "cumple_superior_con_ic"))
setorder(cota, ola, cohorte, edad)
fwrite(cota, file.path(out_dir, "34_cota_consistencia_poder.csv"))
log_msg("Cota de consistencia (diario_ECH <= alguna_vez_diario_GATS <= 1-nunca_ECH):\n%s",
        paste(capture.output(print(cota[, .(ola, cohorte, edad, diario_ech, alguna_vez_diario_gats,
                                             gats_ic_inf, gats_ic_sup, alguna_vez_ech, holgura_superior,
                                             cumple_inferior, cumple_superior_puntual, cumple_superior_con_ic)])),
              collapse = "\n"))
log_msg("\nCota inferior (diario_ECH <= techo del IC de GATS) se cumple en %d de %d celdas",
        sum(cota$cumple_inferior), nrow(cota))
log_msg("Cota superior puntual (GATS <= alguna_vez_ECH) se cumple en %d de %d celdas",
        sum(cota$cumple_superior_puntual), nrow(cota))
log_msg("Cota superior con IC (piso del IC de GATS <= alguna_vez_ECH) se cumple en %d de %d celdas",
        sum(cota$cumple_superior_con_ic), nrow(cota))
log_msg("Holgura superior: media=%.4f, min=%.4f, max=%.4f",
        mean(cota$holgura_superior), min(cota$holgura_superior), max(cota$holgura_superior))

log_msg("\n=== outputs: 34_estados_por_cohorte_onda.csv, 34_incrementos_por_cohorte_onda.csv, 34_indicador_agregado_1960_1990.csv, 34_cota_consistencia_poder.csv ===")
close(log_con)
