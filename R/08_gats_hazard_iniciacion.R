# 08_gats_hazard_iniciacion.R
#
# Age-specific hazards of starting daily smoking by birth cohort and sex from the retrospective age
# of initiation in GATS 2009 and 2017 (survival analysis with age as the time scale).
# Input:  data/raw/GATS/
# Output: output/tables/28_*.csv

library(here)
library(data.table)
library(survival)

here::i_am("R/08_gats_hazard_iniciacion.R")

log_con <- file(here::here("logs", "08_gats_hazard_iniciacion.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 08_gats_hazard_iniciacion.R -- %s ===", format(Sys.time()))

raw_gats <- here::here("data", "raw", "GATS")
out_dir <- here::here("output", "tables")

cargar_gats <- function(path, anio_encuesta) {
  nms0 <- names(haven::read_dta(path, n_max = 1))
  agevar <- nms0[match("age", tolower(nms0))]
  educvar <- intersect(c("A04A", "A04C"), nms0)[1]
  educvar <- educvar[!is.na(educvar)]
  cols <- c("B01", "B02", "B03", "B04", "B08", "B11", "A01", educvar, agevar, "gatsweight")
  d <- haven::read_dta(path, col_select = dplyr::all_of(cols))
  setDT(d)
  setnames(d, agevar, "edad")
  setnames(d, "gatsweight", "peso")
  if (length(educvar) > 0) setnames(d, educvar[1], "educ") else d[, educ := NA_real_]
  for (v in c("B01", "B02", "B03", "B04", "B08", "B11", "educ", "edad", "peso")) d[, (v) := as.numeric(get(v))]
  d[, ever_diario := fifelse(B01 == 1, TRUE,
                     fifelse(B01 == 2 & B02 == 1, TRUE,
                     fifelse(B01 == 3 & B03 == 1, TRUE,
                     fifelse(is.na(B01), NA, FALSE))))]
  d[, edad_inicio_diario := fcase(
    !is.na(B04), B04, !is.na(B08), B08, !is.na(B11), B11, default = NA_real_
  )]
  d[, anio_encuesta := anio_encuesta]
  d[, anio_nac := anio_encuesta - edad]
  d[, sexo := fifelse(A01 == 1, "varon", fifelse(A01 == 2, "mujer", NA_character_))]
  d[]
}

g09 <- cargar_gats(file.path(raw_gats, "gats_2009.dta"), 2009)
g17 <- cargar_gats(file.path(raw_gats, "gats_2017.dta"), 2017)
G <- rbind(g09, g17)
G <- G[!is.na(ever_diario) & !is.na(edad) & !is.na(sexo)]
G[, cohorte := floor(anio_nac / 10) * 10]

log_msg("\n--- 1) Distribucion cruda de edad_inicio_diario (amontonamiento) ---")
crudo <- G[!is.na(edad_inicio_diario), .N, by = .(anio_encuesta, edad_inicio_diario)]
setorder(crudo, anio_encuesta, edad_inicio_diario)
fwrite(crudo, file.path(out_dir, "28_edad_inicio_distribucion_cruda.csv"))
tot_por_edad <- G[!is.na(edad_inicio_diario), .N, by = edad_inicio_diario][order(edad_inicio_diario)]
spikes <- tot_por_edad[edad_inicio_diario %in% c(15, 18, 20)]
log_msg("Casos en 15/18/20 (ambas olas, N=%d con dato): %s", sum(!is.na(G$edad_inicio_diario)),
        paste(capture.output(print(spikes)), collapse = " | "))
log_msg("Total por edad, referencia (14,16,17,19,21 para comparar altura del pico):\n%s",
        paste(capture.output(print(tot_por_edad[edad_inicio_diario %in% 13:22])), collapse = "\n"))

log_msg("\n--- 2) Hazard de iniciacion diaria por cohorte x sexo (por ola) ---")

G[, edad_en_ola := edad]
G[, tiempo := fifelse(ever_diario, edad_inicio_diario, edad_en_ola)]
G[, evento := as.integer(ever_diario)]
Gv <- G[!is.na(tiempo) & tiempo > 0 & edad_en_ola <= 65]
log_msg("Casos excluidos por tope de edad (>65) o tiempo invalido: %d de %d",
        nrow(G) - nrow(Gv), nrow(G))

hazard_life_table <- function(dt, edad_max = 40) {
  out <- list()
  for (a in 10:edad_max) {
    en_riesgo <- dt[tiempo >= a]
    n_riesgo <- en_riesgo[, sum(peso)]
    n_riesgo_np <- nrow(en_riesgo)
    eventos <- dt[evento == 1 & tiempo == a]
    n_eventos <- eventos[, sum(peso)]
    n_eventos_np <- nrow(eventos)
    if (n_riesgo_np < 10) next
    out[[length(out) + 1]] <- data.table(edad = a, n_riesgo_np = n_riesgo_np, n_eventos_np = n_eventos_np,
                                          hazard = round(n_eventos / n_riesgo, 5))
  }
  rbindlist(out)
}

resultados_ola <- list()
for (yr in c(2009, 2017)) {
  for (coh in sort(unique(Gv[anio_encuesta == yr]$cohorte))) {
    for (sx in c("varon", "mujer")) {
      sub <- Gv[anio_encuesta == yr & cohorte == coh & sexo == sx]
      if (nrow(sub) < 30) next
      h <- hazard_life_table(sub, edad_max = min(65, max(sub$edad_en_ola)))
      if (nrow(h) == 0) next
      h[, `:=`(anio_encuesta = yr, cohorte = coh, sexo = sx)]
      resultados_ola[[length(resultados_ola) + 1]] <- h
    }
  }
}
hazard_ola_dt <- rbindlist(resultados_ola)
setcolorder(hazard_ola_dt, c("anio_encuesta", "cohorte", "sexo", "edad"))
fwrite(hazard_ola_dt, file.path(out_dir, "28_hazard_por_cohorte_sexo_ola.csv"))
log_msg("Filas hazard por ola: %d", nrow(hazard_ola_dt))

resultados_comb <- list()
for (coh in sort(unique(Gv$cohorte))) {
  for (sx in c("varon", "mujer")) {
    sub <- Gv[cohorte == coh & sexo == sx]
    if (nrow(sub) < 30) next
    h <- hazard_life_table(sub, edad_max = min(65, max(sub$edad_en_ola)))
    if (nrow(h) == 0) next
    h[, `:=`(cohorte = coh, sexo = sx)]
    resultados_comb[[length(resultados_comb) + 1]] <- h
  }
}
hazard_comb_dt <- rbindlist(resultados_comb)
setcolorder(hazard_comb_dt, c("cohorte", "sexo", "edad"))
fwrite(hazard_comb_dt, file.path(out_dir, "28_hazard_por_cohorte_sexo_combinado.csv"))
log_msg("Filas hazard combinado (ambas olas): %d", nrow(hazard_comb_dt))

Gv[, cohorte_amplia := fcase(
  cohorte %in% c(1940, 1950), "1940-1950",
  cohorte %in% c(1960, 1970), "1960-1970",
  cohorte %in% c(1980, 1990), "1980-1990",
  default = NA_character_
)]
Gkm <- Gv[!is.na(cohorte_amplia)]
km <- survfit(Surv(tiempo, evento) ~ cohorte_amplia, data = Gkm, weights = peso)
km_smry <- summary(km, times = seq(10, 40, by = 5))
km_dt <- data.table(strata = as.character(km_smry$strata), edad = km_smry$time,
                     supervivencia = round(km_smry$surv, 4), n_riesgo = round(km_smry$n.risk, 0))
fwrite(km_dt, file.path(out_dir, "28_km_supervivencia_cohorte_amplia.csv"))
log_msg("Kaplan-Meier por cohorte amplia (nunca-diario, cada 5 anios):\n%s",
        paste(capture.output(print(km_dt)), collapse = "\n"))

log_msg("\n--- 3) Consistencia entre olas 2009 vs 2017 (sesgo de recuerdo) ---")

prop_a_edad <- function(dt, edad_ref) {
  d2 <- dt[edad_en_ola >= edad_ref]
  d2[, ya_inicio := ever_diario & edad_inicio_diario <= edad_ref]
  d2[, .(n = .N, N_pond = sum(peso), prop_ever_diario = round(sum(peso * ya_inicio) / sum(peso), 4)),
     by = .(anio_encuesta, cohorte, sexo)]
}
cons <- rbindlist(lapply(c(20, 25, 30), function(a) {
  p <- prop_a_edad(Gv, a)
  p[, edad_ref := a]
  p
}))
setcolorder(cons, c("edad_ref", "cohorte", "sexo", "anio_encuesta"))
w <- dcast(cons, edad_ref + cohorte + sexo ~ anio_encuesta,
           value.var = c("n", "prop_ever_diario"))
setnames(w, c("n_2009", "n_2017", "prop_ever_diario_2009", "prop_ever_diario_2017"),
         c("n_2009", "n_2017", "prop_2009", "prop_2017"), skip_absent = TRUE)
if (all(c("prop_2009", "prop_2017") %in% names(w))) {
  w[, diferencia_2017_menos_2009 := round(prop_2017 - prop_2009, 4)]
}
setorder(w, edad_ref, cohorte, sexo)
fwrite(w, file.path(out_dir, "28_consistencia_entre_olas.csv"))
log_msg("Consistencia entre olas (solo celdas con dato en ambas):\n%s",
        paste(capture.output(print(w[!is.na(n_2009) & !is.na(n_2017)])), collapse = "\n"))

log_msg("\n=== outputs: 28_edad_inicio_distribucion_cruda.csv, 28_hazard_por_cohorte_sexo_ola.csv, 28_hazard_por_cohorte_sexo_combinado.csv, 28_km_supervivencia_cohorte_amplia.csv, 28_consistencia_entre_olas.csv ===")
close(log_con)
