# 12_escalada.R
#
# Occasional and daily smoking stocks by age and cohort in the ECH, consistency with GATS, and the
# distribution of the five states at age 15 by sex and income tertile (the states of the cohorts
# that enter the simulation each year).
# Input:  panel from R/04, data/raw/GATS/, data/raw/JND_Ensenanza_Media/
# Output: output/tables/32_*.csv

library(here)
library(data.table)

here::i_am("R/12_escalada.R")

log_con <- file(here::here("logs", "12_escalada.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 12_escalada.R -- %s ===", format(Sys.time()))

out_dir <- here::here("output", "tables")
int_dir <- here::here("data", "interim")
raw_gats <- here::here("data", "raw", "GATS")
src_jndem <- here::here("data", "raw", "JND_Ensenanza_Media", "jovenes_2003_2021.dta")

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))

log_msg("\n--- D.1: prevalencia y participacion de ocasional por edad ---")

d1 <- P[estado %in% c(2, 3), .(N_pond = sum(peso)), by = .(anio, edad, estado)]
w <- dcast(d1, anio + edad ~ estado, value.var = "N_pond", fill = 0)
setnames(w, c("2", "3"), c("ocasional", "diario"), skip_absent = TRUE)
for (v in c("ocasional", "diario")) if (!(v %in% names(w))) w[, (v) := 0]
w[, participacion_ocasional := round(ocasional / (ocasional + diario), 4)]

tot15 <- P[, .(N_pond_total = sum(peso)), by = .(anio, edad)]
w <- merge(w, tot15, by = c("anio", "edad"))
w[, prevalencia_ocasional := round(ocasional / N_pond_total, 4)]
setorder(w, anio, edad)
fwrite(w, file.path(out_dir, "32_ocasional_por_edad_anio.csv"))

P[, tramo := cut(edad, breaks = c(15, 25, 35, 45, 55, 65, Inf), right = FALSE,
                  labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))]
d1b <- P[estado %in% c(2, 3), .(N_pond = sum(peso)), by = .(tramo, estado)]
w2 <- dcast(d1b, tramo ~ estado, value.var = "N_pond", fill = 0)
setnames(w2, c("2", "3"), c("ocasional", "diario"), skip_absent = TRUE)
tot_tramo <- P[, .(N_total = sum(peso)), by = tramo]
w2 <- merge(w2, tot_tramo, by = "tramo")
w2[, `:=`(prevalencia_ocasional = round(ocasional / N_total, 4),
          participacion_ocasional = round(ocasional / (ocasional + diario), 4))]
setorder(w2, tramo)
fwrite(w2, file.path(out_dir, "32_ocasional_por_tramo.csv"))
log_msg("Prevalencia y participacion de ocasional por tramo etario amplio (todos los anios):\n%s",
        paste(capture.output(print(w2)), collapse = "\n"))

log_msg("\n--- Parte C.1-C.2: stocks ocasional/diario por cohorte (ECH) y flujo neto ---")

stocks_ech <- P[estado %in% c(1, 2, 3), .(
  N_pond = sum(peso), n = .N
), by = .(cohorte, anio, estado)]
w_stocks <- dcast(stocks_ech, cohorte + anio ~ estado, value.var = c("N_pond", "n"), fill = 0)
setnames(w_stocks,
         old = c("N_pond_1", "N_pond_2", "N_pond_3", "n_1", "n_2", "n_3"),
         new = c("Npond_nunca", "Npond_ocasional", "Npond_diario", "n_nunca", "n_ocasional", "n_diario"),
         skip_absent = TRUE)
for (v in c("Npond_nunca", "Npond_ocasional", "Npond_diario", "n_nunca", "n_ocasional", "n_diario")) {
  if (!(v %in% names(w_stocks))) w_stocks[, (v) := 0]
}
w_stocks[, Npond_total := Npond_nunca + Npond_ocasional + Npond_diario]
w_stocks[, `:=`(prev_ocasional = round(Npond_ocasional / Npond_total, 4),
                 prev_diario = round(Npond_diario / Npond_total, 4))]
setorder(w_stocks, cohorte, anio)
fwrite(w_stocks, file.path(out_dir, "32_stocks_ocasional_diario_cohorte.csv"))
log_msg("Stocks por cohorte x anio escritos en 32_stocks_ocasional_diario_cohorte.csv (%d filas)", nrow(w_stocks))

setorder(w_stocks, cohorte, anio)
w_stocks[, `:=`(d_ocasional = prev_ocasional - shift(prev_ocasional),
                 d_diario = prev_diario - shift(prev_diario),
                 anio_prev = shift(anio)), by = cohorte]
flujo_neto <- w_stocks[!is.na(anio_prev), .(cohorte, anio_prev, anio, prev_ocasional, prev_diario,
                                             flujo_neto_ocasional = round(d_ocasional, 4),
                                             flujo_neto_diario = round(d_diario, 4),
                                             n_ocasional, n_diario)]
fwrite(flujo_neto, file.path(out_dir, "32_flujo_neto_implicito.csv"))
log_msg("ADVERTENCIA: estos son flujos NETOS (entrada menos salida simultanea). Los flujos brutos")
log_msg("(cuantos entraron a ocasional, cuantos salieron, por separado) NO estan identificados")
log_msg("sin estructura adicional -- un flujo neto ~0 es compatible tanto con nada moviendose")
log_msg("como con entradas y salidas grandes que se cancelan.")

log_msg("\n--- Parte C.3: consistencia GATS (hazard entrada a diario) vs ECH (stock diario) ---")

cohorte_10 <- function(anio_nac) floor(anio_nac / 10) * 10

Pc <- copy(P)
Pc[, cohorte := cohorte_10(anio_nac)]
stock_diario_ech <- Pc[, .(N = sum(peso), n = .N, N_diario = sum(peso * (estado == 3))),
                        by = .(cohorte, edad)]
stock_diario_ech[, stock_diario_ech := round(N_diario / N, 4)]

cargar_gats <- function(path, anio_encuesta) {
  nms0 <- names(haven::read_dta(path, n_max = 1))
  agevar <- nms0[match("age", tolower(nms0))]
  cols <- c("B01", "B02", "B03", "B04", "B08", "B11", "A01", agevar, "gatsweight")
  d <- haven::read_dta(path, col_select = dplyr::all_of(cols))
  setDT(d)
  setnames(d, agevar, "edad"); setnames(d, "gatsweight", "peso")
  for (v in c("B01", "B02", "B03", "B04", "B08", "B11", "edad", "peso")) d[, (v) := as.numeric(get(v))]
  d[, ever_diario := fifelse(B01 == 1, TRUE, fifelse(B01 == 2 & B02 == 1, TRUE,
                     fifelse(B01 == 3 & B03 == 1, TRUE, fifelse(is.na(B01), NA, FALSE))))]
  d[, edad_inicio_diario := fcase(!is.na(B04), B04, !is.na(B08), B08, !is.na(B11), B11, default = NA_real_)]
  d[, anio_nac := anio_encuesta - edad]
  d[!is.na(ever_diario)]
}
g09 <- cargar_gats(file.path(raw_gats, "gats_2009.dta"), 2009)
g17 <- cargar_gats(file.path(raw_gats, "gats_2017.dta"), 2017)
G <- rbind(g09, g17)
G[, cohorte := cohorte_10(anio_nac)]

edades_grid <- c(20, 25, 30, 35)
curva_gats_fn <- function(edad_corte) {
  G[edad >= edad_corte, .(N = sum(peso), N_diario = sum(peso * as.numeric(ever_diario & edad_inicio_diario <= edad_corte))),
    by = cohorte][, .(cohorte, edad = edad_corte, prob_acum_ever_diario_gats = round(N_diario / N, 4))]
}
curva_g <- rbindlist(lapply(edades_grid, curva_gats_fn))

consistencia_c3 <- merge(curva_g, stock_diario_ech[edad %in% edades_grid, .(cohorte, edad, stock_diario_ech, n)],
                          by = c("cohorte", "edad"))
consistencia_c3[, cumple_desigualdad_esperada := prob_acum_ever_diario_gats >= stock_diario_ech]
setorder(consistencia_c3, cohorte, edad)
fwrite(consistencia_c3, file.path(out_dir, "32_consistencia_gats_ech_diario.csv"))
log_msg("Consistencia GATS (ever-diario acumulado) vs ECH (stock diario corriente), misma cohorte:\n%s",
        paste(capture.output(print(consistencia_c3)), collapse = "\n"))
log_msg("Cumple la desigualdad esperada (GATS ever-diario >= ECH stock corriente) en %d de %d celdas",
        sum(consistencia_c3$cumple_desigualdad_esperada), nrow(consistencia_c3))

log_msg("\n--- Parte C.4: transiciones no identificadas ---")
log_msg("nunca->ocasional y ocasional->diario NO quedan separadamente identificadas desde los")
log_msg("stocks de la ECH solas: el flujo neto de 'ocasional' (C.2) mezcla la entrada desde nunca")
log_msg("y la salida hacia diario o hacia ex, sin forma de separarlas sin estructura adicional.")
log_msg("diario->ocasional (reduccion) tampoco esta identificada por separado del flujo neto de diario.")
log_msg("Lo que SI se puede fijar con las fuentes de este proyecto: el momento de entrada A DIARIO")
log_msg("(hazard de GATS, validado B.1) y los stocks marginales por edad/cohorte/sexo/NSE (ECH).")
log_msg("El resto -- como se reparte la entrada entre nunca->ocasional->diario vs nunca->diario")
log_msg("directo, y la tasa de salida de ocasional -- requiere calibracion por ajuste a momentos")
log_msg("en la etapa de modelizacion, no se puede estimar directamente de estas fuentes.")

log_msg("\n--- D.4: distribucion de estados a los 15 anios ---")

p15 <- P[edad == 15]
dist15_sexo <- p15[, .(N_pond = sum(peso), n = .N), by = .(sexo, estado, estado_lab)]
dist15_sexo[, share := round(N_pond / sum(N_pond), 4), by = sexo]
setorder(dist15_sexo, sexo, estado)
fwrite(dist15_sexo, file.path(out_dir, "32_estados_a_los_15_por_sexo.csv"))
log_msg("Distribucion de estados a los 15 anios, por sexo (pooled todos los anios del panel):\n%s",
        paste(capture.output(print(dist15_sexo[, .(sexo, estado_lab, n, share)])), collapse = "\n"))

p15_nse <- p15[!is.na(tercil)]
dist15_nse <- p15_nse[, .(N_pond = sum(peso), n = .N), by = .(tercil, estado, estado_lab)]
dist15_nse[, share := round(N_pond / sum(N_pond), 4), by = tercil]
setorder(dist15_nse, tercil, estado)
fwrite(dist15_nse, file.path(out_dir, "32_estados_a_los_15_por_nse.csv"))
log_msg("Distribucion de estados a los 15 anios, por tercil de NSE (n sin ponderar al lado):\n%s",
        paste(capture.output(print(dist15_nse[, .(tercil, estado_lab, n, share)])), collapse = "\n"))

log_msg("\n=== outputs: 32_ocasional_por_edad_anio.csv, 32_ocasional_por_tramo.csv, 32_stocks_ocasional_diario_cohorte.csv, 32_flujo_neto_implicito.csv, 32_consistencia_gats_ech_diario.csv, 32_estados_a_los_15_por_sexo.csv, 32_estados_a_los_15_por_nse.csv ===")
close(log_con)
