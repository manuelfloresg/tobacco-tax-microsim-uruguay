# 19_hazard_recaida.R
#
# Relapse rate by time since quitting, from the current-status distribution of time since quitting
# among former smokers in GATS 2009 and 2017 (STEPS as support). The model uses a hybrid rate: the
# specification pooled over ages up to age 44 and the age-conditional specification from age 45,
# because pooling over ages is biased by age truncation of the duration distribution.
# Input:  data/raw/GATS/, data/raw/STEPS/, panel from R/04
# Output: data/interim/modelo_recaida.rds, output/tables/39_*.csv

library(here)
library(data.table)
library(haven)

here::i_am("R/19_hazard_recaida.R")

log_con <- file(here::here("logs", "19_hazard_recaida.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 19_hazard_recaida.R -- %s ===", format(Sys.time()))

out_dir <- here::here("output", "tables")
int_dir <- here::here("data", "interim")
raw_gats <- here::here("data", "raw", "GATS")
raw_steps <- here::here("data", "raw", "STEPS")

bins <- c(0, 1, 2, 5, 10, 20, Inf)
etiquetas_bins <- c("<1", "1-2", "2-5", "5-10", "10-20", "20+")
anchos_bins <- c(1, 1, 3, 5, 10, NA)

log_msg("\n--- 1) Carga GATS 2009/2017: ex-fumadores reales y duracion desde que dejaron ---")

cargar_gats_recaida <- function(path, anio_encuesta, b13b_dividido) {
  nms0 <- names(haven::read_dta(path, n_max = 1))
  agevar <- nms0[match("age", tolower(nms0))]
  cols_b13b <- if (b13b_dividido) c("B13BYEARS", "B13BMONTHS", "B13BWEEKS", "B13BDAYS") else "B13B"
  cols <- c("A01", agevar, "gatsweight", "B01", "B03", "B13A", cols_b13b)
  d <- haven::read_dta(path, col_select = dplyr::all_of(cols))
  setDT(d)
  setnames(d, agevar, "edad")
  setnames(d, "gatsweight", "peso")
  d[, edad := as.numeric(edad)]
  d[, peso := as.numeric(peso)]
  d[, sexo := fifelse(as.numeric(A01) == 1, "varon", fifelse(as.numeric(A01) == 2, "mujer", NA_character_))]
  d[, B01n := as.numeric(B01)]
  d[, B03n := as.numeric(B03)]
  d[, B13An := as.numeric(B13A)]

  if (b13b_dividido) {
    d[, duracion_anios := fcase(
      B13An == 1, as.numeric(B13BYEARS),
      B13An == 2, as.numeric(B13BMONTHS) / 12,
      B13An == 3, as.numeric(B13BWEEKS) / 52,
      B13An == 4, as.numeric(B13BDAYS) / 365,
      default = NA_real_
    )]
  } else {
    d[, B13Bn := as.numeric(B13B)]
    d[, duracion_anios := fcase(
      B13An == 1, B13Bn,
      B13An == 2, B13Bn / 12,
      B13An == 3, B13Bn / 52,
      B13An == 4, B13Bn / 365,
      default = NA_real_
    )]
  }
  d[duracion_anios >= 97, duracion_anios := NA]
  d[, ex_fumador_real := B01n == 3 & B03n %in% c(1, 2)]
  d[, patron_pasado := fcase(B03n == 1, "diario", B03n == 2, "ocasional", default = NA_character_)]
  d[, anio_encuesta := anio_encuesta]
  d[ex_fumador_real == TRUE & !is.na(duracion_anios) & !is.na(sexo) & !is.na(edad),
    .(anio_encuesta, sexo, edad, patron_pasado, duracion_anios, peso)]
}

g09 <- cargar_gats_recaida(file.path(raw_gats, "gats_2009.dta"), 2009, b13b_dividido = FALSE)
g17 <- cargar_gats_recaida(file.path(raw_gats, "gats_2017.dta"), 2017, b13b_dividido = TRUE)
G <- rbind(g09, g17)
log_msg("Ex-fumadores reales con duracion valida: 2009 = %d, 2017 = %d, total = %d", nrow(g09), nrow(g17), nrow(G))
log_msg("Patron pasado (diario vs ocasional antes de dejar):\n%s",
        paste(capture.output(print(G[, .N, by = .(anio_encuesta, patron_pasado)])), collapse = "\n"))

log_msg("\n--- 2) Supervivencia y hazard por bin de duracion (GATS 2009+2017 apilados) ---")

construir_supervivencia <- function(dt, agrupar_por = "sexo") {
  dt <- copy(dt)
  dt[, bin_duracion := cut(duracion_anios, breaks = bins, labels = etiquetas_bins, right = FALSE)]
  if (agrupar_por == "sexo") {
    resumen <- dt[, .(n = .N, n_pond = sum(peso)), by = .(sexo, bin_duracion)]
    setorder(resumen, sexo, bin_duracion)
    resumen[, total_pond := sum(n_pond), by = sexo]
  } else {
    resumen <- dt[, .(n = .N, n_pond = sum(peso)), by = .(bin_duracion)]
    setorder(resumen, bin_duracion)
    resumen[, total_pond := sum(n_pond)]
  }
  resumen[, prop_bin := n_pond / total_pond]
  if (agrupar_por == "sexo") {
    resumen[, supervivencia := rev(cumsum(rev(prop_bin))), by = sexo]
    resumen[, supervivencia_siguiente := shift(supervivencia, type = "lead"), by = sexo]
  } else {
    resumen[, supervivencia := rev(cumsum(rev(prop_bin)))]
    resumen[, supervivencia_siguiente := shift(supervivencia, type = "lead")]
  }
  resumen[, hazard_acumulado_bin := 1 - supervivencia_siguiente / supervivencia]
  resumen[, ancho_bin := anchos_bins[match(bin_duracion, etiquetas_bins)]]
  resumen[, hazard_anual := 1 - (1 - hazard_acumulado_bin)^(1 / ancho_bin)]
  resumen[]
}

superv_sexo <- construir_supervivencia(G, "sexo")
superv_total <- construir_supervivencia(G, "total")
superv_total[, sexo := "todos"]
supervivencia_gats <- rbind(superv_sexo, superv_total, fill = TRUE)
fwrite(supervivencia_gats, file.path(out_dir, "39_recaida_supervivencia_duracion.csv"))
log_msg("Supervivencia y hazard por bin (GATS), por sexo y total:\n%s",
        paste(capture.output(print(supervivencia_gats[, .(sexo, bin_duracion, n, n_pond, supervivencia, hazard_anual)])), collapse = "\n"))
log_msg("Chequeo de monotonia: la supervivencia debe ser no creciente en duracion (por construccion, cumsum invertido) -- verificado automaticamente por la formula, no requiere chequeo adicional.")

GRUPOS_EDAD_DURACION <- function(edad) {
  cut(edad, breaks = c(-Inf, 34, 44, 54, 64, Inf),
      labels = c("15-34", "35-44", "45-54", "55-64", "65+"))
}
stock_dur <- G[duracion_anios >= 1 & !is.na(edad) & edad >= 15,
               .(grupo_edad = GRUPOS_EDAD_DURACION(edad), duracion_anios, peso)]
stock_dur <- stock_dur[!is.na(grupo_edad)]
setorder(stock_dur, grupo_edad, duracion_anios)
fwrite(stock_dur, file.path(out_dir, "39_duracion_stock_por_edad.csv"))
log_msg("\nDistribucion del STOCK de duracion por edad (para la imputacion de R/22), n = %d:\n%s",
        nrow(stock_dur),
        paste(capture.output(print(stock_dur[, .(n = .N,
              mediana = round(median(duracion_anios), 1),
              media = round(weighted.mean(duracion_anios, peso), 1),
              pct_20mas = round(100 * weighted.mean(duracion_anios >= 20, peso), 1)),
              by = grupo_edad][order(grupo_edad)])), collapse = "\n"))
log_msg("Escrito 39_duracion_stock_por_edad.csv -- lo consume R/22 seccion 2.")

log_msg("\n--- 3) Modelo suavizado del hazard anual en funcion de log(duracion) ---")

datos_modelo <- superv_sexo[!is.na(hazard_anual) & hazard_anual > 0 & hazard_anual < 1]
datos_modelo[, punto_medio := fcase(
  bin_duracion == "<1", 0.5, bin_duracion == "1-2", 1.5, bin_duracion == "2-5", 3.5,
  bin_duracion == "5-10", 7.5, bin_duracion == "10-20", 15, default = NA_real_
)]
log_msg("Puntos usados para el modelo (uno por bin x sexo, sin el bin abierto 20+): %d", nrow(datos_modelo))

m_recaida <- glm(hazard_anual ~ log(punto_medio) + sexo, family = quasibinomial(),
                  weights = n_pond, data = datos_modelo)
log_msg("Coeficientes del modelo (hazard ANUAL ya normalizado por ancho de bin ~ log(punto medio) + sexo):\n%s",
        paste(capture.output(print(summary(m_recaida)$coefficients)), collapse = "\n"))

saveRDS(m_recaida, here::here("data", "interim", "modelo_recaida_sin_edad.rds"))
log_msg("Modelo SIN EDAD guardado en data/interim/modelo_recaida_sin_edad.rds (ya no lo usa el motor).")
log_msg("NOTA: con 5 puntos por sexo, este modelo es una forma funcional simple para interpolar entre bins, no una estimacion con muchos grados de libertad. Uso previsto: dar una curva continua al motor en vez de una funcion escalon. Se modela hazard_anual (ya ajustado por ancho de bin), NO hazard_acumulado_bin -- ese ultimo crece mecanicamente con el ancho del bin y no es comparable entre bins de distinta duracion.")

grilla_pred <- CJ(punto_medio = seq(0.5, 25, by = 0.5), sexo = c("varon", "mujer"))
grilla_pred[, hazard_anual_pred := predict(m_recaida, newdata = grilla_pred, type = "response")]
fwrite(grilla_pred, file.path(out_dir, "39_recaida_hazard_por_duracion.csv"))
log_msg("Escrito 39_recaida_hazard_por_duracion.csv (%d filas)", nrow(grilla_pred))
log_msg("Hazard anual predicho a 1, 5, 10, 20 anios de duracion (varon): %s",
        paste(sprintf("%.4f", grilla_pred[sexo == "varon" & punto_medio %in% c(1, 5, 10, 20)]$hazard_anual_pred), collapse = ", "))

log_msg("\n--- 3b) Hazard de recaida condicional al grupo etario ---")

G[, grupo_edad := GRUPOS_EDAD_DURACION(edad)]

construir_supervivencia_grupos <- function(dt, cols) {
  dt <- copy(dt)[!is.na(grupo_edad) & !is.na(sexo)]
  dt[, bin_duracion := cut(duracion_anios, breaks = bins, labels = etiquetas_bins, right = FALSE)]
  r <- dt[, .(n = .N, n_pond = sum(peso)), by = c(cols, "bin_duracion")]
  setorderv(r, c(cols, "bin_duracion"))
  r[, total_pond := sum(n_pond), by = cols]
  r[, prop_bin := n_pond / total_pond]
  r[, supervivencia := rev(cumsum(rev(prop_bin))), by = cols]
  r[, supervivencia_siguiente := shift(supervivencia, type = "lead"), by = cols]
  r[, hazard_acumulado_bin := 1 - supervivencia_siguiente / supervivencia]
  r[, ancho_bin := anchos_bins[match(bin_duracion, etiquetas_bins)]]
  r[, hazard_anual := 1 - (1 - hazard_acumulado_bin)^(1 / ancho_bin)]
  r[]
}

superv_edad_sexo <- construir_supervivencia_grupos(G, c("grupo_edad", "sexo"))
fwrite(superv_edad_sexo, file.path(out_dir, "39_recaida_supervivencia_edad_sexo.csv"))
log_msg("Celdas grupo etario x sexo x bin, n sin ponderar:\n%s",
        paste(capture.output(print(dcast(superv_edad_sexo, grupo_edad + sexo ~ bin_duracion,
                                          value.var = "n", fill = 0))), collapse = "\n"))

datos_edad <- superv_edad_sexo[!is.na(hazard_anual) & hazard_anual > 0 & hazard_anual < 1]
datos_edad[, punto_medio := fcase(
  bin_duracion == "<1", 0.5, bin_duracion == "1-2", 1.5, bin_duracion == "2-5", 3.5,
  bin_duracion == "5-10", 7.5, bin_duracion == "10-20", 15, default = NA_real_
)]
datos_edad <- datos_edad[!is.na(punto_medio)]
log_msg("Celdas utilizables para el modelo con edad: %d (de %d posibles)",
        nrow(datos_edad), nrow(superv_edad_sexo))

m_recaida_edad <- glm(hazard_anual ~ log(punto_medio) + sexo + grupo_edad,
                       family = quasibinomial(), weights = n_pond, data = datos_edad)
log_msg("Coeficientes del modelo CON EDAD:\n%s",
        paste(capture.output(print(summary(m_recaida_edad)$coefficients)), collapse = "\n"))

log_msg("Pendiente en log(duracion): sin edad %.4f, con edad %.4f.",
        coef(m_recaida)[["log(punto_medio)"]], coef(m_recaida_edad)[["log(punto_medio)"]])

saveRDS(m_recaida_edad, here::here("data", "interim", "modelo_recaida_por_edad.rds"))
log_msg("Modelo CON EDAD guardado en data/interim/modelo_recaida_por_edad.rds.")

grilla_edad <- CJ(punto_medio = c(1, 5, 20), sexo = c("varon", "mujer"),
                  grupo_edad = levels(datos_edad$grupo_edad))
grilla_edad <- grilla_edad[grupo_edad %in% unique(as.character(datos_edad$grupo_edad))]
grilla_edad[, grupo_edad := factor(grupo_edad, levels = levels(datos_edad$grupo_edad))]
grilla_edad[, hazard_pred := predict(m_recaida_edad, newdata = grilla_edad, type = "response")]
fwrite(grilla_edad, file.path(out_dir, "39_recaida_hazard_por_edad_pred.csv"))
log_msg("Hazard anual predicho por grupo etario y duracion:\n%s",
        paste(capture.output(print(dcast(grilla_edad, grupo_edad + sexo ~ punto_medio,
                                          value.var = "hazard_pred"), digits = 3)), collapse = "\n"))

log_msg("\n--- 3c) Hazard de recaida hibrido (agrupado <=44, condicional 45+) ---")

GRUPOS_MAYORES_RECAIDA <- c("45-54", "55-64", "65+")

m_recaida_hibrido <- structure(
  list(joven = m_recaida,
       mayor = m_recaida_edad,
       grupos_mayores = GRUPOS_MAYORES_RECAIDA),
  class = "recaida_hibrida")

saveRDS(m_recaida_hibrido, here::here("data", "interim", "modelo_recaida.rds"))
log_msg("Modelo HIBRIDO guardado en data/interim/modelo_recaida.rds -- ES EL QUE LEE EL MOTOR.")
log_msg("El metodo predict() de la clase recaida_hibrida esta definido en R/24_motor_transiciones.R,")
log_msg("que es el archivo que el motor siempre sourcea. predict() necesita punto_medio, sexo y grupo_edad.")

grilla_hib <- CJ(punto_medio = c(1, 5, 20), sexo = c("varon", "mujer"),
                 grupo_edad = levels(datos_edad$grupo_edad))
grilla_hib[, grupo_edad := factor(grupo_edad, levels = levels(datos_edad$grupo_edad))]
grilla_hib[, es_mayor := as.character(grupo_edad) %in% GRUPOS_MAYORES_RECAIDA]
grilla_hib[, hazard_pred := NA_real_]
grilla_hib[es_mayor == FALSE,
           hazard_pred := predict(m_recaida, newdata = .SD, type = "response")]
grilla_hib[es_mayor == TRUE,
           hazard_pred := predict(m_recaida_edad, newdata = .SD, type = "response")]
fwrite(grilla_hib, file.path(out_dir, "39_recaida_hazard_hibrido_pred.csv"))
log_msg("Hazard anual predicho por el HIBRIDO (columnas = anos desde la cesacion):\n%s",
        paste(capture.output(print(dcast(grilla_hib, grupo_edad + sexo ~ punto_medio,
                                          value.var = "hazard_pred"), digits = 3)), collapse = "\n"))

log_msg("\n--- 4) STEPS 2006/2013 como apoyo secundario ---")

cargar_steps_recaida <- function(path, anio_encuesta, es_2006) {
  d <- haven::read_dta(path)
  setDT(d)
  if (es_2006) {
    d[, sexo := fifelse(tolower(as.character(haven::as_factor(sex))) %in% c("men", "male"), "varon",
               fifelse(tolower(as.character(haven::as_factor(sex))) %in% c("women", "female"), "mujer", NA_character_))]
    d[, peso := 1]
  } else {
    d[, sexo := fifelse(tolower(as.character(haven::as_factor(c1))) %in% c("men", "male"), "varon",
               fifelse(tolower(as.character(haven::as_factor(c1))) %in% c("women", "female"), "mujer", NA_character_))]
    d[, peso := as.numeric(weightsdi)]
  }
  d[, edad := as.numeric(age)]
  d[, fumador_actual := as.numeric(t1) == 1]
  d[, edad_dejo := as.numeric(t7)]
  d[, duracion_anios := edad - edad_dejo]
  d[, ex_fumador_real := fumador_actual == FALSE & !is.na(edad_dejo)]
  d[, anio_encuesta := anio_encuesta]
  d[ex_fumador_real == TRUE & !is.na(duracion_anios) & duracion_anios >= 0 & !is.na(sexo),
    .(anio_encuesta, sexo, edad, duracion_anios, peso)]
}
s06 <- cargar_steps_recaida(file.path(raw_steps, "steps_2006.dta"), 2006, es_2006 = TRUE)
s13 <- cargar_steps_recaida(file.path(raw_steps, "steps_2013.dta"), 2013, es_2006 = FALSE)
S <- rbind(s06, s13)
log_msg("Ex-fumadores reales con duracion valida en STEPS: 2006 = %d, 2013 = %d, total = %d", nrow(s06), nrow(s13), nrow(S))

if (nrow(S) >= 60) {
  superv_steps <- construir_supervivencia(S, "total")
  superv_steps[, sexo := "todos"]
  fwrite(superv_steps, file.path(out_dir, "39_recaida_supervivencia_duracion_steps.csv"))
  log_msg("STEPS: supervivencia y hazard por bin (referencia secundaria, N pooled = %d):\n%s",
          nrow(S), paste(capture.output(print(superv_steps[, .(bin_duracion, n, n_pond, supervivencia, hazard_anual)])), collapse = "\n"))
} else {
  log_msg("N de STEPS (%d) demasiado chico para construir bins de duracion con algun minimo de robustez -- se deja como dato de contexto en el log, no se escribe tabla ni se usa como fuente.", nrow(S))
}

log_msg("\n--- 5) Chequeo de consistencia con el panel ECH ---")
log_msg("Identidad contable: E2(t) = [E2(t-1)+E1(t-1)]*(1-r)*(1-mortalidad). Sin tablas de")
log_msg("mortalidad todavia (no disponibles en este paso), se calcula r ignorando mortalidad --")
log_msg("esto SOBREESTIMA r (parte de la caida de [E1+E2] que en realidad es mortalidad se le")
log_msg("atribuye a recaida). Es una cota superior aproximada, no una estimacion final.")

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))
st_ex <- P[, .(N_pond = sum(peso)), by = .(cohorte, sexo, anio, estado)]
w_ex <- dcast(st_ex, cohorte + sexo + anio ~ estado, value.var = "N_pond", fill = 0)
setnames(w_ex, c("4", "5"), c("N_pond_reciente", "N_pond_consolidado"), skip_absent = TRUE)
for (v in c("N_pond_reciente", "N_pond_consolidado")) if (!(v %in% names(w_ex))) w_ex[, (v) := 0]
setorder(w_ex, cohorte, sexo, anio)
w_ex[, `:=`(
  anio_prev = shift(anio),
  pool_ex_prev = shift(N_pond_reciente + N_pond_consolidado)
), by = .(cohorte, sexo)]
w_ex[, delta_anios := anio - anio_prev]

chequeo <- w_ex[!is.na(anio_prev) & pool_ex_prev > 0]
chequeo[, r_implicito_bruto := 1 - N_pond_consolidado / pool_ex_prev]
chequeo <- chequeo[r_implicito_bruto >= 0 & r_implicito_bruto <= 1]
chequeo[, r_implicito_anual := 1 - (1 - r_implicito_bruto)^(1 / delta_anios)]
fwrite(chequeo, file.path(out_dir, "39_recaida_chequeo_consistencia_panel.csv"))

resumen_chequeo <- chequeo[, .(n_celdas = .N, r_implicito_anual_mediana = median(r_implicito_anual),
                                r_implicito_anual_p25 = quantile(r_implicito_anual, 0.25),
                                r_implicito_anual_p75 = quantile(r_implicito_anual, 0.75)), by = sexo]
log_msg("Recaida anual implicita en el panel ECH (bruta, sin mortalidad, mezcla reciente+consolidado):\n%s",
        paste(capture.output(print(resumen_chequeo)), collapse = "\n"))
log_msg("Comparar con el hazard GATS en duraciones bajas (bin <1: varon %.4f, mujer %.4f) y altas (bin 10-20: varon %.4f, mujer %.4f) -- el panel mezcla ambos regimenes de duracion en una sola cifra, asi que se espera que caiga entre esos dos extremos, no que coincida con ninguno en particular.",
        supervivencia_gats[sexo == "varon" & bin_duracion == "<1"]$hazard_anual,
        supervivencia_gats[sexo == "mujer" & bin_duracion == "<1"]$hazard_anual,
        supervivencia_gats[sexo == "varon" & bin_duracion == "10-20"]$hazard_anual,
        supervivencia_gats[sexo == "mujer" & bin_duracion == "10-20"]$hazard_anual)

log_msg("\n=== outputs: 39_recaida_supervivencia_duracion.csv, 39_recaida_hazard_por_duracion.csv, 39_recaida_chequeo_consistencia_panel.csv (+ 39_recaida_supervivencia_duracion_steps.csv si N alcanzo) ===")
close(log_con)
