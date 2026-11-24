# 18_hazard_iniciacion.R
#
# Initiation (leaving the never-smoked state) from the decline of the never-smoked share of each
# birth cohort across ECH waves, by sex and income tertile, and the entry states at age 15. The
# level of the hazard is replaced in R/29; this script provides the age, sex and tertile structure.
# Input:  panel from R/04, output/tables/28_*.csv, 32_*.csv
# Output: data/interim/modelo_iniciacion.rds (overwritten by R/29), output/tables/38_*.csv

library(here)
library(data.table)
library(splines)

here::i_am("R/18_hazard_iniciacion.R")

log_con <- file(here::here("logs", "18_hazard_iniciacion.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 18_hazard_iniciacion.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))

log_msg("\n--- A) Monotonia de 'nunca fumo' por cohorte, TODAS las ondas consecutivas ---")

st_coh <- P[, .(N_pond = sum(peso)), by = .(cohorte, anio, estado)]
w_coh <- dcast(st_coh, cohorte + anio ~ estado, value.var = "N_pond", fill = 0)
setnames(w_coh, as.character(1:5), paste0("N_pond_", 1:5), skip_absent = TRUE)
for (e in 1:5) { nv <- paste0("N_pond_", e); if (!(nv %in% names(w_coh))) w_coh[, (nv) := 0] }
w_coh[, Npond_total := N_pond_1 + N_pond_2 + N_pond_3 + N_pond_4 + N_pond_5]
w_coh[, prop_nunca := N_pond_1 / Npond_total]
setorder(w_coh, cohorte, anio)
w_coh[, `:=`(d_prop_nunca = prop_nunca - shift(prop_nunca), anio_prev = shift(anio)), by = cohorte]

monotonia <- w_coh[!is.na(d_prop_nunca), .(cohorte, anio_prev, anio, prop_nunca, d_prop_nunca)]
resumen_cohorte <- monotonia[, .(max_d_prop_nunca = max(d_prop_nunca), n_pares = .N), by = cohorte]
resumen_cohorte[, viola_alguna_vez := max_d_prop_nunca > 0.02]
setorder(resumen_cohorte, cohorte)
fwrite(monotonia, file.path(out_dir, "38_monotonia_cohorte_completa.csv"))
log_msg("Violaciones por cohorte (maximo d_prop_nunca entre CUALQUIER par de ondas consecutivas):\n%s",
        paste(capture.output(print(resumen_cohorte)), collapse = "\n"))

cohortes_limpias_previas <- c(1985, 1990, 1995, 2000, 2005)
cohortes_limpias_estrictas <- resumen_cohorte[viola_alguna_vez == FALSE, cohorte]
log_msg("Cohortes limpias bajo el criterio de producto 2.6 (solo extremos 2019 vs 2021-2025): %s",
        paste(cohortes_limpias_previas, collapse = ", "))
log_msg("Cohortes limpias bajo el criterio estricto (cualquier par consecutivo, 2011-2025): %s",
        paste(cohortes_limpias_estrictas, collapse = ", "))
d_1985 <- resumen_cohorte[cohorte == 1985, max_d_prop_nunca]
log_msg("AVISO: bajo el criterio estricto la cohorte 1985 deja de ser 'limpia' (max d_prop_nunca = %.4f, en 2021->2022): el criterio de extremos de 2019-2025 no la detecta porque el neto de todo el tramo da bajo. El criterio de extremos puede ocultar violaciones intermedias.",
        d_1985)
log_msg("Consecuencia practica: el criterio estricto deja muy pocas cohortes utilizables (las mas jovenes, con poco rango de edad observado) -- no alcanza para cubrir todo el rango de edad del hazard. Por eso la seccion C no excluye cohortes por este criterio; en cambio aplica un piso en cero a cada incremento negativo, celda por celda (cohorte x sexo x tercil x par de ondas), y usa ambos subconjuntos (todas las cohortes vs. solo las estrictamente limpias) como version principal y version de validacion respectivamente.")

log_msg("\n--- B) Estados y incrementos por cohorte x sexo x tercil x onda ---")

n_tercil_na <- P[is.na(tercil), .N]
log_msg("Casos con tercil NA excluidos: %d", n_tercil_na)
Pn <- P[!is.na(tercil)]

base <- Pn[, .(Npond_total = sum(peso), n_total = .N, edad_media = weighted.mean(edad, peso)),
           by = .(cohorte, sexo, tercil, anio)]
nunca <- Pn[estado == 1, .(N_pond_nunca = sum(peso), n_nunca = .N), by = .(cohorte, sexo, tercil, anio)]
base <- merge(base, nunca, by = c("cohorte", "sexo", "tercil", "anio"), all.x = TRUE)
base[is.na(N_pond_nunca), `:=`(N_pond_nunca = 0, n_nunca = 0)]
base[, prop_nunca := N_pond_nunca / Npond_total]
setorder(base, cohorte, sexo, tercil, anio)
fwrite(base, file.path(out_dir, "38_estados_por_cohorte_sexo_tercil_onda.csv"))
log_msg("Escrito 38_estados_por_cohorte_sexo_tercil_onda.csv (%d filas)", nrow(base))

base[, `:=`(
  anio_prev = shift(anio),
  N_pond_nunca_prev = shift(N_pond_nunca),
  edad_media_prev = shift(edad_media)
), by = .(cohorte, sexo, tercil)]

incrementos <- base[!is.na(anio_prev)]
incrementos[, delta_anios := anio - anio_prev]
incrementos[, riesgo_pond := N_pond_nunca_prev]
incrementos[, eventos_pond := pmax(0, N_pond_nunca_prev - N_pond_nunca)]
incrementos[, evento_floreado := (N_pond_nunca_prev - N_pond_nunca) < 0]
incrementos <- incrementos[riesgo_pond > 0]
fwrite(incrementos, file.path(out_dir, "38_incrementos_por_cohorte_sexo_tercil_onda.csv"))
log_msg("Escrito 38_incrementos_por_cohorte_sexo_tercil_onda.csv (%d filas, %d con evento floreado a 0 por violacion de monotonia, %.1f%%)",
        nrow(incrementos), sum(incrementos$evento_floreado),
        100 * mean(incrementos$evento_floreado))

log_msg("\n--- C) Modelo de hazard suavizado, con tendencia de calendario ---")

incrementos[, prop_evento := eventos_pond / riesgo_pond]
incrementos[, sexo := factor(sexo, levels = c("varon", "mujer"))]
incrementos[, tercil := factor(tercil, levels = c(1, 2, 3))]
incrementos[, anio_prev_c := anio_prev - 2018]

m_sin_tendencia <- glm(prop_evento ~ ns(edad_media_prev, df = 4) + sexo + tercil + offset(log(delta_anios)),
                        family = quasibinomial(link = "cloglog"), weights = riesgo_pond, data = incrementos)
m_todas <- glm(prop_evento ~ ns(edad_media_prev, df = 4) + anio_prev_c + sexo + tercil + offset(log(delta_anios)),
               family = quasibinomial(link = "cloglog"), weights = riesgo_pond, data = incrementos)
log_msg("Modelo principal (todas las cohortes, piso en 0, CON tendencia de calendario): N celdas = %d, riesgo total ponderado = %.0f",
        nrow(incrementos), sum(incrementos$riesgo_pond))
log_msg("Coeficientes (modelo principal, con anio_prev_c):\n%s",
        paste(capture.output(print(summary(m_todas)$coefficients)), collapse = "\n"))

prueba_tendencia <- anova(m_sin_tendencia, m_todas, test = "F")
log_msg("Prueba F, aporte de la tendencia de calendario (sin vs. con anio_prev_c):\n%s",
        paste(capture.output(print(prueba_tendencia)), collapse = "\n"))
log_msg("Coeficiente de anio_prev_c: %.4f (negativo = el hazard de iniciacion cae con el calendario, a igual edad/sexo/tercil -- consistente con el chequeo directo de arriba)",
        coef(m_todas)["anio_prev_c"])

saveRDS(m_todas, here::here("data", "interim", "modelo_iniciacion.rds"))
log_msg("Modelo guardado en data/interim/modelo_iniciacion.rds (predict con newdata: edad_media_prev, anio_prev_c, sexo, tercil, delta_anios=1)")

incrementos_estrictas <- incrementos[cohorte %in% cohortes_limpias_estrictas]
rango_edad_estrictas <- range(incrementos_estrictas$edad_media_prev)
m_estrictas <- glm(prop_evento ~ ns(edad_media_prev, df = 3) + sexo + tercil + offset(log(delta_anios)),
                    family = quasibinomial(link = "cloglog"), weights = riesgo_pond, data = incrementos_estrictas)
log_msg("Modelo de validacion (solo cohortes estrictamente limpias: %s): N celdas = %d, rango de edad observado = %.0f-%.0f",
        paste(cohortes_limpias_estrictas, collapse = ","), nrow(incrementos_estrictas),
        rango_edad_estrictas[1], rango_edad_estrictas[2])
log_msg("Coeficientes (modelo de validacion, gl de spline reducido a 3 por menos datos):\n%s",
        paste(capture.output(print(summary(m_estrictas)$coefficients)), collapse = "\n"))

grilla_cmp <- CJ(edad_media_prev = seq(rango_edad_estrictas[1], rango_edad_estrictas[2], by = 1),
                 sexo = factor(c("varon", "mujer"), levels = levels(incrementos$sexo)),
                 tercil = factor(1:3, levels = levels(incrementos$tercil)))
grilla_cmp[, `:=`(delta_anios = 1, anio_prev_c = 0)]
grilla_cmp[, hazard_todas := 1 - exp(-exp(predict(m_todas, newdata = grilla_cmp, type = "link")))]
grilla_cmp[, hazard_estrictas := 1 - exp(-exp(predict(m_estrictas, newdata = grilla_cmp, type = "link")))]
grilla_cmp[, diferencia := hazard_todas - hazard_estrictas]
log_msg("Comparacion modelo principal (evaluado en anio_prev_c=0, es decir 2018) vs. validacion en el rango de edad comun (%.0f-%.0f): diferencia absoluta media = %.4f, maxima = %.4f",
        rango_edad_estrictas[1], rango_edad_estrictas[2], mean(abs(grilla_cmp$diferencia)), max(abs(grilla_cmp$diferencia)))

log_msg("\n--- D) Prediccion en grilla fina (modelo principal, 2011 y 2025) ---")

grilla <- CJ(edad = 15:75,
             sexo = factor(c("varon", "mujer"), levels = levels(incrementos$sexo)),
             tercil = factor(1:3, levels = levels(incrementos$tercil)),
             anio_ref = c(2011, 2025))
grilla[, delta_anios := 1]
grilla[, anio_prev_c := anio_ref - 2018]
setnames(grilla, "edad", "edad_media_prev")
pred <- predict(m_todas, newdata = grilla, type = "link", se.fit = TRUE)
grilla[, hazard_iniciacion := 1 - exp(-exp(pred$fit))]
grilla[, hazard_lo := 1 - exp(-exp(pred$fit - 1.96 * pred$se.fit))]
grilla[, hazard_hi := 1 - exp(-exp(pred$fit + 1.96 * pred$se.fit))]
setnames(grilla, "edad_media_prev", "edad")
setcolorder(grilla, c("anio_ref", "edad", "sexo", "tercil", "hazard_iniciacion", "hazard_lo", "hazard_hi"))
fwrite(grilla, file.path(out_dir, "38_hazard_iniciacion_modelo.csv"))
log_msg("Escrito 38_hazard_iniciacion_modelo.csv (%d filas)", nrow(grilla))
log_msg("Rango de hazard predicho: %.4f - %.4f", min(grilla$hazard_iniciacion), max(grilla$hazard_iniciacion))
log_msg("Ejemplo del efecto de la tendencia (varon, tercil 1, edad 20): 2011 = %.4f, 2025 = %.4f",
        grilla[anio_ref == 2011 & sexo == "varon" & tercil == 1 & edad == 20]$hazard_iniciacion,
        grilla[anio_ref == 2025 & sexo == "varon" & tercil == 1 & edad == 20]$hazard_iniciacion)

log_msg("\n--- E) Bondad de ajuste ---")

incrementos[, grupo_etario := fcase(
  edad_media_prev < 25, "15-24", edad_media_prev < 45, "25-44", edad_media_prev < 65, "45-64", default = "65+"
)]
incrementos[, periodo := fifelse(anio_prev < 2018, "2011-2017", "2018-2024")]
incrementos[, tasa_observada_anual := 1 - (1 - prop_evento)^(1 / delta_anios)]
incrementos[, pred_cum := 1 - exp(-exp(predict(m_todas, newdata = incrementos, type = "link")))]
incrementos[, tasa_predicha_anual := 1 - (1 - pred_cum)^(1 / delta_anios)]

bondad <- incrementos[, .(n_pares = .N, riesgo_pond = sum(riesgo_pond),
                           tasa_observada_anual = weighted.mean(tasa_observada_anual, riesgo_pond),
                           tasa_predicha_anual = weighted.mean(tasa_predicha_anual, riesgo_pond)),
                       by = .(grupo_etario, sexo, tercil)]
bondad[, diferencia := tasa_observada_anual - tasa_predicha_anual]
setorder(bondad, grupo_etario, sexo, tercil)
fwrite(bondad, file.path(out_dir, "38_hazard_iniciacion_bondad_ajuste.csv"))
log_msg("Escrito 38_hazard_iniciacion_bondad_ajuste.csv (%d filas)", nrow(bondad))
log_msg("Diferencia absoluta media (obs-pred): %.4f | maxima: %.4f",
        mean(abs(bondad$diferencia)), max(abs(bondad$diferencia)))

bondad_periodo <- incrementos[, .(n_pares = .N, riesgo_pond = sum(riesgo_pond),
                                   tasa_observada_anual = weighted.mean(tasa_observada_anual, riesgo_pond),
                                   tasa_predicha_anual = weighted.mean(tasa_predicha_anual, riesgo_pond)),
                               by = .(periodo)]
bondad_periodo[, diferencia := tasa_observada_anual - tasa_predicha_anual]
fwrite(bondad_periodo, file.path(out_dir, "38_hazard_iniciacion_bondad_ajuste_periodo.csv"))
log_msg("Bondad de ajuste por periodo (chequeo de que la tendencia de calendario no deja un sesgo sistematico temprano/tardio):\n%s",
        paste(capture.output(print(bondad_periodo)), collapse = "\n"))

log_msg("\n--- F) Comparacion de forma con hazard de iniciacion a diario (GATS, R/08) ---")

gats <- fread(file.path(out_dir, "28_hazard_por_cohorte_sexo_combinado.csv"))
gats_por_edad <- gats[, .(hazard_gats_a_diario = weighted.mean(hazard, n_riesgo_np)), by = .(sexo, edad)]

grilla_ech <- CJ(edad_media_prev = 15:65,
                  sexo = factor(c("varon", "mujer"), levels = levels(incrementos$sexo)),
                  tercil = factor(1:3, levels = levels(incrementos$tercil)))
grilla_ech[, `:=`(delta_anios = 1, anio_prev_c = 0)]
grilla_ech[, hazard_ech := 1 - exp(-exp(predict(m_todas, newdata = grilla_ech, type = "link")))]
ech_por_edad <- grilla_ech[, .(hazard_ech_cualquier_consumo = mean(hazard_ech)), by = .(sexo, edad = edad_media_prev)]

comparacion <- merge(ech_por_edad, gats_por_edad, by = c("sexo", "edad"), all = TRUE)
setorder(comparacion, sexo, edad)
fwrite(comparacion, file.path(out_dir, "38_comparacion_gats_ech.csv"))
log_msg("Escrito 38_comparacion_gats_ech.csv (%d filas)", nrow(comparacion))
cor_varon <- comparacion[sexo == "varon" & !is.na(hazard_ech_cualquier_consumo) & !is.na(hazard_gats_a_diario),
                          cor(hazard_ech_cualquier_consumo, hazard_gats_a_diario)]
cor_mujer <- comparacion[sexo == "mujer" & !is.na(hazard_ech_cualquier_consumo) & !is.na(hazard_gats_a_diario),
                          cor(hazard_ech_cualquier_consumo, hazard_gats_a_diario)]
log_msg("Correlacion de forma (por edad) ECH vs GATS -- varon: %.3f | mujer: %.3f", cor_varon, cor_mujer)

log_msg("\n--- G) Distribucion de entrada a los 15 anios, por periodo ---")

p15 <- P[edad == 15]
p15[, sexo := factor(sexo, levels = c("varon", "mujer"))]
m_entrada15 <- glm(I(estado == 1) ~ anio + sexo, family = quasibinomial(), weights = peso, data = p15)
log_msg("Modelo de prop_nunca a los 15 anios ~ anio + sexo (N=%d personas, ponderado):\n%s",
        nrow(p15), paste(capture.output(print(summary(m_entrada15)$coefficients)), collapse = "\n"))

ANIO_MIN_SIM <- 2011
ANIO_MAX_SIM <- 2045
grilla_entrada <- CJ(anio = ANIO_MIN_SIM:ANIO_MAX_SIM, sexo = factor(c("varon", "mujer"), levels = levels(p15$sexo)))
grilla_entrada[, anio_pred := pmin(anio, max(p15$anio))]
grilla_entrada[, prop_nunca := predict(m_entrada15, newdata = data.table(anio = grilla_entrada$anio_pred, sexo = grilla_entrada$sexo), type = "response")]

pooled_15 <- fread(file.path(out_dir, "32_estados_a_los_15_por_sexo.csv"))
pooled_15[, share_no_nunca := share / sum(share[estado != 1]), by = sexo]
reparto_no_nunca <- pooled_15[estado != 1, .(sexo, estado, share_relativo = share_no_nunca)]

entrada_15_por_anio <- rbindlist(lapply(1:5, function(e) {
  if (e == 1) {
    grilla_entrada[, .(anio, sexo, estado = 1L, share = prop_nunca)]
  } else {
    merge(grilla_entrada[, .(anio, sexo, prop_nunca)], reparto_no_nunca[estado == e, .(sexo, share_relativo)], by = "sexo")[
      , .(anio, sexo, estado = e, share = (1 - prop_nunca) * share_relativo)]
  }
}))
setorder(entrada_15_por_anio, anio, sexo, estado)
chequeo_suma <- entrada_15_por_anio[, .(suma = sum(share)), by = .(anio, sexo)]
stopifnot(all(abs(chequeo_suma$suma - 1) < 1e-9))
fwrite(entrada_15_por_anio, file.path(out_dir, "38_entrada_15_por_anio.csv"))
log_msg("Escrito 38_entrada_15_por_anio.csv (%d filas, anios %d-%d, suma=1 verificada por anio x sexo)",
        nrow(entrada_15_por_anio), ANIO_MIN_SIM, ANIO_MAX_SIM)
log_msg("prop_nunca a los 15, varon: 2011=%.4f, 2019=%.4f, 2025=%.4f, 2045=%.4f (plano desde 2025)",
        entrada_15_por_anio[anio == 2011 & sexo == "varon" & estado == 1]$share,
        entrada_15_por_anio[anio == 2019 & sexo == "varon" & estado == 1]$share,
        entrada_15_por_anio[anio == 2025 & sexo == "varon" & estado == 1]$share,
        entrada_15_por_anio[anio == 2045 & sexo == "varon" & estado == 1]$share)

log_msg("\n=== outputs: 38_monotonia_cohorte_completa.csv, 38_estados_por_cohorte_sexo_tercil_onda.csv, 38_incrementos_por_cohorte_sexo_tercil_onda.csv, 38_hazard_iniciacion_modelo.csv, 38_hazard_iniciacion_bondad_ajuste.csv, 38_hazard_iniciacion_bondad_ajuste_periodo.csv, 38_comparacion_gats_ech.csv, 38_entrada_15_por_anio.csv ===")
close(log_con)
