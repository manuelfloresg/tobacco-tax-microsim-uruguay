# 17_hazard_cesacion.R
#
# Annual cessation rate estimated on the ECH: among people who smoke or quit in the past twelve
# months, the share who quit. Logit on a natural spline of age (4 df), sex, sex x age, income
# tertile and a calendar trend, with survey weights.
# Input:  panel from R/04
# Output: data/interim/modelo_cesacion.rds, output/tables/37_*.csv

library(here)
library(data.table)
library(splines)

here::i_am("R/17_hazard_cesacion.R")

log_con <- file(here::here("logs", "17_hazard_cesacion.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 17_hazard_cesacion.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))

log_msg("\n--- 1) Poblacion expuesta (ocasional/diario/ex_reciente) ---")

n_tercil_na <- P[estado %in% c(2, 3, 4) & is.na(tercil), .N]
log_msg("Casos expuestos con tercil NA (excluidos del modelo): %d", n_tercil_na)

E <- P[estado %in% c(2, 3, 4) & !is.na(tercil)]
E[, evento := as.integer(estado == 4)]
E[, sexo := factor(sexo, levels = c("varon", "mujer"))]
E[, tercil := factor(tercil, levels = c(1, 2, 3))]
E[, anio_c := anio - 2018]
E[, corte_2019 := as.integer(anio >= 2019)]
E[, corte_2021 := as.integer(anio >= 2021)]

log_msg("N expuestos para el modelo: %d (ponderado: %.0f)", nrow(E), sum(E$peso))
log_msg("Eventos (ex_reciente): %d (ponderado: %.0f)", sum(E$evento), sum(E$peso[E$evento == 1]))
log_msg("Distribucion por anio (n, n_evento):\n%s",
        paste(capture.output(print(E[, .(n = .N, n_evento = sum(evento)), by = anio][order(anio)])), collapse = "\n"))

log_msg("\n--- 2) Modelos anidados: sensibilidad a los cortes de comparabilidad ---")

m0 <- glm(evento ~ ns(edad, df = 4) + sexo + sexo:edad + tercil + anio_c,
          family = quasibinomial(), weights = peso, data = E)
m1 <- update(m0, . ~ . + corte_2019)
m2 <- update(m1, . ~ . + corte_2021)
modelos <- list(m0_tendencia = m0, m1_corte2019 = m1, m2_corte2021 = m2)

for (nm in names(modelos)) {
  coef_max <- max(abs(coef(modelos[[nm]])), na.rm = TRUE)
  if (coef_max > 15) {
    log_msg("AVISO: %s tiene un coeficiente de magnitud %.1f en escala logit -- indicio de separacion casi perfecta, no de un efecto real. No usar este modelo tal cual.",
            nm, coef_max)
  }
}

prueba_01 <- anova(m0, m1, test = "F")
prueba_12 <- anova(m1, m2, test = "F")
p_corte_2019 <- prueba_01$`Pr(>F)`[2]
p_corte_2021 <- prueba_12$`Pr(>F)`[2]
log_msg("Prueba F, aporte del corte 2019 (m0 vs m1): p = %.4f", p_corte_2019)
log_msg("Prueba F, aporte del corte 2021 (m1 vs m2): p = %s",
        if (is.na(p_corte_2021)) "NA (m2 no confiable, ver aviso de separacion)" else sprintf("%.4f", p_corte_2021))

modelo_elegido_nombre <- if (!is.na(p_corte_2021) && p_corte_2021 < 0.05) {
  "m2_corte2021"
} else if (!is.na(p_corte_2019) && p_corte_2019 < 0.05) {
  "m1_corte2019"
} else {
  "m0_tendencia"
}
modelo_elegido <- modelos[[modelo_elegido_nombre]]
log_msg("Modelo elegido (se agrega un corte solo si su prueba F secuencial da p<0.05): %s",
        modelo_elegido_nombre)

saveRDS(modelo_elegido, here::here("data", "interim", "modelo_cesacion.rds"))
log_msg("Modelo guardado en data/interim/modelo_cesacion.rds (para prediccion en R/24 a cualquier anio, no solo la grilla de este script)")

comp <- rbindlist(lapply(names(modelos), function(nm) {
  mod <- modelos[[nm]]
  data.table(modelo = nm, deviance = mod$deviance, gl_residual = mod$df.residual,
             dispersion = summary(mod)$dispersion)
}))
log_msg("Comparacion de modelos (AIC no definido bajo quasibinomial):\n%s",
        paste(capture.output(print(comp)), collapse = "\n"))

coefs <- rbindlist(lapply(names(modelos), function(nm) {
  s <- summary(modelos[[nm]])$coefficients
  data.table(modelo = nm, termino = rownames(s), estimate = s[, 1], se = s[, 2], p_valor = s[, 4])
}))
coefs[, modelo_elegido := modelo == modelo_elegido_nombre]
fwrite(coefs, file.path(out_dir, "37_hazard_cesacion_sensibilidad.csv"))
log_msg("Escrito 37_hazard_cesacion_sensibilidad.csv (%d filas)", nrow(coefs))

log_msg("\n--- 3) Prediccion en grilla fina ---")

grilla <- CJ(edad = 15:99,
             sexo = factor(c("varon", "mujer"), levels = levels(E$sexo)),
             tercil = factor(1:3, levels = levels(E$tercil)),
             anio = c(2011, 2025))
grilla[, anio_c := anio - 2018]
grilla[, corte_2019 := as.integer(anio >= 2019)]
grilla[, corte_2021 := as.integer(anio >= 2021)]

pred <- predict(modelo_elegido, newdata = grilla, type = "response", se.fit = TRUE)
grilla[, hazard_cesacion := pred$fit]
grilla[, hazard_se := pred$se.fit]
grilla[, modelo := modelo_elegido_nombre]
setcolorder(grilla, c("anio", "edad", "sexo", "tercil", "hazard_cesacion", "hazard_se", "modelo"))
fwrite(grilla, file.path(out_dir, "37_hazard_cesacion_modelo.csv"))
log_msg("Escrito 37_hazard_cesacion_modelo.csv (%d filas)", nrow(grilla))
log_msg("Rango de hazard predicho: %.4f - %.4f", min(grilla$hazard_cesacion), max(grilla$hazard_cesacion))

log_msg("\n--- 4) Bondad de ajuste (celda gruesa: grupo etario x sexo x tercil x anio) ---")

E[, grupo_etario := fcase(
  edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+"
)]
obs <- E[, .(n = .N, n_pond = sum(peso), tasa_observada = sum(peso * evento) / sum(peso)),
         by = .(anio, grupo_etario, sexo, tercil)]

E[, tasa_predicha := predict(modelo_elegido, newdata = E, type = "response")]
pred_agg <- E[, .(tasa_predicha_pond = sum(peso * tasa_predicha) / sum(peso)),
              by = .(anio, grupo_etario, sexo, tercil)]

bondad <- merge(obs, pred_agg, by = c("anio", "grupo_etario", "sexo", "tercil"))
bondad[, diferencia := tasa_observada - tasa_predicha_pond]
setorder(bondad, anio, grupo_etario, sexo, tercil)
fwrite(bondad, file.path(out_dir, "37_hazard_cesacion_bondad_ajuste.csv"))
log_msg("Escrito 37_hazard_cesacion_bondad_ajuste.csv (%d filas)", nrow(bondad))
log_msg("Diferencia absoluta media (obs-pred): %.4f | maxima: %.4f",
        mean(abs(bondad$diferencia)), max(abs(bondad$diferencia)))
log_msg("Peores 5 celdas por |diferencia| (con n_pond de contexto):\n%s",
        paste(capture.output(print(bondad[order(-abs(diferencia))][1:5])), collapse = "\n"))

log_msg("\n=== outputs: 37_hazard_cesacion_sensibilidad.csv, 37_hazard_cesacion_modelo.csv, 37_hazard_cesacion_bondad_ajuste.csv ===")
close(log_con)
