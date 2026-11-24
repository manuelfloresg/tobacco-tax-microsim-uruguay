# 58_evidencia_gradiente_elasticidad.R
#
# Published evidence on the income gradient of the price elasticity of tobacco demand, transcribed
# from the sources, with the T1/T3 ratios computed here. Reported in the supplement.
# Output: output/tables/84_*.csv

library(here)
library(data.table)

here::i_am("R/58_evidencia_gradiente_elasticidad.R")

log_con <- file(here::here("logs", "58_evidencia_gradiente_elasticidad.log"),
                open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 58_evidencia_gradiente_elasticidad.R -- %s ===", format(Sys.time()))

out_dir <- here::here("output", "tables")

ev <- data.table(
  estudio = c(
    "Selvaraj et al. 2015",
    "Choi 2016",
    "Colman & Remler 2008",
    "Mena & Paraje 2024",
    "Keeler et al. 2025",
    "Gallego et al. 2021",
    "Gerstenblüth et al. 2026",
    "Reynales-Shigematsu et al. 2022"),
  pais = c("India", "Korea", "United States", "Ecuador", "United States",
           "Colombia", "Uruguay", "Mexico"),
  ingreso_bm = c("Lower-middle", "High", "High", "Upper-middle", "High",
                 "Upper-middle", "High", "Upper-middle"),
  particion = c("Expenditure tertiles", "Income quartiles", "Income tertiles",
                "Income tertiles", "Federal poverty level groups",
                "Socioeconomic strata", "Expenditure tertiles", "Income quintiles"),
  n = c("101,662 households", "KNHANES 1998-2011", "CEX/NHIS",
        "3,410 households", "337,974 adults", "NPSCS 2008 and 2013",
        "6,889 households", "ECEA, national"),
  resultado = c("Total elasticity", "Total elasticity", "Total elasticity",
                "Total elasticity", "Total elasticity", "Participation elasticity",
                "Total elasticity", "Total elasticity"),
  e_bajo  = c(-0.832, -0.812, -0.37, -1.592, -0.40, NA, -1.391, -0.63),
  e_alto  = c(-0.2645, -0.325, -0.20, -1.343, -0.40, NA, -0.153, -0.12),
  clase = c("estimado", "estimado", "estimado", "estimado", "estimado",
            "estimado", "no identificado", "supuesto"),
  diferencia = c(
    "Not formally tested; T2 anomalous (-0.091)",
    "Not formally tested; authors report a 2.5-fold difference",
    "Not formally tested",
    "Tested: z = -0.29, NOT significant",
    "Total elasticity flat; gradient appears only between margins",
    "No SES gradient detected; overall PPE -0.66",
    "T2 SE 21.2, T3 SE 32.6; neither identified",
    "Not estimated: extrapolated from international literature")
)
ev[, ratio := e_bajo / e_alto]
ev[estudio == "Gallego et al. 2021", ratio := 1.00]

log_msg("\n--- Evidencia transcrita ---\n%s",
        paste(capture.output(print(ev[, .(estudio, pais, ingreso_bm, e_bajo, e_alto,
                                          ratio, clase)], digits = 3)), collapse = "\n"))

RATIO_MOTOR <- 2.0

usables <- ev[clase == "estimado"]
r <- sort(usables$ratio)

resumen <- data.table(
  estadistico = c("n de estudios con gradiente estimado", "minimo", "mediana", "media",
                  "punto medio del rango", "maximo",
                  "ratio adoptado por el motor", "percentil del ratio adoptado"),
  valor = c(length(r), min(r), median(r), mean(r), (min(r) + max(r)) / 2, max(r),
            RATIO_MOTOR, 100 * mean(r <= RATIO_MOTOR))
)
log_msg("\n--- Resumen del rango ---\n%s",
        paste(capture.output(print(resumen, digits = 4)), collapse = "\n"))

log_msg("\nRatios estimados, ordenados: %s", paste(sprintf("%.2f", r), collapse = ", "))
log_msg("El 2,0 del motor cae DENTRO del rango [%.2f; %.2f], por encima de la mediana",
        min(r), max(r))
log_msg("(%.2f) y practicamente en el punto medio del rango (%.2f).", median(r),
        (min(r) + max(r)) / 2)
log_msg("Y es MAS CONSERVADOR que el 5,25 que supone el estudio distributivo mexicano.")

correcciones <- data.table(
  id = c("E1", "E2", "E3"),
  afirmacion_anterior = c(
    "El ratio uruguayo implicito es 2,18x y cae en el centro del rango externo",
    "Ecuador aporta un gradiente de aproximadamente 2,5x",
    "Keeler et al. 2025 aporta un gradiente de aproximadamente 1,3x"),
  problema = c(
    "2,18 = T1 / promedio muestral, no T1 / T3. El ratio de los puntuales es 9,09. El 2,18 era el ratio de la especificacion vieja del propio motor: corroboracion circular.",
    "Mena & Paraje (2024) Tabla 2 da T1 -1,592 y T3 -1,343 (ratio 1,19) y su Tabla 3 da z = -0,29: la diferencia NO es significativa.",
    "El 1,3x de Keeler es en PARTICIPACION. Su elasticidad total es -0,40 en pobres y -0,40 en altos: ratio 1,00."),
  valor_corregido = c("9.09 (no identificado)", "1.19 (no significativo)", "1.00 (total)")
)
log_msg("\n--- Correcciones ---\n%s",
        paste(capture.output(print(correcciones[, .(id, valor_corregido)])), collapse = "\n"))

fwrite(ev,           file.path(out_dir, "84_evidencia_gradiente_elasticidad.csv"))
fwrite(resumen,      file.path(out_dir, "84_resumen_gradiente.csv"))
fwrite(correcciones, file.path(out_dir, "84_correcciones_gradiente.csv"))

log_msg("\n=== outputs: 84_evidencia_gradiente_elasticidad.csv, 84_resumen_gradiente.csv,")
log_msg("    84_correcciones_gradiente.csv ===")
close(log_con)
