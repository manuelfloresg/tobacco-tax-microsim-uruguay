# 47_especificacion_elasticidad.R
#
# Derives the tertile-specific elasticities: the aggregate level is the Uruguayan estimate
# (-0.639), the T1/T3 ratio is imported (2.0) and consumption weights by tertile are fixed, so
# every gradient variant has the same aggregate elasticity. Also defines the sensitivity variants.
# Input:  synthetic population 2011, output/tables/43_*, 59_*
# Output: output/tables/74_*.csv

library(here)
library(data.table)

here::i_am("R/47_especificacion_elasticidad.R")

out_dir <- here::here("output", "tables")

log_con <- file(here::here("logs", "47_especificacion_elasticidad.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 47_especificacion_elasticidad.R -- %s ===", format(Sys.time()))

PESOS_CONGELADOS <- c(`1` = 0.417249969450942, `2` = 0.311554030899455, `3` = 0.271195999649603)
ARCHIVO_PESOS <- file.path(out_dir, "74_pesos_consumo_tercil.csv")

log_msg("\n--- 1) Pesos de consumo por tercil (rama base 2025, especificacion vigente al 2026-08-18) ---")

if (file.exists(ARCHIVO_PESOS)) {
  pesos_dt <- fread(ARCHIVO_PESOS)
  w <- setNames(pesos_dt$share_consumo, as.character(pesos_dt$tercil))
  log_msg("Pesos leidos de %s (ya congelados).", basename(ARCHIVO_PESOS))
} else {
  w <- PESOS_CONGELADOS
  pesos_dt <- data.table(tercil = as.integer(names(w)), share_consumo = as.numeric(w),
                         fuente = "R/38 rama base 2025, especificacion vigente al 2026-08-18",
                         congelado = format(Sys.Date()))
  fwrite(pesos_dt, ARCHIVO_PESOS)
  log_msg("Pesos congelados por primera vez en %s.", basename(ARCHIVO_PESOS))
}
stopifnot(abs(sum(w) - 1) < 1e-8)

arch_tercil <- file.path(out_dir, "59_espacio_escenarios_tercil.csv")
if (file.exists(arch_tercil)) {
  tb <- fread(arch_tercil)[escenario == "base" & anio == 2025][order(tercil)]
  if (nrow(tb) == 3L) {
    w_actual <- setNames(tb$share_consumo, as.character(tb$tercil))
    dif <- max(abs(w_actual[names(w)] - w))
    log_msg("Pesos en la corrida vigente de R/38: T1=%.4f T2=%.4f T3=%.4f (max desvio contra los congelados: %.5f)",
            w_actual["1"], w_actual["2"], w_actual["3"], dif)
    if (dif > 0.02) {
      log_msg("ADVERTENCIA: los pesos se movieron mas de 2pp. El punto fijo dejo de ser de segundo")
      log_msg("orden -- revisar si conviene re-anclar TODAS las variantes (A, B y C) al peso nuevo.")
    }
  }
}
log_msg("Pesos usados: T1=%.6f T2=%.6f T3=%.6f", w["1"], w["2"], w["3"])

resolver_elasticidad <- function(nivel_agregado, ratio, pesos = w) {
  forma <- c(`1` = ratio, `2` = (ratio + 1) / 2, `3` = 1)
  e <- forma * nivel_agregado / sum(pesos * forma[names(pesos)])
  e[names(pesos)]
}

log_msg("\n--- 2) Especificacion principal y sensibilidades ---")

NIVEL_URUGUAY   <- -0.639
NIVEL_FERRE     <- -0.774
RATIO_PRINCIPAL <- 2.0

ESPECS <- list(
  list(caso = "principal", nivel = NIVEL_URUGUAY, ratio = RATIO_PRINCIPAL,
       fundamento = "Nivel: agregado uruguayo (Gerstenbluth et al. 2026). Gradiente: centro del rango externo (Choi 2016 2,5x; Colman & Remler 1,85x; Keeler et al. 2025 1,3x)."),
  list(caso = "S0_sin_gradiente", nivel = NIVEL_URUGUAY, ratio = 1.0,
       fundamento = "Cota inferior del efecto distributivo. Guindon et al. (2023) no puede descartar la ausencia de gradiente en paises de ingreso medio-bajo."),
  list(caso = "S1_gradiente_fuerte", nivel = NIVEL_URUGUAY, ratio = 2.5,
       fundamento = "Choi (2016), Corea, Tob Control: cuartil 1 -0,812 vs cuartil 4 -0,325. Ecuador ~2,5x. Cota superior."),
  list(caso = "S2_gradiente_debil", nivel = NIVEL_URUGUAY, ratio = 1.3,
       fundamento = "Keeler et al. (2025), EE.UU., NTR, n=337.974: gradiente en participacion pobres -0,32 vs medios -0,24."),
  list(caso = "S3_gradiente_invertido", nivel = NIVEL_URUGUAY, ratio = 0.46,
       fundamento = "Signo del gasto revelado uruguayo (Gerstenbluth & Triunfo 2024, Tabla A2: participacion del gasto SUBE en quintil 1, BAJA en quintil 5). Caso extremo: muestra que ni el signo esta cerrado en Uruguay."),
  list(caso = "S5_gradiente_muy_fuerte", nivel = NIVEL_URUGUAY, ratio = 3.0,
       fundamento = "Cota superior simetrica a S0 respecto del principal (2,0). Queda apenas por debajo del maximo estimado del rango corregido (Selvaraj et al. 2015, India, 3,15) y por encima de Choi (2016, 2,50)."),
  list(caso = "S6_gradiente_mediano", nivel = NIVEL_URUGUAY, ratio = 1.5,
       fundamento = "Mediana del rango corregido de seis estudios (1,52). Caso intermedio entre la ausencia de gradiente y el principal, con anclaje propio en vez del 1,3 de S2, cuyo fundamento (Keeler) quedo corregido: ese 1,3 es gradiente en PARTICIPACION y la elasticidad total de Keeler es plana."),
  list(caso = "S7_cruce_bracket", nivel = NIVEL_URUGUAY, ratio = 0.2,
       fundamento = "No es un escenario plausible: existe para ACOTAR por simulacion el punto de cruce de la brecha en pp, que R/59 hoy extrapola por debajo del minimo simulado (0,46). El cruce cae entre 0,21 y 0,35 segun el anio, asi que 0,20 lo deja interpolado en todos los anios informativos."),
  list(caso = "S8_inversion_espejo", nivel = NIVEL_URUGUAY, ratio = 0.5,
       fundamento = "Espejo exacto de la especificacion principal: 1/2,0. NO es una estimacion, y no se apoya en ninguna: de los seis estudios que estiman el gradiente con datos propios, el minimo es 1,00 y ninguno reporta inversion. Es un dispositivo declarado, y por eso el valor es el reciproco del central y no un numero con apariencia de estimado. Reemplaza a S3 (0,46) en el reporte; el 0,46 era el reciproco de la especificacion vieja (T1/T3 = 2,177), o sea el espejo de una especificacion que el proyecto abandono el 2026-08-18."),
  list(caso = "S4_nivel_alternativo", nivel = NIVEL_FERRE, ratio = RATIO_PRINCIPAL,
       fundamento = "Ferre et al. (2025): -0,774 con EE 0,101 (CV 0,13), la estimacion uruguaya mas precisa. Sensibilidad de NIVEL, no de gradiente.")
)

tab <- rbindlist(lapply(ESPECS, function(s) {
  e <- resolver_elasticidad(s$nivel, s$ratio)
  data.table(caso = s$caso, nivel_objetivo = s$nivel, ratio_T1_T3 = s$ratio,
             T1 = e["1"], T2 = e["2"], T3 = e["3"],
             agregada_verificada = sum(w * e[names(w)]),
             ratio_verificado = e["1"] / e["3"],
             fundamento = s$fundamento)
}))

stopifnot(max(abs(tab$agregada_verificada - tab$nivel_objetivo)) < 1e-12)
stopifnot(max(abs(tab$ratio_verificado - tab$ratio_T1_T3)) < 1e-12)

fwrite(tab, file.path(out_dir, "74_especificacion_elasticidad.csv"))
log_msg("Especificaciones resueltas (agregada y ratio verificados a 1e-12):\n%s",
        paste(capture.output(print(tab[, .(caso, nivel_objetivo, ratio_T1_T3, T1, T2, T3, agregada_verificada)],
                                    digits = 5)), collapse = "\n"))

log_msg("\n--- 3) Contraste contra la especificacion vigente (la que se reemplaza) ---")

ELAST_VIGENTE <- c(`1` = -1.391, `2` = -0.639, `3` = -0.639)
agr_vigente <- sum(w * ELAST_VIGENTE[names(w)])
princ <- tab[caso == "principal"]

log_msg("Vigente : T1=%.4f T2=%.4f T3=%.4f | agregada implicita %.4f | ratio T1/T3 %.3f",
        ELAST_VIGENTE["1"], ELAST_VIGENTE["2"], ELAST_VIGENTE["3"], agr_vigente,
        ELAST_VIGENTE["1"] / ELAST_VIGENTE["3"])
log_msg("Nueva   : T1=%.4f T2=%.4f T3=%.4f | agregada objetivo  %.4f | ratio T1/T3 %.3f",
        princ$T1, princ$T2, princ$T3, princ$agregada_verificada, princ$ratio_verificado)
log_msg("El nivel agregado baja %.1f%% en magnitud (%.4f -> %.4f). El gradiente se conserva,",
        100 * (1 - princ$agregada_verificada / agr_vigente), agr_vigente, princ$agregada_verificada)
log_msg("con un ratio de 2,0 en vez de 2,18: casi el mismo, pero importado de evidencia identificada.")
log_msg("Coincidencia digna de nota: T1=%.3f queda practicamente sobre el -0,812 del cuartil 1", princ$T1)
log_msg("coreano de Choi (2016), estimado de forma independiente.")

log_msg("\nLinea para R/24:")
log_msg("ELASTICIDAD_TOTAL_TERCIL <- c(`1` = %.6f, `2` = %.6f, `3` = %.6f)",
        princ$T1, princ$T2, princ$T3)

log_msg("\n--- 4) Backcast 2011-2025: elasticidad vieja contra nueva, misma semilla ---")

suppressMessages({
  library(haven)
  library(splines)
})
source(here::here("R", "24_motor_transiciones.R"))

int_dir <- here::here("data", "interim")
ANIOS_VALIDACION <- c(2014, 2017, 2018, 2019, 2021, 2022, 2023, 2024, 2025)

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor,
            params_cal[parametro == "raw_par3"]$valor)
insumos_bc <- cargar_insumos_motor()

correr_backcast <- function(elast) {
  assign("ELASTICIDAD_TOTAL_TERCIL", elast, envir = .GlobalEnv)
  assign("ELASTICIDAD_EXTENSIVA_TERCIL", elast / 2, envir = .GlobalEnv)
  assign("ELASTICIDAD_INTENSIVA_TERCIL", elast / 2, envir = .GlobalEnv)
  set.seed(20260809)
  pop <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"))
  pop[, sexo := factor(sexo, levels = c("varon", "mujer"))]
  pop[, tercil := factor(tercil, levels = c(1, 2, 3))]
  a <- 2011L; out <- list()
  for (obj in ANIOS_VALIDACION) {
    while (a < obj) {
      pop <- ciclo_anual(pop, a, insumos_bc$modelos, insumos_bc$tabla_superv, insumos_bc$precio_lookup,
                          insumos_bc$pob_ine, insumos_bc$entrada_15_por_anio, params[1:2], params[3])
      a <- a + 1L
    }
    out[[as.character(obj)]] <- data.table(anio = obj, prevalencia_sim = mean(pop$estado %in% c(2, 3)))
  }
  rbindlist(out)
}

ELAST_NUEVA <- c(`1` = princ$T1, `2` = princ$T2, `3` = princ$T3)
bc_vieja <- correr_backcast(ELAST_VIGENTE)[, rama := "vigente"]
bc_nueva <- correr_backcast(ELAST_NUEVA)[, rama := "nueva"]

bc <- merge(bc_vieja[, .(anio, prev_vigente = prevalencia_sim)],
            bc_nueva[, .(anio, prev_nueva = prevalencia_sim)], by = "anio")
bc[, dif_pp := 100 * (prev_nueva - prev_vigente)]
fwrite(bc, file.path(out_dir, "74_backcast_vieja_vs_nueva.csv"))

log_msg("Prevalencia simulada (poblacion completa, sin ponderar por celda):\n%s",
        paste(capture.output(print(bc, digits = 4)), collapse = "\n"))
log_msg("Desvio maximo entre ramas: %.4f pp (en %d).", max(abs(bc$dif_pp)), bc[which.max(abs(dif_pp))]$anio)
if (max(abs(bc$dif_pp)) < 0.25) {
  log_msg("VEREDICTO: por debajo de 0,25pp, un orden menor que el ancho del bracket de medicion")
  log_msg("por informante. La calibracion de R/25 NO necesita rehacerse: el cambio")
  log_msg("de elasticidad opera casi enteramente sobre los ESCENARIOS (diferencial permanente de")
  log_msg("precio), no sobre el backcast (cambios interanuales chicos).")
} else {
  log_msg("VEREDICTO: desvio no despreciable. Evaluar recalibrar escalada/downshift con R/25 bajo la")
  log_msg("elasticidad nueva antes de reportar el backcast.")
}

log_msg("\n=== outputs: 74_pesos_consumo_tercil.csv, 74_especificacion_elasticidad.csv,")
log_msg("    74_backcast_vieja_vs_nueva.csv ===")
close(log_con)
