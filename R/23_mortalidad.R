# 23_mortalidad.R
#
# Survival by age, sex, year and smoking state. General survival is implied by the INE population
# projections (Revision 2025); relative risks by smoking state are rescaled so that the
# population-weighted average reproduces general survival.
# Usage:  Rscript R/23_mortalidad.R                       (primary specification)
#         Rscript R/23_mortalidad.R graduado_nivel_pres   (former-smoker risk graded by time since quitting)
# Input:  data/raw/Proyecciones_poblacion/, panel from R/04
# Output: output/tables/42_supervivencia_por_estado*.csv

library(here)
library(data.table)
library(haven)

here::i_am("R/23_mortalidad.R")

log_con <- file(here::here("logs", "23_mortalidad.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 23_mortalidad.R -- %s ===", format(Sys.time()))

raw_dir <- here::here("data", "raw", "Proyecciones_poblacion")
int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

log_msg("\n--- A) Supervivencia implicita desde poblacion_1950_2070_empalmada.dta ---")

POB <- setDT(read_dta(file.path(raw_dir, "poblacion_1950_2070_empalmada.dta")))
setnames(POB, c("año", "mujer"), c("anio", "mujer"), skip_absent = TRUE)
POB[, sexo := fifelse(mujer == 1, "mujer", "varon")]
log_msg("Filas: %d, rango anio: %d-%d, rango edad: %d-%d", nrow(POB), min(POB$anio), max(POB$anio),
        min(POB$edad), max(POB$edad))

setorder(POB, sexo, edad, anio)
POB_sig <- POB[, .(sexo, edad_sig = edad - 1, anio_sig = anio - 1, poblacion_sig = poblacion)]
S <- merge(POB, POB_sig, by.x = c("sexo", "edad", "anio"), by.y = c("sexo", "edad_sig", "anio_sig"))
S[, supervivencia := poblacion_sig / poblacion]
S <- S[, .(sexo, edad, anio, poblacion, supervivencia)]

n_fuera_rango <- S[supervivencia <= 0 | supervivencia > 1.5, .N]
log_msg("Celdas con supervivencia implicita fuera de (0, 1.5] (migracion neta grande o ruido, edades 15-95): %d de %d",
        S[edad %in% 15:95, sum(supervivencia <= 0 | supervivencia > 1.5)], S[edad %in% 15:95, .N])
log_msg("Percentiles de supervivencia implicita, edades 15-95 (para ver el orden de magnitud):\n%s",
        paste(capture.output(print(quantile(S[edad %in% 15:95]$supervivencia, c(0, .01, .5, .99, 1), na.rm = TRUE))), collapse = "\n"))
S[, mortalidad_basal := pmax(0, 1 - pmin(supervivencia, 1))]

EDAD_TOPE_TABLA <- 99
EDADES_AJUSTE_GOMPERTZ <- 85:(EDAD_TOPE_TABLA - 1)

extrapolar_tope <- function(edades, mort) {
  ok <- is.finite(mort) & mort > 0 & mort < 1
  if (sum(ok) < 3) return(NA_real_)
  fit <- stats::lm(log(mort[ok]) ~ edades[ok])
  as.numeric(exp(coef(fit)[1] + coef(fit)[2] * EDAD_TOPE_TABLA))
}

tope_extrap <- S[edad %in% EDADES_AJUSTE_GOMPERTZ,
                 .(mort_tope = extrapolar_tope(edad, mortalidad_basal)), by = .(sexo, anio)]
mort_tope_previa <- S[edad == EDAD_TOPE_TABLA, .(sexo, anio, mort_previa = mortalidad_basal)]
tope_extrap <- merge(tope_extrap, mort_tope_previa, by = c("sexo", "anio"), all.x = TRUE)
mort_98 <- S[edad == EDAD_TOPE_TABLA - 1, .(sexo, anio, mort_98 = mortalidad_basal)]
tope_extrap <- merge(tope_extrap, mort_98, by = c("sexo", "anio"), all.x = TRUE)
tope_extrap[, mort_final := fifelse(is.finite(mort_tope) & mort_tope > 0 & mort_tope < 1,
                                     mort_tope, mort_98)]
S <- merge(S, tope_extrap[, .(sexo, anio, mort_final)], by = c("sexo", "anio"), all.x = TRUE)
S[edad == EDAD_TOPE_TABLA & is.finite(mort_final), mortalidad_basal := mort_final]
S[, mort_final := NULL]
log_msg("Correccion de edad tope (%d): la mortalidad basal del grupo abierto se reemplaza por extrapolacion Gompertz sobre edades %d-%d.",
        EDAD_TOPE_TABLA, min(EDADES_AJUSTE_GOMPERTZ), max(EDADES_AJUSTE_GOMPERTZ))
log_msg("Antes: %d de %d celdas sexo x anio tenian mortalidad basal CERO a los %d (inmortalidad). Despues: %d.",
        tope_extrap[!is.finite(mort_previa) | mort_previa <= 0, .N], nrow(tope_extrap), EDAD_TOPE_TABLA,
        S[edad == EDAD_TOPE_TABLA & mortalidad_basal <= 0, .N])
log_msg("Ejemplo (2025): mortalidad basal a los %d antes vs despues:\n%s", EDAD_TOPE_TABLA,
        paste(capture.output(print(merge(tope_extrap[anio == 2025, .(sexo, mort_previa, mort_extrapolada = mort_final)],
                                         S[edad == EDAD_TOPE_TABLA - 1 & anio == 2025, .(sexo, mort_edad_98 = mortalidad_basal)],
                                         by = "sexo"))), collapse = "\n"))

log_msg("\n--- B) Riesgo relativo por estado y reescalado ---")

banda_edad <- function(edad) {
  cut(edad, breaks = c(-Inf, 34, 54, 64, 74, Inf),
      labels = c("<35", "35-54", "55-64", "65-74", "75+"), right = TRUE)
}
FACTOR_EDAD_SEXO <- data.table(
  sexo          = rep(c("varon", "mujer"), each = 5),
  banda         = rep(c("<35", "35-54", "55-64", "65-74", "75+"), 2),
  factor_actual = c(1, 1, 2.97 / 2.55, 3.02 / 2.55, 2.40 / 2.55,
                     1, 1, 2.63 / 1.79, 2.87 / 1.79, 2.47 / 1.79),
  factor_ex     = c(1, 1, 1.47 / 1.33, 1.57 / 1.33, 1.41 / 1.33,
                     1, 1, 1.34 / 1.22, 1.53 / 1.22, 1.43 / 1.22)
)
RR_BASE <- data.table(estado = 1:5, RR_base = c(1.0, 1.3, 2.5, 1.8, 1.2),
                       tipo = c("ninguno", "actual", "actual", "ex", "ex"))

RR_DURACION <- data.table(
  tramo_ex = c("<5", "5-9", "10-19", "20-29", "30+"),
  lim_inf  = c(0, 5, 10, 20, 30),
  RR_dur   = c(2.01, 1.74, 1.43, 1.16, 1.04)
)
banda_duracion <- function(anios) {
  cut(anios, breaks = c(RR_DURACION$lim_inf, Inf), labels = RR_DURACION$tramo_ex, right = FALSE)
}

VARIANTE_RR_EX <- {
  a <- commandArgs(trailingOnly = TRUE)
  v <- if (length(a) > 0) a[1] else "vigente"
  if (!v %in% c("vigente", "graduado_literal", "graduado_nivel_pres"))
    stop("Variante desconocida: ", v, ". Usar vigente | graduado_literal | graduado_nivel_pres")
  v
}
log_msg("VARIANTE DE RR DE EX FUMADORES: %s", VARIANTE_RR_EX)

dist_duracion <- function() {
  pobl <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"),
                select = c("edad", "sexo", "estado", "anios_desde_dejar"))
  ex <- pobl[estado %in% c(4L, 5L) & !is.na(anios_desde_dejar) & edad %in% 15:99]
  ex[, tramo_ex := banda_duracion(anios_desde_dejar)]
  d <- ex[, .N, by = .(edad, sexo, estado, tramo_ex)]
  g <- CJ(edad = 15:99, sexo = c("varon", "mujer"), estado = 4:5, tramo_ex = RR_DURACION$tramo_ex,
          unique = TRUE)
  g[, tramo_ex := factor(tramo_ex, levels = RR_DURACION$tramo_ex)]
  d[, tramo_ex := factor(as.character(tramo_ex), levels = RR_DURACION$tramo_ex)]
  g <- merge(g, d, by = c("edad", "sexo", "estado", "tramo_ex"), all.x = TRUE)
  g[is.na(N), N := 0]
  marg <- ex[, .(N_marg = .N), by = .(estado, tramo_ex)]
  marg[, tramo_ex := factor(as.character(tramo_ex), levels = RR_DURACION$tramo_ex)]
  g <- merge(g, marg, by = c("estado", "tramo_ex"), all.x = TRUE)
  g[is.na(N_marg), N_marg := 0]
  g[, n_celda := sum(N), by = .(edad, sexo, estado)]
  g[, share_dur := fifelse(n_celda >= 30, N / pmax(n_celda, 1), N_marg / sum(N_marg)),
    by = .(edad, sexo, estado)]
  g[, share_dur := share_dur / sum(share_dur), by = .(edad, sexo, estado)]
  stopifnot(g[, abs(sum(share_dur) - 1) < 1e-10, by = .(edad, sexo, estado)]$V1)
  g[, .(edad, sexo, estado, tramo_ex, share_dur, celda_propia = n_celda >= 30)]
}

RR_no_ex <- CJ(edad = 15:99, sexo = c("varon", "mujer"), estado = 1:3)
RR_no_ex <- merge(RR_no_ex, RR_BASE, by = "estado")
RR_no_ex[, banda := banda_edad(edad)]
RR_no_ex <- merge(RR_no_ex, FACTOR_EDAD_SEXO, by = c("sexo", "banda"))
RR_no_ex[, RR := fifelse(tipo == "ninguno", RR_base, RR_base * factor_actual)]
RR_no_ex[, tramo_ex := "-"]
RR_no_ex <- RR_no_ex[, .(edad, sexo, estado, tramo_ex, RR)]

RR_ex <- CJ(edad = 15:99, sexo = c("varon", "mujer"), estado = 4:5,
            tramo_ex = RR_DURACION$tramo_ex, unique = TRUE)
RR_ex <- merge(RR_ex, RR_BASE[, .(estado, RR_base)], by = "estado")
RR_ex[, banda := banda_edad(edad)]
RR_ex <- merge(RR_ex, FACTOR_EDAD_SEXO[, .(sexo, banda, factor_ex)], by = c("sexo", "banda"))
RR_ex <- merge(RR_ex, RR_DURACION[, .(tramo_ex, RR_dur)], by = "tramo_ex")
RR_ex[, `:=`(RR_vig = RR_base * factor_ex, RR_grad = RR_dur * factor_ex)]
RR_ex <- RR_ex[, .(edad, sexo, estado, tramo_ex, RR_vig, RR_grad)]
log_msg("RR de mortalidad por estado x edad x sexo (Surgeon General 2014, Tabla 12.3 -- ver nota arriba). Ejemplo, estado 3 (diario), varon:\n%s",
        paste(capture.output(print(RR_no_ex[sexo == "varon" & estado == 3 & edad %in% c(20, 40, 60, 70, 80, 95)])), collapse = "\n"))
log_msg("RR de ex fumadores por tramo de duracion (edad 70, varon), antes de elegir variante:\n%s",
        paste(capture.output(print(RR_ex[sexo == "varon" & edad == 70][order(estado, tramo_ex)])), collapse = "\n"))

Pn <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))
composicion <- Pn[edad %in% 15:99, .(N_pond = sum(peso)), by = .(edad, sexo, estado)]

grilla_completa <- CJ(edad = 15:99, sexo = c("varon", "mujer"), estado = 1:5)
composicion <- merge(grilla_completa, composicion, by = c("edad", "sexo", "estado"), all.x = TRUE)
setorder(composicion, sexo, estado, edad)
composicion[, share_obs := N_pond / sum(N_pond, na.rm = TRUE), by = .(edad, sexo)]
n_faltantes <- composicion[is.na(N_pond), .N]
composicion[, share_llenado := nafill(nafill(share_obs, "locf"), "nocb"), by = .(sexo, estado)]
composicion[, imputada := is.na(N_pond)]
composicion[, share := share_llenado / sum(share_llenado), by = .(edad, sexo)]
log_msg("Completado de grilla (23.7): %d celdas edad x sexo x estado sin dato en la ECH, completadas por LOCF sobre edad y renormalizadas.",
        n_faltantes)
if (n_faltantes > 0) {
  log_msg("Celdas imputadas por estado:\n%s",
          paste(capture.output(print(composicion[imputada == TRUE, .(n_celdas = .N, edad_min = min(edad), edad_max = max(edad)), by = estado][order(estado)])), collapse = "\n"))
  log_msg("Share imputado: minimo %.3e, maximo %.3e, mediana %.3e (son estados raros a edades altas; si algun share imputado fuera grande, revisar)",
          min(composicion[imputada == TRUE]$share), max(composicion[imputada == TRUE]$share),
          median(composicion[imputada == TRUE]$share))
}
stopifnot(nrow(composicion) == 85 * 2 * 5, composicion[, all(!is.na(share))])
composicion[, .(desvio_suma_1 = max(abs(sum(share) - 1))), by = .(edad, sexo)][
  , stopifnot(max(desvio_suma_1) < 1e-12)]
composicion <- composicion[, .(edad, sexo, estado, share, imputada)]

dd <- dist_duracion()
log_msg("Distribucion de duracion: %d de %d celdas edad x sexo x estado usan su propia celda (>=30 obs); el resto usa el marginal del estado.",
        dd[celda_propia == TRUE, uniqueN(paste(edad, sexo, estado))],
        dd[, uniqueN(paste(edad, sexo, estado))])

comp_no_ex <- composicion[estado <= 3L][, tramo_ex := "-"]
comp_ex <- merge(composicion[estado >= 4L], dd, by = c("edad", "sexo", "estado"), allow.cartesian = TRUE)
comp_ex[, share := share * share_dur]
composicion <- rbind(comp_no_ex[, .(edad, sexo, estado, tramo_ex, share, imputada)],
                     comp_ex[, .(edad, sexo, estado, tramo_ex = as.character(tramo_ex), share, imputada)])
composicion[, .(d = abs(sum(share) - 1)), by = .(edad, sexo)][, stopifnot(max(d) < 1e-10)]

RR_todos <- rbind(RR_no_ex[, .(edad, sexo, estado, tramo_ex, RR_vig = RR, RR_grad = RR)], RR_ex)
composicion <- merge(composicion, RR_todos, by = c("estado", "edad", "sexo", "tramo_ex"))

N_celda <- Pn[edad %in% 15:99, .(N_cell = sum(peso)), by = .(edad, sexo)]
composicion <- merge(composicion, N_celda, by = c("edad", "sexo"), all.x = TRUE)
composicion[is.na(N_cell), N_cell := 0]
composicion[, w_pob := N_cell * share]

media_ex_vig  <- composicion[estado >= 4L, weighted.mean(RR_vig, w_pob)]
media_ex_grad <- composicion[estado >= 4L, weighted.mean(RR_grad, w_pob)]
k_nivel <- switch(VARIANTE_RR_EX,
                  vigente = NA_real_,
                  graduado_literal = 1,
                  graduado_nivel_pres = media_ex_vig / media_ex_grad)
log_msg("RR medio de ex fumadores (ponderado por poblacion): vigente %.4f | graduado sin normalizar %.4f (%+.1f%%)",
        media_ex_vig, media_ex_grad, 100 * (media_ex_grad / media_ex_vig - 1))
if (!is.na(k_nivel)) log_msg("Constante de nivel k aplicada al gradiente: %.5f", k_nivel)

composicion[, RR := if (VARIANTE_RR_EX == "vigente") RR_vig else
                    fifelse(estado >= 4L, RR_grad * k_nivel, RR_grad)]
log_msg("RR de ex fumadores por tramo (edad 70, varon):\n%s",
        paste(capture.output(print(unique(composicion[edad == 70 & sexo == "varon" & estado >= 4L,
              .(estado, tramo_ex, RR = round(RR, 3))])[order(estado, tramo_ex)])), collapse = "\n"))

factor_celda <- composicion[, .(suma_share_RR = sum(share * RR)), by = .(edad, sexo)]
log_msg("Composicion de estados calculada con el panel ECH pooled 2011-2025 (fija, no varia por anio dentro del motor)")
log_msg("Rango de suma(share*RR) por celda edad-sexo (factor de reescalado): %.3f a %.3f",
        min(factor_celda$suma_share_RR), max(factor_celda$suma_share_RR))

sup_estado <- merge(S[edad %in% 15:99], factor_celda, by = c("edad", "sexo"))
sup_estado <- merge(sup_estado, composicion[, .(edad, sexo, estado, tramo_ex, RR, imputada)],
                    by = c("edad", "sexo"), allow.cartesian = TRUE)
sup_estado[, lambda := mortalidad_basal / suma_share_RR]
sup_estado[, mortalidad_estado := pmin(1, RR * lambda)]
sup_estado[, prob_sobrevive := 1 - mortalidad_estado]
sup_estado <- sup_estado[, .(edad, sexo, anio, estado, tramo_ex, mortalidad_basal, RR, prob_sobrevive,
                              composicion_imputada = imputada)]
setorder(sup_estado, sexo, estado, edad, tramo_ex, anio)

grilla_est_tramo <- rbind(
  CJ(estado = 1:3, tramo_ex = "-"),
  CJ(estado = 4:5, tramo_ex = RR_DURACION$tramo_ex, unique = TRUE))
n_anios <- uniqueN(sup_estado$anio)
n_esperado <- 85 * 2 * nrow(grilla_est_tramo) * n_anios
faltan_grilla <- CJ(edad = 15:99, sexo = c("varon", "mujer"), anio = unique(sup_estado$anio),
                    unique = TRUE)[, k := 1][grilla_est_tramo[, k := 1], on = "k", allow.cartesian = TRUE][
                    , k := NULL][!sup_estado, on = .(edad, sexo, estado, tramo_ex, anio)]
log_msg("Chequeo de grilla completa: %d filas generadas, %d esperadas (85 edades x 2 sexos x %d estado-tramo x %d anios); celdas faltantes: %d",
        nrow(sup_estado), n_esperado, nrow(grilla_est_tramo), n_anios, nrow(faltan_grilla))
stopifnot(nrow(faltan_grilla) == 0, nrow(sup_estado) == n_esperado)

fwrite(sup_estado, file.path(out_dir, "42_supervivencia_por_estado.csv"))
log_msg("Escrito 42_supervivencia_por_estado.csv (%d filas: edad 15-99 x sexo x anio 1950-2069 x estado x tramo)", nrow(sup_estado))

peso_tramo <- composicion[, .(edad, sexo, estado, tramo_ex, share)]
peso_tramo[, w_tramo := share / sum(share), by = .(edad, sexo, estado)]
sup_agg <- merge(sup_estado, peso_tramo[, .(edad, sexo, estado, tramo_ex, w_tramo)],
                 by = c("edad", "sexo", "estado", "tramo_ex"))
sup_agg <- sup_agg[, .(mortalidad_basal = mortalidad_basal[1],
                        RR = sum(RR * w_tramo),
                        prob_sobrevive = sum(prob_sobrevive * w_tramo),
                        composicion_imputada = any(composicion_imputada)),
                    by = .(edad, sexo, anio, estado)]
setorder(sup_agg, sexo, estado, edad, anio)
stopifnot(nrow(sup_agg) == 85 * 2 * 5 * n_anios)
fwrite(sup_agg, file.path(out_dir, "42_supervivencia_por_estado_agregada.csv"))
log_msg("Escrito 42_supervivencia_por_estado_agregada.csv (%d filas, colapsada por tramo para R/30)", nrow(sup_agg))

fwrite(data.table(variante_rr_ex = VARIANTE_RR_EX, k_nivel = k_nivel,
                  rr_medio_ex_vigente = media_ex_vig, rr_medio_ex_graduado = media_ex_grad,
                  fecha = format(Sys.time()), filas = nrow(sup_estado)),
       file.path(out_dir, "42_variante_mortalidad.csv"))

log_msg("Ejemplo de verificacion (edad 70, sexo varon, anio 2020) -- prob_sobrevive debe ser menor cuanto mayor el RR:\n%s",
        paste(capture.output(print(sup_estado[edad == 70 & sexo == "varon" & anio == 2020])), collapse = "\n"))

log_msg("\n=== outputs: output/tables/42_supervivencia_por_estado.csv, 42_variante_mortalidad.csv ===")
close(log_con)
