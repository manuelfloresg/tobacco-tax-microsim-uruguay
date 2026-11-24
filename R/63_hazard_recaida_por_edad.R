# 63_hazard_recaida_por_edad.R
#
# The relapse rate derived separately within each age group: shows the age truncation of the
# duration distribution that biases the rate pooled over ages, and the effect of the
# age-conditional rate on the retrospective run.
# Input:  as R/62, plus output/tables/86_*.csv
# Output: output/tables/87_*.csv

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/63_hazard_recaida_por_edad.R")

log_con <- file(here::here("logs", "63_hazard_recaida_por_edad.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con); flush(log_con) }
log_msg("=== 63_hazard_recaida_por_edad.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

source(here::here("R", "24_motor_transiciones.R"))

ANIOS_VALIDACION <- c(2014, 2017, 2018, 2019, 2021, 2022, 2023, 2024, 2025)
ANIO_INI <- 2011; ANIO_FIN <- 2025; SEMILLA <- 20260809
ge_de <- function(edad) fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")

log_msg("\n=== 1) Hazard implicito por el metodo de R/19, dentro de cada grupo etario ===")

D <- fread(file.path(out_dir, "39_duracion_stock_por_edad.csv"))
BINS <- c(0, 1, 2, 5, 10, 20, Inf)
ETIQ <- c("<1", "1-2", "2-5", "5-10", "10-20", "20+")
ANCHO <- c(1, 1, 3, 5, 10, NA)

supervivencia_de <- function(x, etiqueta) {
  x <- copy(x)
  x[, bin := cut(duracion_anios, breaks = BINS, labels = ETIQ, right = FALSE)]
  r <- x[, .(n = .N, n_pond = sum(peso)), by = bin]
  r <- merge(data.table(bin = factor(ETIQ, levels = ETIQ)), r, by = "bin", all.x = TRUE)
  r[is.na(n_pond), `:=`(n = 0L, n_pond = 0)]
  setorder(r, bin)
  r[, prop_bin := n_pond / sum(n_pond)]
  r[, supervivencia := rev(cumsum(rev(prop_bin)))]
  r[, supervivencia_siguiente := shift(supervivencia, type = "lead")]
  r[, hazard_acumulado_bin := 1 - supervivencia_siguiente / supervivencia]
  r[, ancho_bin := ANCHO]
  r[, hazard_anual := 1 - (1 - hazard_acumulado_bin)^(1 / ancho_bin)]
  r[, grupo := etiqueta]
  r[]
}

grupos <- sort(unique(D$grupo_edad))
tabla <- rbindlist(c(list(supervivencia_de(D, "pooled (lo que usa R/19)")),
                     lapply(grupos, function(g) supervivencia_de(D[grupo_edad == g], g))))
fwrite(tabla, file.path(out_dir, "87_hazard_implicito_por_grupo_edad.csv"))
log_msg("Hazard anual implicito por bin de duracion, por grupo etario:\n%s",
        paste(capture.output(print(dcast(tabla[!is.na(hazard_anual)], grupo ~ bin,
              value.var = "hazard_anual", fun.aggregate = function(x) round(mean(x), 4)))), collapse = "\n"))
log_msg("Tamanio de muestra por celda:\n%s",
        paste(capture.output(print(dcast(tabla, grupo ~ bin, value.var = "n",
              fun.aggregate = function(x) as.integer(sum(x))))), collapse = "\n"))

rango <- tabla[!is.na(hazard_anual) & grupo != "pooled (lo que usa R/19)",
               .(hz_medio = weighted.mean(hazard_anual, n_pond)), by = grupo]
log_msg("Hazard medio implicito por grupo etario (ponderado por el stock de cada bin):\n%s",
        paste(capture.output(print(rango[order(grupo)][, .(grupo, hazard = round(hz_medio, 4))])), collapse = "\n"))
log_msg("Razon entre el grupo mas alto y el mas bajo: %.1f veces.", max(rango$hz_medio) / min(rango$hz_medio))
log_msg("El motor aplica UN SOLO hazard, sin edad (aplicar_recaida() de R/24 solo pasa duracion y sexo).")

log_msg("\n=== 2) Truncamiento: cuanta duracion es siquiera POSIBLE en cada grupo etario ===")

EDAD_MIN_INICIO <- 12
lim <- data.table(grupo = c("15-34", "35-44", "45-54", "55-64", "65+"),
                  edad_min = c(15, 35, 45, 55, 65), edad_max = c(34, 44, 54, 64, 99))
lim[, `:=`(dur_max_posible_min_edad = edad_min - EDAD_MIN_INICIO,
           dur_max_posible_max_edad = edad_max - EDAD_MIN_INICIO)]
obs_share20 <- D[, .(n = .N, share_20mas = weighted.mean(duracion_anios >= 20, peso),
                     mediana = median(duracion_anios)), by = .(grupo = grupo_edad)]
lim <- merge(lim, obs_share20, by = "grupo")
lim[, share_20mas_posible := pmax(0, pmin(1, (edad_max - EDAD_MIN_INICIO - 20) / (edad_max - edad_min + 1)))]
fwrite(lim, file.path(out_dir, "87_truncamiento_duracion_por_edad.csv"))
log_msg("Duracion maxima ARITMETICAMENTE posible y share observado de 20+ anios:\n%s",
        paste(capture.output(print(lim[, .(grupo, edad_min, edad_max,
              dur_max_en_el_mas_joven = dur_max_posible_min_edad,
              dur_max_en_el_mas_viejo = dur_max_posible_max_edad,
              mediana_obs = round(mediana, 1), share_20mas_obs = round(share_20mas, 3))])), collapse = "\n"))
log_msg("En 15-34 la duracion maxima posible para el mas joven del grupo es %d anios: la caida del stock",
        lim[grupo == "15-34"]$dur_max_posible_min_edad)
log_msg("con la duracion en ese grupo esta determinada por la edad, no por la recaida. El grupo 65+ es el")
log_msg("UNICO donde el soporte de duracion no esta acotado por la edad, y es el que da el hazard mas bajo.")

log_msg("\n=== 3) Variante C: hazard re-estimado sobre los grupos de 45+ ===")

m_rec <- readRDS(file.path(int_dir, "modelo_recaida.rds"))
log_msg("Modelo vigente (R/19): %s", paste(sprintf("%s=%.4f", names(coef(m_rec)), coef(m_rec)), collapse = " "))

datos_45 <- supervivencia_de(D[grupo_edad %in% c("45-54", "55-64", "65+")], "45+")
datos_45[, punto_medio := fcase(bin == "<1", 0.5, bin == "1-2", 1.5, bin == "2-5", 3.5,
                                 bin == "5-10", 7.5, bin == "10-20", 15, default = NA_real_)]
ajuste <- datos_45[!is.na(hazard_anual) & hazard_anual > 0 & hazard_anual < 1 & !is.na(punto_medio)]
log_msg("Puntos de ajuste de la variante C (45+ de GATS):\n%s",
        paste(capture.output(print(ajuste[, .(bin, n, punto_medio, hazard_anual = round(hazard_anual, 4))])), collapse = "\n"))
m_45 <- glm(hazard_anual ~ log(punto_medio), family = quasibinomial(), data = ajuste, weights = n_pond)
m_C <- m_rec
m_C$coefficients <- c(`(Intercept)` = unname(coef(m_45)[1]),
                      `log(punto_medio)` = unname(coef(m_45)[2]),
                      sexovaron = unname(coef(m_rec)["sexovaron"]))
log_msg("Variante C: %s", paste(sprintf("%s=%.4f", names(coef(m_C)), coef(m_C)), collapse = " "))

gr <- data.table(punto_medio = c(0.5, 1, 2, 5, 10, 20, 30, 40, 50))
gr[, vigente := predict(m_rec, newdata = data.frame(punto_medio = punto_medio, sexo = "varon"), type = "response")]
gr[, variante_C := predict(m_C, newdata = data.frame(punto_medio = punto_medio, sexo = "varon"), type = "response")]
fwrite(gr, file.path(out_dir, "87_hazard_vigente_vs_C.csv"))
log_msg("Hazard por duracion, varones (vigente contra variante C):\n%s",
        paste(capture.output(print(gr[, .(punto_medio, vigente = round(vigente, 4), variante_C = round(variante_C, 4))])), collapse = "\n"))

log_msg("\n=== 4) Backcast con la variante C ===")

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
pp <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor,
        params_cal[parametro == "raw_par3"]$valor)

correr <- function(modelo_recaida_override, etiqueta) {
  set.seed(SEMILLA)
  pop <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"))
  pop[, sexo := factor(sexo, levels = c("varon", "mujer"))]
  pop[, tercil := factor(tercil, levels = c(1, 2, 3))]
  insumos <- cargar_insumos_motor()
  if (!is.null(modelo_recaida_override)) insumos$modelos$recaida <- modelo_recaida_override
  comp <- list(); a <- ANIO_INI
  t0 <- Sys.time()
  while (a < ANIO_FIN) {
    pop <- ciclo_anual(pop, a, insumos$modelos, insumos$tabla_superv, insumos$precio_lookup,
                       insumos$pob_ine, insumos$entrada_15_por_anio, pp[1:2], pp[3])
    a <- a + 1
    if (a %in% ANIOS_VALIDACION) comp[[as.character(a)]] <- pop[, .N, by = .(ge = ge_de(edad), estado)][, anio := a][]
  }
  log_msg("Corrida '%s' terminada en %.1f segundos", etiqueta, as.numeric(difftime(Sys.time(), t0, units = "secs")))
  rbindlist(comp)
}

comp_C <- correr(m_C, "variante_C")

anchar <- function(dt, etiqueta) {
  w <- dcast(dt, anio + ge ~ estado, value.var = "N", fill = 0)
  for (k in as.character(1:5)) if (!(k %in% names(w))) w[, (k) := 0]
  setnames(w, as.character(1:5), paste0("n", 1:5))
  w[, tot := n1 + n2 + n3 + n4 + n5]
  w[, .(variante = etiqueta, anio, ge, prev_sim = (n2 + n3) / tot,
        ever_sim = (n2 + n3 + n4 + n5) / tot, phi_sim = (n2 + n3) / (n2 + n3 + n4 + n5))]
}

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))[!is.na(tercil)]
P[, ge := ge_de(edad)]
obs <- dcast(P[anio %in% ANIOS_VALIDACION, .(w = sum(peso)), by = .(anio, ge, estado)],
             anio + ge ~ estado, value.var = "w", fill = 0)
setnames(obs, as.character(1:5), paste0("n", 1:5))
obs[, tot := n1 + n2 + n3 + n4 + n5]
obs[, `:=`(prev_obs = (n2 + n3) / tot, ever_obs = (n2 + n3 + n4 + n5) / tot,
           phi_obs = (n2 + n3) / (n2 + n3 + n4 + n5))]

res <- merge(anchar(comp_C, "variante_C"), obs[, .(anio, ge, prev_obs, ever_obs, phi_obs)], by = c("anio", "ge"))
res[, error_pp := 100 * (prev_sim - prev_obs)]

prev <- tryCatch(fread(file.path(out_dir, "86_contrafactico_recaida.csv")), error = function(e) NULL)
if (!is.null(prev)) {
  res <- rbindlist(list(prev[, .(anio, ge, variante, prev_sim, ever_sim, phi_sim, prev_obs, ever_obs, phi_obs, error_pp)],
                        res[, .(anio, ge, variante, prev_sim, ever_sim, phi_sim, prev_obs, ever_obs, phi_obs, error_pp)]),
                   use.names = TRUE)
}
setorder(res, ge, variante, anio)
fwrite(res, file.path(out_dir, "87_contrafactico_todas_las_variantes.csv"))
log_msg("Error de prevalencia (pp) por variante y anio:\n%s",
        paste(capture.output(print(dcast(res, ge + anio ~ variante, value.var = "error_pp",
              fun.aggregate = function(x) round(mean(x), 2)))), collapse = "\n"))
log_msg("Error absoluto medio de los nueve anios:\n%s",
        paste(capture.output(print(dcast(res[, .(eam = mean(abs(error_pp))), by = .(ge, variante)],
              ge ~ variante, value.var = "eam", fun.aggregate = function(x) round(x, 2)))), collapse = "\n"))

brk <- fread(file.path(out_dir, "67_bracket_grupo_etario.csv"))
brk <- brk[metrica == "prevalencia_actual", .(anio, ge = grupo_etario, piso = obs, techo = std)]
res2 <- merge(res, brk, by = c("anio", "ge"))
res2[, `:=`(dentro = prev_sim >= pmin(piso, techo) & prev_sim <= pmax(piso, techo),
            exceso_techo_pp = 100 * pmax(0, prev_sim - pmax(piso, techo)))]
fwrite(res2, file.path(out_dir, "87_posicion_bracket_variantes.csv"))
log_msg("Anios dentro del bracket de medicion (de 9) por variante:\n%s",
        paste(capture.output(print(dcast(res2[, .(dentro = sum(dentro)), by = .(ge, variante)],
              ge ~ variante, value.var = "dentro", fun.aggregate = function(x) as.integer(sum(x))))), collapse = "\n"))
log_msg("Exceso maximo sobre el techo del bracket (pp) por variante:\n%s",
        paste(capture.output(print(dcast(res2[, .(exc = max(exceso_techo_pp)), by = .(ge, variante)],
              ge ~ variante, value.var = "exc", fun.aggregate = function(x) round(max(x), 2)))), collapse = "\n"))

log_msg("\n=== 5) Equilibrio de flujo con un hazard condicional a la edad (aritmetica) ===")

fl <- fread(file.path(out_dir, "86_flujos_brutos_motor.csv"))
q_motor <- fl[evento == "cesacion", .(q = mean(tasa)), by = ge]
r_motor <- fl[evento == "recaida", .(r_vigente = mean(tasa)), by = ge]

peso_bin <- tabla[!is.na(hazard_anual) & grupo != "pooled (lo que usa R/19)"]
hz_grupo <- peso_bin[, .(hz = weighted.mean(hazard_anual, n_pond), n_pond = sum(n_pond)), by = grupo]
hz_4564 <- hz_grupo[grupo %in% c("45-54", "55-64"), weighted.mean(hz, n_pond)]
hz_65 <- hz_grupo[grupo == "65+"]$hz
r_cond <- data.table(ge = c("45-64", "65+"), r_condicional = c(hz_4564, hz_65))

eq <- merge(merge(q_motor, r_motor, by = "ge"), r_cond, by = "ge")
eq <- merge(eq, res[variante == "variante_C" & anio == 2025, .(ge, phi_obs)], by = "ge")
eq[, `:=`(phi_eq_vigente = r_vigente / (r_vigente + q),
          phi_eq_condicional = r_condicional / (r_condicional + q))]
fwrite(eq, file.path(out_dir, "87_equilibrio_hazard_condicional.csv"))
log_msg("Equilibrio de flujo phi* = r/(q+r) con la cesacion del motor y el hazard de recaida\ncondicional al grupo etario de GATS, contra el phi observado en la ECH 2025:\n%s",
        paste(capture.output(print(eq[, .(ge, q_motor = round(q, 4), r_vigente = round(r_vigente, 4),
              r_condicional = round(r_condicional, 4), phi_eq_vigente = round(phi_eq_vigente, 3),
              phi_eq_condicional = round(phi_eq_condicional, 3), phi_obs_2025 = round(phi_obs, 3))])), collapse = "\n"))
log_msg("Lectura: con el hazard condicional el equilibrio al que tiende el motor deja de estar")
log_msg("a %.0f%% del observado y pasa a estar a %.0f%%, en el tramo 65+.",
        100 * (eq[ge == "65+"]$phi_eq_vigente / eq[ge == "65+"]$phi_obs - 1),
        100 * (eq[ge == "65+"]$phi_eq_condicional / eq[ge == "65+"]$phi_obs - 1))

log_msg("\n=== outputs: 87_hazard_implicito_por_grupo_edad.csv, 87_truncamiento_duracion_por_edad.csv, 87_hazard_vigente_vs_C.csv, 87_contrafactico_todas_las_variantes.csv, 87_posicion_bracket_variantes.csv, 87_equilibrio_hazard_condicional.csv ===")
close(log_con)
