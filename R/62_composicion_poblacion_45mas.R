# 62_composicion_poblacion_45mas.R
#
# Mechanism of the excess simulated prevalence above age 45: decomposes the error of the
# retrospective run into the share who ever smoked and the share of those who still smoke, and
# accounts for the gross flows of the model by age. Includes an ECH flow-balance estimate of
# relapse at older ages.
# Input:  synthetic population 2011, panel from R/04, output/tables/39_*, 42_*, 43_*, 48_*, 67_*
# Output: output/tables/86_*.csv

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/62_composicion_poblacion_45mas.R")

log_con <- file(here::here("logs", "62_composicion_poblacion_45mas.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con); flush(log_con) }
log_msg("=== 62_composicion_poblacion_45mas.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

source(here::here("R", "24_motor_transiciones.R"))

ANIOS_VALIDACION <- c(2014, 2017, 2018, 2019, 2021, 2022, 2023, 2024, 2025)
ANIO_INI <- 2011
ANIO_FIN <- 2025
SEMILLA <- 20260809

ge_de <- function(edad) fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor,
            params_cal[parametro == "raw_par3"]$valor)
params_escalada <- params[1:2]
param_downshift <- params[3]

log_msg("\n=== 1) Backcast instrumentado (replica exacta de R/26 + contabilidad de flujos brutos) ===")

ciclo_anual_instrumentado <- function(pop, anio_actual, modelos, tabla_superv, precio_lookup, pob_ine,
                                       entrada_15_por_anio, params_escalada, param_downshift) {
  reg <- list()
  ratio <- precio_lookup[anio == anio_actual]$p_reales / precio_lookup[anio == anio_actual - 1]$p_reales
  precio_ratio_tercil <- c(`1` = 1, `2` = 1, `3` = 1) * ratio

  contar <- function(dt) dt[, .N, by = .(ge = ge_de(edad), estado)]

  antes_mort <- contar(pop)
  pop <- aplicar_mortalidad(pop, anio_actual, tabla_superv)
  despues_mort <- contar(pop)
  mu <- merge(antes_mort, despues_mort, by = c("ge", "estado"), all = TRUE, suffixes = c("_antes", "_desp"))
  mu[is.na(N_antes), N_antes := 0L][is.na(N_desp), N_desp := 0L]
  reg[["muerte"]] <- mu[, .(anio = anio_actual, ge, estado_origen = estado, evento = "muerte",
                            n = as.numeric(N_antes - N_desp), expuestos = as.numeric(N_antes))]
  reg[["stock"]] <- despues_mort[, .(anio = anio_actual, ge, estado_origen = estado, evento = "stock_vivo",
                                     n = as.numeric(N), expuestos = as.numeric(N))]

  registrar <- function(e0, g0, nombre, cond_origen, cond_destino) {
    ocurre <- cond_origen(e0) & cond_destino(pop$estado)
    data.table(ge = g0, estado_origen = e0, ocurre = ocurre, expuesto = cond_origen(e0)
    )[, .(n = sum(ocurre), expuestos = sum(expuesto)), by = .(ge, estado_origen)][
      , .(anio = anio_actual, ge, estado_origen, evento = nombre, n = as.numeric(n), expuestos = as.numeric(expuestos))]
  }

  e0 <- copy(pop$estado); g0 <- ge_de(pop$edad)
  pop <- aplicar_cesacion(pop, anio_actual, modelos$cesacion, precio_ratio_tercil)
  reg[["cesacion"]] <- registrar(e0, g0, "cesacion", function(x) x %in% c(2L, 3L), function(x) x == 4L)

  e0 <- copy(pop$estado); g0 <- ge_de(pop$edad)
  pop <- aplicar_recaida(pop, modelos$recaida)
  reg[["recaida"]] <- registrar(e0, g0, "recaida", function(x) x %in% c(4L, 5L), function(x) x %in% c(2L, 3L))

  e0 <- copy(pop$estado); g0 <- ge_de(pop$edad)
  pop <- aplicar_iniciacion(pop, anio_actual, modelos$iniciacion, precio_ratio_tercil)
  reg[["iniciacion"]] <- registrar(e0, g0, "iniciacion", function(x) x == 1L, function(x) x == 2L)

  e0 <- copy(pop$estado); g0 <- ge_de(pop$edad)
  pop <- aplicar_escalada(pop, params_escalada)
  reg[["escalada"]] <- registrar(e0, g0, "escalada", function(x) x == 2L, function(x) x == 3L)

  e0 <- copy(pop$estado); g0 <- ge_de(pop$edad)
  pop <- aplicar_downshift(pop, param_downshift)
  reg[["downshift"]] <- registrar(e0, g0, "downshift", function(x) x == 3L, function(x) x == 2L)

  pop <- avanzar_edad(pop)
  entrantes <- generar_entrantes_15(anio_actual + 1, pob_ine, entrada_15_por_anio)
  pop <- rbind(pop, entrantes, fill = TRUE)
  list(pop = pop, reg = rbindlist(reg, use.names = TRUE))
}

correr_backcast <- function(modelo_recaida_override = NULL, etiqueta = "vigente", instrumentar = FALSE) {
  set.seed(SEMILLA)
  pop <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"))
  pop[, sexo := factor(sexo, levels = c("varon", "mujer"))]
  pop[, tercil := factor(tercil, levels = c(1, 2, 3))]
  insumos <- cargar_insumos_motor()
  if (!is.null(modelo_recaida_override)) insumos$modelos$recaida <- modelo_recaida_override

  registro <- list(); comp_anual <- list(); dur_45mas <- list()
  contar_anual <- function(dt, a) dt[, .N, by = .(ge = ge_de(edad), estado)][, anio := a][]
  anio_actual <- ANIO_INI
  comp_anual[[as.character(anio_actual)]] <- contar_anual(pop, anio_actual)
  dur_45mas[[as.character(anio_actual)]] <- pop[edad >= 45 & estado %in% c(4L, 5L),
                                                 .(anio = anio_actual, ge = ge_de(edad), dur = anios_desde_dejar)]
  t0 <- Sys.time()
  while (anio_actual < ANIO_FIN) {
    if (instrumentar) {
      sal <- ciclo_anual_instrumentado(pop, anio_actual, insumos$modelos, insumos$tabla_superv,
                                        insumos$precio_lookup, insumos$pob_ine, insumos$entrada_15_por_anio,
                                        params_escalada, param_downshift)
      pop <- sal$pop
      registro[[as.character(anio_actual)]] <- sal$reg
    } else {
      pop <- ciclo_anual(pop, anio_actual, insumos$modelos, insumos$tabla_superv, insumos$precio_lookup,
                          insumos$pob_ine, insumos$entrada_15_por_anio, params_escalada, param_downshift)
    }
    anio_actual <- anio_actual + 1
    comp_anual[[as.character(anio_actual)]] <- contar_anual(pop, anio_actual)
    if (anio_actual %in% c(2018L, 2025L)) {
      dur_45mas[[as.character(anio_actual)]] <- pop[edad >= 45 & estado %in% c(4L, 5L),
                                                     .(anio = anio_actual, ge = ge_de(edad), dur = anios_desde_dejar)]
    }
  }
  log_msg("Corrida '%s' terminada en %.1f segundos", etiqueta, as.numeric(difftime(Sys.time(), t0, units = "secs")))
  list(comp = rbindlist(comp_anual), flujos = if (instrumentar) rbindlist(registro) else NULL,
       dur = rbindlist(dur_45mas))
}

base_run <- correr_backcast(instrumentar = TRUE, etiqueta = "vigente (instrumentada)")
comp_sim <- base_run$comp
flujos <- base_run$flujos

anchar <- function(dt) {
  w <- dcast(dt, anio + ge ~ estado, value.var = "N", fill = 0)
  for (k in as.character(1:5)) if (!(k %in% names(w))) w[, (k) := 0]
  setnames(w, as.character(1:5), paste0("n", 1:5))
  w[, tot := n1 + n2 + n3 + n4 + n5]
  w[, `:=`(prevalencia_sim = (n2 + n3) / tot, share_diario_sim = n3 / (n2 + n3),
           ever_sim = (n2 + n3 + n4 + n5) / tot, phi_sim = (n2 + n3) / (n2 + n3 + n4 + n5))]
  w[]
}
w_sim <- anchar(comp_sim)

ref <- fread(file.path(out_dir, "48_descomposicion_error_edad.csv"))
chk <- merge(ref[, .(anio, ge = grupo_etario, prev_ref = prevalencia_sim, sd_ref = share_diario_sim)],
             w_sim[, .(anio, ge, prevalencia_sim, share_diario_sim)], by = c("anio", "ge"))
d_prev <- max(abs(chk$prevalencia_sim - chk$prev_ref))
d_sd <- max(abs(chk$share_diario_sim - chk$sd_ref))
log_msg("GUARDARRAIL. Desvio maximo contra 48_descomposicion_error_edad.csv sobre %d celdas: prevalencia %.3e, share diario %.3e",
        nrow(chk), d_prev, d_sd)
if (nrow(chk) != 36 || d_prev > 1e-12 || d_sd > 1e-12) {
  stop("La corrida instrumentada NO reproduce el backcast vigente. Nada de lo que sigue es interpretable.")
}
log_msg("La corrida instrumentada reproduce el backcast vigente bit a bit. Los flujos son los del motor publicado.")

log_msg("\n=== 2) Descomposicion del error en stock (ever) x split (actual entre ever) ===")

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))
P <- P[!is.na(tercil)]
P[, ge := ge_de(edad)]
obs <- P[anio %in% ANIOS_VALIDACION, .(w = sum(peso)), by = .(anio, ge, estado)]
w_obs <- dcast(obs, anio + ge ~ estado, value.var = "w", fill = 0)
setnames(w_obs, as.character(1:5), paste0("n", 1:5))
w_obs[, tot := n1 + n2 + n3 + n4 + n5]
w_obs[, `:=`(ever_obs = (n2 + n3 + n4 + n5) / tot,
             phi_obs = (n2 + n3) / (n2 + n3 + n4 + n5),
             prev_obs = (n2 + n3) / tot)]

desc <- merge(w_obs[, .(anio, ge, ever_obs, phi_obs, prev_obs)],
              w_sim[, .(anio, ge, ever_sim, phi_sim, prev_sim = prevalencia_sim)],
              by = c("anio", "ge"))
desc[, `:=`(error_total = prev_sim - prev_obs,
            comp_ever = (ever_sim - ever_obs) * phi_sim,
            comp_split = ever_obs * (phi_sim - phi_obs))]
desc[, `:=`(pct_ever = 100 * comp_ever / error_total, pct_split = 100 * comp_split / error_total)]
setorder(desc, ge, anio)
fwrite(desc, file.path(out_dir, "86_descomposicion_ever_split.csv"))
log_msg("Descomposicion del error (pp de prevalencia):\n%s",
        paste(capture.output(print(desc[, .(anio, ge, error = round(100 * error_total, 2),
          por_ever = round(100 * comp_ever, 2), por_split = round(100 * comp_split, 2),
          pct_split = round(pct_split, 0), ever_obs = round(ever_obs, 3), ever_sim = round(ever_sim, 3),
          phi_obs = round(phi_obs, 3), phi_sim = round(phi_sim, 3))])), collapse = "\n"))

log_msg("\n=== 3) Flujos brutos del motor por grupo etario ===")

fl_ag <- flujos[evento != "stock_vivo", .(n = sum(n), expuestos = sum(expuestos)), by = .(anio, ge, evento)]
fl_ag[, tasa := n / expuestos]
stock_tot <- flujos[evento == "stock_vivo", .(pob = sum(n)), by = .(anio, ge)]
fl_ag <- merge(fl_ag, stock_tot, by = c("anio", "ge"))
fl_ag[, pp_de_la_poblacion := 100 * n / pob]
setorder(fl_ag, ge, evento, anio)
fwrite(fl_ag, file.path(out_dir, "86_flujos_brutos_motor.csv"))

log_msg("Tasas brutas anuales del motor (media 2011-2024), sobre sus propios expuestos:\n%s",
        paste(capture.output(print(dcast(fl_ag[evento %in% c("cesacion", "recaida", "iniciacion", "muerte")],
              ge ~ evento, value.var = "tasa", fun.aggregate = function(x) round(mean(x), 4)))), collapse = "\n"))
log_msg("Aporte bruto anual de cada flujo en PUNTOS PORCENTUALES de la poblacion del tramo (media):\n%s",
        paste(capture.output(print(dcast(fl_ag[evento %in% c("cesacion", "recaida", "iniciacion")],
              ge ~ evento, value.var = "pp_de_la_poblacion", fun.aggregate = function(x) round(mean(x), 3)))), collapse = "\n"))

bal <- dcast(fl_ag[evento %in% c("cesacion", "recaida", "iniciacion")], anio + ge + pob ~ evento, value.var = "n")
for (j in c("cesacion", "recaida", "iniciacion")) set(bal, which(is.na(bal[[j]])), j, 0)
bal[, `:=`(recaida_pp = 100 * recaida / pob, iniciacion_pp = 100 * iniciacion / pob,
           cesacion_pp = 100 * cesacion / pob)]
bal[, neto_pp := recaida_pp + iniciacion_pp - cesacion_pp]
setorder(bal, ge, anio)
fwrite(bal, file.path(out_dir, "86_balance_flujos_45mas.csv"))
log_msg("Balance neto anual del stock de fumadores (pp de la poblacion del tramo), 45-64 y 65+:\n%s",
        paste(capture.output(print(bal[ge %in% c("45-64", "65+"),
          .(anio, ge, recaida_pp = round(recaida_pp, 3), iniciacion_pp = round(iniciacion_pp, 3),
            cesacion_pp = round(cesacion_pp, 3), neto_pp = round(neto_pp, 3))])), collapse = "\n"))

eq <- dcast(fl_ag[evento %in% c("cesacion", "recaida"), .(tasa = mean(tasa)), by = .(ge, evento)],
            ge ~ evento, value.var = "tasa")
eq[, phi_equilibrio_motor := recaida / (recaida + cesacion)]
eq <- merge(eq, desc[anio == 2025, .(ge, phi_sim_2025 = phi_sim, phi_obs_2025 = phi_obs)], by = "ge")
fwrite(eq, file.path(out_dir, "86_equilibrio_flujo.csv"))
log_msg("Equilibrio de flujo del motor phi* = r/(q+r), contra lo simulado y lo observado en 2025:\n%s",
        paste(capture.output(print(eq[, .(ge, q_motor = round(cesacion, 4), r_motor = round(recaida, 4),
              phi_eq_motor = round(phi_equilibrio_motor, 3), phi_sim_2025 = round(phi_sim_2025, 3),
              phi_obs_2025 = round(phi_obs_2025, 3))])), collapse = "\n"))

log_msg("\n=== 4) Identificacion de la recaida con la ECH (balance de flujos por cohorte) ===")

sup <- fread(file.path(out_dir, "42_supervivencia_por_estado_agregada.csv"))
sup <- sup[anio %in% ANIO_INI:ANIO_FIN & edad >= 40 & edad <= 99 & estado %in% 2:5]
sup_wide <- dcast(sup, edad + sexo + anio ~ estado, value.var = "prob_sobrevive")
setnames(sup_wide, as.character(2:5), paste0("s", 2:5))
peso_int <- P[edad >= 45, .(w = sum(peso)), by = estado]
gw <- function(e) peso_int[estado == e]$w
p_oc <- gw(2) / (gw(2) + gw(3)); p_rec <- gw(4) / (gw(4) + gw(5))
sup_wide[, `:=`(sN = p_oc * s2 + (1 - p_oc) * s3, sX = p_rec * s4 + (1 - p_rec) * s5)]
setkey(sup_wide, edad, sexo, anio)
log_msg("Composicion interna para agregar supervivencia en 45+: ocasional %.3f de los fumadores, reciente %.3f de los ex.", p_oc, p_rec)
log_msg("Supervivencia relativa fumador/ex en 45+ (mediana sN/sX): %.5f",
        sup_wide[edad >= 45, median(sN / sX)])

P[, edad5 := pmin(95L, 5L * (edad %/% 5L))]
q_ech <- P[, .(oc = sum(peso * (estado == 2)), di = sum(peso * (estado == 3)), er = sum(peso * (estado == 4))),
           by = .(anio, edad5, sexo)]
q_ech[, q_obs := er / (oc + di + er)]
grid_q <- CJ(anio = ANIO_INI:ANIO_FIN, edad5 = sort(unique(q_ech$edad5)), sexo = c("varon", "mujer"))
q_full <- merge(grid_q, q_ech[, .(anio, edad5, sexo, q_obs)], by = c("anio", "edad5", "sexo"), all.x = TRUE)
setorder(q_full, edad5, sexo, anio)
q_full[, q := {
  ok <- !is.na(q_obs)
  if (sum(ok) >= 2) approx(anio[ok], q_obs[ok], xout = anio, rule = 2)$y
  else if (sum(ok) == 1) rep(q_obs[ok], .N) else rep(NA_real_, .N)
}, by = .(edad5, sexo)]
setkey(q_full, anio, edad5, sexo)

P[, coh5 := 5L * (anio_nac %/% 5L)]
coh <- P[, .(n_cur = sum(peso * (estado %in% c(2, 3))), n_ex = sum(peso * (estado %in% c(4, 5))),
             n_nun = sum(peso * (estado == 1)), edad_media = weighted.mean(edad, peso), n_obs = .N),
         by = .(coh5, sexo, anio)]
coh <- coh[edad_media >= 45 & (n_cur + n_ex) > 0 & n_obs >= 100]
setorder(coh, coh5, sexo, anio)
coh[, `:=`(anio_sig = shift(anio, type = "lead"), n_cur_sig = shift(n_cur, type = "lead"),
           n_ex_sig = shift(n_ex, type = "lead")), by = .(coh5, sexo)]
coh <- coh[!is.na(anio_sig)]
coh[, `:=`(delta = anio_sig - anio, phi = n_cur / (n_cur + n_ex),
           phi_sig = n_cur_sig / (n_cur_sig + n_ex_sig))]

proyectar_phi <- function(phi0, r, qs, sNs, sXs) {
  n <- phi0; x <- 1 - phi0
  for (k in seq_along(qs)) {
    n2 <- sNs[k] * (n * (1 - qs[k]) + x * r)
    x2 <- sXs[k] * (x * (1 - r) + n * qs[k])
    n <- n2; x <- x2
  }
  n / (n + x)
}

r_implicito <- function(i) {
  f_anio <- coh$anio[i]; d <- coh$delta[i]; sx <- coh$sexo[i]; e0 <- coh$edad_media[i]
  anios <- f_anio + seq(0, d - 1)
  edades <- e0 + seq(0, d - 1)
  qs <- q_full[.(anios, pmin(95L, 5L * (as.integer(edades) %/% 5L)), rep(sx, d)), q]
  sw <- sup_wide[.(pmin(99L, as.integer(edades)), rep(sx, d), anios)]
  sNs <- sw$sN; sXs <- sw$sX
  if (any(is.na(qs)) || any(is.na(sNs)) || any(is.na(sXs))) return(NA_real_)
  f <- function(r) proyectar_phi(coh$phi[i], r, qs, sNs, sXs) - coh$phi_sig[i]
  lo <- f(-0.30); hi <- f(0.50)
  if (is.na(lo) || is.na(hi)) return(NA_real_)
  if (lo > 0) return(-0.30)
  if (hi < 0) return(0.50)
  uniroot(f, c(-0.30, 0.50), tol = 1e-9)$root
}

coh[, r_imp := sapply(seq_len(.N), r_implicito)]
coh[, r_imp_cens := pmax(r_imp, 0)]
coh[, peso_celda := n_cur + n_ex]
coh[, tramo := ifelse(edad_media < 65, "45-64", "65+")]
setorder(coh, coh5, sexo, anio)
fwrite(coh, file.path(out_dir, "86_recaida_implicita_ech.csv"))

r_res <- coh[!is.na(r_imp), .(n_celdas = .N,
                              r_mediana = median(r_imp), r_media_pond = weighted.mean(r_imp, peso_celda),
                              r_p25 = quantile(r_imp, 0.25), r_p75 = quantile(r_imp, 0.75),
                              r_cens_mediana = median(r_imp_cens), r_cens_media_pond = weighted.mean(r_imp_cens, peso_celda)),
             by = tramo]
r_glob <- coh[!is.na(r_imp), .(tramo = "45+", n_celdas = .N, r_mediana = median(r_imp),
                               r_media_pond = weighted.mean(r_imp, peso_celda),
                               r_p25 = quantile(r_imp, 0.25), r_p75 = quantile(r_imp, 0.75),
                               r_cens_mediana = median(r_imp_cens),
                               r_cens_media_pond = weighted.mean(r_imp_cens, peso_celda))]
r_res <- rbindlist(list(r_res, r_glob), use.names = TRUE)
fwrite(r_res, file.path(out_dir, "86_recaida_implicita_resumen.csv"))
log_msg("Tasa de recaida anual implicita en la ECH para cohortes de 45+:\n%s",
        paste(capture.output(print(r_res[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 4) else x)])), collapse = "\n"))

coh[, q_ref := q_full[.(coh$anio, pmin(95L, 5L * (as.integer(coh$edad_media) %/% 5L)), coh$sexo), q]]
coh[, cota_eq := q_ref * phi / (1 - phi)]
log_msg("Cota de equilibrio r <= q*phi/(1-phi), con la cesacion observada de la ECH:\n%s",
        paste(capture.output(print(coh[!is.na(cota_eq), .(n = .N, cota_mediana = round(median(cota_eq), 4),
              q_mediano = round(median(q_ref, na.rm = TRUE), 4), phi_mediano = round(median(phi), 3)),
              by = tramo])), collapse = "\n"))

R_ECH <- r_res[tramo == "45+"]$r_cens_media_pond
log_msg("Valor de referencia adoptado para la seccion 6 (media ponderada censurada en 0, la MAS conservadora): %.4f", R_ECH)

log_msg("\n=== 5) Cota superior del efecto de recomponer anios_desde_dejar ===")

m_rec <- readRDS(file.path(int_dir, "modelo_recaida.rds"))
grilla <- data.table(punto_medio = c(0.5, 1, 2, 5, 10, 20, 30, 40, 50, 70))
grilla[, haz_varon := predict(m_rec, newdata = data.frame(punto_medio = punto_medio, sexo = "varon"), type = "response")]
grilla[, haz_mujer := predict(m_rec, newdata = data.frame(punto_medio = punto_medio, sexo = "mujer"), type = "response")]
fwrite(grilla, file.path(out_dir, "86_hazard_recaida_por_duracion.csv"))
log_msg("Hazard de recaida del motor por duracion (el objeto sobre el que actuaria cualquier recomposicion):\n%s",
        paste(capture.output(print(grilla[, .(punto_medio, varon = round(haz_varon, 4), mujer = round(haz_mujer, 4))])), collapse = "\n"))

dur_sim <- base_run$dur
dur_sim[, haz := predict(m_rec, newdata = data.frame(punto_medio = pmax(dur, 0.05), sexo = "varon"), type = "response")]
h_de <- function(d) predict(m_rec, newdata = data.frame(punto_medio = d, sexo = "varon"), type = "response")
sens <- dur_sim[, .(n = .N, dur_mediana = median(dur), dur_max = max(dur),
                    haz_medio_actual = mean(haz), haz_todos_dur_1 = h_de(1), haz_todos_dur_max = h_de(max(dur))),
                by = .(anio, ge)]
sens[, rango_alcanzable_pp := 100 * (haz_todos_dur_1 - haz_todos_dur_max)]
setorder(sens, ge, anio)
fwrite(sens, file.path(out_dir, "86_sensibilidad_composicion_duracion.csv"))
log_msg("Rango COMPLETO alcanzable moviendo toda la masa de la distribucion de duracion, en 45+:\n%s",
        paste(capture.output(print(sens[, .(anio, ge, dur_mediana = round(dur_mediana, 1), dur_max = round(dur_max, 0),
              haz_actual = round(haz_medio_actual, 4), haz_min_posible = round(haz_todos_dur_max, 4),
              haz_max_posible = round(haz_todos_dur_1, 4), rango_pp = round(rango_alcanzable_pp, 2))])), collapse = "\n"))
log_msg("Piso absoluto del hazard alcanzable por recomposicion: %.4f. Implicito de la ECH: %.4f. %s",
        min(sens$haz_todos_dur_max), R_ECH,
        ifelse(R_ECH < min(sens$haz_todos_dur_max),
               "EL IMPLICITO DE LA ECH ESTA POR DEBAJO DEL PISO: ninguna recomposicion de la duracion puede llegar.",
               "el implicito de la ECH es alcanzable por recomposicion."))

log_msg("\n=== 6) Corridas de confirmacion con el hazard de recaida corregido ===")

dur0 <- base_run$dur[anio == ANIO_INI]
haz_medio_actual <- mean(predict(m_rec, newdata = data.frame(punto_medio = pmax(dur0$dur, 0.05), sexo = "varon"), type = "response"))
buscar_delta <- function(objetivo) {
  f <- function(d) {
    m2 <- m_rec; m2$coefficients <- coef(m_rec) + c(d, 0, 0)
    mean(predict(m2, newdata = data.frame(punto_medio = pmax(dur0$dur, 0.05), sexo = "varon"), type = "response")) - objetivo
  }
  uniroot(f, c(-8, 4), tol = 1e-11)$root
}
delta_A <- buscar_delta(R_ECH)
m_A <- m_rec; m_A$coefficients <- coef(m_rec) + c(delta_A, 0, 0)
log_msg("Variante A (nivel_ech): desplazamiento del intercepto %.5f. Hazard medio en 45+ 2011: %.4f -> %.4f",
        delta_A, haz_medio_actual, R_ECH)

sup_gats <- fread(file.path(out_dir, "39_recaida_supervivencia_duracion.csv"))
cortos <- sup_gats[sexo != "todos" & bin_duracion %in% c("<1", "1-2")]
cortos[, punto_medio := fifelse(bin_duracion == "<1", 0.5, 1.5)]
m_B <- suppressWarnings(glm(hazard_anual ~ log(punto_medio) + sexo, family = quasibinomial(),
                            data = cortos, weights = n_pond))
haz_B <- mean(predict(m_B, newdata = data.frame(punto_medio = pmax(dur0$dur, 0.05), sexo = "varon"), type = "response"))
log_msg("Variante B (pendiente_corta): coeficientes %s (vigente: %s)",
        paste(sprintf("%.4f", coef(m_B)), collapse = " / "), paste(sprintf("%.4f", coef(m_rec)), collapse = " / "))
log_msg("Variante B: hazard medio en 45+ 2011 = %.4f (vigente %.4f, implicito ECH %.4f)", haz_B, haz_medio_actual, R_ECH)
grilla[, haz_varon_B := predict(m_B, newdata = data.frame(punto_medio = punto_medio, sexo = "varon"), type = "response")]
grilla[, haz_varon_A := predict(m_A, newdata = data.frame(punto_medio = punto_medio, sexo = "varon"), type = "response")]
fwrite(grilla, file.path(out_dir, "86_hazard_recaida_por_duracion.csv"))

run_A <- correr_backcast(modelo_recaida_override = m_A, etiqueta = "nivel_ech")
run_B <- correr_backcast(modelo_recaida_override = m_B, etiqueta = "pendiente_corta")

resumir <- function(comp, etiqueta) {
  w <- anchar(comp)
  w[anio %in% ANIOS_VALIDACION, .(variante = etiqueta, anio, ge, prev_sim = prevalencia_sim, ever_sim, phi_sim)]
}
cf <- rbindlist(list(resumir(comp_sim, "vigente"), resumir(run_A$comp, "nivel_ech"),
                     resumir(run_B$comp, "pendiente_corta")))
cf <- merge(cf, w_obs[, .(anio, ge, prev_obs, ever_obs, phi_obs)], by = c("anio", "ge"))
cf[, error_pp := 100 * (prev_sim - prev_obs)]
setorder(cf, ge, variante, anio)
fwrite(cf, file.path(out_dir, "86_contrafactico_recaida.csv"))

log_msg("Error de prevalencia (pp) por variante, grupo etario y anio:\n%s",
        paste(capture.output(print(dcast(cf, ge + anio ~ variante, value.var = "error_pp",
              fun.aggregate = function(x) round(mean(x), 2)))), collapse = "\n"))
log_msg("Error absoluto medio de los nueve anios, por variante:\n%s",
        paste(capture.output(print(dcast(cf[, .(eam = mean(abs(error_pp))), by = .(ge, variante)],
              ge ~ variante, value.var = "eam", fun.aggregate = function(x) round(x, 2)))), collapse = "\n"))
log_msg("phi y ever simulados contra observados en 2025:\n%s",
        paste(capture.output(print(cf[anio == 2025, .(ge, variante, phi_sim = round(phi_sim, 3),
              phi_obs = round(phi_obs, 3), ever_sim = round(ever_sim, 3), ever_obs = round(ever_obs, 3))])), collapse = "\n"))

brk <- tryCatch(fread(file.path(out_dir, "67_bracket_grupo_etario.csv")), error = function(e) NULL)
if (!is.null(brk)) {
  log_msg("Columnas de 67_bracket_grupo_etario.csv: %s", paste(names(brk), collapse = ", "))
  log_msg("Contenido (primeras filas):\n%s", paste(capture.output(print(head(brk, 8))), collapse = "\n"))
}

agg <- rbindlist(list(
  comp_sim[, .(variante = "vigente", prev = sum(N * (estado %in% c(2, 3))) / sum(N)), by = anio],
  run_A$comp[, .(variante = "nivel_ech", prev = sum(N * (estado %in% c(2, 3))) / sum(N)), by = anio],
  run_B$comp[, .(variante = "pendiente_corta", prev = sum(N * (estado %in% c(2, 3))) / sum(N)), by = anio]))
obs_nac <- P[anio %in% ANIOS_VALIDACION, .(prev_obs = sum(peso * (estado %in% c(2, 3))) / sum(peso)), by = anio]
agg <- merge(agg, obs_nac, by = "anio")
fwrite(agg, file.path(out_dir, "86_agregado_nacional_variantes.csv"))
log_msg("Prevalencia nacional agregada, simulada por variante contra observada:\n%s",
        paste(capture.output(print(dcast(agg, anio + prev_obs ~ variante, value.var = "prev",
              fun.aggregate = function(x) round(mean(x), 4)))), collapse = "\n"))

log_msg("\n=== outputs: 86_descomposicion_ever_split.csv, 86_flujos_brutos_motor.csv, 86_balance_flujos_45mas.csv, 86_equilibrio_flujo.csv, 86_recaida_implicita_ech.csv, 86_recaida_implicita_resumen.csv, 86_hazard_recaida_por_duracion.csv, 86_sensibilidad_composicion_duracion.csv, 86_contrafactico_recaida.csv, 86_agregado_nacional_variantes.csv ===")
close(log_con)
