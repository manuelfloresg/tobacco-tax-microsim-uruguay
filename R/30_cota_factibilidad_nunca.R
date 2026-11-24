# 30_cota_factibilidad_nunca.R
#
# Feasibility bound on the rise of the never-smoked share within closed birth cohorts: only
# differential mortality can raise it, so the observed rise is compared with the maximum mortality
# can produce.
# Input:  panel from R/04, output/tables/36_*, 42_*, 43_*, 46_*
# Output: output/tables/52_*.csv

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/30_cota_factibilidad_nunca.R")

log_con <- file(here::here("logs", "30_cota_factibilidad_nunca.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 30_cota_factibilidad_nunca.R -- %s ===", format(Sys.time()))
log_msg("here::here() = %s", here::here())

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

source(here::here("R", "24_motor_transiciones.R"))

ANIO_BASE <- 2011L
ANIOS_VALIDACION <- c(2014L, 2017L, 2018L, 2019L, 2021L, 2022L, 2023L, 2024L, 2025L)
ANIOS_SERIE <- c(ANIO_BASE, ANIOS_VALIDACION)

cohorte_5 <- function(anio_nac) {
  pmin(pmax(floor(anio_nac / 5) * 5, 1925), 2005)
}

grupo_etario_f <- function(edad) fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")

log_msg("\n--- 1) Backcast estandar (mismos parametros calibrados y seed que R/28) ---")

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor,
            params_cal[parametro == "raw_par3"]$valor)
params_escalada <- params[1:2]
param_downshift <- params[3]
log_msg("Parametros calibrados: escalada_15=%.4f, decaimiento=%.4f, downshift=%.6f",
        plogis(params[1]), exp(params[2]), plogis(params[3]))

set.seed(20260809)
pop0 <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"))
pop0[, sexo := factor(sexo, levels = c("varon", "mujer"))]
pop0[, tercil := factor(tercil, levels = c(1, 2, 3))]
log_msg("Poblacion inicial 2011: %d individuos", nrow(pop0))

pop <- copy(pop0)
insumos <- cargar_insumos_motor()

snapshots <- list(`2011` = copy(pop0)[, anio_snapshot := ANIO_BASE])
anio_actual <- ANIO_BASE
t0 <- Sys.time()
for (anio_obj in ANIOS_VALIDACION) {
  while (anio_actual < anio_obj) {
    pop <- ciclo_anual(pop, anio_actual, insumos$modelos, insumos$tabla_superv, insumos$precio_lookup,
                        insumos$pob_ine, insumos$entrada_15_por_anio, params_escalada, param_downshift)
    anio_actual <- anio_actual + 1
  }
  snapshots[[as.character(anio_obj)]] <- copy(pop)[, anio_snapshot := anio_obj]
  log_msg("Snapshot %d: %d individuos", anio_obj, nrow(pop))
}
log_msg("Backcast terminado en %.1f segundos", as.numeric(difftime(Sys.time(), t0, units = "secs")))

for (nm in names(snapshots)) {
  snapshots[[nm]][, cohorte := cohorte_5(anio_snapshot - edad)]
  snapshots[[nm]][, grupo_etario := grupo_etario_f(edad)]
}

log_msg("\n--- 2) Serie observada (panel ECH) por cohorte x anio, incluyendo 2011 ---")

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))
P[, grupo_etario := grupo_etario_f(edad)]
log_msg("Panel ECH: %d filas, anios %s", nrow(P), paste(sort(unique(P$anio)), collapse = ", "))
stopifnot(all(ANIOS_SERIE %in% unique(P$anio)))

criterio_limpias <- fread(file.path(out_dir, "36_criterio_cohortes_limpias.csv"))[, .(cohorte, cohorte_limpia)]
COHORTES_CONTAMINADAS <- criterio_limpias[cohorte_limpia == FALSE]$cohorte
COHORTES_INFORMATIVAS <- c(1950, 1955, 1960, 1965, 1970, 1975, 1980, 1985)
log_msg("Cohortes contaminadas (%d): %s", length(COHORTES_CONTAMINADAS), paste(COHORTES_CONTAMINADAS, collapse = ", "))

obs_cohorte <- P[anio %in% ANIOS_SERIE, .(
  nunca_obs   = sum(peso * (estado == 1)) / sum(peso),
  prev_obs    = sum(peso * (estado %in% c(2, 3))) / sum(peso),
  diario_obs  = sum(peso * (estado == 3)) / sum(peso),
  n_obs       = .N
), by = .(anio, cohorte)]

log_msg("\n--- 3) Cota de factibilidad deterministica ---")

arch_superv <- file.path(out_dir, "42_supervivencia_por_estado_agregada.csv")
if (!file.exists(arch_superv)) arch_superv <- file.path(out_dir, "42_supervivencia_por_estado.csv")
superv <- fread(arch_superv)
superv[, sexo := as.character(sexo)]

base_cota <- pop0[, .(n = .N), by = .(edad, sexo, estado)]
base_cota[, sexo := as.character(sexo)]
base_cota[, cohorte := cohorte_5(ANIO_BASE - edad)]
log_msg("Celdas (edad x sexo x estado) de la poblacion inicial: %d, suma n = %d", nrow(base_cota), sum(base_cota$n))

propagar_cota <- function(base, tabla_prob, etiqueta) {
  d <- copy(base)
  d[, edad_t := edad]
  d[, w := as.numeric(n)]
  res <- list()
  res[[as.character(ANIO_BASE)]] <- d[, .(anio = ANIO_BASE, cohorte, estado, w)]
  for (a in ANIO_BASE:(max(ANIOS_SERIE) - 1L)) {
    s <- tabla_prob[anio == a, .(edad_lookup = edad, sexo, estado, p)]
    d[, edad_lookup := pmin(edad_t, 99L)]
    d <- merge(d, s, by = c("edad_lookup", "sexo", "estado"), all.x = TRUE, sort = FALSE)
    n_na <- sum(is.na(d$p))
    if (n_na > 0) {
      peso_na <- sum(d$w[is.na(d$p)]) / sum(d$w)
      edades_na <- sort(unique(d$edad_lookup[is.na(d$p)]))
      log_msg("  [%s] anio %d: %d celdas sin prob de supervivencia (%.4f%% del peso; edades %s) -- se tratan como p=1",
              etiqueta, a, n_na, 100 * peso_na,
              paste(range(edades_na), collapse = "-"))
    }
    d[is.na(p), p := 1]
    d[, w := w * p]
    d[, c("p", "edad_lookup") := NULL]
    d[, edad_t := edad_t + 1L]
    res[[as.character(a + 1L)]] <- d[, .(anio = a + 1L, cohorte, estado, w)]
  }
  todo <- rbindlist(res)
  todo[anio %in% ANIOS_SERIE, .(
    share_nunca = sum(w[estado == 1]) / sum(w),
    superv_cohorte = sum(w)
  ), by = .(anio, cohorte)]
}

prob_motor <- superv[, .(edad, sexo, anio, estado, p = prob_sobrevive)]
cota_motor <- propagar_cota(base_cota, prob_motor, "cota_motor")
setnames(cota_motor, c("share_nunca", "superv_cohorte"), c("cota_motor", "superv_cohorte"))

composicion <- P[, .(N_pond = sum(peso)), by = .(edad, sexo, estado)]
composicion[, share := N_pond / sum(N_pond), by = .(edad, sexo)]
log_msg("Composicion de estados para el reescalado TwIST: panel ECH pooled 2011-2025 (misma logica que R/23)")

cota_RR_k <- function(k) {
  rr <- unique(superv[, .(edad, sexo, estado, RR)])
  rr[, RR_k := fifelse(estado == 1L, RR, RR * k)]
  cmp <- merge(composicion[, .(edad, sexo, estado, share)], rr, by = c("edad", "sexo", "estado"))
  fac <- cmp[, .(suma_share_RR = sum(share * RR_k)), by = .(edad, sexo)]
  tb <- merge(superv[, .(edad, sexo, anio, estado, mortalidad_basal)], rr[, .(edad, sexo, estado, RR_k)],
              by = c("edad", "sexo", "estado"))
  tb <- merge(tb, fac, by = c("edad", "sexo"))
  tb[, p := 1 - pmin(1, RR_k * mortalidad_basal / suma_share_RR)]
  propagar_cota(base_cota, tb[, .(edad, sexo, anio, estado, p)], sprintf("cota_RR_x%.1f", k))
}
cotas_rr <- rbindlist(lapply(c(1.5, 2, 3), function(k) {
  z <- cota_RR_k(k)[, .(anio, cohorte, valor = share_nunca)]
  z[, k := k][]
}))
cotas_rr_w <- dcast(cotas_rr, anio + cohorte ~ sprintf("cota_RR_x%s", k), value.var = "valor")

prob_basal <- unique(superv[, .(edad, sexo, anio, mortalidad_basal)])
prob_basal <- prob_basal[, .(edad, sexo, anio, p = 1 - mortalidad_basal)]
prob_basal <- rbindlist(lapply(1:5, function(e) copy(prob_basal)[, estado := e]))
superv_basal <- propagar_cota(base_cota, prob_basal[, .(edad, sexo, anio, estado, p)], "basal")
superv_basal <- superv_basal[, .(anio, cohorte, S_cohorte = superv_cohorte)]
superv_basal[, S_cohorte := S_cohorte / S_cohorte[anio == ANIO_BASE], by = cohorte]

log_msg("\n--- 4) Trayectoria por cohorte x anio ---")

sim_cohorte <- rbindlist(lapply(snapshots, function(dt) {
  dt[, .(nunca_sim = mean(estado == 1), prev_sim = mean(estado %in% c(2, 3)),
         diario_sim = mean(estado == 3), n_sim = .N), by = .(anio = anio_snapshot, cohorte)]
}))

tray <- merge(obs_cohorte, sim_cohorte, by = c("anio", "cohorte"))
tray <- merge(tray, cota_motor[, .(anio, cohorte, cota_motor)], by = c("anio", "cohorte"), all.x = TRUE)
tray <- merge(tray, cotas_rr_w, by = c("anio", "cohorte"), all.x = TRUE)
tray <- merge(tray, superv_basal, by = c("anio", "cohorte"), all.x = TRUE)
tray <- merge(tray, criterio_limpias, by = "cohorte", all.x = TRUE)

tray[, nunca_obs_2011 := nunca_obs[anio == ANIO_BASE][1], by = cohorte]
tray[, cota_absoluta := pmin(1, nunca_obs_2011 / S_cohorte)]

tray[, brecha := nunca_sim - nunca_obs]
tray[, exceso_infactible := pmax(0, nunca_obs - cota_absoluta)]
tray[, nunca_obs_rep := pmin(nunca_obs, cota_absoluta)]
tray[, brecha_rep := nunca_sim - nunca_obs_rep]
tray[, factor_rep := fifelse(nunca_obs < 1, 1 + exceso_infactible / (1 - nunca_obs), 1)]
tray[, prev_obs_rep := pmin(1, prev_obs * factor_rep)]
tray[, brecha_prev := prev_sim - prev_obs]
tray[, brecha_prev_rep := prev_sim - prev_obs_rep]

for (v in c("nunca_obs", "nunca_sim", "cota_motor", "cota_RR_x1.5", "cota_RR_x2", "cota_RR_x3", "cota_absoluta")) {
  tray[, (paste0("d_", v)) := get(v) - get(v)[anio == ANIO_BASE][1], by = cohorte]
}

setcolorder(tray, c("cohorte", "anio", "cohorte_limpia", "nunca_obs", "nunca_sim", "brecha",
                    "cota_motor", "cota_RR_x1.5", "cota_RR_x2", "cota_RR_x3", "cota_absoluta",
                    "exceso_infactible", "nunca_obs_rep", "brecha_rep",
                    "prev_obs", "prev_sim", "prev_obs_rep", "brecha_prev", "brecha_prev_rep"))
setorder(tray, cohorte, anio)
fwrite(tray, file.path(out_dir, "52_trayectoria_nunca_cohorte.csv"))
log_msg("Escrito 52_trayectoria_nunca_cohorte.csv (%d filas)", nrow(tray))

log_msg("\n--- 5) Tabla central por cohorte ---")

pend <- function(y, x) {
  ok <- !is.na(y) & !is.na(x)
  if (sum(ok) < 2 || length(unique(x[ok])) < 2) return(NA_real_)
  unname(coef(lm(y[ok] ~ x[ok]))[2])
}

resumen <- tray[, {
  i0 <- which(anio == ANIO_BASE); i1 <- which(anio == max(ANIOS_VALIDACION))
  .(edad_2011 = ANIO_BASE - cohorte - 2L,
    edad_2025 = max(ANIOS_VALIDACION) - cohorte - 2L,
    nunca_obs_2011 = nunca_obs[i0], nunca_obs_2025 = nunca_obs[i1],
    delta_obs      = nunca_obs[i1] - nunca_obs[i0],
    delta_sim      = nunca_sim[i1] - nunca_sim[i0],
    cota_motor_d   = cota_motor[i1] - cota_motor[i0],
    cota_RR_x2_d   = get("cota_RR_x2")[i1] - get("cota_RR_x2")[i0],
    cota_RR_x3_d   = get("cota_RR_x3")[i1] - get("cota_RR_x3")[i0],
    cota_absoluta_d = cota_absoluta[i1] - cota_absoluta[i0],
    superv_2025    = S_cohorte[i1],
    exceso_infactible_2025 = exceso_infactible[i1],
    brecha_2011    = brecha[i0], brecha_2025 = brecha[i1],
    crecimiento_brecha = brecha[i1] - brecha[i0],
    brecha_rep_2025 = brecha_rep[i1],
    pend_obs = pend(nunca_obs, anio), pend_sim = pend(nunca_sim, anio),
    pend_cota_absoluta = pend(cota_absoluta, anio))
}, by = .(cohorte, cohorte_limpia)]

resumen[, informativa := cohorte %in% COHORTES_INFORMATIVAS]
resumen[, frac_infactible := fifelse(abs(crecimiento_brecha) > 1e-10,
                                      exceso_infactible_2025 / abs(crecimiento_brecha), NA_real_)]
setorder(resumen, cohorte)
fwrite(resumen, file.path(out_dir, "52_cota_factibilidad_nunca.csv"))
log_msg("Escrito 52_cota_factibilidad_nunca.csv (%d filas)", nrow(resumen))
log_msg("Tabla central (cohortes contaminadas informativas):\n%s",
        paste(capture.output(print(resumen[cohorte %in% COHORTES_INFORMATIVAS & cohorte_limpia == FALSE,
              .(cohorte, edad_2011, edad_2025, delta_obs, delta_sim, cota_motor_d, cota_RR_x3_d,
                cota_absoluta_d, exceso_infactible_2025, crecimiento_brecha, frac_infactible)])), collapse = "\n"))

log_msg("\n--- 6) Reexpresion por grupo etario ---")

pesos_ge <- P[anio %in% ANIOS_SERIE, .(w_ge = sum(peso)), by = .(anio, grupo_etario, cohorte)]
pesos_ge[, w_rel := w_ge / sum(w_ge), by = .(anio, grupo_etario)]

reexp <- merge(pesos_ge, tray[, .(anio, cohorte, exceso_infactible, prev_obs, prev_obs_rep, cohorte_limpia)],
               by = c("anio", "cohorte"))
reexp <- reexp[, .(
  exceso_infactible_nunca = sum(w_rel * exceso_infactible),
  prev_obs_ge     = sum(w_rel * prev_obs),
  prev_obs_rep_ge = sum(w_rel * prev_obs_rep),
  share_contaminada = sum(w_rel * (cohorte_limpia == FALSE))
), by = .(anio, grupo_etario)]

prev_sim_ge <- rbindlist(lapply(snapshots, function(dt) {
  dt[, .(prev_sim_ge = mean(estado %in% c(2, 3)), nunca_sim_ge = mean(estado == 1)),
     by = .(anio = anio_snapshot, grupo_etario)]
}))
reexp <- merge(reexp, prev_sim_ge, by = c("anio", "grupo_etario"))
reexp[, error_total := prev_sim_ge - prev_obs_ge]
reexp[, error_total_rep := prev_sim_ge - prev_obs_rep_ge]
reexp[, explicado_por_cota := error_total - error_total_rep]
setorder(reexp, grupo_etario, anio)
fwrite(reexp, file.path(out_dir, "52_reexpresion_grupo_etario.csv"))
log_msg("Escrito 52_reexpresion_grupo_etario.csv (%d filas)", nrow(reexp))
log_msg("Reexpresion 2025 por grupo etario:\n%s",
        paste(capture.output(print(reexp[anio == 2025])), collapse = "\n"))

edad_max_limpia <- P[anio %in% ANIOS_SERIE & cohorte %in% criterio_limpias[cohorte_limpia == TRUE]$cohorte,
                     .(edad_max = max(edad)), by = anio]
log_msg("Edad MAXIMA alcanzada por una cohorte limpia en cada anio de la ventana:\n%s",
        paste(capture.output(print(edad_max_limpia)), collapse = "\n"))
log_msg("Si esa edad no llega a 45, los grupos 45-64 y 65+ son 100%% cohortes contaminadas y un")
log_msg("defecto etario del motor NO esta identificado por separado de la deriva de cohorte.")

log_msg("\n--- 7) Cinco estados en cohortes contaminadas, con el anio base 2011 ---")

obs_est <- P[anio %in% ANIOS_SERIE & cohorte %in% COHORTES_CONTAMINADAS,
             .(N_pond = sum(peso)), by = .(anio, estado)]
obs_est[, share_obs := N_pond / sum(N_pond), by = anio][, N_pond := NULL]
sim_est <- rbindlist(lapply(snapshots, function(dt) {
  dt[cohorte %in% COHORTES_CONTAMINADAS, .N, by = .(anio = anio_snapshot, estado)][
    , share_sim := N / sum(N), by = anio][, N := NULL]
}))
estados_cont <- merge(obs_est, sim_est, by = c("anio", "estado"))
estados_cont[, diferencia := share_sim - share_obs]
setorder(estados_cont, estado, anio)
fwrite(estados_cont, file.path(out_dir, "52_estados_cohorte_con_2011.csv"))
log_msg("Escrito 52_estados_cohorte_con_2011.csv:\n%s",
        paste(capture.output(print(dcast(estados_cont, anio ~ estado, value.var = "diferencia"))), collapse = "\n"))

log_msg("\n--- 8) Hipotesis 22.4: composicion de tercil en cohortes contaminadas ---")

terc <- rbindlist(lapply(snapshots, function(dt) {
  dt[cohorte %in% COHORTES_CONTAMINADAS, .N, by = .(anio = anio_snapshot, tercil)][
    , share := N / sum(N), by = anio][, N := NULL]
}))
terc_w <- dcast(terc, anio ~ tercil, value.var = "share")
log_msg("Share de tercil en cohortes contaminadas simuladas, por anio:\n%s",
        paste(capture.output(print(terc_w)), collapse = "\n"))
rango_terc <- terc[, .(rango_pp = 100 * (max(share) - min(share))), by = tercil]
log_msg("Rango maximo de variacion por tercil (pp): %s",
        paste(sprintf("T%s=%.3f", rango_terc$tercil, rango_terc$rango_pp), collapse = ", "))
log_msg("Variacion != 0 solo por mortalidad diferencial entre terciles, no por imputacion aleatoria.")

log_msg("\n--- 9) Chequeos internos ---")

resumen[, chk_identidad := crecimiento_brecha - (delta_sim - delta_obs)]
log_msg("9.1 Identidad crecimiento_brecha == delta_sim - delta_obs: max |desvio| = %.2e (%d cohortes sin dato: %s)",
        max(abs(resumen$chk_identidad), na.rm = TRUE), sum(is.na(resumen$chk_identidad)),
        paste(resumen[is.na(chk_identidad)]$cohorte, collapse = ", "))

crit <- fread(file.path(out_dir, "36_criterio_cohortes_limpias.csv"))[, .(cohorte, delta_nunca)]
chk2 <- merge(resumen[, .(cohorte, delta_obs)], crit, by = "cohorte")
chk2[, desvio_pp := 100 * (delta_obs - delta_nunca)]
log_msg("9.2 delta_obs vs delta_nunca de R/16 (mismo objeto por otro camino), max |desvio| = %.2e pp:\n%s",
        max(abs(chk2$desvio_pp), na.rm = TRUE), paste(capture.output(print(chk2)), collapse = "\n"))

chk3 <- tray[!is.na(d_cota_absoluta), .(ok = all(d_cota_motor <= get("d_cota_RR_x1.5") + 1e-9) &
                       all(get("d_cota_RR_x1.5") <= get("d_cota_RR_x2") + 1e-9) &
                       all(get("d_cota_RR_x2") <= get("d_cota_RR_x3") + 1e-9) &
                       all(get("d_cota_RR_x3") <= d_cota_absoluta + 1e-9)), by = cohorte]
log_msg("9.3 Monotonia d_cota_motor <= RRx1.5 <= RRx2 <= RRx3 <= d_cota_absoluta: %d de %d cohortes OK",
        sum(chk3$ok, na.rm = TRUE), nrow(chk3))
if (any(!chk3$ok, na.rm = TRUE)) log_msg("    cohortes que fallan: %s", paste(chk3[ok == FALSE]$cohorte, collapse = ", "))

ref <- fread(file.path(out_dir, "46_validacion_por_estado_cohorte.csv"))[cohorte_limpia == FALSE]
chk4 <- merge(estados_cont[anio %in% ANIOS_VALIDACION], ref[, .(anio, estado, share_sim_ref = share_sim)],
              by = c("anio", "estado"))
chk4[, desvio_pp := 100 * (share_sim - share_sim_ref)]
log_msg("9.4 Backcast vs. R/28 (46_validacion_por_estado_cohorte.csv), max |desvio| en share simulado: %.4f pp",
        max(abs(chk4$desvio_pp)))

log_msg("9.5 S_cohorte(2025) por cohorte, rango: %.4f a %.4f",
        min(resumen$superv_2025, na.rm = TRUE), max(resumen$superv_2025, na.rm = TRUE))

log_msg("9.6 Poblacion sin celda de supervivencia en la corrida real (inmortales del ciclo):")
inmortales <- rbindlist(lapply(names(snapshots), function(nm) {
  a <- as.integer(nm)
  dt <- snapshots[[nm]][, .(edad_lookup = pmin(edad, 99L), sexo = as.character(sexo), estado, grupo_etario)]
  s <- superv[anio == a, .(edad_lookup = edad, sexo, estado, tiene = TRUE)]
  dt <- merge(dt, s, by = c("edad_lookup", "sexo", "estado"), all.x = TRUE, sort = FALSE)
  data.table(anio = a, n_total = nrow(dt), n_sin_celda = sum(is.na(dt$tiene)),
             pct = 100 * mean(is.na(dt$tiene)),
             edad_min_sin = suppressWarnings(min(dt$edad_lookup[is.na(dt$tiene)])),
             pct_65mas = 100 * sum(is.na(dt$tiene) & dt$grupo_etario == "65+") /
                         max(1, sum(dt$grupo_etario == "65+")))
}))
log_msg("%s", paste(capture.output(print(inmortales)), collapse = "\n"))
fwrite(inmortales, file.path(out_dir, "52_poblacion_sin_celda_mortalidad.csv"))

log_msg("\n=== outputs: 52_cota_factibilidad_nunca.csv, 52_trayectoria_nunca_cohorte.csv, 52_reexpresion_grupo_etario.csv, 52_estados_cohorte_con_2011.csv ===")
close(log_con)
