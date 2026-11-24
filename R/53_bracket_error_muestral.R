# 53_bracket_error_muestral.R
#
# Adds ECH sampling error to the measurement bracket of R/40, with the design effect estimated by
# clustering on household.
# Input:  panel from R/04, output/tables/67_*.csv
# Output: output/tables/80_*.csv

library(here)
library(data.table)
library(survey)

here::i_am("R/53_bracket_error_muestral.R")

log_con <- file(here::here("logs", "53_bracket_error_muestral.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 53_bracket_error_muestral.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

options(survey.lonely.psu = "adjust")

ANIOS_VALIDACION <- c(2014, 2017, 2018, 2019, 2021, 2022, 2023, 2024, 2025)

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))
if (!"hogar_id" %in% names(P))
  stop("El panel no trae hogar_id. Re-correr R/04_validez_medicion.R con la correccion ",
       "del 2026-08-20 (seleccion de columnas al final del bucle).")

P[, grupo_etario := fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")]
P[, ind_prev := as.integer(estado %in% c(2, 3))]
P[, ind_nunca := as.integer(estado == 1)]

log_msg("Panel: %d filas, %d anios, %d hogares distintos (anio x hogar_id)",
        nrow(P), uniqueN(P$anio), nrow(unique(P[, .(anio, hogar_id)])))
tam <- P[, .(personas = .N, hogares = uniqueN(hogar_id)), by = anio][order(anio)]
tam[, personas_por_hogar := personas / hogares]
log_msg("Tamano de conglomerado por anio (personas 15+ por hogar en la muestra):\n%s",
        paste(capture.output(print(tam, digits = 4)), collapse = "\n"))

log_msg("\n--- 2) Error estandar por anio x grupo etario, con y sin conglomerado ---")

METRICAS <- c(prevalencia_actual = "ind_prev", nunca_fumo = "ind_nunca")

ee_por_anio <- function(anio_i) {
  D <- P[anio == anio_i]
  d_clu <- svydesign(ids = ~hogar_id, weights = ~peso, data = D)
  d_srs <- svydesign(ids = ~1,        weights = ~peso, data = D)
  rbindlist(lapply(names(METRICAS), function(mt) {
    f <- as.formula(paste0("~", METRICAS[[mt]]))
    a <- svyby(f, ~grupo_etario, d_clu, svymean, na.rm = TRUE)
    b <- svyby(f, ~grupo_etario, d_srs, svymean, na.rm = TRUE)
    data.table(anio = anio_i, grupo_etario = a$grupo_etario, metrica = mt,
               prop = as.numeric(a[[2]]),
               ee_disenio = as.numeric(a[[3]]),
               ee_simple  = as.numeric(b[[3]]),
               n_muestral = D[, .N, by = grupo_etario][order(grupo_etario)]$N,
               n_eff_kish = D[, sum(peso)^2 / sum(peso^2), by = grupo_etario][order(grupo_etario)]$V1)
  }))
}

EE <- rbindlist(lapply(ANIOS_VALIDACION, function(a) {
  t0 <- Sys.time()
  r <- ee_por_anio(a)
  log_msg("  %d listo en %.1f s", a, as.numeric(difftime(Sys.time(), t0, units = "secs")))
  r
}))
EE[, deff := (ee_disenio / ee_simple)^2]
EE[, ic95_semiancho_pp := 100 * 1.96 * ee_disenio]
setorder(EE, metrica, grupo_etario, anio)
fwrite(EE, file.path(out_dir, "80_error_muestral_ech.csv"))

res_deff <- EE[, .(deff_min = min(deff), deff_mediana = median(deff), deff_max = max(deff),
                   ee_disenio_medio_pp = 100 * mean(ee_disenio),
                   semiancho_medio_pp = mean(ic95_semiancho_pp)),
               by = .(metrica, grupo_etario)]
setorder(res_deff, metrica, grupo_etario)
fwrite(res_deff, file.path(out_dir, "80_deff_medido_resumen.csv"))
log_msg("\nEfecto de diseno MEDIDO (conglomerado por hogar, sin estratos = cota superior):\n%s",
        paste(capture.output(print(res_deff, digits = 4)), collapse = "\n"))
log_msg("\nReferencia: R/41 usa una grilla supuesta de deff 1 / 1,5 / 2 / 3 y R/42 un 1,5 fijo.")
log_msg("El unico deff medido que habia en el proyecto es el de R/10, sobre GATS, no sobre ECH.")

log_msg("\n--- 3) Bracket de informante mas error muestral ---")

br <- fread(file.path(out_dir, "67_bracket_grupo_etario.csv"))
br[, piso := pmin(obs, std)]
br[, techo := pmax(obs, std)]

B <- merge(br, EE[, .(anio, grupo_etario, metrica, ee_disenio, deff, n_muestral, n_eff_kish)],
           by = c("anio", "grupo_etario", "metrica"))
B[, piso_ancho := piso - 1.96 * ee_disenio]
B[, techo_ancho := techo + 1.96 * ee_disenio]
B[, ancho_informante_pp := 100 * (techo - piso)]
B[, ancho_total_pp := 100 * (techo_ancho - piso_ancho)]
B[, aporte_muestral_pct := 100 * (ancho_total_pp - ancho_informante_pp) / ancho_total_pp]
setorder(B, metrica, grupo_etario, anio)
fwrite(B, file.path(out_dir, "80_bracket_ampliado.csv"))

comp_ancho <- B[, .(ancho_informante_pp = mean(ancho_informante_pp),
                    ancho_total_pp = mean(ancho_total_pp),
                    aporte_muestral_pct = mean(aporte_muestral_pct)),
                by = .(metrica, grupo_etario)]
setorder(comp_ancho, metrica, grupo_etario)
log_msg("Cuanto agrega el error muestral al ancho del bracket:\n%s",
        paste(capture.output(print(comp_ancho, digits = 4)), collapse = "\n"))

log_msg("\n--- 4) Posicion del motor contra el bracket angosto y contra el ancho ---")

pos <- fread(file.path(out_dir, "67_posicion_motor_grupo_etario.csv"))[
         , .(anio, grupo_etario, metrica, sim)]
Q <- merge(B[metrica == "prevalencia_actual"], pos, by = c("anio", "grupo_etario", "metrica"))
Q[, dentro_angosto := sim >= piso & sim <= techo]
Q[, dentro_ancho := sim >= piso_ancho & sim <= techo_ancho]
Q[, exceso_angosto_pp := 100 * pmax(0, sim - techo)]
Q[, exceso_ancho_pp := 100 * pmax(0, sim - techo_ancho)]
setorder(Q, grupo_etario, anio)
fwrite(Q, file.path(out_dir, "80_posicion_motor_bracket_ampliado.csv"))

resu <- Q[, .(anios = .N, dentro_angosto = sum(dentro_angosto), dentro_ancho = sum(dentro_ancho),
              exceso_max_angosto_pp = max(exceso_angosto_pp),
              exceso_max_ancho_pp = max(exceso_ancho_pp)), by = grupo_etario]
setorder(resu, grupo_etario)
fwrite(resu, file.path(out_dir, "80_resumen_dentro_bracket.csv"))
log_msg("Anios dentro del bracket, angosto vs ancho:\n%s",
        paste(capture.output(print(resu, digits = 4)), collapse = "\n"))

log_msg("\nLECTURA. Se esperaban dos cosas y las dos se pueden chequear aca:")
log_msg("(a) refuerza 33.3, porque la restriccion de identificacion del APC se apoya en que")
log_msg("    15-24 cae dentro del bracket los nueve anios, y ensanchar solo puede ayudar;")
log_msg("(b) NO rescata 45-64/65+, cuyo exceso es de 5 a 10pp, un orden por encima de")
log_msg("    cualquier error muestral con estos tamanos de celda.")
for (g in c("15-24", "25-44", "45-64", "65+")) {
  rr <- resu[grupo_etario == g]
  log_msg("  %-6s dentro %d/%d -> %d/%d ; exceso maximo %.2f -> %.2f pp", g,
          rr$dentro_angosto, rr$anios, rr$dentro_ancho, rr$anios,
          rr$exceso_max_angosto_pp, rr$exceso_max_ancho_pp)
}

log_msg("\n=== outputs: 80_error_muestral_ech.csv, 80_deff_medido_resumen.csv,")
log_msg("    80_bracket_ampliado.csv, 80_posicion_motor_bracket_ampliado.csv,")
log_msg("    80_resumen_dentro_bracket.csv ===")
close(log_con)
