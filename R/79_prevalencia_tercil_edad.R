# 79_prevalencia_tercil_edad.R
#
# Prevalence by income tertile and age group, crude and directly age-standardised (2025 status quo
# population), for the status quo and the primary scenario. Reproduces the corresponding branches
# of R/38 and aborts if crude prevalence differs.
# Output: output/tables/96_*.csv

library(here)
library(data.table)
library(haven)
library(splines)
here::i_am("R/79_prevalencia_tercil_edad.R")

log_con <- file(here::here("logs", "79_prevalencia_tercil_edad.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 79_prevalencia_tercil_edad.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")
source(here::here("R", "24_motor_transiciones.R"))

ANIO_INICIO_ESCENARIO <- 2027L
HORIZONTE_GRADUAL <- 5L
ANIOS_SNAP <- c(2025L, seq(2030L, 2060L, by = 5L))
ANIO_MAX_PRECIO <- 2070L
SHARE_OMS <- 0.75

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params_escalada <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor)
param_downshift <- params_cal[parametro == "raw_par3"]$valor

insumos <- cargar_insumos_motor(regla_precio = "mantenimiento_politica", anio_max_precio = ANIO_MAX_PRECIO)
precio_base <- insumos$precio_lookup
traj <- trayectoria_fiscal(precio_base, SHARE_OMS, ANIO_INICIO_ESCENARIO, "gradual", HORIZONTE_GRADUAL, ANIO_MAX_PRECIO)
precio_esc <- construir_precio_lookup(2010, ANIO_MAX_PRECIO, regla = "mantenimiento_politica",
                                      trayectoria_impuesto = traj[, .(anio, factor_impuesto_real)])

correr_edad <- function(precio_lookup, precio_ref, etiqueta, semilla = 20260809) {
  set.seed(semilla)
  pop <- cargar_poblacion_proyeccion()
  a <- ANIO_POB_PROYECCION; out <- list()
  for (obj in ANIOS_SNAP) {
    while (a < obj) {
      ov <- if (is.null(precio_ref)) NULL else {
        r <- precio_lookup[anio == a]$p_reales / precio_ref[anio == a]$p_reales
        if (isTRUE(all.equal(r, 1, tolerance = 1e-12))) NULL else r
      }
      pop <- ciclo_anual(pop, a, insumos$modelos, insumos$tabla_superv, precio_lookup,
                         insumos$pob_ine, insumos$entrada_15_por_anio, params_escalada,
                         param_downshift, precio_ratio_override = ov)
      a <- a + 1L
    }
    cig <- calcular_cig_dia(pop, obj, insumos$intensidad, precio_lookup)
    d <- data.table(tercil = as.integer(as.character(pop$tercil)), grupo_edad = grupo_etario_intensidad(pop$edad), fuma = cig > 0)
    out[[as.character(obj)]] <- d[, .(N = .N, fumadores = sum(fuma)), by = .(tercil, grupo_edad)][
      , `:=`(escenario = etiqueta, anio = obj)]
    log_msg("%s %d listo", etiqueta, obj)
  }
  rbindlist(out)
}

t0 <- Sys.time()
E <- rbind(correr_edad(precio_base, NULL, "base"), correr_edad(precio_esc, precio_base, "conv75_gradual"))
log_msg("Corridas en %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))
setcolorder(E, c("escenario", "anio", "tercil", "grupo_edad", "N", "fumadores"))
setorder(E, escenario, anio, tercil, grupo_edad)
saveRDS(E, file.path(int_dir, "r79_snapshots_tercil_edad.rds"))

ref <- fread(file.path(out_dir, "59_espacio_escenarios_tercil.csv"))[escenario %in% c("base", "conv75_gradual")]
cru <- E[, .(prev79 = sum(fumadores) / sum(N)), by = .(escenario, anio, tercil)]
chk <- merge(cru, ref[, .(escenario, anio, tercil, prevalencia)], by = c("escenario", "anio", "tercil"))
dif <- max(abs(chk$prev79 - chk$prevalencia))
log_msg("Guardarrail contra 59_espacio_escenarios_tercil.csv: %d celdas, max |dif| = %.3g", nrow(chk), dif)
if (nrow(chk) != nrow(cru) || dif > 1e-12) stop("R/79 no reproduce R/38: no se escriben salidas")
fwrite(E, file.path(out_dir, "96_prevalencia_tercil_edad.csv"))

std <- E[escenario == "base" & anio == 2025L, .(n = sum(N)), by = grupo_edad][, peso := n / sum(n)]
P <- merge(E, std[, .(grupo_edad, peso)], by = "grupo_edad")
P <- P[, .(prev_cruda = 100 * sum(fumadores) / sum(N),
           prev_estandarizada = 100 * sum(peso * fumadores / N),
           pct_65mas = 100 * sum(N[grupo_edad == "65+"]) / sum(N)), by = .(escenario, anio, tercil)]
W <- dcast(P, escenario + anio ~ tercil, value.var = c("prev_cruda", "prev_estandarizada", "pct_65mas"))
W[, `:=`(brecha_cruda = prev_cruda_1 - prev_cruda_3, razon_cruda = prev_cruda_1 / prev_cruda_3,
         brecha_estandarizada = prev_estandarizada_1 - prev_estandarizada_3,
         razon_estandarizada = prev_estandarizada_1 / prev_estandarizada_3)]
b <- W[escenario == "base"]; e <- W[escenario == "conv75_gradual"]
b25 <- b[anio == 2025L]
rem <- function(m) 100 * (b[[m]] - e[[m]]) / (b[[m]] - b25[[m]])
W <- merge(W, data.table(anio = b$anio,
                         pct_rem_brecha_cruda = rem("brecha_cruda"), pct_rem_razon_cruda = rem("razon_cruda"),
                         pct_rem_brecha_estandarizada = rem("brecha_estandarizada"),
                         pct_rem_razon_estandarizada = rem("razon_estandarizada")), by = "anio")
W[escenario == "base", grep("^pct_rem", names(W), value = TRUE) := NA_real_]
W[anio == 2025L, grep("^pct_rem", names(W), value = TRUE) := NA_real_]
setorder(W, escenario, anio)
fwrite(W, file.path(out_dir, "96_prevalencia_tercil_estandarizada.csv"))
log_msg("Pesos de estandarizacion (statu quo 2025):\n%s", paste(capture.output(print(std)), collapse = "\n"))
log_msg("Resultado:\n%s", paste(capture.output(print(W[anio %in% c(2025L, 2060L)], digits = 4)), collapse = "\n"))
log_msg("=== outputs: 96_prevalencia_tercil_edad.csv, 96_prevalencia_tercil_estandarizada.csv ===")
close(log_con)
