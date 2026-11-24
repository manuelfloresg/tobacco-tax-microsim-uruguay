# 68_equilibrio_margen_extensivo.R
#
# Equilibrium elasticity of prevalence produced by the model compared with the extensive-margin
# elasticity it is given. The tertile-specific correction factor of R/24 makes the two coincide;
# this script checks it.
# Input:  output/tables/43_parametros_calibrados.csv
# Output: log

library(here)
library(data.table)
here::i_am("R/68_equilibrio_margen_extensivo.R")
suppressMessages(source(here::here("R", "24_motor_transiciones.R")))

ARGS <- commandArgs(trailingOnly = TRUE)
ANIO <- if (length(ARGS) >= 1) as.integer(ARGS[1]) else 2025L
N_SEXO <- if (length(ARGS) >= 2) as.integer(ARGS[2]) else 600000L
MODO <- if (length(ARGS) >= 3) ARGS[3] else "medir"
stopifnot(MODO %in% c("medir", "derivar"))
if (MODO == "derivar") FACTOR_TRASLADO_EXTENSIVO_TERCIL[] <- 1
RATIOS <- c(1, 1.45)
SEMILLAS <- 20261031L + 0:2

log_con <- file(here::here("logs", "68_equilibrio_margen_extensivo.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { m <- sprintf(...); cat(m, "\n"); cat(m, "\n", file = log_con) }
log_msg("=== 68_equilibrio_margen_extensivo.R -- %s ===", format(Sys.time()))
log_msg("Modo %s. Tasas congeladas en %d; %d entrantes por sexo; ratios de precio %s; %d semillas",
        MODO, ANIO, N_SEXO, paste(RATIOS, collapse = ", "), length(SEMILLAS))
log_msg("Factor de traslado en uso: %s", paste(sprintf("%.17g", FACTOR_TRASLADO_EXTENSIVO_TERCIL), collapse = " / "))

ins <- cargar_insumos_motor()
par <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params_escalada <- c(par[parametro == "raw_par1"]$valor, par[parametro == "raw_par2"]$valor)
param_downshift <- par[parametro == "raw_par3"]$valor
anio_sup <- min(ANIO, max(ins$tabla_superv$anio))
log_msg("Tabla de supervivencia del anio %d", anio_sup)

cohorte <- function() {
  d15 <- ins$entrada_15_por_anio[anio == pmin(pmax(ANIO, 2011), 2045)]
  pop <- rbindlist(lapply(c("mujer", "varon"), function(sx) {
    ds <- d15[sexo == sx]
    data.table(edad = 15L, sexo = sx,
               estado = sample(ds$estado, N_SEXO, replace = TRUE, prob = ds$share))
  }))
  pop[, tercil := factor(sample(1:3, .N, replace = TRUE), levels = c(1, 2, 3))]
  pop[, anios_desde_dejar := NA_real_]
  pop[estado == 4, anios_desde_dejar := runif(.N, 0, 1)]
  pop[estado == 5, anios_desde_dejar := runif(.N, 1, 5)]
  pop[, patron_antes_de_dejar := NA_character_]
  pop[estado %in% c(4, 5), patron_antes_de_dejar := fifelse(runif(.N) < 1898 / (1898 + 795), "diario", "ocasional")]
  pop
}

correr <- function(r, semilla) {
  set.seed(semilla)
  pop <- cohorte()
  pr <- c(`1` = r, `2` = r, `3` = r)
  acum <- list()
  while (nrow(pop) > 0 && max(pop$edad) <= 110) {
    acum[[length(acum) + 1L]] <- pop[, .(n = .N), by = .(edad, tercil, estado)]
    pop <- aplicar_mortalidad(pop, anio_sup, ins$tabla_superv)
    pop <- aplicar_cesacion(pop, ANIO, ins$modelos$cesacion, pr)
    pop <- aplicar_recaida(pop, ins$modelos$recaida)
    pop <- aplicar_iniciacion(pop, ANIO, ins$modelos$iniciacion, pr)
    pop <- aplicar_escalada(pop, params_escalada)
    pop <- aplicar_downshift(pop, param_downshift)
    pop <- avanzar_edad(pop)
  }
  a <- rbindlist(acum)[, .(n = sum(n)), by = .(tercil, estado)]
  a[, `:=`(ratio = r, semilla = semilla)]
  a
}

res <- rbindlist(lapply(SEMILLAS, function(s) rbindlist(lapply(RATIOS, function(r) {
  log_msg("  corriendo semilla %d, r = %.2f", s, r); correr(r, s) }))))
res[, tercil := as.character(tercil)]
prev <- res[, .(P = sum(n[estado %in% c(2, 3)]) / sum(n)), by = .(ratio, tercil)]
prev <- rbind(prev, res[, .(tercil = "0", P = sum(n[estado %in% c(2, 3)]) / sum(n)), by = ratio])
por_semilla <- res[, .(P = sum(n[estado %in% c(2, 3)]) / sum(n)), by = .(ratio, tercil, semilla)]
por_semilla <- dcast(por_semilla, tercil + semilla ~ ratio, value.var = "P")
setnames(por_semilla, c("tercil", "semilla", "P1", "Pr"))
por_semilla[, e := log(Pr / P1) / log(RATIOS[2])]
base <- prev[ratio == 1, .(tercil, P1 = P)]
out <- merge(prev[ratio != 1], base, by = "tercil")
out[, elasticidad_equilibrio := log(P / P1) / log(ratio)]
eobj <- c(`0` = sum(ELASTICIDAD_EXTENSIVA_TERCIL * base[tercil != "0"][order(tercil)]$P1) /
                sum(base[tercil != "0"]$P1),
          ELASTICIDAD_EXTENSIVA_TERCIL)
out[, elasticidad_objetivo := eobj[tercil]]
out[, cociente := elasticidad_equilibrio / elasticidad_objetivo]
out <- merge(out, por_semilla[, .(de_entre_semillas = sd(e)), by = tercil], by = "tercil", all.x = TRUE)
out[, factor_en_uso := c(`0` = NA, FACTOR_TRASLADO_EXTENSIVO_TERCIL)[tercil]]
out[, factor_propuesto := factor_en_uso / cociente]
out[, modo := MODO]
setorder(out, tercil)
log_msg("\nPrevalencia estacionaria (15+) y elasticidad de equilibrio (tercil 0 = total):\n%s",
        paste(capture.output(print(out, digits = 4)), collapse = "\n"))
if (MODO == "derivar")
  log_msg("\nFactor propuesto para R/24 (17 digitos):\nFACTOR_TRASLADO_EXTENSIVO_TERCIL <- c(%s)",
          paste(sprintf("`%s` = %.17g", 1:3, out[tercil != "0"]$factor_propuesto), collapse = ", "))
salida <- sprintf("92_equilibrio_margen_extensivo_%s.csv", MODO)
fwrite(out, file.path(out_dir, salida))
log_msg("\n=== output: %s ===", salida)
close(log_con)
