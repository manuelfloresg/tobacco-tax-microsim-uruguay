# 80_descomposicion_numero_fumadores.R
#
# Checks the decomposition of the consumption change in 59_espacio_escenarios_comparado.csv: the
# extensive margin is the log change in the number of people who smoke, so that extensive plus
# intensive equals the log change in consumption. Changes nothing if R/38 is current.
# Input:  output/tables/59_espacio_escenarios_agregado.csv, 59_espacio_escenarios_comparado.csv

library(here)
library(data.table)
here::i_am("R/80_descomposicion_numero_fumadores.R")

log_con <- file(here::here("logs", "80_descomposicion_numero_fumadores.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 80_descomposicion_numero_fumadores.R -- %s ===", format(Sys.time()))

out_dir <- here::here("output", "tables")
arch <- file.path(out_dir, "59_espacio_escenarios_comparado.csv")
cmp <- fread(arch)
base <- fread(file.path(out_dir, "59_espacio_escenarios_agregado.csv"))[escenario_impuesto == "base", .(anio, N_base_ = N)]
orden <- names(cmp)
viejo <- copy(cmp$aporte_extensivo)
cmp <- merge(cmp, base, by = "anio", sort = FALSE)
cmp[, aporte_extensivo := log(N * prevalencia / (N_base_ * prevalencia_base))]
cmp[, N_base_ := NULL]
setcolorder(cmp, orden)
setorder(cmp, escenario_impuesto, anio)
res <- cmp[, max(abs(aporte_extensivo + aporte_intensivo - d_ln_consumo))]
log_msg("Identidad extensivo + intensivo - d_ln_consumo: max |residuo| = %.3g", res)
if (res > 1e-12) stop("La identidad no cierra: no se escribe nada")
cambio <- max(abs(sort(viejo) - sort(cmp$aporte_extensivo)))
log_msg("Cambio maximo en aporte_extensivo respecto del archivo en disco: %.3g", cambio)
fwrite(cmp, arch)
log_msg("Participacion del extensivo, conv75_gradual:\n%s", paste(capture.output(print(
  cmp[escenario_impuesto == "conv75_gradual" & anio %in% c(2030L, 2045L, 2060L),
      .(anio, share_ext_pct = 100 * aporte_extensivo / d_ln_consumo)], digits = 4)), collapse = "\n"))
log_msg("=== output: 59_espacio_escenarios_comparado.csv (reescrito) ===")
close(log_con)
