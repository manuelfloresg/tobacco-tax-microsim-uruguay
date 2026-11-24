# 22_poblacion_sintetica.R
#
# Synthetic initial population: one simulated individual per unit of ECH weight, with age, sex,
# income tertile and smoking state. Former smokers receive a time since quitting drawn from the
# GATS distribution conditional on age group.
# Usage:  Rscript R/22_poblacion_sintetica.R            (2011, the starting population)
#         Rscript R/22_poblacion_sintetica.R 2025       (2025 survey population, as recorded)
#         Rscript R/22_poblacion_sintetica.R 2025 corr  (2025, corrected for proxy reporting)
# Input:  panel from R/04, output/tables/39_duracion_stock_por_edad.csv
# Output: data/interim/poblacion_sintetica_<year>.csv.gz

library(here)
library(data.table)

here::i_am("R/22_poblacion_sintetica.R")

ARGS_22 <- commandArgs(trailingOnly = TRUE)
ANIO_BASE <- if (length(ARGS_22) >= 1L) as.integer(ARGS_22[1]) else 2011L
stopifnot(!is.na(ANIO_BASE))
CORREGIR_INFORMANTE <- length(ARGS_22) >= 2L && ARGS_22[2] == "corr"
SUFIJO <- paste0(if (ANIO_BASE == 2011L) "" else sprintf("_%d", ANIO_BASE), if (CORREGIR_INFORMANTE) "_corr" else "")
set.seed(20260809)

log_con <- file(here::here("logs", sprintf("22_poblacion_sintetica%s.log", SUFIJO)), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 22_poblacion_sintetica.R -- %s ===", format(Sys.time()))
log_msg("Anio base: %d. Semilla: 20260809.", ANIO_BASE)

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))
base <- P[anio == ANIO_BASE]
n_tercil_na <- base[is.na(tercil), .N]
base <- base[!is.na(tercil)]
log_msg("Personas ECH en %d: %d (peso total %.0f). Excluidas por tercil NA: %d",
        ANIO_BASE, nrow(base) + n_tercil_na, sum(base$peso) , n_tercil_na)

log_msg("\n--- 1) Expansion de la muestra ECH a poblacion sintetica completa ---")

base[, peso_entero := as.integer(round(peso))]
stopifnot(all(abs(base$peso - base$peso_entero) < 1e-6))

pop <- base[rep(1:.N, peso_entero), .(edad, sexo, tercil, estado, directo)]
pop[, id := .I]
log_msg("Poblacion sintetica: %d individuos (== suma de peso, verificado sin perdida)", nrow(pop))
stopifnot(nrow(pop) == sum(base$peso_entero))

if (CORREGIR_INFORMANTE) {
  log_msg("
--- 1b) Correccion por informante: estado de proxies sorteado de autorrespondentes de su celda ---")
  celda <- function(d) d[, paste(fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+"), sexo, tercil)]
  pop[, celda := celda(pop)]; base[, celda := celda(base)]
  prev_antes <- pop[, mean(estado %in% 2:3)]
  ref <- base[directo == 1, .(w = sum(peso)), by = .(celda, estado)]
  stopifnot(all(unique(pop[directo == 0]$celda) %in% ref$celda))
  for (cc in sort(unique(pop[directo == 0]$celda))) {
    idx <- which(pop$directo == 0 & pop$celda == cc); r <- ref[celda == cc]
    pop[idx, estado := r$estado[sample.int(nrow(r), length(idx), replace = TRUE, prob = r$w)]]
  }
  log_msg("Prevalencia (ocasional + diario) antes %.2f%%, despues %.2f%%. Por grupo etario:
%s",
          100 * prev_antes, 100 * pop[, mean(estado %in% 2:3)],
          paste(capture.output(print(pop[, .(prev = round(100 * mean(estado %in% 2:3), 2), pct_proxy = round(100 * mean(directo == 0), 1)),
                by = .(g = sub(" .*", "", celda))][order(g)])), collapse = "
"))
  pop[, celda := NULL]
}
pop[, directo := NULL]

log_msg("\n--- 2) Imputacion de duracion desde que dejo (exfumadores) ---")

stock_dur <- fread(file.path(out_dir, "39_duracion_stock_por_edad.csv"))
GRUPOS_EDAD_DURACION <- function(edad) {
  as.character(cut(edad, breaks = c(-Inf, 34, 44, 54, 64, Inf),
                   labels = c("15-34", "35-44", "45-54", "55-64", "65+")))
}
log_msg("Distribucion empirica de duracion por grupo etario (GATS, n = %d):\n%s", nrow(stock_dur),
        paste(capture.output(print(stock_dur[, .(n = .N, mediana = round(median(duracion_anios), 1),
              pct_20mas = round(100 * weighted.mean(duracion_anios >= 20, peso), 1)),
              by = grupo_edad][order(grupo_edad)])), collapse = "\n"))

pop[, anios_desde_dejar := NA_real_]
pop[, patron_antes_de_dejar := NA_character_]

n_reciente <- pop[estado == 4, .N]
pop[estado == 4, anios_desde_dejar := runif(.N, 0, 1)]
log_msg("Ex_reciente (estado 4): %d individuos, anios_desde_dejar ~ Uniform(0,1)", n_reciente)

n_consolidado <- pop[estado == 5, .N]
pop[estado == 5, grupo_edad_dur := GRUPOS_EDAD_DURACION(edad)]
for (g in sort(unique(pop[estado == 5]$grupo_edad_dur))) {
  idx <- which(pop$estado == 5 & pop$grupo_edad_dur == g)
  pool <- stock_dur[grupo_edad == g]
  if (nrow(pool) == 0) stop("Sin duraciones de GATS para el grupo etario ", g)
  d <- sample(pool$duracion_anios, size = length(idx), replace = TRUE, prob = pool$peso)
  pop[idx, anios_desde_dejar := pmax(1, d + runif(length(idx), -0.5, 0.5))]
}
pop[, grupo_edad_dur := NULL]
pop[estado == 5, anios_desde_dejar := pmin(anios_desde_dejar, pmax(edad - 10, 1))]
log_msg("Ex_consolidado (estado 5): %d individuos, duracion sorteada de la distribucion empirica de GATS CONDICIONAL AL GRUPO ETARIO (39_duracion_stock_por_edad.csv), topeada en (edad-10)", n_consolidado)
log_msg("Duracion imputada resultante, por grupo etario:\n%s",
        paste(capture.output(print(pop[estado %in% c(4, 5), .(n = .N,
              mediana = round(median(anios_desde_dejar), 1),
              pct_20mas = round(100 * mean(anios_desde_dejar >= 20), 1)),
              by = .(grupo_edad = GRUPOS_EDAD_DURACION(edad))][order(grupo_edad)])), collapse = "\n"))
log_msg("Resumen de anios_desde_dejar imputado (consolidado):\n%s",
        paste(capture.output(summary(pop[estado == 5]$anios_desde_dejar)), collapse = "\n"))

prop_diario_antes <- 1898 / (1898 + 795)
n_ex <- pop[estado %in% c(4, 5), .N]
pop[estado %in% c(4, 5), patron_antes_de_dejar := fifelse(runif(.N) < prop_diario_antes, "diario", "ocasional")]
log_msg("Patron antes de dejar imputado para %d exfumadores, P(diario)=%.4f (proporcion agregada GATS)",
        n_ex, prop_diario_antes)
log_msg("Verificacion (deberia acercarse a %.4f): %s",
        prop_diario_antes,
        paste(capture.output(print(pop[estado %in% c(4,5), .N, by = patron_antes_de_dejar][, prop := N/sum(N)])), collapse = "\n"))

log_msg("\n--- 3) Chequeo de coherencia (sintetico vs ECH ponderado) ---")

pop[, grupo_etario := fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")]
base[, grupo_etario := fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")]

chequeo <- merge(
  pop[, .(N_sintetico = .N), by = .(grupo_etario, sexo, tercil)],
  base[, .(N_pond_ech = sum(peso)), by = .(grupo_etario, sexo, tercil)],
  by = c("grupo_etario", "sexo", "tercil")
)
chequeo[, diferencia := N_sintetico - N_pond_ech]
setorder(chequeo, grupo_etario, sexo, tercil)
fwrite(chequeo, file.path(out_dir, sprintf("42_chequeo_poblacion_sintetica%s.csv", SUFIJO)))
log_msg("Diferencia maxima entre sintetico y ECH ponderado por celda (deberia ser 0, peso ya es entero): %d",
        max(abs(chequeo$diferencia)))

pop[, anio := ANIO_BASE]
setcolorder(pop, c("id", "anio", "edad", "sexo", "tercil", "estado", "anios_desde_dejar", "patron_antes_de_dejar", "grupo_etario"))
arch_pop <- sprintf("poblacion_sintetica_%d%s.csv.gz", ANIO_BASE, if (CORREGIR_INFORMANTE) "_corr" else "")
fwrite(pop, file.path(int_dir, arch_pop))
log_msg("Escrito data/interim/%s (%d filas, %d columnas)", arch_pop, nrow(pop), ncol(pop))

log_msg("\n=== outputs: data/interim/poblacion_sintetica_%d.csv.gz, output/tables/42_chequeo_poblacion_sintetica.csv ===", ANIO_BASE)
close(log_con)
