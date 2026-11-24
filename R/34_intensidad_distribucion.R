# 34_intensidad_distribucion.R
#
# Empirical distribution of cigarettes per day by cell (frequency, sex, age group, income tertile)
# from GATS microdata, and the reference price for the intensive margin. The model draws each
# person's cigarettes per day from the distribution of their cell.
# Input:  data/raw/GATS/, output/tables/35_*, 41_*
# Output: output/tables/56_*.csv

library(here)
library(data.table)
library(haven)
library(dplyr)

here::i_am("R/34_intensidad_distribucion.R")

log_con <- file(here::here("logs", "34_intensidad_distribucion.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 34_intensidad_distribucion.R -- %s ===", format(Sys.time()))

raw_gats <- here::here("data", "raw", "GATS")
out_dir  <- here::here("output", "tables")

N_MIN_CELDA <- 50
GRILLA_U <- seq(0.005, 0.995, by = 0.005)

grupo_etario <- function(edad) {
  as.character(cut(edad, breaks = c(15, 25, 45, 65, Inf), right = FALSE,
                   labels = c("15-24", "25-44", "45-64", "65+")))
}

log_msg("\n--- 1) Carga de GATS 2009 y 2017 ---")

prod_lab_gats <- c(A = "manufacturado", B = "armado_mano", C = "picadura_naco",
                    D = "pipa", E = "puro", F = "pipa_agua", G = "otro")

cargar_gats <- function(path, anio_encuesta) {
  nms0 <- names(haven::read_dta(path, n_max = 1))
  agevar <- nms0[match("age", tolower(nms0))]
  cols <- c("A01", agevar, "gatsweight", "B01",
            paste0("B06", names(prod_lab_gats)), paste0("B10", names(prod_lab_gats)))
  d <- setDT(haven::read_dta(path, col_select = dplyr::all_of(cols)))
  setnames(d, agevar, "edad"); setnames(d, "gatsweight", "peso")
  d[, `:=`(edad = as.numeric(edad), peso = as.numeric(peso))]
  d[, sexo := fifelse(as.numeric(A01) == 1, "varon", fifelse(as.numeric(A01) == 2, "mujer", NA_character_))]
  d[, frecuencia := fifelse(as.numeric(B01) == 1, "diario",
                     fifelse(as.numeric(B01) == 2, "ocasional", NA_character_))]
  d[, grupo_etario := grupo_etario(edad)]
  for (letra in names(prod_lab_gats)) {
    vd <- paste0("B06", letra); vo <- paste0("B10", letra)
    cant_d <- fifelse(as.numeric(d[[vd]]) >= 0 & as.numeric(d[[vd]]) < 700, as.numeric(d[[vd]]), NA_real_)
    cant_o <- fifelse(as.numeric(d[[vo]]) >= 0 & as.numeric(d[[vo]]) < 700, as.numeric(d[[vo]]) / 7, NA_real_)
    d[, (paste0(letra, "_ok")) := fifelse(frecuencia == "diario", cant_d,
                                   fifelse(frecuencia == "ocasional", cant_o, NA_real_))]
  }
  ok <- paste0(names(prod_lab_gats), "_ok")
  d[, cig_dia := rowSums(.SD, na.rm = TRUE), .SDcols = ok]
  d[, n_prod := rowSums(!is.na(.SD) & .SD > 0), .SDcols = ok]
  d[n_prod == 0, cig_dia := NA_real_]
  d[, anio_encuesta := anio_encuesta]
  d[edad >= 15 & !is.na(frecuencia) & !is.na(sexo) & !is.na(cig_dia) & cig_dia > 0 & peso > 0,
    .(anio_encuesta, sexo, edad, grupo_etario, frecuencia, peso, cig_dia)]
}

G <- rbindlist(list(cargar_gats(file.path(raw_gats, "gats_2009.dta"), 2009),
                    cargar_gats(file.path(raw_gats, "gats_2017.dta"), 2017)))
log_msg("Fumadores actuales con intensidad valida: %d (2009: %d, 2017: %d)",
        nrow(G), G[anio_encuesta == 2009, .N], G[anio_encuesta == 2017, .N])
log_msg("Intensidad por frecuencia (media ponderada, ambas olas juntas):\n%s",
        paste(capture.output(print(G[, .(n = .N, media = round(weighted.mean(cig_dia, peso), 2),
                                          p50 = round(median(cig_dia), 1)), by = frecuencia])), collapse = "\n"))

G[, peso_rel := peso / sum(peso) * .N, by = anio_encuesta]

log_msg("\n--- 2) Cuantiles ponderados por celda ---")

cuantil_pond <- function(x, w, probs) {
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  x[findInterval(probs, cw, left.open = TRUE) + 1L]
}

celdas <- CJ(frecuencia = c("diario", "ocasional"), sexo = c("varon", "mujer"),
             grupo_etario = c("15-24", "25-44", "45-64", "65+"), unique = TRUE)

resolver_celda <- function(fr, sx, ge) {
  sub <- G[frecuencia == fr & sexo == sx & grupo_etario == ge]
  if (nrow(sub) >= N_MIN_CELDA) return(list(datos = sub, fallback = "celda_completa"))
  sub <- G[frecuencia == fr & sexo == sx]
  if (nrow(sub) >= N_MIN_CELDA) return(list(datos = sub, fallback = "frecuencia_sexo"))
  list(datos = G[frecuencia == fr], fallback = "frecuencia")
}

res <- lapply(seq_len(nrow(celdas)), function(i) {
  fr <- celdas$frecuencia[i]; sx <- celdas$sexo[i]; ge <- celdas$grupo_etario[i]
  r <- resolver_celda(fr, sx, ge)
  d <- r$datos
  q <- cuantil_pond(d$cig_dia, d$peso_rel, GRILLA_U)
  list(
    cuantiles = data.table(frecuencia = fr, sexo = sx, grupo_etario = ge,
                           u = GRILLA_U, cig_dia = q),
    resumen = data.table(frecuencia = fr, sexo = sx, grupo_etario = ge,
                         n_celda = G[frecuencia == fr & sexo == sx & grupo_etario == ge, .N],
                         n_usado = nrow(d), fallback = r$fallback,
                         media_pond = weighted.mean(d$cig_dia, d$peso_rel),
                         p25 = cuantil_pond(d$cig_dia, d$peso_rel, 0.25),
                         p50 = cuantil_pond(d$cig_dia, d$peso_rel, 0.50),
                         p75 = cuantil_pond(d$cig_dia, d$peso_rel, 0.75))
  )
})
CUANT <- rbindlist(lapply(res, `[[`, "cuantiles"))
RESUMEN <- rbindlist(lapply(res, `[[`, "resumen"))

log_msg("Celdas: %d. Resueltas con datos propios: %d; con respaldo frecuencia x sexo: %d; con respaldo frecuencia: %d",
        nrow(RESUMEN), RESUMEN[fallback == "celda_completa", .N],
        RESUMEN[fallback == "frecuencia_sexo", .N], RESUMEN[fallback == "frecuencia", .N])
log_msg("%s", paste(capture.output(print(RESUMEN[, .(frecuencia, sexo, grupo_etario, n_celda, fallback,
                                                      media = round(media_pond, 2), p25 = round(p25, 2),
                                                      p50 = round(p50, 2), p75 = round(p75, 2))])), collapse = "\n"))

fwrite(CUANT, file.path(out_dir, "56_intensidad_cuantiles_celda.csv"))
fwrite(RESUMEN, file.path(out_dir, "56_intensidad_celdas_resumen.csv"))

log_msg("\n--- 2bis) Precio real de referencia de la distribucion ---")
panel_fiscal <- fread(file.path(out_dir, "41_panel_fiscal_anual.csv"))
n_ola <- G[, .(n = .N), by = anio_encuesta]
n_ola <- merge(n_ola, panel_fiscal[, .(anio_encuesta = anio, p_reales)], by = "anio_encuesta")
PRECIO_REF <- weighted.mean(n_ola$p_reales, n_ola$n)
log_msg("Olas y precios reales: %s", paste(sprintf("%d (n=%d, p=%.2f)", n_ola$anio_encuesta, n_ola$n,
                                                    n_ola$p_reales), collapse = " | "))
log_msg("Precio real de referencia (media ponderada por n): %.3f", PRECIO_REF)
fwrite(data.table(concepto = "precio_real_referencia_intensidad", valor = PRECIO_REF,
                  detalle = paste(sprintf("%d:n=%d,p=%.2f", n_ola$anio_encuesta, n_ola$n, n_ola$p_reales),
                                  collapse = "; ")),
       file.path(out_dir, "56_intensidad_precio_referencia.csv"))

log_msg("\n--- 3) Chequeos ---")

chk1 <- merge(CUANT[, .(media_grilla = mean(cig_dia)), by = .(frecuencia, sexo, grupo_etario)],
              RESUMEN[, .(frecuencia, sexo, grupo_etario, media_pond)],
              by = c("frecuencia", "sexo", "grupo_etario"))
chk1[, dif_pct := 100 * (media_grilla / media_pond - 1)]
log_msg("3.1 Media de la grilla de cuantiles vs. media ponderada de la celda: desvio max %.2f%%, medio %.2f%%",
        max(abs(chk1$dif_pct)), mean(abs(chk1$dif_pct)))

no_mono <- CUANT[, .(ok = !is.unsorted(cig_dia)), by = .(frecuencia, sexo, grupo_etario)][ok == FALSE]
log_msg("3.2 Celdas con funcion cuantil no monotona (deberia ser 0): %d", nrow(no_mono))

r15 <- fread(file.path(out_dir, "35_intensidad_condicional_gats.csv"))
r15 <- r15[nivel == "frecuencia_sexo_edad" & celda_chica == FALSE,
           .(media_r15 = weighted.mean(media_pond, n_pond), n_r15 = sum(n)),
           by = .(frecuencia, sexo, grupo_etario)]
chk3 <- merge(RESUMEN[fallback == "celda_completa", .(frecuencia, sexo, grupo_etario, media_pond)],
              r15, by = c("frecuencia", "sexo", "grupo_etario"))
chk3[, dif_pct := 100 * (media_pond / media_r15 - 1)]
log_msg("3.3 Media por celda vs. R/15 (35_intensidad_condicional_gats.csv, promedio de las dos olas):")
log_msg("%s", paste(capture.output(print(chk3[, .(frecuencia, sexo, grupo_etario,
                                                   media = round(media_pond, 2),
                                                   media_r15 = round(media_r15, 2),
                                                   dif_pct = round(dif_pct, 2))])), collapse = "\n"))
log_msg("    Desvio maximo: %.2f%%, medio absoluto: %.2f%%.", max(abs(chk3$dif_pct)), mean(abs(chk3$dif_pct)))
log_msg("    No tiene por que ser cero: R/15 pondera con el ponderador de expansion crudo de cada")
log_msg("    ola y aca las olas se re-pesan por n efectivo, para que 2017 no quede aplastada por la")
log_msg("    escala del ponderador de 2009. Ademas R/15 descarta por ola las celdas con n<10,")
log_msg("    asi que en algunas celdas su promedio usa una sola ola y este usa las dos.")
fwrite(chk3, file.path(out_dir, "56_intensidad_chequeo_r15.csv"))

log_msg("\n=== outputs: 56_intensidad_cuantiles_celda.csv, 56_intensidad_celdas_resumen.csv, 56_intensidad_chequeo_r15.csv ===")
close(log_con)
