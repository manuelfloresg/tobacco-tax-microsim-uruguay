# 15_deriva_intensidad.R
#
# Cigarettes per day conditional on frequency, sex, age group and income in the five surveys that
# measure intensity (STEPS 2006 and 2013, GATS 2009 and 2017, ENS 2014). Comparisons are made only
# within the same instrument.
# Input:  data/raw/STEPS/, data/raw/GATS/, data/raw/ENS/
# Output: output/tables/35_*.csv

library(here)
library(data.table)
library(haven)
library(survey)

here::i_am("R/15_deriva_intensidad.R")

log_con <- file(here::here("logs", "15_deriva_intensidad.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 15_deriva_intensidad.R -- %s ===", format(Sys.time()))

raw_steps <- here::here("data", "raw", "STEPS")
raw_gats  <- here::here("data", "raw", "GATS")
raw_ens   <- here::here("data", "raw", "ENS")
out_dir   <- here::here("output", "tables")

grupo_etario <- function(edad) {
  cut(edad, breaks = c(15, 25, 45, 65, Inf), right = FALSE,
      labels = c("15-24", "25-44", "45-64", "65+"))
}

terciles_pond <- function(v, w) {
  out <- rep(NA_real_, length(v))
  ok <- !is.na(v) & !is.na(w) & w > 0
  if (sum(ok) < 30) return(out)
  vv <- v[ok]; ww <- w[ok]
  o  <- order(vv, method = "radix")
  cw <- cumsum(ww[o]) / sum(ww)
  c1 <- vv[o][which(cw >= 1/3)[1]]
  c2 <- vv[o][which(cw >= 2/3)[1]]
  out[ok] <- fifelse(vv <= c1, 1, fifelse(vv <= c2, 2, 3))
  out
}

media_pond <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) < 5) return(c(media = NA_real_, se = NA_real_, n = sum(ok)))
  xx <- x[ok]; ww <- w[ok]
  m  <- sum(xx * ww) / sum(ww)
  v  <- sum(ww * (xx - m)^2) / sum(ww)
  neff <- sum(ww)^2 / sum(ww^2)
  c(media = m, se = sqrt(v / neff), n = sum(ok))
}

a_sexo <- function(x) {
  s <- tolower(trimws(as.character(x)))
  fifelse(s %in% c("men", "male", "varon", "hombre", "1"), "varon",
  fifelse(s %in% c("women", "female", "mujer", "2"), "mujer", NA_character_))
}

log_msg("\n--- 1) Carga STEPS 2006 y 2013 ---")

cargar_steps <- function(path, anio_encuesta) {
  d <- haven::read_dta(path)
  setDT(d)
  if (anio_encuesta == 2006) {
    d[, sexo := a_sexo(haven::as_factor(sex))]
    d[, edad := as.numeric(age)]
    d[, ingreso_ord := as.numeric(x1)]
    d[, ingreso_ord := fifelse(ingreso_ord %in% 1:5, ingreso_ord, NA_real_)]
    d[, exfumador_diario_pasado := as.numeric(t6) == 1]
    d[, peso := 1]
  } else {
    d[, sexo := a_sexo(haven::as_factor(c1))]
    d[, edad := as.numeric(age)]
    d[, ingreso_ord := as.numeric(x80)]
    d[, ingreso_ord := fifelse(ingreso_ord %in% 1:5, ingreso_ord, NA_real_)]
    d[, exfumador_diario_pasado := as.numeric(x10) == 1]
    d[, peso := as.numeric(weightsdi)]
  }
  d[, fuma_actual := as.numeric(t1) == 1]
  d[, fuma_diario  := as.numeric(t2) == 1]
  d[, frecuencia := fifelse(!fuma_actual, NA_character_,
                    fifelse(fuma_diario, "diario", "ocasional"))]
  d[, anio_encuesta := anio_encuesta]
  d[, grupo_etario := grupo_etario(edad)]
  d[, nse := terciles_pond(ingreso_ord, peso)]
  for (v in c("t5a", "t5b", "t5c", "t5d", "t5e")) d[, (v) := as.numeric(get(v))]
  cols_out <- c("sexo", "edad", "grupo_etario", "nse", "frecuencia", "peso",
                "anio_encuesta", "exfumador_diario_pasado",
                "t5a", "t5b", "t5c", "t5d", "t5e")
  d[edad >= 15, ..cols_out]
}

s06 <- cargar_steps(file.path(raw_steps, "steps_2006.dta"), 2006)
s13 <- cargar_steps(file.path(raw_steps, "steps_2013.dta"), 2013)
log_msg("STEPS 2006: %d obs 15+, %d fumadores actuales", nrow(s06), sum(!is.na(s06$frecuencia)))
log_msg("STEPS 2013: %d obs 15+, %d fumadores actuales", nrow(s13), sum(!is.na(s13$frecuencia)))

prod_lab_steps <- c(t5a = "manufacturado", t5b = "armado_mano", t5c = "pipa",
                     t5d = "puro", t5e = "otro")
steps_a_largo <- function(d) {
  filas <- list()
  for (v in names(prod_lab_steps)) {
    dd <- copy(d)
    cant <- as.numeric(dd[[v]])
    dd[, cantidad_dia := fifelse(cant >= 0 & cant < 95, cant, NA_real_)]
    dd[, producto := prod_lab_steps[[v]]]
    sub <- dd[!is.na(cantidad_dia) & cantidad_dia > 0 & !is.na(frecuencia),
              .(anio_encuesta, sexo, edad, grupo_etario, nse, frecuencia, peso,
                producto, cantidad_dia)]
    if (nrow(sub) > 0) filas[[v]] <- sub
  }
  rbindlist(filas)
}
steps_largo <- rbindlist(list(steps_a_largo(s06), steps_a_largo(s13)))
log_msg("STEPS formato largo (obs producto x persona, cantidad>0): %d filas", nrow(steps_largo))

steps_total <- rbindlist(list(s06, s13), fill = TRUE)[!is.na(frecuencia)]
cols5 <- names(prod_lab_steps)
for (v in cols5) {
  cant <- as.numeric(steps_total[[v]])
  steps_total[, (paste0(v, "_ok")) := fifelse(cant >= 0 & cant < 95, cant, NA_real_)]
}
steps_total[, intensidad_total_dia := rowSums(.SD, na.rm = TRUE),
            .SDcols = paste0(cols5, "_ok")]
steps_total[, n_productos_reportados := rowSums(!is.na(.SD) & .SD > 0),
            .SDcols = paste0(cols5, "_ok")]
steps_total[n_productos_reportados == 0, intensidad_total_dia := NA_real_]

log_msg("STEPS intensidad total por persona: %d con valor no-missing",
        sum(!is.na(steps_total$intensidad_total_dia)))

log_msg("\n--- 2) Carga GATS 2009 y 2017 (con B06*/B10*) ---")

prod_lab_gats <- c(A = "manufacturado", B = "armado_mano", C = "picadura_naco",
                    D = "pipa", E = "puro", F = "pipa_agua", G = "otro")
assets_gats <- paste0("A06", LETTERS[1:21])

cargar_gats_intensidad <- function(path, anio_encuesta) {
  nms0 <- names(haven::read_dta(path, n_max = 1))
  agevar <- nms0[match("age", tolower(nms0))]
  cols <- c("A01", agevar, "gatsweight", "gatsstrata", "gatscluster",
            "B01", paste0("B06", names(prod_lab_gats)), paste0("B10", names(prod_lab_gats)),
            intersect(assets_gats, nms0))
  d <- haven::read_dta(path, col_select = dplyr::all_of(cols))
  setDT(d)
  setnames(d, agevar, "edad"); setnames(d, "gatsweight", "peso")
  d[, edad := as.numeric(edad)]; d[, peso := as.numeric(peso)]
  d[, gatsstrata := as.numeric(gatsstrata)]; d[, gatscluster := as.numeric(gatscluster)]
  d[, sexo := fifelse(as.numeric(A01) == 1, "varon", fifelse(as.numeric(A01) == 2, "mujer", NA_character_))]
  d[, B01n := as.numeric(B01)]
  d[, frecuencia := fifelse(B01n == 1, "diario", fifelse(B01n == 2, "ocasional", NA_character_))]
  d[, anio_encuesta := anio_encuesta]
  d[, grupo_etario := grupo_etario(edad)]
  a_cols <- intersect(assets_gats, names(d))
  for (v in a_cols) d[, (v) := as.numeric(get(v))]
  d[, indice_activos := rowSums(.SD == 1, na.rm = TRUE), .SDcols = a_cols]
  d[, nse := terciles_pond(indice_activos, peso)]
  for (v in c(paste0("B06", names(prod_lab_gats)), paste0("B10", names(prod_lab_gats)))) {
    d[, (v) := as.numeric(get(v))]
  }
  cols_out <- c("sexo", "edad", "grupo_etario", "nse", "frecuencia", "peso",
                "gatsstrata", "gatscluster", "anio_encuesta",
                paste0("B06", names(prod_lab_gats)), paste0("B10", names(prod_lab_gats)))
  d[edad >= 15, ..cols_out]
}

g09 <- cargar_gats_intensidad(file.path(raw_gats, "gats_2009.dta"), 2009)
g17 <- cargar_gats_intensidad(file.path(raw_gats, "gats_2017.dta"), 2017)
log_msg("GATS 2009: %d obs 15+, %d fumadores actuales", nrow(g09), sum(!is.na(g09$frecuencia)))
log_msg("GATS 2017: %d obs 15+, %d fumadores actuales", nrow(g17), sum(!is.na(g17$frecuencia)))

gats_a_largo <- function(d) {
  filas <- list()
  for (letra in names(prod_lab_gats)) {
    vd <- paste0("B06", letra); vo <- paste0("B10", letra)
    dd <- copy(d)
    cant_d <- fifelse(dd[[vd]] >= 0 & dd[[vd]] < 700, dd[[vd]], NA_real_)
    cant_o <- fifelse(dd[[vo]] >= 0 & dd[[vo]] < 700, dd[[vo]] / 7, NA_real_)
    dd[, cantidad_dia := fifelse(frecuencia == "diario", cant_d,
                          fifelse(frecuencia == "ocasional", cant_o, NA_real_))]
    dd[, producto := prod_lab_gats[[letra]]]
    sub <- dd[!is.na(cantidad_dia) & cantidad_dia > 0 & !is.na(frecuencia),
              .(anio_encuesta, sexo, edad, grupo_etario, nse, frecuencia, peso, gatsstrata, gatscluster,
                producto, cantidad_dia)]
    if (nrow(sub) > 0) filas[[letra]] <- sub
  }
  rbindlist(filas)
}
gats_largo <- rbindlist(list(gats_a_largo(g09), gats_a_largo(g17)))
log_msg("GATS formato largo (obs producto x persona, cantidad>0): %d filas", nrow(gats_largo))

gats_total <- rbindlist(list(g09, g17))[!is.na(frecuencia)]
for (letra in names(prod_lab_gats)) {
  vd <- paste0("B06", letra); vo <- paste0("B10", letra)
  cant_d <- fifelse(gats_total[[vd]] >= 0 & gats_total[[vd]] < 700, gats_total[[vd]], NA_real_)
  cant_o <- fifelse(gats_total[[vo]] >= 0 & gats_total[[vo]] < 700, gats_total[[vo]] / 7, NA_real_)
  gats_total[, (paste0(letra, "_ok")) := fifelse(frecuencia == "diario", cant_d,
                                          fifelse(frecuencia == "ocasional", cant_o, NA_real_))]
}
gats_total[, intensidad_total_dia := rowSums(.SD, na.rm = TRUE), .SDcols = paste0(names(prod_lab_gats), "_ok")]
gats_total[, n_productos_reportados := rowSums(!is.na(.SD) & .SD > 0), .SDcols = paste0(names(prod_lab_gats), "_ok")]
gats_total[n_productos_reportados == 0, intensidad_total_dia := NA_real_]

log_msg("GATS intensidad total por persona: %d con valor no-missing",
        sum(!is.na(gats_total$intensidad_total_dia)))

log_msg("\n--- 3) Carga ENS 2014 (punto suelto) ---")

cargar_ens <- function(path) {
  d <- haven::read_sav(path)
  setDT(d)
  d[, sexo := fifelse(as.numeric(pssexo_ok) == 1, "varon",
             fifelse(as.numeric(pssexo_ok) == 2, "mujer", NA_character_))]
  d[, edad := as.numeric(psedad_ok)]
  d[, peso := as.numeric(w_persona)]
  d[, ahv96n := as.numeric(ahv96)]
  d[, frecuencia := fifelse(ahv96n == 1, "diario", fifelse(ahv96n == 2, "ocasional", NA_character_))]
  d[, cant_diario   := as.numeric(ahv101a1)]
  d[, cant_semanal  := as.numeric(ahv101b1)]
  n_implausible <- sum(d$cant_diario > 90 & d$cant_diario < 999, na.rm = TRUE)
  log_msg("ENS ahv101a1: %d respuestas > 90 cig/dia excluidas como implausibles (de %d no-missing)",
          n_implausible, sum(!is.na(d$cant_diario) & d$cant_diario < 999))
  d[, cant_diario   := fifelse(cant_diario  >= 0 & cant_diario  <= 90, cant_diario, NA_real_)]
  d[, cant_semanal  := fifelse(cant_semanal >= 0 & cant_semanal < 999, cant_semanal, NA_real_)]
  d[, cantidad_dia := fifelse(frecuencia == "diario", cant_diario,
                      fifelse(frecuencia == "ocasional", cant_semanal / 7, NA_real_))]
  d[, grupo_etario := grupo_etario(edad)]
  d[edad >= 15, .(sexo, edad, grupo_etario, frecuencia, peso, cantidad_dia)]
}

ens <- cargar_ens(file.path(raw_ens, "ens_2014.sav"))
log_msg("ENS 2014: %d obs 15+, %d fumadores actuales, %d con intensidad no-missing",
        nrow(ens), sum(!is.na(ens$frecuencia)), sum(!is.na(ens$cantidad_dia)))

log_msg("\n--- 4) Distribuciones condicionales por fuente ---")

resumen_intensidad <- function(dt, by_cols, val_col = "cantidad_dia", w_col = "peso", n_min = 10) {
  dt2 <- dt[!is.na(get(val_col))]
  out <- dt2[, {
    x <- get(val_col); w <- get(w_col)
    n <- length(x)
    if (n < n_min || all(is.na(w)) || sum(w, na.rm = TRUE) <= 0) {
      list(n = n, n_pond = NA_real_, media_pond = NA_real_,
           mediana = NA_real_, p25 = NA_real_, p75 = NA_real_, celda_chica = n < n_min)
    } else {
      mp <- media_pond(x, w)
      list(n = n, n_pond = round(sum(w, na.rm = TRUE)), media_pond = unname(mp["media"]),
           mediana = median(x), p25 = quantile(x, .25, names = FALSE),
           p75 = quantile(x, .75, names = FALSE), celda_chica = n < n_min)
    }
  }, by = by_cols]
  out
}

niveles_totales <- function(dt, fuente, n_min) {
  a <- resumen_intensidad(dt, c("anio_encuesta", "frecuencia", "sexo", "grupo_etario"),
                           val_col = "intensidad_total_dia", n_min = n_min)
  a[, nivel := "frecuencia_sexo_edad"]; a[, nse := NA_real_]
  b <- resumen_intensidad(dt[!is.na(nse)], c("anio_encuesta", "frecuencia", "sexo", "grupo_etario", "nse"),
                           val_col = "intensidad_total_dia", n_min = n_min)
  b[, nivel := "frecuencia_sexo_edad_nse"]
  cols_comunes <- c("anio_encuesta", "frecuencia", "sexo", "grupo_etario", "nse", "nivel",
                     "n", "n_pond", "media_pond", "mediana", "p25", "p75", "celda_chica")
  out <- rbindlist(list(a[, ..cols_comunes], b[, ..cols_comunes]))
  out[, fuente := fuente]
  out[]
}

niveles_producto <- function(dt_largo, fuente, n_min) {
  a <- resumen_intensidad(dt_largo, c("anio_encuesta", "frecuencia", "producto"), n_min = n_min)
  a[, nivel := "frecuencia_producto"]
  a[, fuente := fuente]
  a[]
}

dist_steps <- niveles_totales(steps_total, "STEPS", n_min = 10)
dist_gats  <- niveles_totales(gats_total,  "GATS",  n_min = 20)
dist_steps_prod <- niveles_producto(steps_largo, "STEPS", n_min = 10)
dist_gats_prod  <- niveles_producto(gats_largo,  "GATS",  n_min = 20)

log_msg("STEPS: %d celdas frecuencia x sexo x edad (%d chicas), %d celdas +nse (%d chicas)",
        sum(dist_steps$nivel == "frecuencia_sexo_edad"), sum(dist_steps$nivel == "frecuencia_sexo_edad" & dist_steps$celda_chica),
        sum(dist_steps$nivel == "frecuencia_sexo_edad_nse"), sum(dist_steps$nivel == "frecuencia_sexo_edad_nse" & dist_steps$celda_chica))
log_msg("GATS: %d celdas frecuencia x sexo x edad (%d chicas), %d celdas +nse (%d chicas)",
        sum(dist_gats$nivel == "frecuencia_sexo_edad"), sum(dist_gats$nivel == "frecuencia_sexo_edad" & dist_gats$celda_chica),
        sum(dist_gats$nivel == "frecuencia_sexo_edad_nse"), sum(dist_gats$nivel == "frecuencia_sexo_edad_nse" & dist_gats$celda_chica))

dist_ens <- resumen_intensidad(ens, c("frecuencia", "sexo", "grupo_etario"), n_min = 10)
dist_ens[, nivel := "frecuencia_sexo_edad"]; dist_ens[, fuente := "ENS"]; dist_ens[, anio_encuesta := 2014]
log_msg("ENS: %d celdas frecuencia x sexo x edad (%d chicas)", nrow(dist_ens), sum(dist_ens$celda_chica))

log_msg("\n--- 5) Comparaciones dentro de instrumento ---")

composicion_diario <- function(dt, w_col = "peso") {
  dt2 <- dt[!is.na(frecuencia)]
  dt2[, es_diario := as.numeric(frecuencia == "diario")]
  dt2[, .(n = .N, n_pond = sum(get(w_col), na.rm = TRUE),
          share_diario = sum(es_diario * get(w_col), na.rm = TRUE) / sum(get(w_col), na.rm = TRUE)),
      by = .(anio_encuesta, sexo, grupo_etario)]
}

comparar_instrumento <- function(dt_total, fuente) {
  comp <- composicion_diario(dt_total)
  inten <- dt_total[frecuencia == "diario", .(n_diario = .N, n_pond_diario = sum(peso, na.rm = TRUE),
                    media_diario = { mp <- media_pond(intensidad_total_dia, peso); unname(mp["media"]) }),
                    by = .(anio_encuesta, sexo, grupo_etario)]
  anios <- sort(unique(dt_total$anio_encuesta))
  a0 <- anios[1]; a1 <- anios[2]
  wide_comp <- dcast(comp, sexo + grupo_etario ~ anio_encuesta, value.var = c("share_diario", "n", "n_pond"))
  wide_int  <- dcast(inten, sexo + grupo_etario ~ anio_encuesta, value.var = c("media_diario", "n_diario", "n_pond_diario"))
  out <- merge(wide_comp, wide_int, by = c("sexo", "grupo_etario"), all = TRUE)
  setnames(out, paste0("share_diario_", a0), "share_diario_ini"); setnames(out, paste0("share_diario_", a1), "share_diario_fin")
  setnames(out, paste0("media_diario_", a0), "media_diario_ini"); setnames(out, paste0("media_diario_", a1), "media_diario_fin")
  out[, delta_share_diario := share_diario_fin - share_diario_ini]
  out[, delta_media_diario := media_diario_fin - media_diario_ini]
  out[, `:=`(anio_inicial = a0, anio_final = a1, fuente = fuente)]
  out[]
}

comp_steps <- comparar_instrumento(steps_total, "STEPS")
comp_gats  <- comparar_instrumento(gats_total, "GATS")
log_msg("STEPS 2006->2013: delta share_diario medio = %.4f, delta media diario (cig-eq/dia) medio = %.3f",
        mean(comp_steps$delta_share_diario, na.rm = TRUE), mean(comp_steps$delta_media_diario, na.rm = TRUE))
log_msg("GATS 2009->2017: delta share_diario medio = %.4f, delta media diario (cig-eq/dia) medio = %.3f",
        mean(comp_gats$delta_share_diario, na.rm = TRUE), mean(comp_gats$delta_media_diario, na.rm = TRUE))

log_msg("\nGATS -- robustez con diseno muestral (svydesign), nivel pais:")
gats_svy_anio <- function(anio) {
  sub <- gats_total[anio_encuesta == anio & !is.na(frecuencia)]
  sub[, es_diario := as.numeric(frecuencia == "diario")]
  des <- survey::svydesign(ids = ~gatscluster, strata = ~gatsstrata, weights = ~peso, data = sub, nest = TRUE)
  m_share <- survey::svymean(~es_diario, des)
  des_diario <- subset(des, frecuencia == "diario" & !is.na(intensidad_total_dia))
  m_int <- tryCatch(survey::svymean(~intensidad_total_dia, des_diario), error = function(e) NULL)
  list(anio = anio,
       share_diario = unname(coef(m_share)), share_ic = confint(m_share),
       media_diario = if (!is.null(m_int)) unname(coef(m_int)) else NA_real_,
       media_ic = if (!is.null(m_int)) confint(m_int) else c(NA_real_, NA_real_))
}
r09 <- gats_svy_anio(2009); r17 <- gats_svy_anio(2017)
log_msg("2009: share_diario=%.4f [%.4f,%.4f]; media_diario=%.3f [%.3f,%.3f]",
        r09$share_diario, r09$share_ic[1], r09$share_ic[2], r09$media_diario, r09$media_ic[1], r09$media_ic[2])
log_msg("2017: share_diario=%.4f [%.4f,%.4f]; media_diario=%.3f [%.3f,%.3f]",
        r17$share_diario, r17$share_ic[1], r17$share_ic[2], r17$media_diario, r17$media_ic[1], r17$media_ic[2])
gats_robustez <- data.table(
  anio_encuesta = c(2009, 2017),
  share_diario = c(r09$share_diario, r17$share_diario),
  share_ic_inf = c(r09$share_ic[1], r17$share_ic[1]), share_ic_sup = c(r09$share_ic[2], r17$share_ic[2]),
  media_diario = c(r09$media_diario, r17$media_diario),
  media_ic_inf = c(r09$media_ic[1], r17$media_ic[1]), media_ic_sup = c(r09$media_ic[2], r17$media_ic[2]))

log_msg("\n--- 6) Sintesis shift-share: intensidad condicional vs composicion ---")

shift_share_intensidad <- function(dt_total, fuente) {
  anios <- sort(unique(dt_total$anio_encuesta))
  a0 <- anios[1]; a1 <- anios[2]
  calc_anio <- function(a) {
    sub <- dt_total[anio_encuesta == a & !is.na(frecuencia)]
    n_tot_pond <- sum(sub$peso, na.rm = TRUE)
    out <- list()
    for (f in c("diario", "ocasional")) {
      s <- sub[frecuencia == f]
      share <- sum(s$peso, na.rm = TRUE) / n_tot_pond
      mp <- media_pond(s$intensidad_total_dia, s$peso)
      out[[f]] <- c(share = share, media = unname(mp["media"]))
    }
    out
  }
  v0 <- calc_anio(a0); v1 <- calc_anio(a1)
  comp_intensidad <- 0; comp_composicion <- 0
  for (f in c("diario", "ocasional")) {
    s0 <- v0[[f]]["share"]; s1 <- v1[[f]]["share"]
    m0 <- v0[[f]]["media"]; m1 <- v1[[f]]["media"]
    if (is.na(m0) || is.na(m1)) next
    comp_intensidad  <- comp_intensidad  + (s1 + s0) / 2 * (m1 - m0)
    comp_composicion <- comp_composicion + (s1 - s0) * (m1 + m0) / 2
  }
  pi0 <- sum(sapply(c("diario", "ocasional"), function(f) v0[[f]]["share"] * v0[[f]]["media"]), na.rm = TRUE)
  pi1 <- sum(sapply(c("diario", "ocasional"), function(f) v1[[f]]["share"] * v1[[f]]["media"]), na.rm = TRUE)
  data.table(fuente = fuente, anio_inicial = a0, anio_final = a1,
             intensidad_media_todos_fumadores_ini = pi0, intensidad_media_todos_fumadores_fin = pi1,
             delta_total_observado = pi1 - pi0,
             componente_intensidad_condicional = unname(comp_intensidad),
             componente_composicion = unname(comp_composicion),
             identidad_ok = abs((unname(comp_intensidad) + unname(comp_composicion)) - (pi1 - pi0)) < 1e-6)
}

sintesis_steps <- shift_share_intensidad(steps_total, "STEPS")
sintesis_gats  <- shift_share_intensidad(gats_total, "GATS")
sintesis <- rbindlist(list(sintesis_steps, sintesis_gats))
log_msg("Sintesis shift-share:\n%s", paste(capture.output(print(sintesis)), collapse = "\n"))

log_msg("\n--- 7) ENS como punto suelto ---")

ens_media_diario <- media_pond(ens[frecuencia == "diario", cantidad_dia], ens[frecuencia == "diario", peso])
steps13_media_diario <- media_pond(steps_total[anio_encuesta == 2013 & frecuencia == "diario", intensidad_total_dia],
                                    steps_total[anio_encuesta == 2013 & frecuencia == "diario", peso])
gats17_media_diario <- media_pond(gats_total[anio_encuesta == 2017 & frecuencia == "diario", intensidad_total_dia],
                                   gats_total[anio_encuesta == 2017 & frecuencia == "diario", peso])

ens_punto_suelto <- data.table(
  fuente = c("ENS 2014", "STEPS 2013", "GATS 2017"),
  media_diario = c(unname(ens_media_diario["media"]), unname(steps13_media_diario["media"]), unname(gats17_media_diario["media"])),
  se = c(unname(ens_media_diario["se"]), unname(steps13_media_diario["se"]), unname(gats17_media_diario["se"])),
  n = c(unname(ens_media_diario["n"]), unname(steps13_media_diario["n"]), unname(gats17_media_diario["n"])))
log_msg("ENS como punto suelto (nivel, no deriva):\n%s", paste(capture.output(print(ens_punto_suelto)), collapse = "\n"))
log_msg("ADVERTENCIA: la comparacion de nivel entre ENS y STEPS/GATS puede ser artefacto de cuestionario (ENS solo pregunta cigarrillos, sin distincion por producto), no evidencia de deriva temporal.")

log_msg("\n--- 8) Escritura de outputs ---")

fwrite(rbindlist(list(dist_steps)), file.path(out_dir, "35_intensidad_condicional_steps.csv"))
fwrite(rbindlist(list(dist_gats)),  file.path(out_dir, "35_intensidad_condicional_gats.csv"))
fwrite(dist_ens, file.path(out_dir, "35_intensidad_condicional_ens.csv"))
fwrite(dist_steps_prod, file.path(out_dir, "35_intensidad_por_producto_steps.csv"))
fwrite(dist_gats_prod,  file.path(out_dir, "35_intensidad_por_producto_gats.csv"))
fwrite(comp_steps, file.path(out_dir, "35_comparacion_steps_06_13.csv"))
fwrite(comp_gats, file.path(out_dir, "35_comparacion_gats_09_17.csv"))
fwrite(gats_robustez, file.path(out_dir, "35_gats_robustez_diseno_muestral.csv"))
fwrite(sintesis, file.path(out_dir, "35_descomposicion_intensidad_vs_composicion.csv"))
fwrite(ens_punto_suelto, file.path(out_dir, "35_ens_punto_suelto_comparacion.csv"))

log_msg("Outputs escritos en %s", out_dir)
close(log_con)
