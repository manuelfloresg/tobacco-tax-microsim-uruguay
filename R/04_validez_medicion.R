# 04_validez_medicion.R
#
# Measurement checks on the ECH panel: proxy reporting (variable e557), cessation on the correct
# denominator, a mortality bound for the never-smoked share by cohort, and the stock identity for
# former smokers. Adds the proxy indicator and the household identifier to the panel.
# Input:  data/interim/panel_personas_15mas.csv.gz
# Output: data/interim/panel_personas_15mas_con_informante.csv.gz (not distributed),
#         output/tables/24_*.csv

library(here)
library(data.table)

here::i_am("R/04_validez_medicion.R")

log_con <- file(here::here("logs", "04_validez_medicion.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) {
  msg <- sprintf(...)
  cat(msg, "\n"); cat(msg, "\n", file = log_con)
}
log_msg("=== 04_validez_medicion.R -- %s ===", format(Sys.time()))

raw_dir <- here::here("data", "raw", "ECH")
out_dir <- here::here("output", "tables")
int_dir <- here::here("data", "interim")

estado_lab <- c(`1` = "1 nunca fumo", `2` = "2 fumador ocasional", `3` = "3 fumador diario",
                `4` = "4 exfumador reciente (<1 anio)", `5` = "5 exfumador consolidado (>=1 anio)")

grupo_etario <- function(edad) {
  cut(edad, breaks = c(15, 25, 45, 65, Inf), right = FALSE,
      labels = c("15-24", "25-44", "45-64", "65+"))
}
cohorte_5 <- function(anio_nac) pmin(pmax(floor(anio_nac / 5) * 5, 1925), 2005)

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

cfg <- list(
  list(anio = 2011, arch = "ech11_2011.dta", fmt = "dta", esq = "1a4_prod",
       peso = "pesoano", hid = "numero", tiene_e557 = FALSE,
       freq = c("e190_1_0", "e190_2_1", "e190_3_1"),
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2014, arch = "ech14_2014.dta", fmt = "dta", esq = "1a4_prod",
       peso = "pesoano", hid = "numero", tiene_e557 = TRUE,
       freq = c("e190_1_10", "e190_2_1", "e190_3_1"),
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2017, arch = "ech17_2017.dta", fmt = "dta", esq = "1a4_prod",
       peso = "pesoano", hid = "numero", tiene_e557 = TRUE,
       freq = c("e190_1_10", "e190_2_1", "e190_3_1"),
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2018, arch = "ech18_2018.dta", fmt = "dta", esq = "1a4_prod",
       peso = "pesoano", hid = "numero", tiene_e557 = TRUE,
       freq = c("e190_1_0", "e190_2_1", "e190_3_1"),
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2019, arch = "ech19_2019.dta", fmt = "dta", esq = "2a6",
       peso = "pesoano", hid = "numero", tiene_e557 = TRUE, freq = NULL,
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2021, arch = "ech21_2021.dta", fmt = "dta", esq = "2a6",
       peso = "w_sem", hid = "id", tiene_e557 = TRUE, freq = NULL,
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2022, arch = "ech22_2022.dta", fmt = "dta", esq = "2a6",
       peso = "w_ano", hid = "id", tiene_e557 = TRUE, freq = NULL,
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2023, arch = "ech23_impl_2023.dta", fmt = "dta", esq = "2a6",
       peso = "w_ano", hid = "id", tiene_e557 = TRUE, freq = NULL,
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2024, arch = "ech24_2024.dta", fmt = "dta", esq = "2a6",
       peso = "w_ano", hid = "id", tiene_e557 = TRUE, freq = NULL,
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2025, arch = "ECH_2025_implantacion.csv", fmt = "csv", esq = "2a6",
       peso = "W_ANO", hid = "ID", tiene_e557 = TRUE, freq = NULL,
       y_tot = "YDA_SVL", y_vl = NA_character_, n_hog = "HT19")
)

paneles <- list(); diag <- list()

for (k in cfg) {
  log_msg("--- %d: %s (e557=%s) ---", k$anio, k$arch, k$tiene_e557)
  cols <- c("e26", "e27", "e190", "mes", k$peso, k$hid, "nper", k$y_tot, k$n_hog)
  if (!is.na(k$y_vl)) cols <- c(cols, k$y_vl)
  if (!is.null(k$freq)) cols <- c(cols, k$freq)
  if (k$tiene_e557) cols <- c(cols, "e557")
  cols <- unique(cols)

  if (k$fmt == "dta") {
    d <- haven::read_dta(file.path(raw_dir, k$arch), col_select = dplyr::all_of(cols))
  } else {
    d <- data.table::fread(file.path(raw_dir, k$arch), select = cols, encoding = "Latin-1")
  }
  setDT(d)
  setnames(d, c(k$peso, k$hid, k$y_tot, k$n_hog), c("peso", "hogar_id", "y_tot", "n_hog"))
  if (!is.na(k$y_vl)) setnames(d, k$y_vl, "y_vl") else d[, y_vl := 0]

  numvars <- c("e26", "e27", "e190", "mes", "peso", "y_tot", "y_vl", "n_hog", "nper")
  if (k$tiene_e557) numvars <- c(numvars, "e557")
  for (v in numvars) d[, (v) := as.numeric(get(v))]
  if (!k$tiene_e557) d[, e557 := NA_real_]

  d[, `:=`(y_tot_h = max(y_tot, na.rm = TRUE), y_vl_h = max(y_vl, na.rm = TRUE),
           n_hog_h = max(n_hog, na.rm = TRUE)), by = hogar_id]
  d[, ypc := (y_tot_h - y_vl_h) / n_hog_h]
  d[!is.finite(ypc) | ypc <= 0, ypc := NA_real_]

  if (k$esq == "2a6") {
    d[, estado := fcase(e190 == 4, 1, e190 == 6, 2, e190 == 5, 3,
                        e190 == 2, 4, e190 == 3, 5, default = NA_real_)]
  } else {
    fr <- k$freq
    d[, diario := Reduce(`|`, lapply(fr, function(v) !is.na(get(v)) & get(v) == 1))]
    d[, ocas   := Reduce(`|`, lapply(fr, function(v) !is.na(get(v)) & get(v) == 2))]
    d[, estado := fcase(e190 == 4, 1, e190 == 1 & diario, 3,
                        e190 == 1 & ocas & !diario, 2,
                        e190 == 2, 4, e190 == 3, 5, default = NA_real_)]
  }

  b <- d[e27 >= 15 & !is.na(estado) & !is.na(peso) & peso > 0]
  b[, tercil := terciles_pond(ypc, peso), by = mes]
  b[, `:=`(sexo = fcase(e26 == 1, "varon", e26 == 2, "mujer", default = "otro/NA"),
           anio = k$anio, anio_nac = k$anio - e27,
           directo = fcase(is.na(e557), NA, e557 == 1, TRUE, !is.na(e557), FALSE))]
  b[, `:=`(cohorte = cohorte_5(anio_nac), grupo_etario = grupo_etario(e27))]

  paneles[[length(paneles) + 1]] <-
    b[, .(anio, cohorte, anio_nac, edad = e27, grupo_etario, sexo, tercil, estado, peso, ypc, directo,
          hogar_id)]

  diag[[length(diag) + 1]] <- data.table(anio = k$anio, n_base = nrow(b),
                                          tiene_e557 = k$tiene_e557,
                                          n_directo = b[directo == TRUE, .N],
                                          n_proxy = b[directo == FALSE, .N])
  log_msg("    base=%d directo=%d proxy=%d", nrow(b), b[directo == TRUE, .N], b[directo == FALSE, .N])
}

P <- rbindlist(paneles)
P[, estado_lab := estado_lab[as.character(estado)]]
fwrite(P, file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))
log_msg("Panel con informante: %d filas -> data/interim/panel_personas_15mas_con_informante.csv.gz", nrow(P))

log_msg("\n--- PARTE 1: informante ---")

disp <- rbindlist(diag)
fwrite(disp, file.path(out_dir, "24_e557_disponibilidad.csv"))

Pe <- P[!is.na(directo)]
prop_anio <- Pe[, .(n = .N, prop_directo = round(sum(peso * directo) / sum(peso), 4)), by = anio]
prop_celda <- Pe[, .(n = .N, prop_directo = round(sum(peso * directo) / sum(peso), 4)),
                  by = .(anio, sexo, grupo_etario)][order(anio, sexo, grupo_etario)]
fwrite(rbindlist(list(prop_anio[, .(anio, sexo = "TOTAL", grupo_etario = NA, n, prop_directo)],
                       prop_celda), use.names = TRUE),
       file.path(out_dir, "24_proporcion_directo.csv"))

est_inf <- Pe[, .(n = .N, N_pond = sum(peso)), by = .(anio, directo, estado, estado_lab)]
est_inf[, share := round(N_pond / sum(N_pond), 4), by = .(anio, directo)]
setorder(est_inf, anio, directo, estado)
fwrite(est_inf, file.path(out_dir, "24_estados_por_informante.csv"))

nf_directo <- Pe[directo == TRUE, .(n = .N, N_pond = sum(peso),
                                     n_nunca = sum(estado == 1), N_pond_nunca = sum(peso * (estado == 1))),
                  by = .(cohorte, anio)]
nf_directo[, prop_nunca := round(N_pond_nunca / N_pond, 4)]
setorder(nf_directo, cohorte, anio)
fwrite(nf_directo, file.path(out_dir, "24_nunca_fumo_cohorte_directo.csv"))

nf_todos <- P[, .(n = .N, N_pond = sum(peso), N_pond_nunca = sum(peso * (estado == 1))), by = .(cohorte, anio)]
nf_todos[, prop_nunca := round(N_pond_nunca / N_pond, 4)]
setorder(nf_todos, cohorte, anio)
fwrite(nf_todos, file.path(out_dir, "24_nunca_fumo_cohorte_todos.csv"))

cesacion_anio <- function(dt) {
  x <- dt[estado %in% 2:4, .(N_pond = sum(peso)), by = .(anio, estado)]
  w <- dcast(x, anio ~ estado, value.var = "N_pond", fill = 0)
  setnames(w, as.character(2:4), c("ocasional", "diario", "ex_reciente"), skip_absent = TRUE)
  for (v in c("ocasional", "diario", "ex_reciente")) if (!(v %in% names(w))) w[, (v) := 0]
  w[, expuestos := ocasional + diario + ex_reciente]
  w[, tasa_cesacion := round(ex_reciente / expuestos, 4)]
  w[order(anio)]
}
tasa_todos <- cesacion_anio(P)[, .(anio, tasa_cesacion_todos = tasa_cesacion)]
tasa_directo <- cesacion_anio(Pe[directo == TRUE])[, .(anio, tasa_cesacion_directo = tasa_cesacion)]
comp_cesacion <- merge(tasa_todos, tasa_directo, by = "anio", all = TRUE)
fwrite(comp_cesacion, file.path(out_dir, "24_cesacion_directo.csv"))
log_msg("Parte 1 lista. Anios sin e557: %s", paste(disp[tiene_e557 == FALSE]$anio, collapse = ","))

log_msg("\n--- PARTE 2: cesacion / denominador correcto ---")

comp <- P[!is.na(tercil) & estado %in% c(2, 3, 4), .(
  n = .N, N_pond = sum(peso)
), by = .(anio, grupo_etario, sexo, tercil, estado)]
wide <- dcast(comp, anio + grupo_etario + sexo + tercil ~ estado, value.var = c("n", "N_pond"), fill = 0)
setnames(wide,
         old = c("n_2", "n_3", "n_4", "N_pond_2", "N_pond_3", "N_pond_4"),
         new = c("n_ocasional", "n_diario", "n_ex_reciente", "Npond_ocasional", "Npond_diario", "Npond_ex_reciente"),
         skip_absent = TRUE)
for (v in c("n_ocasional", "n_diario", "n_ex_reciente", "Npond_ocasional", "Npond_diario", "Npond_ex_reciente")) {
  if (!(v %in% names(wide))) wide[, (v) := 0]
}
wide[, n_expuestos := n_ocasional + n_diario + n_ex_reciente]
wide[, Npond_expuestos := Npond_ocasional + Npond_diario + Npond_ex_reciente]
wide[, tasa_cesacion := round(Npond_ex_reciente / Npond_expuestos, 4)]
setorder(wide, anio, grupo_etario, sexo, tercil)
fwrite(wide, file.path(out_dir, "24_cesacion_denominador_correcto.csv"))

por_edad <- P[!is.na(tercil) & estado %in% c(2, 3, 4), .(N_pond = sum(peso)), by = .(anio, grupo_etario, estado)]
wide_edad <- dcast(por_edad, anio + grupo_etario ~ estado, value.var = "N_pond", fill = 0)
setnames(wide_edad, old = c("2", "3", "4"), new = c("ocasional", "diario", "ex_reciente"), skip_absent = TRUE)
for (v in c("ocasional", "diario", "ex_reciente")) if (!(v %in% names(wide_edad))) wide_edad[, (v) := 0]
wide_edad[, expuestos := ocasional + diario + ex_reciente]
wide_edad[, tasa_cesacion := round(ex_reciente / expuestos, 4)]
setorder(wide_edad, grupo_etario, anio)
fwrite(wide_edad, file.path(out_dir, "24_cesacion_por_grupo_etario.csv"))
log_msg("Parte 2 lista.")

log_msg("\n--- PARTE 3: cota de mortalidad ---")

cota_mortalidad <- function(dt) {
  tab <- dt[, .(N = sum(peso), N1 = sum(peso * (estado == 1))), by = .(cohorte, anio)]
  tab[, N_resto := N - N1]
  setorder(tab, cohorte, anio)
  tab[, `:=`(N_prev = shift(N), N1_prev = shift(N1), N_resto_prev = shift(N_resto), anio_prev = shift(anio)),
      by = cohorte]
  tab <- tab[!is.na(N_prev)]
  tab[, atricion := pmax(0, N_prev - N)]
  tab[, atricion_no_never := pmin(atricion, N_resto_prev)]
  tab[, resto_atricion := atricion - atricion_no_never]
  tab[, N1_max_sobrevive := N1_prev - resto_atricion]
  tab[, cota_proporcion := N1_max_sobrevive / N]
  tab[, proporcion_observada := N1 / N]
  tab[, exceso := round(proporcion_observada - cota_proporcion, 4)]
  tab[, `:=`(N = round(N, 1), N1 = round(N1, 1), N_prev = round(N_prev, 1), N1_prev = round(N1_prev, 1),
             atricion = round(atricion, 1), cota_proporcion = round(cota_proporcion, 4),
             proporcion_observada = round(proporcion_observada, 4))]
  tab[, .(cohorte, anio_prev, anio, N_prev, N1_prev, N, N1, atricion,
          proporcion_observada, cota_proporcion, exceso)]
}

cota_todos <- cota_mortalidad(P)
fwrite(cota_todos, file.path(out_dir, "24_cota_mortalidad_cohortes.csv"))
fwrite(cota_todos[, .(cohorte, anio_prev, anio, exceso, mortalidad_descartada = exceso > 0)],
       file.path(out_dir, "24_exceso_cota_mortalidad.csv"))

cota_directo <- cota_mortalidad(Pe[directo == TRUE])
fwrite(cota_directo, file.path(out_dir, "24_cota_mortalidad_directo.csv"))

comp_exceso <- merge(
  cota_todos[, .(cohorte, anio_prev, anio, exceso_todos = exceso)],
  cota_directo[, .(cohorte, anio_prev, anio, exceso_directo = exceso)],
  by = c("cohorte", "anio_prev", "anio"), all = TRUE
)
comp_exceso[, contribucion_informante := round(exceso_todos - exceso_directo, 4)]
fwrite(comp_exceso, file.path(out_dir, "24_exceso_comparacion_informante.csv"))

resid <- cota_directo[, .(cohorte, anio_prev, anio, exceso_directo = exceso)]
resid[, edad_cohorte_en_anio := anio - cohorte]
resid_cohorte <- resid[, .(exceso_medio = round(mean(exceso_directo, na.rm = TRUE), 4),
                            edad_media = round(mean(edad_cohorte_en_anio), 1),
                            n_pares = .N), by = cohorte][order(cohorte)]
fwrite(resid, file.path(out_dir, "24_reclasificacion_residual.csv"))
fwrite(resid_cohorte, file.path(out_dir, "24_reclasificacion_residual_por_cohorte.csv"))
cor_edad_exceso <- resid_cohorte[n_pares > 0 & is.finite(exceso_medio), cor(edad_media, exceso_medio, use = "complete.obs")]
log_msg("Parte 3 lista. Correlacion (exceso residual directo) x (edad media de la cohorte) = %.3f", cor_edad_exceso)

log_msg("\n--- PARTE 4: identidad de stocks de exfumadores ---")

pares <- list(c(2017, 2018), c(2018, 2019), c(2021, 2022), c(2022, 2023), c(2023, 2024), c(2024, 2025))

stock_por <- function(dt, by_vars) {
  dt[, .(E1 = sum(peso * (estado == 4)), E2 = sum(peso * (estado == 5))), by = by_vars]
}

id_nac <- list(); id_edad <- list()
stock_nac_anio <- stock_por(P, "anio")
stock_edad_anio <- stock_por(P, c("anio", "grupo_etario"))

for (p in pares) {
  t0 <- p[1]; t1 <- p[2]
  s0 <- stock_nac_anio[anio == t0]; s1 <- stock_nac_anio[anio == t1]
  E2_esperado <- s0$E1 + s0$E2
  E2_obs <- s1$E2
  id_nac[[length(id_nac) + 1]] <- data.table(
    anio_t0 = t0, anio_t1 = t1, E1_t0 = round(s0$E1, 1), E2_t0 = round(s0$E2, 1),
    E2_esperado = round(E2_esperado, 1), E2_observado = round(E2_obs, 1),
    atricion_implicita = round(1 - E2_obs / E2_esperado, 4)
  )
  for (ge in levels(P$grupo_etario)) {
    a0 <- stock_edad_anio[anio == t0 & grupo_etario == ge]
    a1 <- stock_edad_anio[anio == t1 & grupo_etario == ge]
    if (nrow(a0) == 0 || nrow(a1) == 0) next
    E2e <- a0$E1 + a0$E2
    id_edad[[length(id_edad) + 1]] <- data.table(
      anio_t0 = t0, anio_t1 = t1, grupo_etario = ge,
      E1_t0 = round(a0$E1, 1), E2_t0 = round(a0$E2, 1),
      E2_esperado = round(E2e, 1), E2_observado = round(a1$E2, 1),
      atricion_implicita = round(1 - a1$E2 / E2e, 4)
    )
  }
}
id_nac_dt <- rbindlist(id_nac)
id_edad_dt <- rbindlist(id_edad)
fwrite(id_nac_dt, file.path(out_dir, "24_identidad_exfumadores.csv"))
fwrite(id_edad_dt, file.path(out_dir, "24_identidad_exfumadores_por_edad.csv"))
log_msg("Parte 4 lista.")

log_msg("\n=== outputs en output/tables/: 24_*.csv (14 tablas) ===")
close(log_con)

print(id_nac_dt)
