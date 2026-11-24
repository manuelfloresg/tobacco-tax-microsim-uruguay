# 03_panel_cohortes.R
#
# Harmonised ECH person panel, 2011-2025, ages 15 and over: five smoking states (never, occasional,
# daily, quit <1 year, quit >=1 year), household income tertile (per capita income without imputed
# rent, deflated, tertiles within year and month of interview), sex, age and birth cohort.
# In 2011-2018 the occasional/daily split is rebuilt from frequency by product: a person is a daily
# smoker if daily for any product.
# Input:  data/raw/ECH/
# Output: data/interim/panel_personas_15mas.csv.gz (microdata, not distributed),
#         output/tables/22_*.csv (cohort panel)

library(here)
library(data.table)
library(haven)

here::i_am("R/03_panel_cohortes.R")

log_con <- file(here::here("logs", "03_panel_cohortes.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) {
  msg <- sprintf(...)
  cat(msg, "\n"); cat(msg, "\n", file = log_con)
}
log_msg("=== 03_panel_cohortes.R -- %s ===", format(Sys.time()))

raw_dir <- here::here("data", "raw", "ECH")
out_dir <- here::here("output", "tables")
int_dir <- here::here("data", "interim")
dir.create(int_dir, showWarnings = FALSE, recursive = TRUE)

estado_lab <- c(`1` = "1 nunca fumo",
                `2` = "2 fumador ocasional",
                `3` = "3 fumador diario",
                `4` = "4 exfumador reciente (<1 anio)",
                `5` = "5 exfumador consolidado (>=1 anio)")

grupo_etario <- function(edad) {
  cut(edad, breaks = c(15, 25, 45, 65, Inf), right = FALSE,
      labels = c("15-24", "25-44", "45-64", "65+"))
}

cohorte_5 <- function(anio_nac) {
  pmin(pmax(floor(anio_nac / 5) * 5, 1925), 2005)
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

cfg <- list(
  list(anio = 2011, arch = "ech11_2011.dta", fmt = "dta", esq = "1a4_prod",
       peso = "pesoano", hid = "numero",
       freq = c("e190_1_0", "e190_2_1", "e190_3_1"),
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2014, arch = "ech14_2014.dta", fmt = "dta", esq = "1a4_prod",
       peso = "pesoano", hid = "numero",
       freq = c("e190_1_10", "e190_2_1", "e190_3_1"),
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2017, arch = "ech17_2017.dta", fmt = "dta", esq = "1a4_prod",
       peso = "pesoano", hid = "numero",
       freq = c("e190_1_10", "e190_2_1", "e190_3_1"),
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2018, arch = "ech18_2018.dta", fmt = "dta", esq = "1a4_prod",
       peso = "pesoano", hid = "numero",
       freq = c("e190_1_0", "e190_2_1", "e190_3_1"),
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2019, arch = "ech19_2019.dta", fmt = "dta", esq = "2a6",
       peso = "pesoano", hid = "numero", freq = NULL,
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2021, arch = "ech21_2021.dta", fmt = "dta", esq = "2a6",
       peso = "w_sem", hid = "id", freq = NULL,
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2022, arch = "ech22_2022.dta", fmt = "dta", esq = "2a6",
       peso = "w_ano", hid = "id", freq = NULL,
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2023, arch = "ech23_impl_2023.dta", fmt = "dta", esq = "2a6",
       peso = "w_ano", hid = "id", freq = NULL,
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2024, arch = "ech24_2024.dta", fmt = "dta", esq = "2a6",
       peso = "w_ano", hid = "id", freq = NULL,
       y_tot = "ht11", y_vl = "ht13", n_hog = "ht19"),
  list(anio = 2025, arch = "ECH_2025_implantacion.csv", fmt = "csv", esq = "2a6",
       peso = "W_ANO", hid = "ID", freq = NULL,
       y_tot = "YDA_SVL", y_vl = NA_character_, n_hog = "HT19")
)

paneles <- list(); diag <- list()

for (k in cfg) {
  log_msg("--- %d: %s ---", k$anio, k$arch)
  cols <- c("e26", "e27", "e190", "mes", k$peso, k$hid, "nper", k$y_tot, k$n_hog)
  if (!is.na(k$y_vl)) cols <- c(cols, k$y_vl)
  if (!is.null(k$freq)) cols <- c(cols, k$freq)
  cols <- unique(cols)

  if (k$fmt == "dta") {
    d <- haven::read_dta(file.path(raw_dir, k$arch), col_select = dplyr::all_of(cols))
  } else {
    d <- data.table::fread(file.path(raw_dir, k$arch), select = cols, encoding = "Latin-1")
  }
  setDT(d)
  setnames(d, c(k$peso, k$hid, k$y_tot, k$n_hog), c("peso", "hogar_id", "y_tot", "n_hog"))
  if (!is.na(k$y_vl)) setnames(d, k$y_vl, "y_vl") else d[, y_vl := 0]

  for (v in c("e26", "e27", "e190", "mes", "peso", "y_tot", "y_vl", "n_hog", "nper")) {
    d[, (v) := as.numeric(get(v))]
  }
  n_reg <- nrow(d)

  d[, `:=`(y_tot_h = max(y_tot, na.rm = TRUE),
           y_vl_h  = max(y_vl,  na.rm = TRUE),
           n_hog_h = max(n_hog, na.rm = TRUE)), by = hogar_id]
  n_prop <- d[y_tot_h != y_tot, .N]

  d[, ypc := (y_tot_h - y_vl_h) / n_hog_h]
  d[!is.finite(ypc) | ypc <= 0, ypc := NA_real_]

  if (k$esq == "2a6") {
    d[, estado := fcase(e190 == 4, 1, e190 == 6, 2, e190 == 5, 3,
                        e190 == 2, 4, e190 == 3, 5, default = NA_real_)]
    n_mixto <- 0L; n_sinfreq <- 0L
  } else {
    fr <- k$freq
    d[, diario := Reduce(`|`, lapply(fr, function(v) !is.na(get(v)) & get(v) == 1))]
    d[, ocas   := Reduce(`|`, lapply(fr, function(v) !is.na(get(v)) & get(v) == 2))]
    d[, estado := fcase(e190 == 4, 1,
                        e190 == 1 & diario, 3,
                        e190 == 1 & ocas & !diario, 2,
                        e190 == 2, 4,
                        e190 == 3, 5, default = NA_real_)]
    n_mixto   <- d[e190 == 1 & diario & ocas, .N]
    n_sinfreq <- d[e190 == 1 & !diario & !ocas, .N]
  }

  b <- d[e27 >= 15 & !is.na(estado) & !is.na(peso) & peso > 0]
  n_15mas <- d[e27 >= 15, .N]

  b[, tercil := terciles_pond(ypc, peso), by = mes]

  b[, `:=`(sexo = fcase(e26 == 1, "varon", e26 == 2, "mujer", default = "otro/NA"),
           anio = k$anio,
           anio_nac = k$anio - e27)]
  b[, `:=`(cohorte = cohorte_5(anio_nac), grupo_etario = grupo_etario(e27))]

  paneles[[length(paneles) + 1]] <-
    b[, .(anio, cohorte, anio_nac, edad = e27, grupo_etario, sexo, tercil, estado, peso, ypc)]

  diag[[length(diag) + 1]] <- data.table(
    anio = k$anio, n_registros = n_reg, n_15mas = n_15mas,
    n_base_con_estado = nrow(b), n_propagados_ingreso = n_prop,
    n_sin_ingreso_pc = b[is.na(ypc), .N], n_sin_tercil = b[is.na(tercil), .N],
    fumadores_mixtos = n_mixto, fumadores_sin_frecuencia = n_sinfreq, peso = k$peso)

  log_msg("    registros=%d 15+=%d base=%d propagados=%d sin_ypc=%d mixtos=%d",
          n_reg, n_15mas, nrow(b), n_prop, b[is.na(ypc), .N], n_mixto)
}

P <- rbindlist(paneles)
fwrite(P, file.path(int_dir, "panel_personas_15mas.csv.gz"))
diag_dt <- rbindlist(diag)
fwrite(diag_dt, file.path(out_dir, "22_diagnostico_construccion.csv"))

P[, estado_lab := estado_lab[as.character(estado)]]

panel <- P[!is.na(tercil),
           .(n = .N, N_ponderado = round(sum(peso), 1)),
           by = .(cohorte, anio, sexo, tercil, estado, estado_lab)][
             order(cohorte, anio, sexo, tercil, estado)]
fwrite(panel, file.path(out_dir, "22_panel_cohortes.csv"))

panel_sin <- P[, .(n = .N, N_ponderado = round(sum(peso), 1)),
               by = .(cohorte, anio, sexo, estado, estado_lab)][
                 order(cohorte, anio, sexo, estado)]
fwrite(panel_sin, file.path(out_dir, "22_panel_cohortes_sin_nse.csv"))

ces <- P[!is.na(tercil),
         .(n_total = .N,
           n_exfum_reciente = sum(estado == 4),
           N_pond_total = round(sum(peso), 1),
           N_pond_exfum_reciente = round(sum(peso * (estado == 4)), 1)),
         by = .(anio, grupo_etario, sexo, tercil)][
           order(anio, grupo_etario, sexo, tercil)]
fwrite(ces, file.path(out_dir, "22_cesacion_celdas.csv"))

dist <- P[, .(n = .N, N_ponderado = sum(peso)), by = .(anio, estado, estado_lab)]
dist[, share_pond := round(N_ponderado / sum(N_ponderado), 4), by = anio]
dist[, N_ponderado := round(N_ponderado, 0)]
setorder(dist, anio, estado)
fwrite(dist, file.path(out_dir, "22_distribucion_estados.csv"))

log_msg("\nfilas del panel completo: %d", nrow(panel))
log_msg("celdas con n<5: %d (%.1f%%)", panel[n < 5, .N], 100 * mean(panel$n < 5))
log_msg("estado 4: %d celdas, n<5 en %d (%.1f%%)",
        panel[estado == 4, .N], panel[estado == 4 & n < 5, .N],
        100 * mean(panel[estado == 4]$n < 5))
log_msg("Outputs: 22_panel_cohortes.csv, 22_panel_cohortes_sin_nse.csv, 22_cesacion_celdas.csv, 22_distribucion_estados.csv, 22_diagnostico_construccion.csv")
close(log_con)

print(diag_dt)
print(dcast(dist, anio ~ estado_lab, value.var = "share_pond"))
