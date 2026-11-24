# 02_conteo_producto21.R
#
# ECH tobacco module: valid responses by survey year, weighted prevalence and raw distribution of
# the smoking question, from the raw ECH files (2006-2025).
# Input:  data/raw/ECH/
# Output: output/tables/21_*.csv

library(here)
library(data.table)

here::i_am("R/02_conteo_producto21.R")

log_con <- file(here::here("logs", "02_conteo_producto21.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) {
  msg <- sprintf(...)
  cat(msg, "\n")
  cat(msg, "\n", file = log_con)
}
log_msg("=== 02_conteo_producto21.R -- %s ===", format(Sys.time()))

raw_dir <- here::here("data", "raw", "ECH")

grupo_etario <- function(edad) {
  cut(edad, breaks = c(12, 18, 30, 45, 65, Inf), right = FALSE,
      labels = c("12-17", "18-29", "30-44", "45-64", "65+"))
}

cfg <- list(
  list(anio = 2006, archivo = "ech6_2006.dta", formato = "dta", esquema = "binario_2006",
       var = "ms36", peso = "pesotri", filtro_mes = 7:9),
  list(anio = 2011, archivo = "ech11_2011.dta", formato = "dta", esquema = "1a4",
       var = "e190", peso = "pesoano", filtro_mes = NULL),
  list(anio = 2014, archivo = "ech14_2014.dta", formato = "dta", esquema = "1a4",
       var = "e190", peso = "pesoano", filtro_mes = NULL),
  list(anio = 2017, archivo = "ech17_2017.dta", formato = "dta", esquema = "1a4",
       var = "e190", peso = "pesoano", filtro_mes = NULL),
  list(anio = 2018, archivo = "ech18_2018.dta", formato = "dta", esquema = "1a4",
       var = "e190", peso = "pesoano", filtro_mes = NULL),
  list(anio = 2019, archivo = "ech19_2019.dta", formato = "dta", esquema = "2a6",
       var = "e190", peso = "pesoano", filtro_mes = NULL),
  list(anio = 2021, archivo = "ech21_2021.dta", formato = "dta", esquema = "2a6",
       var = "e190", peso = "w_sem", filtro_mes = NULL),
  list(anio = 2022, archivo = "ech22_2022.dta", formato = "dta", esquema = "2a6",
       var = "e190", peso = "w_ano", filtro_mes = NULL),
  list(anio = 2023, archivo = "ech23_impl_2023.dta", formato = "dta", esquema = "2a6",
       var = "e190", peso = "w_ano", filtro_mes = NULL),
  list(anio = 2024, archivo = "ech24_2024.dta", formato = "dta", esquema = "2a6",
       var = "e190", peso = "w_ano", filtro_mes = NULL),
  list(anio = 2025, archivo = "ECH_2025_implantacion.csv", formato = "csv", esquema = "2a6",
       var = "e190", peso = "W_ANO", filtro_mes = NULL)
)

etiquetas <- list(
  binario_2006 = c(`1` = "si (fuma)", `2` = "no (no fuma)"),
  `1a4` = c(`1` = "si, fuma", `2` = "dejo hace menos de un anio",
            `3` = "dejo hace mas de un anio", `4` = "nunca fumo"),
  `2a6` = c(`2` = "dejo hace menos de un anio", `3` = "dejo hace mas de un anio",
            `4` = "nunca fumo", `5` = "si, diariamente", `6` = "si, algunos dias")
)
codigo_fuma_actual <- list(binario_2006 = 1, `1a4` = 1, `2a6` = c(5, 6))
codigo_dejo_reciente <- list(binario_2006 = NA, `1a4` = 2, `2a6` = 2)

tab_muestra <- list()
tab_dist <- list()
tab_reciente <- list()
tab_prev <- list()

for (c in cfg) {
  path <- file.path(raw_dir, c$archivo)
  log_msg("--- %d: %s ---", c$anio, c$archivo)
  cols <- c("e26", "e27", c$var, c$peso)
  if (!is.null(c$filtro_mes)) cols <- c(cols, "mes")
  cols <- unique(cols)

  if (c$formato == "dta") {
    d <- haven::read_dta(path, col_select = dplyr::all_of(cols))
  } else {
    d <- data.table::fread(path, select = cols, encoding = "UTF-8")
  }
  setDT(d)
  setnames(d, c(c$var, c$peso, "e26", "e27"), c("var", "peso", "sexo", "edad"))

  if (!is.null(c$filtro_mes)) {
    d <- d[mes %in% c$filtro_mes]
  }

  d[, var := as.numeric(var)]
  d[, edad := as.numeric(edad)]
  d[, peso := as.numeric(peso)]

  codigos_validos <- as.numeric(names(etiquetas[[c$esquema]]))

  n_total <- nrow(d)
  n_12mas <- d[edad >= 12, .N]
  n_valida <- d[edad >= 12 & var %in% codigos_validos, .N]
  ratio <- n_valida / n_12mas

  tab_muestra[[length(tab_muestra) + 1]] <- data.table(
    anio = c$anio, esquema = c$esquema, archivo = c$archivo,
    n_muestra_total = n_total, n_12mas = n_12mas,
    n_12mas_valida = n_valida, ratio_valida_sobre_12mas = round(ratio, 4)
  )

  dist <- d[edad >= 12, .N, by = var][order(var)]
  dist[, etiqueta := ifelse(as.character(var) %in% names(etiquetas[[c$esquema]]),
                             etiquetas[[c$esquema]][as.character(var)],
                             "otro/no corresponde/no sabe")]
  dist[, anio := c$anio]
  tab_dist[[length(tab_dist) + 1]] <- dist[, .(anio, codigo = var, etiqueta, n = N)]

  cod_reciente <- codigo_dejo_reciente[[c$esquema]]
  if (!is.na(cod_reciente)) {
    dr <- d[edad >= 12]
    dr[, grupo_etario := grupo_etario(edad)]
    dr[, sexo_lab := fifelse(sexo == 1, "varon", fifelse(sexo == 2, "mujer", "otro/NA"))]
    total_reciente <- dr[var == cod_reciente, .N]
    por_celda <- dr[var == cod_reciente, .N, by = .(sexo_lab, grupo_etario)]
    por_celda[, anio := c$anio]
    tab_reciente[[length(tab_reciente) + 1]] <-
      rbind(
        data.table(anio = c$anio, sexo_lab = "TOTAL", grupo_etario = NA, N = total_reciente),
        por_celda[, .(anio, sexo_lab, grupo_etario, N)]
      )
  } else {
    tab_reciente[[length(tab_reciente) + 1]] <- data.table(
      anio = c$anio, sexo_lab = "no aplica (instrumento binario, sin categoria)",
      grupo_etario = NA, N = NA_integer_
    )
  }

  cod_actual <- codigo_fuma_actual[[c$esquema]]
  dv <- d[edad >= 12 & var %in% codigos_validos]
  dv[, fuma_actual := as.numeric(var %in% cod_actual)]
  prev <- dv[, .(n = .N, prev_ponderada = sum(peso * fuma_actual) / sum(peso))]
  prev[, anio := c$anio]
  tab_prev[[length(tab_prev) + 1]] <- prev[, .(anio, n_base = n, prev_ponderada = round(prev_ponderada, 4))]

  log_msg("  n_total=%d n_12mas=%d n_12mas_valida=%d ratio=%.3f",
          n_total, n_12mas, n_valida, ratio)
}

muestra_dt <- rbindlist(tab_muestra)
dist_dt <- rbindlist(tab_dist)
reciente_dt <- rbindlist(tab_reciente, fill = TRUE)
prev_dt <- rbindlist(tab_prev)

out_dir <- here::here("output", "tables")
fwrite(muestra_dt, file.path(out_dir, "21_muestra_y_validez.csv"))
fwrite(dist_dt, file.path(out_dir, "21_distribucion_cruda.csv"))
fwrite(reciente_dt, file.path(out_dir, "21_exfumador_reciente.csv"))
fwrite(prev_dt, file.path(out_dir, "21_prevalencia_ponderada.csv"))

log_msg("Outputs escritos en output/tables/: 21_muestra_y_validez.csv, 21_distribucion_cruda.csv, 21_exfumador_reciente.csv, 21_prevalencia_ponderada.csv")
close(log_con)

print(muestra_dt)
print(prev_dt)
