# 60_parametros_modelo.R
#
# Parameter tables of the model: coefficients of the transition models, calibrated parameters,
# mortality relative risks, price response, initial population and intensity cells.
# Usage:  Rscript R/60_parametros_modelo.R      (English: paper/TC/tables/)
#         Rscript R/60_parametros_modelo.R es   (Spanish: informe/tablas/)
# Input:  data/interim/modelo_*.rds, output/tables/
# Output: P*.csv and P*.md

library(here)
library(data.table)
library(splines)

here::i_am("R/60_parametros_modelo.R")

ARGS   <- commandArgs(trailingOnly = TRUE)
IDIOMA <- if (length(ARGS) >= 1L && ARGS[1] %in% c("en", "es")) ARGS[1] else "en"

out_dir <- if (IDIOMA == "en") here::here("paper", "TC", "tables") else here::here("informe", "tablas")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tab_dir <- here::here("output", "tables")
int_dir <- here::here("data", "interim")

log_con <- file(here::here("logs", sprintf("60_parametros_modelo_%s.log", IDIOMA)),
                open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 60_parametros_modelo.R (idioma = %s) -- %s ===", IDIOMA, format(Sys.time()))

leer <- function(f) {
  p <- file.path(tab_dir, f)
  if (!file.exists(p)) stop("falta ", f, ": correr el script que lo produce antes que este.")
  fread(p)
}

DIC <- list(
  termino    = c(en = "Term",              es = "Término"),
  estim      = c(en = "Estimate",          es = "Estimación"),
  ee         = c(en = "SE",                es = "EE"),
  valor_t    = c(en = "t",                 es = "t"),
  valor_p    = c(en = "p",                 es = "p"),
  param      = c(en = "Parameter",         es = "Parámetro"),
  valor      = c(en = "Value",             es = "Valor"),
  fuente     = c(en = "Source",            es = "Fuente"),
  estado     = c(en = "Smoking state",     es = "Estado de tabaquismo"),
  rr         = c(en = "Relative risk",     es = "Riesgo relativo"),
  grupo      = c(en = "Age group",         es = "Grupo etario"),
  sexo       = c(en = "Sex",               es = "Sexo"),
  tercil     = c(en = "Income tertile",    es = "Tercil de ingreso"),
  share_down = c(en = "Share downshifting",es = "Proporción que baja a ocasional"),
  n_pond     = c(en = "Weighted n",        es = "n ponderado"),
  elast_tot  = c(en = "Total elasticity",  es = "Elasticidad total"),
  elast_ext  = c(en = "Extensive",         es = "Extensiva"),
  elast_int  = c(en = "Intensive",         es = "Intensiva"),
  share_ext  = c(en = "Extensive share",   es = "Proporción extensiva"),
  n_pers     = c(en = "Persons",           es = "Personas"),
  pct        = c(en = "%",                 es = "%"),
  cig_dia    = c(en = "Cigarettes per day",es = "Cigarrillos por día"),
  e1 = c(en = "1 Never smoked",           es = "1 Nunca fumó"),
  e2 = c(en = "2 Smokes occasionally",      es = "2 Fumador ocasional"),
  e3 = c(en = "3 Smokes daily",           es = "3 Fumador diario"),
  e4 = c(en = "4 Quit < 1 year ago",   es = "4 Exfumador reciente"),
  e5 = c(en = "5 Quit ≥ 1 year ago", es = "5 Exfumador consolidado"),
  frec       = c(en = "Frequency",         es = "Frecuencia"),
  n_celda    = c(en = "n in cell",         es = "n en la celda"),
  origen     = c(en = "Distribution from", es = "Distribución tomada de"),
  media      = c(en = "Weighted mean",     es = "Media ponderada"),
  varon = c(en = "Men", es = "Varones"), mujer = c(en = "Women", es = "Mujeres")
)
L <- function(k) { v <- DIC[[k]]; if (is.null(v)) stop("falta clave: ", k); unname(v[[IDIOMA]]) }

DEC  <- if (IDIOMA == "es") "," else "."
MILE <- if (IDIOMA == "es") "." else ","
RAYA <- "—"
n_fmt <- function(x, d = 2L) ifelse(is.na(x), RAYA,
                                    formatC(x, format = "f", digits = d, decimal.mark = DEC))
i_fmt <- function(x) formatC(round(x), format = "d", big.mark = MILE,
                             decimal.mark = DEC)
L2 <- function(en, es_) if (IDIOMA == "es") es_ else en

ETIQ_ESTADO <- c(`1` = L("e1"), `2` = L("e2"), `3` = L("e3"), `4` = L("e4"), `5` = L("e5"))

md_tabla <- function(d, alinear_izq = 1L) {
  enc <- names(d)
  sep <- ifelse(seq_along(enc) %in% alinear_izq, ":---", "---:")
  filas <- apply(d, 1L, function(r) paste0("| ", paste(as.character(r), collapse = " | "), " |"))
  c(paste0("| ", paste(enc, collapse = " | "), " |"),
    paste0("|", paste(sep, collapse = "|"), "|"), filas)
}

emitir <- function(d, nombre, alinear_izq = 1L) {
  fwrite(d, file.path(out_dir, paste0(nombre, ".csv")))
  writeLines(md_tabla(d, alinear_izq), file.path(out_dir, paste0(nombre, ".md")))
  log_msg("  %s  (%d filas)", nombre, nrow(d))
}

tabla_coef <- function(m) {
  s <- if (!is.null(m$coef_resumen)) m$coef_resumen else summary(m)$coefficients
  d <- data.table(termino = rownames(s),
                  estimacion = s[, 1], ee = s[, 2], t = s[, 3], p = s[, 4])
  falt <- setdiff(names(coef(m)), d$termino)
  if (length(falt))
    d <- rbind(d, data.table(termino = falt, estimacion = NA_real_, ee = NA_real_,
                             t = NA_real_, p = NA_real_))
  d[]
}

ES_TERMINO <- c(
  `(Intercept)`      = "Constante",
  sexomujer          = "Mujer",
  sexovaron          = "Varón",
  tercil2            = "Tercil 2",
  tercil3            = "Tercil 3",
  anio_c             = "Año (tendencia, 2018 = 0)",
  anio_prev_c        = "Año (tendencia, 2018 = 0)",
  `sexovaron:edad`   = "Varón × edad",
  `I(edad_media_prev <= EDAD_CORTE_JNDEM)TRUE` = "Hasta 17 años (encuesta de enseñanza media)",
  `anio_prev_c:I(edad_media_prev <= EDAD_CORTE_JNDEM)TRUE` = "Año × hasta 17 años",
  `log(punto_medio)` = "Años desde la cesación (log d)",
  `grupo_edad35-44`  = "Grupo etario 35-44",
  `grupo_edad45-54`  = "Grupo etario 45-54",
  `grupo_edad55-64`  = "Grupo etario 55-64",
  `grupo_edad65+`    = "Grupo etario 65 y más")
etiquetar_terminos_es <- function(d) {
  d <- d[termino != "sexomujer:edad"]
  base_ns <- grepl("^ns\\(", d$termino)
  d[base_ns, termino := sub("^ns\\([^)]*\\)([0-9]+)$", "Edad (spline natural), base \\1", termino)]
  hay <- d$termino %in% names(ES_TERMINO)
  d[hay, termino := ES_TERMINO[termino]]
  falt <- d$termino[!hay & !base_ns]
  if (length(falt)) stop("termino sin etiqueta en castellano: ", paste(falt, collapse = ", "))
  d[]
}

fmt_coef <- function(d) {
  if (IDIOMA == "es") d <- etiquetar_terminos_es(d)
  out <- data.table(
    A = d$termino,
    B = ifelse(is.na(d$estimacion), "colineal", n_fmt(d$estimacion, 4)),
    C = n_fmt(d$ee, 4),
    D = n_fmt(d$t, 2),
    E = ifelse(is.na(d$p), RAYA,
               ifelse(d$p < 0.001, paste0("<0", DEC, "001"), n_fmt(d$p, 3))))
  setnames(out, c(L("termino"), L("estim"), L("ee"), L("valor_t"), L("valor_p")))
  out[]
}

log_msg("\n--- P1-P3 hazards estimados ---")

modelos <- list(
  P1_hazard_cesacion  = "modelo_cesacion",
  P2_hazard_iniciacion = "modelo_iniciacion",
  P3_hazard_recaida   = "modelo_recaida"
)

anotar_ficha <- function(tabla, nombre_modelo, m) {
  data.table(
    tabla = tabla,
    modelo = nombre_modelo,
    formula = paste(deparse(formula(m)), collapse = " "),
    familia = family(m)$family,
    enlace = family(m)$link,
    n_obs = if (!is.null(m$n_obs)) m$n_obs else tryCatch(nobs(m), error = function(e) NA_integer_),
    n_coef = length(coef(m)))
}

ficha <- list()
for (nm in names(modelos)) {
  f <- file.path(int_dir, paste0(modelos[[nm]], ".rds"))
  if (!file.exists(f)) stop("falta ", f)
  m <- readRDS(f)
  if (inherits(m, "recaida_hibrida")) {
    piezas <- list(a = list(m = m$joven, suf = "_a",
                            eti = L2("pooled, ages 15-44", "agrupado, 15 a 44 a\u00f1os")),
                   b = list(m = m$mayor, suf = "_b",
                            eti = L2("age-conditional, 45 and over",
                                     "condicional a la edad, 45 y m\u00e1s")))
    for (pz in piezas) {
      nombre <- paste0(nm, pz$suf)
      emitir(fmt_coef(tabla_coef(pz$m)), nombre)
      fi <- anotar_ficha(nombre, paste0(modelos[[nm]], " (", pz$eti, ")"), pz$m)
      ficha[[length(ficha) + 1L]] <- fi
    }
    log_msg("  %s: objeto hibrido, emitido en dos piezas (%s_a y %s_b).", nm, nm, nm)
  } else {
    emitir(fmt_coef(tabla_coef(m)), nm)
    ficha[[length(ficha) + 1L]] <- anotar_ficha(nm, modelos[[nm]], m)
  }
}
fichas <- rbindlist(ficha)
fwrite(fichas, file.path(out_dir, "P0_fichas_modelos.csv"))
log_msg("  P0_fichas_modelos  (formula, familia, n de cada modelo)")

log_msg("\n--- P4 escalada y downshift ---")

cal <- leer("43_parametros_calibrados.csv")
down <- leer("40_downshift_diario_ocasional_edad_sexo_combinado.csv")

p4a <- copy(cal)
setnames(p4a, c(L("param"), L("valor")))
p4a[[2]] <- n_fmt(as.numeric(p4a[[2]]), 6)
if (IDIOMA == "es") {
  ES_PARAM <- c(escalada_nivel_15    = "Escalada: nivel a los 15 años",
                escalada_decaimiento = "Escalada: decrecimiento con la edad",
                downshift_constante  = "Descenso a ocasional: constante")
  hay <- p4a[[1]] %in% names(ES_PARAM)
  p4a[[1]][hay] <- ES_PARAM[p4a[[1]][hay]]
}
emitir(p4a, "P4a_parametros_calibrados")

p4b <- data.table(A = down$grupo_etario,
                  B = ifelse(down$sexo == "varon", L("varon"), L("mujer")),
                  C = i_fmt(down$n_pond),
                  D = n_fmt(down$share_downshift, 3))
setnames(p4b, c(L("grupo"), L("sexo"), L("n_pond"), L("share_down")))
emitir(p4b, "P4b_downshift_observado", alinear_izq = 1:2)

log_msg("\n--- P5 mortalidad ---")

RR_ESPERADO <- c(1.0, 1.3, 2.5, 1.8, 1.2)
src23 <- paste(readLines(here::here("R", "23_mortalidad.R"), warn = FALSE), collapse = "\n")
patron <- sprintf("RR_base = c\\(%s\\)",
                  paste(sprintf("%.1f", RR_ESPERADO), collapse = ", "))
if (!grepl(patron, src23))
  stop("los RR por estado de R/23_mortalidad.R no coinciden con los de esta tabla.\n",
       "  esperado: RR_base = c(", paste(sprintf("%.1f", RR_ESPERADO), collapse = ", "), ")\n",
       "  actualizar R/60 y volver a correrlo, o revisar que cambio en R/23.")
log_msg("  guardarrail OK: los RR coinciden con R/23_mortalidad.R")

p5 <- data.table(A = unname(ETIQ_ESTADO), B = n_fmt(RR_ESPERADO, 1))
setnames(p5, c(L("estado"), L("rr")))
emitir(p5, "P5_mortalidad_rr")

log_msg("\n--- P6 respuesta al precio ---")

esp <- leer("74_especificacion_elasticidad.csv")
pri <- esp[caso == "principal"]
if (nrow(pri) != 1L) stop("74_especificacion_elasticidad.csv no trae exactamente un caso 'principal'.")

SHARE_EXT <- c(0.63174155472705973, 0.46283854935876201, 0.34000000000000002)
src24 <- paste(readLines(here::here("R", "24_motor_transiciones.R"), warn = FALSE), collapse = "\n")
for (v in SHARE_EXT)
  if (!grepl(sprintf("%.17f", v), src24, fixed = TRUE))
    stop("el reparto extensivo/intensivo de R/24 no coincide con el de esta tabla: falta ",
         sprintf("%.17f", v))
log_msg("  guardarrail OK: el reparto por margen coincide con R/24_motor_transiciones.R")

et <- as.numeric(pri[, .(T1, T2, T3)])
p6 <- data.table(
  A = c("T1", "T2", "T3"),
  B = n_fmt(et, 3),
  C = n_fmt(SHARE_EXT, 3),
  D = n_fmt(et * SHARE_EXT, 3),
  E = n_fmt(et * (1 - SHARE_EXT), 3))
setnames(p6, c(L("tercil"), L("elast_tot"), L("share_ext"), L("elast_ext"), L("elast_int")))
emitir(p6, "P6_respuesta_precio")

log_msg("\n--- P7 poblacion inicial ---")

f_pob <- file.path(int_dir, "poblacion_sintetica_2011.csv.gz")
if (!file.exists(f_pob)) stop("falta ", f_pob, ": correr R/22_poblacion_sintetica.R")
pob <- fread(f_pob)
if (!"grupo_etario" %in% names(pob))
  pob[, grupo_etario := cut(edad, breaks = c(15, 25, 45, 65, Inf),
                            labels = c("15-24", "25-44", "45-64", "65+"), right = FALSE)]
comp <- pob[, .N, by = .(grupo_etario, estado)][order(grupo_etario, estado)]
comp[, pct := 100 * N / sum(N), by = grupo_etario]
p7 <- data.table(A = as.character(comp$grupo_etario),
                 B = ETIQ_ESTADO[as.character(comp$estado)],
                 C = i_fmt(comp$N),
                 D = n_fmt(comp$pct, 1))
setnames(p7, c(L("grupo"), L("estado"), L("n_pers"), L("pct")))
emitir(p7, "P7_poblacion_inicial", alinear_izq = 1:2)

log_msg("\n--- P8 intensidad ---")

inten <- leer("56_intensidad_celdas_resumen.csv")

ETIQ_FREC <- if (IDIOMA == "es") {
  c(diario = "Diario", ocasional = "Ocasional")
} else {
  c(diario = "Daily", ocasional = "Occasional")
}
ETIQ_FB <- if (IDIOMA == "es") {
  c(celda_completa = "celda propia", frecuencia_sexo = "colapsada por edad")
} else {
  c(celda_completa = "own cell", frecuencia_sexo = "collapsed over age")
}

p8 <- data.table(
  A = unname(ETIQ_FREC[inten$frecuencia]),
  B = ifelse(inten$sexo == "varon", L("varon"), L("mujer")),
  C = inten$grupo_etario,
  D = i_fmt(inten$n_celda),
  E = unname(ETIQ_FB[inten$fallback]),
  F = n_fmt(inten$media_pond, 2),
  G = n_fmt(inten$p25, 2), H = n_fmt(inten$p50, 2), I = n_fmt(inten$p75, 2))
setnames(p8, c(L("frec"), L("sexo"), L("grupo"), L("n_celda"), L("origen"),
               L("media"), "p25", "p50", "p75"))
emitir(p8, "P8_intensidad_celdas", alinear_izq = c(1:3, 5L))

log_msg("\n=== outputs en %s ===", out_dir)
close(log_con)
