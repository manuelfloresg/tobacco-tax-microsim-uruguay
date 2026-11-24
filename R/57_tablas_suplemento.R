# 57_tablas_suplemento.R
#
# Tables of the online supplement in LaTeX, numbered in order of appearance. Does not run the model.
# Input:  results in output/tables/ and the outputs of R/56 and R/60
# Output: paper/TC/supp/tables/*.tex

library(here)
library(data.table)

here::i_am("R/57_tablas_suplemento.R")

log_con <- file(here::here("logs", "57_tablas_suplemento.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 57_tablas_suplemento.R -- %s ===", format(Sys.time()))

tab_dir <- here::here("output", "tables")
tex_dir <- here::here("paper", "TC", "supp", "tables")
dir.create(tex_dir, recursive = TRUE, showWarnings = FALSE)

leer <- function(f) {
  p <- file.path(tab_dir, f)
  if (!file.exists(p)) stop("falta ", f, ": correr el script que lo produce antes que este.")
  fread(p)
}

BARRA <- "\\"

tex_esc <- function(x) {
  x <- as.character(x)
  x <- gsub(BARRA, paste0(BARRA, "textbackslash{}"), x, fixed = TRUE)
  for (ch in c("&", "%", "$", "#", "_", "{", "}"))
    x <- gsub(ch, paste0(BARRA, ch), x, fixed = TRUE)
  x
}

DICC <- c(
  anio = "Year", escenario = "Scenario", escenario_impuesto = "Scenario",
  factor_impuesto_real = "Real excise factor",
  grupo_etario = "Age group", anios = "Years", grupo = "Group", metrica = "Metric",
  variante = "Variant", tercil = "Tertile", reparto = "Split", serie = "Series",
  resultado = "Outcome", fuente = "Layer", tipo = "Interval type",
  dentro_angosto = "Inside (informant only)", dentro_ancho = "Inside (informant + sampling)",
  exceso_max_angosto_pp = "Max excess, pp (informant only)",
  exceso_max_ancho_pp = "Max excess, pp (both sources)",
  prevalencia_obs_pond = "Observed prevalence", prevalencia_sim_pond = "Simulated prevalence",
  dif_abs_media = "Mean absolute difference",
  deff_min = "Design effect, min", deff_mediana = "Design effect, median",
  deff_max = "Design effect, max", ee_disenio_medio_pp = "Mean design SE, pp",
  semiancho_medio_pp = "Mean CI half-width, pp",
  central = "Central", ancho = "Width", ancho_total = "Total width",
  ancho_elasticidad = "Width, elasticity layer", ancho_hazard = "Width, hazard prior",
  aporte_hazard_pct = "Hazard share of width, %", banda_sobre_ruido = "Width / simulation noise",
  `p2.5` = "2.5th percentile", `p97.5` = "97.5th percentile",
  rmse_loo = "LOO RMSE", max_abs_loo = "Max absolute LOO error",
  rango_nodos = "Range across design nodes", error_loo_sobre_rango = "LOO error / range",
  consumo_mill = "Consumption (millions)", prevalencia = "Prevalence",
  aporte_extensivo = "Extensive contribution (log)",
  aporte_intensivo = "Intensive contribution (log)",
  recaudacion_relativa = "Revenue ratio", share_consumo = "Share of consumption",
  prev_T1 = "Prevalence T1", prev_T2 = "Prevalence T2", prev_T3 = "Prevalence T3",
  razon_T1_T3 = "T1/T3 ratio", brecha_T1_T3_pp = "T1-T3 gap, pp",
  valor_base = "Status quo", valor_esc = "Scenario", dif_pp = "Difference, pp",
  valor_2003_05 = "Value 2003-05", valor_2010_11 = "Value 2010-11",
  anio_ini = "Start year", anio_fin = "End year", cambio_pct = "Change, %",
  agregada = "Aggregate elasticity", gradiente_T1_sobre_T3 = "T1/T3 elasticity ratio",
  share_ext_T1 = "Extensive share T1", share_ext_T2 = "Extensive share T2",
  share_ext_T3 = "Extensive share T3",
  elast_ext_T1 = "Extensive elasticity T1", elast_ext_T3 = "Extensive elasticity T3",
  elast_int_T1 = "Intensive elasticity T1", elast_int_T3 = "Intensive elasticity T3",
  observed = "Observed", simulated = "Simulated", lower = "Bracket lower",
  upper = "Bracket upper", inside = "Inside", excess_pp = "Excess, pp",
  estudio = "Study", pais = "Country", ingreso_bm = "World Bank income group",
  particion = "Stratifier", e_bajo = "Lowest group", e_alto = "Highest group",
  ratio = "Ratio", diferencia = "Notes on the difference", clase = "Class",
  conv75_abrupto = "WHO 75%, abrupt", conv75_gradual = "WHO 75%, gradual",
  convEU_gradual = "EU 81.55%, gradual"
)
traducir <- function(x) ifelse(x %in% names(DICC), DICC[x], gsub("_", " ", x))

VAL_GEN <- c(
  nunca_fumo = "Never smoked", prevalencia_actual = "Current smoking prevalence",
  caida_consumo_pct = "Consumption change, %", caida_prevalencia_pct = "Prevalence change, %",
  recaudacion_relativa = "Revenue ratio",
  "capa 1 (elasticidad)" = "Layer 1 (elasticity)",
  "capa 1 + prior del hazard" = "Layer 1 + hazard prior",
  "solo prior del hazard" = "Hazard prior only",
  "IC frecuentista" = "Frequentist CI", "IC de credibilidad" = "Credible interval",
  keeler_acotado = "Bounded (primary)", uniforme_50 = "Uniform 50%", keeler_literal = "Literal",
  homogenea = "Homogeneous", iarc = "IARC-bounded", invertida = "Inverted",
  agregado_639 = "Aggregate -0.639", ferre_774 = "Aggregate -0.774",
  C_S7_cruce_bracket = "Ratio 0.20 (crossing bracket)",
  C_S3_gradiente_invertido = "Ratio 0.46",
  C_S8_inversion_espejo = "Ratio 0.50 (assumed inversion)",
  C_S0_sin_gradiente = "Ratio 1.0 (no gradient)",
  C_S2_gradiente_debil = "Ratio 1.3",
  C_S6_gradiente_mediano = "Ratio 1.5 (median of evidence)",
  C_principal = "Ratio 2.0 (primary)",
  C_S1_gradiente_fuerte = "Ratio 2.5",
  C_S5_gradiente_muy_fuerte = "Ratio 3.0 (upper bound)",
  C_S4_nivel_alternativo = "Ratio 2.0, aggregate -0.774",
  "Precio real" = "Real price", "Impuesto real" = "Real tax per pack",
  "Participacion impositiva en el precio (pp)" = "Tax share of price (pp)",
  "Volumen legal DGI" = "Legal sales volume",
  "Prevalencia ECH (15+)" = "Prevalence, continuous household survey (15+)",
  "Prevalencia JND-Hogares" = "Prevalence, national household drug survey"
)
VAL_ESC <- c(base = "Status quo", conv75_abrupto = "WHO 75%, abrupt",
             conv75_gradual = "WHO 75%, gradual", convEU_gradual = "EU 81.55%, gradual")
VAL_VAR <- c(base = "Legacy specification")

aplicar <- function(v, mapa) ifelse(v %in% names(mapa), mapa[v], v)

traducir_valores <- function(d) {
  for (cn in names(d)) {
    v <- d[[cn]]
    if (!is.character(v)) next
    if (cn == "Scenario") v <- aplicar(v, VAL_ESC)
    if (cn == "Variant")  v <- aplicar(v, VAL_VAR)
    d[[cn]] <- aplicar(v, VAL_GEN)
  }
  d
}

envolver_encabezado <- function(x, ancho = 13L) {
  vapply(x, function(s) {
    piezas <- strsplit(s, " ", fixed = TRUE)[[1]]
    lineas <- character(0); actual <- ""
    for (p in piezas) {
      cand <- if (nzchar(actual)) paste(actual, p) else p
      if (nchar(cand) > ancho && nzchar(actual)) { lineas <- c(lineas, actual); actual <- p }
      else actual <- cand
    }
    lineas <- c(lineas, actual)
    if (length(lineas) == 1L) return(lineas)
    sprintf("%sshortstack[l]{%s}", BARRA, paste(lineas, collapse = paste0(BARRA, BARRA)))
  }, "")
}

n_tabla <- 0L
ORDEN_TABLAS <- c("S1a_model_cards", "S1b_cessation", "S1c_initiation", "S1d_relapse_young",
  "S1e_relapse_old", "S1f_mortality_rr", "S1g_initial_population", "S1h_intensity", "S1_trajectories",
  "S3b_gradient_evidence", "S3_elasticities", "S3c_price_response", "S4_margin_split", "S5a_calibrated",
  "S5b_downshift", "S6a_backcast", "S6c_design_effect", "S6b_bracket_summary", "S6d_bracket_position",
  "S9_historical", "S7c_emulator", "S7a_bands", "S7b_band_sources", "S7d_noise",
  "S8a_scenarios", "S8b_tertiles", "S8c_envelope", "S8d_margin_tertiles",
  "S8i_static_frozen", "S8e_share_metric", "S8j_age_standardised", "S8f_extensive_composition",
  "S8g_deaths", "S8k_deaths_rates", "S8h_sex",
  "S9_sensitivity", "S9b_timing", "S9c_distributional", "S9d_fiscal_start")

escribir_tex <- function(d, archivo, caption, digitos = 3, tam = NULL,
                         alin_override = NULL, tabcolsep = NULL) {
  n_tabla <<- match(sub("[.]tex$", "", archivo), ORDEN_TABLAS)
  if (is.na(n_tabla)) stop("Tabla sin lugar en ORDEN_TABLAS: ", archivo)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  names(d) <- traducir(names(d))
  d <- traducir_valores(d)
  if (is.null(tam)) tam <- if (ncol(d) >= 6L) "scriptsize" else "footnotesize"
  cols <- lapply(d, function(v)
    if (is.numeric(v) && !is.integer(v)) formatC(v, format = "f", digits = digitos)
    else tex_esc(v))
  m <- do.call(cbind, lapply(cols, as.character))
  alin <- if (!is.null(alin_override)) alin_override
          else paste(vapply(d, function(v) if (is.numeric(v)) "r" else "l", ""), collapse = "")
  enc_txt <- tex_esc(names(d))
  if (is.null(alin_override)) enc_txt <- envolver_encabezado(enc_txt)
  enc <- paste(sprintf("%stextbf{%s}", BARRA, enc_txt), collapse = " & ")
  fin <- paste0(" ", BARRA, BARRA)
  cuerpo <- paste0(apply(m, 1L, paste, collapse = " & "), fin)
  writeLines(c(
    paste0(BARRA, tam),
    if (!is.null(tabcolsep))
      sprintf("%ssetlength{%stabcolsep}{%s}", BARRA, BARRA, tabcolsep) else character(0),
    sprintf("%sbegin{longtable}{%s}", BARRA, alin),
    sprintf("%scaption{%stextbf{Table S%d.} %s}%s%s",
            BARRA, BARRA, n_tabla, tex_esc(caption), BARRA, BARRA),
    paste0(BARRA, "toprule"), paste0(enc, fin), paste0(BARRA, "midrule"),
    paste0(BARRA, "endfirsthead"),
    paste0(BARRA, "toprule"), paste0(enc, fin), paste0(BARRA, "midrule"),
    paste0(BARRA, "endhead"),
    paste0(BARRA, "bottomrule"), paste0(BARRA, "endfoot"),
    cuerpo,
    paste0(BARRA, "end{longtable}"),
    paste0(BARRA, "normalsize")
  ), file.path(tex_dir, archivo))
  log_msg("  S%-2d  %-28s %3d filas x %2d cols", n_tabla, archivo, nrow(d), ncol(d))
}

leer_p <- function(f) fread(here::here("paper", "TC", "tables", f), colClasses = "character")

fichas <- leer_p("P0_fichas_modelos.csv")[, .(tabla, formula, familia, enlace, n_obs, n_coef)]
fichas[, tabla := c(P1_hazard_cesacion = "Cessation", P2_hazard_iniciacion = "Initiation",
                    P3_hazard_recaida_a = "Relapse, ages 15-44",
                    P3_hazard_recaida_b = "Relapse, ages 45+")[tabla]]
ESPEC_EN <- c(
  Cessation = paste("logit of the annual rate on a natural spline of age (4 df), sex, sex × age,",
                    "income tertile and calendar year (centred on 2018)"),
  Initiation = paste("cloglog of the probability of starting between survey waves on a natural spline of",
                     "age (4 df), sex, calendar year (centred on 2018) interacted with an indicator of",
                     "school-survey ages (≤17), a fixed income-tertile offset and log(years between waves)"),
  `Relapse, ages 15-44` = "logit of the annual rate on log(years since quitting) and sex",
  `Relapse, ages 45+` = "logit of the annual rate on log(years since quitting), sex and age group")
if (!identical(sort(names(ESPEC_EN)), sort(fichas$tabla))) stop("Cambio la lista de modelos de P0")
fichas[, formula := ESPEC_EN[tabla]]
setnames(fichas, c("Transition", "Specification", "Family", "Link", "Observations", "Coefficients"))
termino_en <- function(x) {
  fijo <- c("(Intercept)" = "Intercept", "sexomujer" = "Female", "sexovaron" = "Male",
            "tercil2" = "Income tertile 2", "tercil3" = "Income tertile 3",
            "anio_c" = "Calendar year (centred on 2018)",
            "anio_prev_c" = "Calendar year (centred on 2018)",
            "sexovaron:edad" = "Male × age",
            "I(edad_media_prev <= EDAD_CORTE_JNDEM)TRUE" = "School-survey ages (≤17)",
            "anio_prev_c:I(edad_media_prev <= EDAD_CORTE_JNDEM)TRUE" = "Calendar year × school-survey ages",
            "log(punto_medio)" = "log(years since quitting)",
            "grupo_edad35-44" = "Age 35-44", "grupo_edad45-54" = "Age 45-54",
            "grupo_edad55-64" = "Age 55-64", "grupo_edad65+" = "Age 65+")
  out <- fijo[x]
  sp <- grepl("^ns\\((edad|edad_media_prev), df = 4\\)[0-9]$", x)
  out[sp] <- paste0("Age spline, basis ", sub(".*\\)", "", x[sp]))
  if (anyNA(out)) stop("Termino sin traduccion: ", paste(x[is.na(out)], collapse = ", "))
  unname(out)
}
coef_en <- function(f) {
  d <- leer_p(f)
  d <- d[get(names(d)[1]) != "sexomujer:edad"]
  d[, (names(d)[1]) := termino_en(get(names(d)[1]))]
  d
}
escribir_tex(fichas, "S1a_model_cards.tex",
             "Estimated transition-rate models: specification and sample.",
             alin_override = "p{2.3cm}p{6.6cm}p{1.6cm}p{1.1cm}rr", tabcolsep = "3pt")
escribir_tex(coef_en("P1_hazard_cesacion.csv"), "S1b_cessation.tex",
             paste("Annual cessation rate: estimated coefficients (logit scale), continuous household survey 2011-2025.",
                   "Reference categories: male, lowest income tertile. The age slope enters through the spline; the",
                   "sex × age term is the additional slope for men."))
m_ini <- readRDS(here::here("data", "interim", "modelo_iniciacion.rds"))
off_ini <- get("TERCIL_OFFSET", envir = environment(formula(m_ini)))
if (length(off_ini) != 3L || off_ini[["1"]] != 0) stop("TERCIL_OFFSET inesperado en modelo_iniciacion.rds")
escribir_tex(coef_en("P2_hazard_iniciacion.csv"), "S1c_initiation.tex",
             sprintf(paste("Annual initiation rate: estimated coefficients (cloglog scale), national drug surveys.",
                           "The income gradient enters as a fixed offset estimated on the household survey:",
                           "%.3f (tertile 2) and %.3f (tertile 3), lowest tertile as reference."),
                     off_ini[["2"]], off_ini[["3"]]))
escribir_tex(coef_en("P3_hazard_recaida_a.csv"), "S1d_relapse_young.tex",
             "Annual relapse rate up to age 44: estimated coefficients, Global Adult Tobacco Survey 2009 and 2017.")
escribir_tex(coef_en("P3_hazard_recaida_b.csv"), "S1e_relapse_old.tex",
             paste("Annual relapse rate from age 45, conditional on age: estimated coefficients, Global Adult Tobacco",
                   "Survey 2009 and 2017. Reference categories: female, ages 15-34."))
escribir_tex(leer_p("P5_mortalidad_rr.csv"), "S1f_mortality_rr.tex",
             "Relative risk of death by smoking state before rescaling to national survival.")
escribir_tex(leer_p("P7_poblacion_inicial.csv"), "S1g_initial_population.tex",
             "Initial synthetic population, 2011, by age group and smoking state.")
escribir_tex(leer_p("P8_intensidad_celdas.csv"), "S1h_intensity.tex",
             "Cigarettes per day among people who smoke, by frequency, sex and age group.")
trj <- leer("59_trayectorias_impuesto.csv")[
  (escenario_impuesto == "conv75_abrupto" & anio %in% c(2027L, 2060L)) |
  (escenario_impuesto != "conv75_abrupto" & anio %in% c(2027:2031, 2060L))]
trj[, Scenario := c(conv75_gradual = "75%, gradual", conv75_abrupto = "75%, abrupt",
                    convEU_gradual = "EU average (81.55%), gradual")[escenario_impuesto]]
trj[, ord := match(escenario_impuesto, c("conv75_gradual", "conv75_abrupto", "convEU_gradual"))]
setorder(trj, ord, anio)
trj <- trj[, .(Scenario, Year = anio, `Excise factor` = round(factor_imesi, 3),
               `Total tax per pack factor` = round(factor_tributo_total, 3),
               `Price ratio` = round(ratio_precio, 3))]
escribir_tex(trj, "S1_trajectories.tex",
             paste("Tax trajectories relative to the status quo, by scenario and year: real excise",
                   "(IMESI) factor, factor on total tax per pack (excise plus VAT), and ratio of the",
                   "retail price to its status quo value, which is what the model receives. Values",
                   "from the target year onwards are constant."))

grad <- leer("84_evidencia_gradiente_elasticidad.csv")[
  , .(estudio, pais, ingreso_bm, particion, e_bajo, e_alto, ratio, diferencia)]
escribir_tex(grad, "S3b_gradient_evidence.tex",
             paste("Published estimates of the socioeconomic gradient in cigarette price",
                   "elasticity. The ratio is the elasticity of the lowest socioeconomic group",
                   "divided by that of the highest. All estimates are of the total elasticity",
                   "except Colombia, which reports a participation elasticity."),
             digitos = 2,
             alin_override = "p{2.3cm}p{1.4cm}p{1.6cm}p{1.8cm}rrrp{3.1cm}",
             tabcolsep = "3pt")
escribir_tex(leer("72_variantes_elasticidad.csv"), "S3_elasticities.tex",
             "Price elasticity specifications by income tertile, all variants tested.")
escribir_tex(leer_p("P6_respuesta_precio.csv"), "S3c_price_response.tex",
             "Primary specification of the price response by income tertile.")

escribir_tex(leer("75_reparto_margen.csv")[, .(reparto, share_ext_T1, share_ext_T2, share_ext_T3,
                                               elast_ext_T1, elast_ext_T3,
                                               elast_int_T1, elast_int_T3)],
             "S4_margin_split.tex",
             "Extensive-margin shares by tertile and the implied margin-specific elasticities.")

p4a <- leer_p("P4a_parametros_calibrados.csv")
p4a[, Parameter := c(escalada_nivel_15 = "Occasional-to-daily escalation, level at age 15",
                     escalada_decaimiento = "Occasional-to-daily escalation, decline with age",
                     downshift_constante = "Daily-to-occasional downshift, constant",
                     raw_par1 = "Optimiser scale, parameter 1", raw_par2 = "Optimiser scale, parameter 2",
                     raw_par3 = "Optimiser scale, parameter 3")[Parameter]]
escribir_tex(p4a, "S5a_calibrated.tex", "Calibrated parameters.")
escribir_tex(leer_p("P4b_downshift_observado.csv"), "S5b_downshift.tex",
             "Observed downshift from daily to occasional smoking, by age group and sex (reference for the calibration).")

escribir_tex(leer("44_validacion_prevalencia_resumen_anual.csv"), "S6a_backcast.tex",
             "Observed and simulated adult smoking prevalence, 2014-2025.", digitos = 4)
escribir_tex(leer("80_deff_medido_resumen.csv"), "S6c_design_effect.tex",
             "Design effect of the household survey, estimated by clustering on household.")
escribir_tex(leer("80_resumen_dentro_bracket.csv"), "S6b_bracket_summary.tex",
             "Years inside the measurement bracket, by age group.")
pos <- leer("80_posicion_motor_bracket_ampliado.csv")[
  metrica == "prevalencia_actual",
  .(anio, grupo_etario, observed = obs, simulated = sim,
    lower = piso_ancho, upper = techo_ancho, inside = dentro_ancho, excess_pp = exceso_ancho_pp)]
setorder(pos, grupo_etario, anio)
escribir_tex(pos, "S6d_bracket_position.tex",
             "Simulated prevalence against the measurement bracket, by year and age group.",
             digitos = 4)
escribir_tex(leer("60_validacion_historica_fctc_episodio.csv"), "S9_historical.tex",
             "Uruguay 2003-2011 tax episode: observed changes in prices, taxes, volumes and prevalence.",
             digitos = 2)

escribir_tex(leer("77_mc_validacion_emulador.csv"), "S7c_emulator.tex",
             "Leave-one-out validation of the Gaussian-process emulator.", digitos = 4)
escribir_tex(leer("77_mc_bandas.csv"), "S7a_bands.tex",
             "Credible intervals by year, outcome and uncertainty layer.", digitos = 4)
escribir_tex(leer("77_mc_aporte_fuentes.csv")[, .(anio, resultado, ancho_total, ancho_elasticidad,
                                                  ancho_hazard, aporte_hazard_pct,
                                                  banda_sobre_ruido)],
             "S7b_band_sources.tex",
             "Interval width by source, and interval width relative to simulation noise.")
ruido <- leer("77_mc_ruido_resumen.csv")
setnames(ruido, c("anio", "resultado", "media", "de", "rango"),
         c("Year", "Outcome", "Mean across seeds", "Standard deviation", "Range"))
escribir_tex(ruido, "S7d_noise.tex",
             "Simulation noise: effect of gradual convergence to 75% across 100 random seeds.")

cmp <- leer("59_espacio_escenarios_comparado.csv")[
  , .(escenario, anio, consumo_mill, prevalencia, aporte_extensivo, aporte_intensivo,
      recaudacion_relativa)]
setorder(cmp, escenario, anio)
escribir_tex(cmp, "S8a_scenarios.tex", "Full aggregate results by scenario and year.", digitos = 4)

ter <- leer("59_espacio_escenarios_tercil.csv")[
  , .(escenario, anio, tercil, prevalencia, consumo_mill, share_consumo)]
setorder(ter, escenario, anio, tercil)
escribir_tex(ter, "S8b_tertiles.tex", "Full results by income tertile, scenario and year.",
             digitos = 4)

env <- leer("79_razon_prevalencia_envolvente.csv")
setorder(env, variante, escenario, anio)
setnames(env, c("prev_T1", "prev_T2", "prev_T3", "razon_T1_T3", "brecha_T1_T3_pp"),
         c("T1", "T2", "T3", "T1/T3", "Gap, pp"), skip_absent = TRUE)
escribir_tex(env, "S8c_envelope.tex",
             "Prevalence by tertile, T1-T3 gap and T1/T3 ratio across all elasticity-gradient variants.",
             digitos = 4, tabcolsep = "4pt")

pro <- leer("75_margen_progresividad.csv")[metrica == "prevalencia",
                                           .(reparto, anio, tercil, valor_base, valor_esc, dif_pp)]
setorder(pro, reparto, anio, tercil)
escribir_tex(pro, "S8d_margin_tertiles.tex",
             "Prevalence by tertile under each extensive/intensive split.", digitos = 4)

pc <- fread(here::here("paper", "TC", "tables", "table2_panelC_envelope.csv"), colClasses = "character")
escribir_tex(pc, "S8e_share_metric.tex",
             paste("Change in the lowest tertile's share of total consumption, gradual convergence to 75%:",
                   "central estimate and envelope across the elasticity gradients of main-text table 2, panel A."))

ext <- leer("88_descomposicion_margen_extensivo.csv")[
  , .(Year = anio, Cessation = pct_cesacion, Initiation = pct_iniciacion,
      Interaction = pct_interaccion_sobre_total)]
setnames(ext, c("Year", "Cessation, % of extensive margin", "Avoided initiation, % of extensive margin",
                "Interaction, % of extensive margin"))
escribir_tex(ext, "S8f_extensive_composition.tex",
             paste("Composition of the extensive margin, gradual convergence to 75%. Each mechanism is",
                   "obtained by switching off the price response of one rate at a time; the interaction",
                   "is reported separately."), digitos = 1)

sr <- fread(here::here("output", "tables", "91_semillas_resumen.csv"))[
  grepl("^muertes_evitadas_", magnitud) & anio == 2060L]
sr[, grupo := c(muertes_evitadas_T1 = "T1 (lowest income)", muertes_evitadas_T2 = "T2",
                muertes_evitadas_T3 = "T3 (highest income)", muertes_evitadas_total = "Total")[magnitud]]
sr[, celda := sprintf("%s (%s)", format(round(media), big.mark = ","), round(ee_media))]
mu <- dcast(sr, grupo ~ escenario, value.var = "celda")
mu <- mu[match(c("T1 (lowest income)", "T2", "T3 (highest income)", "Total"), grupo)]
setnames(mu, "grupo", "Income tertile")
escribir_tex(mu, "S8g_deaths.tex",
             paste("Deaths averted, cumulative 2027-2060, by income tertile and scenario: mean across",
                   "100 simulation runs (simulation standard error)."))

sx <- leer("59_espacio_escenarios_sexo.csv")[escenario %in% c("base", "conv75_gradual") &
                                             anio %in% c(2025L, 2030L, 2045L, 2060L)]
sx <- dcast(sx, sexo + anio ~ escenario, value.var = "prevalencia")
sx[, base := 100 * base]
sx[, conv75_gradual := 100 * conv75_gradual]
sx[, dif := conv75_gradual - base]
sx[, sexo := c(mujer = "Women", varon = "Men")[sexo]]
setnames(sx, c("Sex", "Year", "Status quo, %", "WHO 75% gradual, %", "Difference, pp"))
escribir_tex(sx, "S8h_sex.tex",
             "Adult smoking prevalence by sex, status quo and gradual convergence to 75%.", digitos = 2)

agr <- leer("59_espacio_escenarios_agregado.csv")
ter_all <- leer("59_espacio_escenarios_tercil.csv")
r_meta <- leer("59_trayectorias_impuesto.csv")[escenario_impuesto == "conv75_gradual" & anio == 2060L]$ratio_precio
p6 <- leer_p("P6_respuesta_precio.csv")
el <- data.table(tercil = 1:3, e = as.numeric(p6$`Total elasticity`), s = as.numeric(p6$`Extensive share`))
estatica <- function(a) {
  t <- merge(ter_all[escenario == "base" & anio == a], el, by = "tercil")
  c(cons = 100 * (sum(t$consumo_mill * r_meta^t$e) / sum(t$consumo_mill) - 1),
    prev = 100 * sum(t$N_tercil * t$prevalencia * (r_meta^(t$e * t$s) - 1)) / sum(t$N_tercil),
    brecha = 100 * (t[tercil == 1L, prevalencia * (r_meta^(e * s) - 1)] -
                    t[tercil == 3L, prevalencia * (r_meta^(e * s) - 1)]))
}
e25 <- estatica(2025L); e60 <- estatica(2060L)
est_cons <- e25[["cons"]]; est_prev <- e25[["prev"]]; est_brecha <- e25[["brecha"]]
ag <- function(esc, a, col) agr[escenario == esc & anio == a][[col]]
br <- function(esc, a) 100 * (ter_all[escenario == esc & anio == a & tercil == 1L]$prevalencia -
                                ter_all[escenario == esc & anio == a & tercil == 3L]$prevalencia)
stat <- data.table(
  Appraisal = c("Dynamic model (main text)", "Static calculation", "Static calculation",
                "Frozen baseline"),
  Comparison = c("Policy 2060 vs status quo 2060",
                 "Status quo 2025 at the long-run price, adjusted at once",
                 "Status quo 2060 at the long-run price, adjusted at once",
                 "Policy 2060 vs status quo 2025"),
  CONS = c(100 * (ag("conv75_gradual", 2060L, "consumo_mill") / ag("base", 2060L, "consumo_mill") - 1),
           est_cons, e60[["cons"]],
           100 * (ag("conv75_gradual", 2060L, "consumo_mill") / ag("base", 2025L, "consumo_mill") - 1)),
  PREV = c(100 * (ag("conv75_gradual", 2060L, "prevalencia") - ag("base", 2060L, "prevalencia")),
           est_prev, e60[["prev"]],
           100 * (ag("conv75_gradual", 2060L, "prevalencia") - ag("base", 2025L, "prevalencia"))),
  GAP = c(br("conv75_gradual", 2060L) - br("base", 2060L), est_brecha, e60[["brecha"]],
          br("conv75_gradual", 2060L) - br("base", 2025L)))
log_msg("  S33 estatica: r = %.4f, consumo %.2f%%, prevalencia %.3f pp, brecha %.3f pp (dinamica: %.2f%%, %.3f pp)",
        r_meta, est_cons, est_prev, est_brecha, stat$CONS[1], stat$PREV[1])
setnames(stat, c("CONS", "PREV", "GAP"), c("Consumption change, %", "Prevalence change, pp",
                                         "Change in T1-T3 gap, pp"))
escribir_tex(stat, "S8i_static_frozen.tex",
             paste("Effect of gradual convergence to 75% under three appraisals: the dynamic model against",
                   "the same-year status quo, a static calculation that applies the long-run elasticities",
                   "at once to the 2025 or the 2060 status quo population, and a comparison against the status quo frozen at its",
                   "2025 value. Primary simulation run."), digitos = 2,
             alin_override = paste0("p{3.1cm}p{4.4cm}", strrep(">{\\raggedleft\\arraybackslash}p{1.9cm}", 3)),
             tabcolsep = "4pt")

pe <- leer("96_prevalencia_tercil_estandarizada.csv")[anio %in% c(2025L, 2030L, 2045L, 2060L)]
pe_l <- rbind(
  pe[, .(Measure = "Crude", escenario, anio, T1 = prev_cruda_1, T2 = prev_cruda_2, T3 = prev_cruda_3,
         GAP = brecha_cruda, RAT = razon_cruda, RG = pct_rem_brecha_cruda, RR = pct_rem_razon_cruda)],
  pe[, .(Measure = "Age-standardised", escenario, anio, T1 = prev_estandarizada_1, T2 = prev_estandarizada_2,
         T3 = prev_estandarizada_3, GAP = brecha_estandarizada, RAT = razon_estandarizada,
         RG = pct_rem_brecha_estandarizada, RR = pct_rem_razon_estandarizada)])
pe_l[, escenario := c(base = "Status quo", conv75_gradual = "WHO 75%, gradual")[escenario]]
pe_l[, Measure := factor(Measure, c("Crude", "Age-standardised"))]
setorder(pe_l, Measure, escenario, anio)
pe_l[, Measure := as.character(Measure)]
pe_l[, `:=`(RG = fifelse(is.na(RG), "--", formatC(RG, format = "f", digits = 1)),
            RR = fifelse(is.na(RR), "--", formatC(RR, format = "f", digits = 1)))]
setnames(pe_l, c("Measure", "Scenario", "Year", "T1, %", "T2, %", "T3, %", "Gap, pp", "T1/T3 ratio",
                 "Widening removed, gap, %", "Widening removed, ratio, %"))
escribir_tex(pe_l, "S8j_age_standardised.tex",
             paste("Smoking prevalence by income tertile, crude and directly age-standardised to the",
                   "status quo population of 2025 (age groups 15-24, 25-44, 45-64, 65+), with the T1-T3",
                   "gap, the T1/T3 ratio and the share of the projected widening of each that the tax",
                   "removes. Primary simulation run."), digitos = 2, tabcolsep = "3pt")

mtr <- leer("95_tc_muertes_tercil.csv")
mtr[, pct := 100 * evitadas / sum(evitadas), by = variante]
tot <-mtr[, .(tercil = 0L, n = n[1], evitadas = sum(evitadas), evitadas_ee = NA_real_, pct = 100,
               tasa_bruta = NA_real_, tasa_estandarizada = NA_real_, tasa_estandarizada_ee = NA_real_),
           by = variante]
ef_res <- leer("95_tc_efectos_resumen.csv")
for (v in unique(tot$variante))
  tot[variante == v, evitadas_ee := ef_res[variante == v & anio == 2060L]$muertes_evitadas_cum_ee]
mtr <- rbind(mtr, tot)
mtr[, Risk := c(principal = "Constant (primary)", rr_graduado = "Graded by time since quitting")[variante]]
mtr[, Tertile := c(`0` = "Total", `1` = "T1 (lowest income)", `2` = "T2", `3` = "T3 (highest income)")[as.character(tercil)]]
mtr[, DEATHS := sprintf("%s (%s)", format(round(evitadas), big.mark = ","), round(evitadas_ee))]
mtr[, RATE := fifelse(is.na(tasa_estandarizada), "--",
                      sprintf("%.2f (%.2f)", tasa_estandarizada, tasa_estandarizada_ee))]
mtr[, CRUDE := fifelse(is.na(tasa_bruta), "--", formatC(tasa_bruta, format = "f", digits = 2))]
mtr[, PCT := formatC(pct, format = "f", digits = 1)]
mtr[, ord := fifelse(tercil == 0L, 4L, tercil)]
setorder(mtr, variante, ord)
mtr <- mtr[, .(Risk, Tertile, DEATHS, PCT, CRUDE, RATE)]
setnames(mtr, c("Former-smoking risk", "Income tertile", "Deaths averted (SE)", "Share, %",
                "Crude rate", "Age-standardised rate (SE)"))
escribir_tex(mtr, "S8k_deaths_rates.tex",
             paste("Deaths averted 2027-2060 by income tertile, gradual convergence to 75%, under a constant",
                   "and a graded relative risk of death for people who have quit: means across 100 paired",
                   "runs (simulation standard error), share of the total, and rates per 100,000 person-years,",
                   "crude and age-standardised to the age structure of the status quo population."),
             alin_override = "p{3.6cm}p{2.4cm}rrrr", tabcolsep = "3pt")

d10 <- here::here("output", "tables", "diagnosticos")
t30 <- fread(file.path(d10, "tope2030", "resumen_tope2030.csv"))[anio == 2060L]
rec <- fread(file.path(d10, "recaida_cota", "resumen_recaida_cota.csv"))[anio == 2060L]
s93 <- leer("93_sensibilidades_calibracion.csv")[anio == 2060L]
fila <- function(nombre, d, v, pct_col) {
  r <- d[variante == v]
  if (nrow(r) != 1L) stop("falta la variante ", v)
  data.table(Variant = nombre,
             SQ = r$prev_base,
             CONS = r$cons_pct,
             PREV = r$prev_pp,
             DEATHS = round(if ("muertes" %in% names(r)) r$muertes else r$muertes_evitadas),
             PCT = if (is.null(pct_col)) NA_real_ else r[[pct_col]])
}
if (length(unique(rec$variante)) != 3L) stop("resumen_recaida_cota.csv incompleto")
sens <- rbindlist(list(
  fila("Primary specification (3 seeds)", t30, "principal", "pct_ensanch"),
  fila("Cessation trend continues to 2030", t30, "tope_2030", "pct_ensanch"),
  fila("Alternative calibration point", s93, "punto_alt", NULL),
  fila("Primary specification (20 seeds)", rec, "principal", "pct_remueve"),
  fila("Relapse at lower bound from age 45", rec, "cota_45", "pct_remueve"),
  fila("Relapse at lower bound from age 65", rec, "cota_65", "pct_remueve")
))
sens[, DEATHS := NULL]
sens[, PCT := fifelse(is.na(PCT), "--", formatC(PCT, format = "f", digits = 1))]
setnames(sens, c("Variant", "Status quo prevalence 2060, %", "Consumption change, %",
                 "Prevalence change, pp", "Widening removed, %"))
escribir_tex(sens, "S9_sensitivity.tex",
             paste("Additional sensitivity analyses at 2060, gradual convergence to 75%: means of paired",
                   "seeds, three for the cessation-trend and calibration rows and 20 for the relapse rows,",
                   "each block with its own primary row."), digitos = 2,
             alin_override = paste0("p{4.2cm}", strrep(">{\\raggedleft\\arraybackslash}p{1.6cm}", 4)),
             tabcolsep = "4pt")

efs <- leer("95_tc_efectos_por_semilla.csv")
efs[, ext_pct := fifelse(anio >= 2027L & consumo_mill_esc != consumo_mill_base,
                         100 * log(fumadores_esc / fumadores_base) / log(consumo_mill_esc / consumo_mill_base),
                         NA_real_)]
efm <- efs[, .(n = .N, cons = mean(cons_pct), cons_int = mean(cons_pct_int_parcial), prev = mean(prev_pp),
               ext = mean(ext_pct), cig = mean(cum_cig_evitados_mill),
               py = mean(cum_anios_fumando_evitados) / 1e3, rec = mean(cum_rec_extra_pct),
               prev_base = mean(prev_base), brecha_base = mean(brecha_base), brecha_esc = mean(brecha_esc),
               d_T1 = mean(d_T1), d_T3 = mean(d_T3), rem = mean(pct_ensanche_removido),
               rec_rel = mean(recaudacion_relativa), rec_imesi = mean(recaudacion_imesi_relativa),
               b25 = mean(b25)),
           by = .(variante, anio)]
n_sem <- efm[, .(n = max(n)), by = variante]
log_msg("  R/78: semillas por variante: %s", paste(sprintf("%s=%d", n_sem$variante, n_sem$n), collapse = ", "))
guion <- function(x, d = 2) fifelse(is.na(x), "--", formatC(x, format = "f", digits = d))

ANIOS_T <- c(2031L, 2036L, 2046L, 2060L)
NOM_T <- c(principal = "Primary: gradual, 2027-2031", abrupto = "Abrupt, 2027",
           inicio_2032 = "Gradual, 2032-2036", ces_transitoria = "Cessation responds only to 2033",
           ini_mitad = "Initiation elasticity x0.5", ini_doble = "Initiation elasticity x2")
tt <- efm[variante %in% names(NOM_T) & anio %in% ANIOS_T]
tt[, ord := match(variante, names(NOM_T))]
parc <- efm[variante == "principal" & anio %in% ANIOS_T][
  , `:=`(variante = "parcial", ord = 7L, cons = cons_int, prev = NA_real_, ext = NA_real_,
         cig = NA_real_, py = NA_real_, rec = NA_real_)]
tt <- rbind(tt, parc)
tt[, Variant := c(NOM_T, parcial = "Partial intensive adjustment")[variante]]
setorder(tt, ord, anio)
tt <- tt[, .(Variant, Year = anio, CONS = guion(cons), PREV = guion(prev, 3), EXT = guion(ext, 1),
             CIG = guion(cig, 0), PY = guion(py, 0), REC = guion(rec, 1))]
setnames(tt, c("Variant", "Year", "Consumption change, %", "Prevalence change, pp",
               "Extensive share, %", "Cum. cigarettes averted, millions",
               "Cum. person-years of smoking averted, thousands", "Cum. extra revenue, %"))
escribir_tex(tt, "S9b_timing.tex",
             paste("Timing assumptions: effects relative to the status quo in 2031, 2036, 2046 (5, 10 and",
                   "20 years after 2027) and 2060, with cumulative cigarettes and person-years of smoking",
                   "averted and cumulative additional tax revenue (excise plus VAT, % of status quo revenue)",
                   "from 2027. Means across seeds: 100 for the primary and abrupt specifications, 20 for",
                   "each other variant.",
                   "Partial intensive adjustment changes consumption only."),
             alin_override = paste0("p{3.4cm}r", strrep(">{\\raggedleft\\arraybackslash}p{1.6cm}", 6)),
             tabcolsep = "3pt")

NOM_D <- c(principal = "Primary specification", resp_igual = "Equal response by tertile")
dd <- efm[variante %in% names(NOM_D) & anio %in% c(2030L, 2045L, 2060L)]
dd[, ord := match(variante, names(NOM_D))]
setorder(dd, ord, anio)
dd <- dd[, .(Variant = NOM_D[variante], Year = anio, CONS = cons, PREV = prev, D1 = d_T1, D3 = d_T3,
             BB = brecha_base - b25, BE = brecha_esc - brecha_base, REM = rem)]
setnames(dd, c("Variant", "Year", "Consumption change, %", "Prevalence change, pp", "Change T1, pp",
               "Change T3, pp", "Status quo widening of gap since 2025, pp", "Change in gap with policy, pp",
               "Widening removed, %"))
escribir_tex(dd, "S9c_distributional.tex",
             paste("Distributional mechanism: primary specification against equal target elasticities, a",
                   "total elasticity of -0.639 and an extensive share of 0.50 in every tertile. Each tertile",
                   "keeps its correction factor, so its equilibrium elasticity of prevalence equals the common",
                   "target. Means across seeds (100 and 20)."),
             digitos = 2, alin_override = paste0("p{2.6cm}r", strrep(">{\\raggedleft\\arraybackslash}p{1.6cm}", 7)),
             tabcolsep = "2pt")

NOM_F <- c(principal = "Primary", traslado_desvio = "80% pass-through, illicit diversion",
           ryo_proporcional = "Roll-your-own excise raised in proportion",
           inicio_2025_obs = "Start 2025, as recorded", inicio_2025_corr = "Start 2025, proxy-corrected")
ff <- efm[variante %in% names(NOM_F) & anio %in% c(2025L, 2030L, 2060L)]
ff[, ord := match(variante, names(NOM_F))]
setorder(ff, ord, anio)
ff <- ff[, .(Variant = NOM_F[variante], Year = anio, SQ = prev_base, CONS = cons, PREV = prev,
             BB = brecha_base, REM = guion(fifelse(anio == 2025L, NA_real_, rem), 1),
             REC = rec_rel, RECI = rec_imesi)]
setnames(ff, c("Variant", "Year", "Status quo prevalence, %", "Consumption change, %", "Prevalence change, pp",
               "Status quo T1-T3 gap, pp", "Widening removed, %", "Revenue ratio, excise + VAT",
               "Revenue ratio, excise"))
escribir_tex(ff, "S9d_fiscal_start.tex",
             paste("Fiscal assumptions and starting point. Revenue is tax on legal volume relative to the",
                   "status quo in the same year. The 2025 starts use their own status quo branch. Means",
                   "across seeds (100 for the primary specification, 20 for each variant)."),
             digitos = 2, alin_override = paste0("p{2.6cm}r", strrep(">{\\raggedleft\\arraybackslash}p{1.6cm}", 7)),
             tabcolsep = "2pt")

log_msg("=== %d tablas -> paper/TC/supp/tables/*.tex ===", n_tabla)
close(log_con)
