# 24_motor_transiciones.R
#
# Simulation engine. Defines the annual cycle and every transition function; sourced by every
# script that runs the model.
# Annual cycle: mortality -> cessation -> relapse -> initiation -> escalation (occasional to daily)
# -> downshift (daily to occasional) -> ageing and time since quitting -> entry of the cohort aged 15.
# Price response: tertile-specific price elasticities (T1 -0.812, T2 -0.609, T3 -0.406; aggregate
# -0.639) split between the extensive margin (cessation and initiation rates) and the intensive
# margin (cigarettes per day). In policy scenarios rates respond to the price level relative to
# the status quo. The fiscal module computes excise, value added tax and the retail price for a
# given tax share.
# Constants are stored with 15-17 significant digits: the simulation is sensitive to the last
# digits of the parameters, and rounding changes individual random draws.
# Input:  data/interim/modelo_*.rds, data/interim/poblacion_sintetica_*.csv.gz,
#         output/tables/41_*, 42_*, 56_*, data/raw/Proyecciones_poblacion/
# Output: none (running it directly performs a one-cycle self-test)

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/24_motor_transiciones.R")

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")
raw_dir <- here::here("data", "raw")

ELASTICIDAD_TOTAL_TERCIL <- c(`1` = -0.812446329444692, `2` = -0.609334747083519,
                              `3` = -0.406223164722346)

SHARE_EXTENSIVO_TERCIL <- c(`1` = 0.63174155472705973,
                            `2` = 0.46283854935876201,
                            `3` = 0.34000000000000002)
ELASTICIDAD_EXTENSIVA_TERCIL <- ELASTICIDAD_TOTAL_TERCIL * SHARE_EXTENSIVO_TERCIL
ELASTICIDAD_INTENSIVA_TERCIL <- ELASTICIDAD_TOTAL_TERCIL * (1 - SHARE_EXTENSIVO_TERCIL)

FACTOR_TRASLADO_EXTENSIVO_TERCIL <- c(`1` = 0.84620863217801601,
                                      `2` = 0.76446262095470618,
                                      `3` = 0.76293195436685812)

EDAD_TOPE_INICIACION <- 50
ANIO_TOPE_TENDENCIA <- 2025

ELASTICIDAD_LEGAL_FERRE <- -0.77

INFLACION_PROYECTADA <- 0.045

proyectar_impuesto_rezagado <- function(tau_ancla, anio_ancla, anios, inflacion) {
  anios <- sort(anios)
  todos <- anio_ancla:max(anios)
  if (is.data.table(inflacion)) {
    pi_anual <- merge(data.table(anio = todos), inflacion, by = "anio", all.x = TRUE)$pi
    if (anyNA(pi_anual)) stop("proyectar_impuesto_rezagado(): faltan tasas de inflacion para ",
                               paste(todos[is.na(pi_anual)], collapse = ", "))
  } else {
    pi_anual <- rep(inflacion, length(todos))
  }

  ipc <- 1
  H <- numeric(length(todos))
  for (i in seq_along(todos)) {
    m <- (1 + pi_anual[i])^(1 / 12) - 1
    inv <- numeric(12)
    for (j in 1:12) { inv[j] <- 1 / ipc; ipc <- ipc * (1 + m) }
    H[i] <- mean(inv)
  }
  N <- numeric(length(todos))
  N[1] <- 1
  for (i in seq_along(todos)[-1]) N[i] <- N[i - 1] * (1 + pi_anual[i - 1])

  tau <- tau_ancla * (N * H) / (N[1] * H[1])
  data.table(anio = todos, tau_real = as.numeric(tau))[anio %in% anios]
}

construir_precio_lookup <- function(anio_min = 2011, anio_max = 2025,
                                     regla = "ultimo_observado_plano",
                                     inflacion = INFLACION_PROYECTADA,
                                     trayectoria_impuesto = NULL) {
  regla <- match.arg(regla, c("ultimo_observado_plano", "mantenimiento_politica",
                               "impuesto_real_constante"))
  panel_fiscal <- fread(file.path(out_dir, "41_panel_fiscal_anual.csv"))
  precio <- panel_fiscal[, .(anio, p_reales, porcentaje_tax_p, n_meses_precio)]
  ultimo_anio_dato <- max(precio[!is.na(p_reales)]$anio)
  anio_ancla <- max(precio[!is.na(p_reales) & !is.na(porcentaje_tax_p) & n_meses_precio == 12]$anio)

  grilla <- data.table(anio = anio_min:anio_max)
  grilla <- merge(grilla, precio[, .(anio, p_reales)], by = "anio", all.x = TRUE)

  if (regla == "ultimo_observado_plano") {
    grilla[anio > ultimo_anio_dato, p_reales := precio[anio == ultimo_anio_dato]$p_reales]
  } else {
    p_ancla <- precio[anio == anio_ancla]$p_reales
    share_tax <- precio[anio == anio_ancla]$porcentaje_tax_p / 100
    tau_ancla <- p_ancla * share_tax
    neto_ancla <- p_ancla * (1 - share_tax)

    anios_proy <- grilla[anio > ultimo_anio_dato]$anio
    if (length(anios_proy) > 0) {
      tau <- if (regla == "impuesto_real_constante") {
        data.table(anio = anios_proy, tau_real = tau_ancla)
      } else {
        proyectar_impuesto_rezagado(tau_ancla, anio_ancla, anios_proy, inflacion)
      }
      if (!is.null(trayectoria_impuesto)) {
        tau <- merge(tau, trayectoria_impuesto, by = "anio", all.x = TRUE)
        tau[is.na(factor_impuesto_real), factor_impuesto_real := 1]
        tau[, tau_real := tau_real * factor_impuesto_real]
      }
      tau_ult <- if (ultimo_anio_dato <= anio_ancla) tau_ancla else {
        proyectar_impuesto_rezagado(tau_ancla, anio_ancla, ultimo_anio_dato, inflacion)$tau_real
      }
      p_ult_obs <- precio[anio == ultimo_anio_dato]$p_reales
      grilla <- merge(grilla, tau[, .(anio, tau_real)], by = "anio", all.x = TRUE)
      grilla[anio > ultimo_anio_dato, p_reales := p_ult_obs + (tau_real - tau_ult)]
      grilla[, tau_real := NULL]
      setattr(grilla, "tau_ultimo_anio_dato", tau_ult)
    }
    setattr(grilla, "anio_ancla", anio_ancla)
    setattr(grilla, "tau_ancla", tau_ancla)
    setattr(grilla, "neto_ancla", neto_ancla)
  }
  setattr(grilla, "ultimo_anio_dato", ultimo_anio_dato)
  setattr(grilla, "regla", regla)
  grilla
}

CORTES_TRAMO_EX <- c(0, 5, 10, 20, 30)
NIVELES_TRAMO_EX <- c("<5", "5-9", "10-19", "20-29", "30+")
tramo_ex_de <- function(estado, anios_desde_dejar) {
  fifelse(estado %in% c(4L, 5L),
          as.character(cut(anios_desde_dejar, breaks = c(CORTES_TRAMO_EX, Inf),
                            labels = NIVELES_TRAMO_EX, right = FALSE)),
          "-")
}

REGISTRO_MUERTES <- new.env(parent = emptyenv())

activar_registro_muertes <- function() {
  REGISTRO_MUERTES$activo <- TRUE
  REGISTRO_MUERTES$filas  <- list()
  invisible(NULL)
}

desactivar_registro_muertes <- function() {
  REGISTRO_MUERTES$activo <- NULL
  REGISTRO_MUERTES$filas  <- NULL
  invisible(NULL)
}

muertes_registradas <- function() {
  if (is.null(REGISTRO_MUERTES$filas) || length(REGISTRO_MUERTES$filas) == 0L)
    return(data.table(anio = integer(), estado = integer(), tercil = integer(),
                      muertes = integer()))
  rbindlist(REGISTRO_MUERTES$filas)
}

aplicar_mortalidad <- function(pop, anio_actual, tabla_superv) {
  pop[, edad_lookup := pmin(edad, 99)]
  usa_tramo <- "tramo_ex" %in% names(tabla_superv)
  if (usa_tramo) {
    sup_anio <- tabla_superv[anio == anio_actual, .(edad = edad, sexo, estado, tramo_ex, prob_sobrevive)]
    pop[, tramo_ex := tramo_ex_de(estado, anios_desde_dejar)]
    pop[is.na(tramo_ex), tramo_ex := NIVELES_TRAMO_EX[1]]
    pop <- merge(pop, sup_anio, by.x = c("edad_lookup", "sexo", "estado", "tramo_ex"),
                 by.y = c("edad", "sexo", "estado", "tramo_ex"), all.x = TRUE)
    pop[, tramo_ex := NULL]
  } else {
    sup_anio <- tabla_superv[anio == anio_actual, .(edad = edad, sexo, estado, prob_sobrevive)]
    pop <- merge(pop, sup_anio, by.x = c("edad_lookup", "sexo", "estado"), by.y = c("edad", "sexo", "estado"), all.x = TRUE)
  }
  n_sin_celda <- pop[is.na(prob_sobrevive), .N]
  if (n_sin_celda > 0) {
    warning(sprintf("aplicar_mortalidad(%d): %d individuos sin celda en la tabla de supervivencia -- no mueren. Revisar la grilla de R/23.",
                    anio_actual, n_sin_celda))
    pop[is.na(prob_sobrevive), prob_sobrevive := 1]
  }
  registrando <- !is.null(REGISTRO_MUERTES$activo)
  if (registrando) antes <- pop[, .(n_antes = .N), by = .(estado, tercil, grupo_edad = grupo_etario_intensidad(edad))]
  pop <- pop[runif(.N) < prob_sobrevive]
  if (registrando) {
    despues <- pop[, .(n_despues = .N), by = .(estado, tercil, grupo_edad = grupo_etario_intensidad(edad))]
    m <- merge(antes, despues, by = c("estado", "tercil", "grupo_edad"), all.x = TRUE)
    m[is.na(n_despues), n_despues := 0L]
    m[, `:=`(anio = anio_actual, muertes = n_antes - n_despues)]
    REGISTRO_MUERTES$filas[[length(REGISTRO_MUERTES$filas) + 1L]] <-
      m[, .(anio, estado, tercil, grupo_edad, muertes)]
  }
  pop[, c("prob_sobrevive", "edad_lookup") := NULL]
  pop
}

aplicar_cesacion <- function(pop, anio_actual, modelo_cesacion, precio_ratio_tercil,
                              mult_cesacion_edad = 1, edad_corte_cesacion = 45) {
  idx <- which(pop$estado %in% c(2, 3))
  if (length(idx) == 0) return(pop)
  sub <- pop[idx]
  nd <- sub[, .(edad, sexo, tercil, anio_c = pmin(anio_actual, ANIO_TOPE_TENDENCIA) - 2018,
                corte_2019 = as.integer(anio_actual >= 2019), corte_2021 = as.integer(anio_actual >= 2021))]
  haz <- predict(modelo_cesacion, newdata = nd, type = "response")
  if (mult_cesacion_edad != 1) {
    en_cola <- sub$edad >= edad_corte_cesacion
    haz[en_cola] <- haz[en_cola] * mult_cesacion_edad
  }
  mult <- precio_ratio_tercil[as.character(sub$tercil)]^(-ELASTICIDAD_EXTENSIVA_TERCIL[as.character(sub$tercil)] *
                                                       FACTOR_TRASLADO_EXTENSIVO_TERCIL[as.character(sub$tercil)])
  haz_ajustado <- pmin(1, pmax(0, haz * mult))
  evento <- runif(nrow(sub)) < haz_ajustado
  idx_evento <- idx[evento]
  pop[idx_evento, `:=`(estado = 4, anios_desde_dejar = 0,
                        patron_antes_de_dejar = fifelse(estado == 3, "diario", "ocasional"))]
  pop
}

GRUPOS_EDAD_RECAIDA <- function(edad) {
  cut(edad, breaks = c(-Inf, 34, 44, 54, 64, Inf),
      labels = c("15-34", "35-44", "45-54", "55-64", "65+"))
}

predict.recaida_hibrida <- function(object, newdata, type = "response", ...) {
  es_mayor <- as.character(newdata$grupo_edad) %in% object$grupos_mayores
  out <- numeric(nrow(newdata))
  if (any(!es_mayor))
    out[!es_mayor] <- predict(object$joven, newdata = newdata[!es_mayor], type = type)
  if (any(es_mayor))
    out[es_mayor] <- predict(object$mayor, newdata = newdata[es_mayor], type = type)
  out
}

recaida_usa_edad <- function(modelo) {
  if (inherits(modelo, "recaida_hibrida")) return(TRUE)
  "grupo_edad" %in% all.vars(formula(modelo))
}

RECAIDA_EN_CICLO_DE_CESACION <- FALSE

aplicar_recaida <- function(pop, modelo_recaida) {
  expuesto <- pop$estado %in% c(4, 5)
  if (!RECAIDA_EN_CICLO_DE_CESACION) expuesto <- expuesto & pop$anios_desde_dejar > 0
  idx <- which(expuesto)
  if (length(idx) == 0) return(pop)
  sub <- pop[idx]
  usa_edad <- recaida_usa_edad(modelo_recaida)
  nd <- if (usa_edad) {
    sub[, .(punto_medio = pmax(anios_desde_dejar, 0.05), sexo,
            grupo_edad = GRUPOS_EDAD_RECAIDA(edad))]
  } else {
    sub[, .(punto_medio = pmax(anios_desde_dejar, 0.05), sexo)]
  }
  haz <- predict(modelo_recaida, newdata = nd, type = "response")
  evento <- runif(nrow(sub)) < pmin(1, pmax(0, haz))
  idx_evento <- idx[evento]
  pop[idx_evento, estado := fifelse(patron_antes_de_dejar == "diario", 3, 2)]
  pop[idx_evento, `:=`(anios_desde_dejar = NA_real_, patron_antes_de_dejar = NA_character_)]
  pop
}

aplicar_iniciacion <- function(pop, anio_actual, modelo_iniciacion, precio_ratio_tercil,
                                mult_iniciacion = 1) {
  idx <- which(pop$estado == 1)
  if (length(idx) == 0) return(pop)
  sub <- pop[idx]
  nd <- sub[, .(edad_media_prev = pmin(edad, EDAD_TOPE_INICIACION),
                anio_prev_c = pmin(anio_actual, ANIO_TOPE_TENDENCIA) - 2018,
                sexo, tercil, delta_anios = 1)]
  haz <- predict(modelo_iniciacion, newdata = nd, type = "response")
  mult <- precio_ratio_tercil[as.character(sub$tercil)]^(ELASTICIDAD_EXTENSIVA_TERCIL[as.character(sub$tercil)] *
                                                      FACTOR_TRASLADO_EXTENSIVO_TERCIL[as.character(sub$tercil)])
  haz_ajustado <- pmin(1, pmax(0, haz * mult * mult_iniciacion))
  evento <- runif(nrow(sub)) < haz_ajustado
  pop[idx[evento], estado := 2]
  pop
}

hazard_escalada <- function(edad, params) {
  edad_efectiva <- pmin(edad, EDAD_TOPE_INICIACION)
  p0 <- plogis(params[1])
  k <- exp(params[2])
  p0 * exp(-k * pmax(edad_efectiva - 15, 0))
}

aplicar_escalada <- function(pop, params_escalada) {
  idx <- which(pop$estado == 2)
  if (length(idx) == 0) return(pop)
  haz <- hazard_escalada(pop$edad[idx], params_escalada)
  evento <- runif(length(idx)) < haz
  pop[idx[evento], estado := 3]
  pop
}

aplicar_downshift <- function(pop, param_downshift) {
  idx <- which(pop$estado == 3)
  if (length(idx) == 0) return(pop)
  p <- plogis(param_downshift)
  evento <- runif(length(idx)) < p
  pop[idx[evento], estado := 2]
  pop
}

avanzar_edad <- function(pop) {
  pop[, edad := edad + 1]
  pop[estado == 4 & anios_desde_dejar >= 1, estado := 5]
  pop[estado %in% c(4, 5), anios_desde_dejar := anios_desde_dejar + 1]
  pop
}

generar_entrantes_15 <- function(anio_nuevo, pob_ine, entrada_15_por_anio) {
  anio_lookup <- pmin(pmax(anio_nuevo, 2011), 2045)
  n15 <- pob_ine[edad == 15 & anio == anio_nuevo, .(sexo, poblacion)]
  entrantes <- rbindlist(lapply(seq_len(nrow(n15)), function(i) {
    sx <- n15$sexo[i]
    n <- round(n15$poblacion[i])
    dist_sx <- entrada_15_por_anio[anio == anio_lookup & sexo == sx]
    estado_asignado <- sample(dist_sx$estado, size = n, replace = TRUE, prob = dist_sx$share)
    data.table(edad = 15L, sexo = sx, estado = estado_asignado)
  }))
  entrantes[, tercil := sample(1:3, .N, replace = TRUE)]
  entrantes[, anios_desde_dejar := NA_real_]
  entrantes[estado == 4, anios_desde_dejar := runif(.N, 0, 1)]
  entrantes[estado == 5, anios_desde_dejar := runif(.N, 1, 5)]
  entrantes[, patron_antes_de_dejar := NA_character_]
  entrantes[estado %in% c(4, 5), patron_antes_de_dejar := fifelse(runif(.N) < 1898 / (1898 + 795), "diario", "ocasional")]
  entrantes <- asignar_u_intensidad(entrantes, SEMILLA_INTENSIDAD + anio_nuevo)
  entrantes
}

con_semilla_aislada <- function(semilla, expr) {
  tenia <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (tenia) viejo <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (tenia) assign(".Random.seed", viejo, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(semilla)
  force(expr)
}

SEMILLA_INTENSIDAD <- 20260811L

asignar_u_intensidad <- function(pop, semilla = SEMILLA_INTENSIDAD) {
  u <- con_semilla_aislada(semilla, runif(nrow(pop)))
  pop[, u_intensidad := u]
  pop
}

grupo_etario_intensidad <- function(edad) {
  fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")
}

cargar_cuantiles_intensidad <- function() {
  q <- fread(file.path(out_dir, "56_intensidad_cuantiles_celda.csv"))
  q[, u_idx := as.integer(round(u / min(u)))]
  setkey(q, frecuencia, sexo, grupo_etario, u_idx)
  pref <- fread(file.path(out_dir, "56_intensidad_precio_referencia.csv"))
  list(cuantiles = q, paso_u = min(q$u), n_u = max(q$u_idx),
       precio_ref = pref[concepto == "precio_real_referencia_intensidad"]$valor)
}

calcular_cig_dia <- function(pop, anio_actual, intensidad, precio_lookup) {
  cig <- numeric(nrow(pop))
  fuma <- which(pop$estado %in% c(2L, 3L))
  if (length(fuma) == 0) return(cig)
  if (!"u_intensidad" %in% names(pop))
    stop("calcular_cig_dia(): la poblacion no trae u_intensidad; correr asignar_u_intensidad() primero")

  clave <- data.table(
    frecuencia   = fifelse(pop$estado[fuma] == 3L, "diario", "ocasional"),
    sexo         = as.character(pop$sexo[fuma]),
    grupo_etario = grupo_etario_intensidad(pop$edad[fuma]),
    u_idx        = pmin(pmax(as.integer(ceiling(pop$u_intensidad[fuma] / intensidad$paso_u)), 1L),
                        intensidad$n_u)
  )
  base <- intensidad$cuantiles[clave, on = .(frecuencia, sexo, grupo_etario, u_idx), x.cig_dia]
  if (anyNA(base)) stop("calcular_cig_dia(): hay celdas sin cuantil en 56_intensidad_cuantiles_celda.csv")

  p_t <- precio_lookup[anio == anio_actual]$p_reales
  if (length(p_t) != 1 || is.na(p_t)) stop("calcular_cig_dia(): sin precio para el anio ", anio_actual)
  eps <- ELASTICIDAD_INTENSIVA_TERCIL[as.character(pop$tercil[fuma])]
  cig[fuma] <- base * (p_t / intensidad$precio_ref)^eps
  cig
}

ciclo_anual <- function(pop, anio_actual, modelos, tabla_superv, precio_lookup, pob_ine, entrada_15_por_anio,
                         params_escalada, param_downshift, precio_ratio_override = NULL,
                         mult_iniciacion = 1, mult_cesacion_edad = 1, edad_corte_cesacion = 45,
                         resp_precio_cesacion = TRUE, resp_precio_iniciacion = TRUE,
                         precio_ratio_cesacion = NULL, precio_ratio_iniciacion = NULL) {
  ratio <- if (is.null(precio_ratio_override)) {
    precio_lookup[anio == anio_actual]$p_reales / precio_lookup[anio == anio_actual - 1]$p_reales
  } else {
    precio_ratio_override
  }
  precio_ratio_tercil <- c(`1` = 1, `2` = 1, `3` = 1) * ratio

  ratio_cesacion   <- precio_ratio_tercil
  ratio_iniciacion <- precio_ratio_tercil
  if (!resp_precio_cesacion)   ratio_cesacion   <- c(`1` = 1, `2` = 1, `3` = 1)
  if (!resp_precio_iniciacion) ratio_iniciacion <- c(`1` = 1, `2` = 1, `3` = 1)
  if (!is.null(precio_ratio_cesacion))   ratio_cesacion   <- c(`1` = 1, `2` = 1, `3` = 1) * precio_ratio_cesacion
  if (!is.null(precio_ratio_iniciacion)) ratio_iniciacion <- c(`1` = 1, `2` = 1, `3` = 1) * precio_ratio_iniciacion

  pop <- aplicar_mortalidad(pop, anio_actual, tabla_superv)
  pop <- aplicar_cesacion(pop, anio_actual, modelos$cesacion, ratio_cesacion,
                           mult_cesacion_edad, edad_corte_cesacion)
  pop <- aplicar_recaida(pop, modelos$recaida)
  pop <- aplicar_iniciacion(pop, anio_actual, modelos$iniciacion, ratio_iniciacion, mult_iniciacion)
  pop <- aplicar_escalada(pop, params_escalada)
  pop <- aplicar_downshift(pop, param_downshift)
  pop <- avanzar_edad(pop)
  entrantes <- generar_entrantes_15(anio_actual + 1, pob_ine, entrada_15_por_anio)
  pop <- rbind(pop, entrantes, fill = TRUE)
  pop
}

ANIO_POB_PROYECCION <- 2011L

cargar_poblacion_proyeccion <- function(anio = ANIO_POB_PROYECCION, sufijo = "") {
  pop <- fread(file.path(int_dir, sprintf("poblacion_sintetica_%d%s.csv.gz", anio, sufijo)))
  pop[, sexo := factor(sexo, levels = c("varon", "mujer"))]
  pop[, tercil := factor(tercil, levels = c(1, 2, 3))]
  asignar_u_intensidad(pop)
}

FISCAL_OMS_2024 <- list(precio_cajilla = 210, share_total = 0.6653, share_imesi = 0.4850, iva = 0.22,
                        ryo_share_total = 0.5043, ryo_share_imesi = 0.3240,
                        ryo_share_unidades = 0.27)

trayectoria_fiscal <- function(precio_base, share_objetivo, anio_inicio, modo = c("abrupto", "gradual"),
                               horizonte_gradual = 5L, anio_max = 2070L,
                               ryo = c("igual_precio", "proporcional"), traslado = 1, desvio = 0,
                               desvio_gradual = FALSE, fis = FISCAL_OMS_2024) {
  modo <- match.arg(modo); ryo <- match.arg(ryo)
  V <- fis$iva / (1 + fis$iva)
  N <- 1 - fis$share_total; I <- fis$share_imesi
  stopifnot(abs((N + I) * (1 + fis$iva) - 1) < 2e-4)
  P_obj <- N / (1 - share_objetivo)
  fI_obj <- (P_obj / (1 + fis$iva) - N) / I
  anios <- anio_inicio:anio_max
  fI <- if (modo == "abrupto") rep(fI_obj, length(anios)) else {
    rampa <- seq(1, fI_obj, length.out = horizonte_gradual + 1L)[-1]
    c(rampa, rep(fI_obj, max(0L, length(anios) - length(rampa))))[seq_along(anios)]
  }
  r_cig <- (N + I * fI) * (1 + fis$iva)
  r <- if (ryo == "igual_precio") r_cig else {
    Nr <- 1 - fis$ryo_share_total; Ir <- fis$ryo_share_imesi
    r_ryo <- (Nr + Ir * fI) * (1 + fis$iva)
    (1 - fis$ryo_share_unidades) * r_cig + fis$ryo_share_unidades * r_ryo
  }
  r <- 1 + traslado * (r - 1)
  S_V <- fis$share_total - fis$share_imesi
  f_tot <- (I * fI + S_V * (1 + traslado * (r_cig - 1))) / fis$share_total
  d <- if (desvio_gradual && fI_obj > 1) desvio * (fI - 1) / (fI_obj - 1) else desvio
  r <- 1 + (1 - d) * (r - 1)
  p_ult <- precio_base[anio == attr(precio_base, "ultimo_anio_dato")]$p_reales
  tau_ult <- attr(precio_base, "tau_ultimo_anio_dato")
  if (is.null(tau_ult)) tau_ult <- attr(precio_base, "tau_ancla")
  p_b <- precio_base[match(anios, anio)]$p_reales
  tau_t <- p_b - p_ult + tau_ult
  f_eq <- 1 + (r - 1) * p_b / tau_t
  data.table(anio = anios, factor_impuesto_real = f_eq, factor_imesi = fI,
             factor_tributo_total = f_tot, ratio_precio = r, ratio_precio_cajilla = r_cig, desvio = d)
}

cargar_insumos_motor <- function(regla_precio = "ultimo_observado_plano",
                                  inflacion = INFLACION_PROYECTADA,
                                  trayectoria_impuesto = NULL,
                                  anio_max_precio = 2045) {
  list(
    modelos = list(
      cesacion = readRDS(file.path(int_dir, "modelo_cesacion.rds")),
      iniciacion = readRDS(file.path(int_dir, "modelo_iniciacion.rds")),
      recaida = readRDS(file.path(int_dir, "modelo_recaida.rds"))
    ),
    tabla_superv = fread(file.path(out_dir, "42_supervivencia_por_estado.csv")),
    precio_lookup = construir_precio_lookup(2010, anio_max_precio, regla = regla_precio,
                                             inflacion = inflacion,
                                             trayectoria_impuesto = trayectoria_impuesto),
    pob_ine = { d <- setDT(read_dta(file.path(raw_dir, "Proyecciones_poblacion", "poblacion_1950_2070_empalmada.dta")))
                setnames(d, c("año", "mujer"), c("anio", "mujer"), skip_absent = TRUE)
                d[, sexo := fifelse(mujer == 1, "mujer", "varon")]
                d[, .(anio, edad, sexo, poblacion)] },
    entrada_15_por_anio = fread(file.path(out_dir, "38_entrada_15_por_anio.csv")),
    intensidad = tryCatch(cargar_cuantiles_intensidad(), error = function(e) NULL)
  )
}

if (sys.nframe() == 0) {
  log_con <- file(here::here("logs", "24_motor_transiciones.log"), open = "wt", encoding = "UTF-8")
  log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
  log_msg("=== 24_motor_transiciones.R -- %s (autotest) ===", format(Sys.time()))

  set.seed(20260809)
  insumos <- cargar_insumos_motor()
  pop0 <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"))
  pop0[, sexo := factor(sexo, levels = c("varon", "mujer"))]
  pop0[, tercil := factor(tercil, levels = c(1, 2, 3))]

  log_msg("Poblacion inicial 2011: %d individuos. Distribucion de estados:\n%s",
          nrow(pop0), paste(capture.output(print(pop0[, .N, by = estado][order(estado)])), collapse = "\n"))

  params_escalada_inicial <- c(qlogis(0.05), log(0.05))
  param_downshift_inicial <- qlogis(0.03)

  t0 <- Sys.time()
  pop1 <- ciclo_anual(copy(pop0), 2011, insumos$modelos, insumos$tabla_superv, insumos$precio_lookup,
                       insumos$pob_ine, insumos$entrada_15_por_anio, params_escalada_inicial, param_downshift_inicial)
  log_msg("Un ciclo (2011->2012) corrido en %.1f segundos", as.numeric(difftime(Sys.time(), t0, units = "secs")))

  log_msg("Poblacion 2012 (post-ciclo): %d individuos (%d en 2011; diferencia = mortalidad neta - entrantes).\nDistribucion de estados:\n%s",
          nrow(pop1), nrow(pop0), paste(capture.output(print(pop1[, .N, by = estado][order(estado)])), collapse = "\n"))
  log_msg("Chequeo de rango de edad tras el ciclo: %d a %d", min(pop1$edad), max(pop1$edad))
  log_msg("Individuos en estado 4/5 con anios_desde_dejar NA (no deberia haber ninguno): %d",
          pop1[estado %in% c(4, 5) & is.na(anios_desde_dejar), .N])

  close(log_con)
}
