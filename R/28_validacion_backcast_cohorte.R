# 28_validacion_backcast_cohorte.R
#
# The retrospective run broken down by birth cohort, age group, income tertile and smoking state,
# including the decomposition of the prevalence error by age.
# Input:  as R/26, plus output/tables/25_*, 36_*, 39_*, 42_*, 44_*
# Output: output/tables/46_*.csv to 51_*.csv

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/28_validacion_backcast_cohorte.R")

log_con <- file(here::here("logs", "28_validacion_backcast_cohorte.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 28_validacion_backcast_cohorte.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

source(here::here("R", "24_motor_transiciones.R"))

ANIOS_VALIDACION <- c(2014, 2017, 2018, 2019, 2021, 2022, 2023, 2024, 2025)

cohorte_5 <- function(anio_nac) {
  pmin(pmax(floor(anio_nac / 5) * 5, 1925), 2005)
}

log_msg("\n--- 1) Corrida completa 2011-2025 (mismos parametros calibrados que R/26) ---")

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor,
            params_cal[parametro == "raw_par3"]$valor)
params_escalada <- params[1:2]
param_downshift <- params[3]
log_msg("Parametros calibrados cargados: escalada_15=%.4f, decaimiento=%.4f, downshift=%.4f",
        plogis(params[1]), exp(params[2]), plogis(params[3]))

set.seed(20260809)
pop <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"))
pop[, sexo := factor(sexo, levels = c("varon", "mujer"))]
pop[, tercil := factor(tercil, levels = c(1, 2, 3))]
insumos <- cargar_insumos_motor()

anio_actual <- 2011
snapshots <- list()
t0 <- Sys.time()
for (anio_obj in ANIOS_VALIDACION) {
  while (anio_actual < anio_obj) {
    pop <- ciclo_anual(pop, anio_actual, insumos$modelos, insumos$tabla_superv, insumos$precio_lookup,
                        insumos$pob_ine, insumos$entrada_15_por_anio, params_escalada, param_downshift)
    anio_actual <- anio_actual + 1
  }
  snapshots[[as.character(anio_obj)]] <- copy(pop)[, anio_snapshot := anio_obj]
  log_msg("Snapshot %d: %d individuos", anio_obj, nrow(pop))
}
log_msg("Corrida completa terminada en %.1f segundos", as.numeric(difftime(Sys.time(), t0, units = "secs")))

log_msg("\n--- 2) Cohorte y clasificacion limpia/contaminada ---")

criterio_limpias <- fread(file.path(out_dir, "36_criterio_cohortes_limpias.csv"))
criterio_limpias <- criterio_limpias[, .(cohorte, cohorte_limpia)]
log_msg("Cohortes limpias (%d): %s", sum(criterio_limpias$cohorte_limpia),
        paste(criterio_limpias[cohorte_limpia == TRUE]$cohorte, collapse = ", "))

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))
P[, grupo_etario := fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")]
P <- merge(P, criterio_limpias, by = "cohorte", all.x = TRUE)
log_msg("Observado (panel ECH): %d filas sin cohorte_limpia asignada (cohorte fuera de 36_criterio_cohortes_limpias.csv)",
        sum(is.na(P$cohorte_limpia)))

for (nm in names(snapshots)) {
  dt <- snapshots[[nm]]
  dt[, cohorte := cohorte_5(anio_snapshot - edad)]
  dt[, grupo_etario := fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")]
  dt2 <- merge(dt, criterio_limpias, by = "cohorte", all.x = TRUE)
  snapshots[[nm]] <- dt2
}
log_msg("Simulado: cohorte asignada en cada snapshot con la misma formula de R/03 (cohorte_5).")

log_msg("\n--- 3) Prevalencia actual, restringida a cohortes limpias (1985-2005) ---")

observado_limpias <- P[anio %in% ANIOS_VALIDACION & !is.na(tercil) & cohorte_limpia == TRUE, .(
  prevalencia_obs = sum(peso * (estado %in% c(2, 3))) / sum(peso),
  N_pond = sum(peso)
), by = .(anio)]

simulado_limpias <- rbindlist(lapply(snapshots, function(dt) {
  dt[cohorte_limpia == TRUE, .(prevalencia_sim = mean(estado %in% c(2, 3)), N = .N), by = .(anio = anio_snapshot)]
}))

comparacion_limpias <- merge(observado_limpias, simulado_limpias, by = "anio")
comparacion_limpias[, diferencia := prevalencia_sim - prevalencia_obs]
setorder(comparacion_limpias, anio)
fwrite(comparacion_limpias, file.path(out_dir, "46_validacion_prevalencia_cohortes_limpias.csv"))
log_msg("Prevalencia actual (2+3), simulado vs observado, SOLO cohortes limpias:\n%s",
        paste(capture.output(print(comparacion_limpias)), collapse = "\n"))

resumen_nacional <- fread(file.path(out_dir, "44_validacion_prevalencia_resumen_anual.csv"))
log_msg("Referencia -- brecha nacional SIN filtrar (de 44_validacion_prevalencia_resumen_anual.csv, generada por R/26):\n%s",
        paste(capture.output(print(resumen_nacional)), collapse = "\n"))

log_msg("\n--- 4) Desglose por estado, separado cohorte limpia / contaminada ---")

totales_obs <- P[anio %in% ANIOS_VALIDACION & !is.na(cohorte_limpia), .(total_pond = sum(peso)), by = .(anio, cohorte_limpia)]
estados_obs <- P[anio %in% ANIOS_VALIDACION & !is.na(cohorte_limpia), .(N_pond = sum(peso)), by = .(anio, cohorte_limpia, estado)]
estados_obs <- merge(estados_obs, totales_obs, by = c("anio", "cohorte_limpia"))
estados_obs[, share_obs := N_pond / total_pond]
estados_obs[, c("N_pond", "total_pond") := NULL]

estados_sim <- rbindlist(lapply(snapshots, function(dt) {
  dt[!is.na(cohorte_limpia), .N, by = .(anio = anio_snapshot, cohorte_limpia, estado)][
    , share_sim := N / sum(N), by = .(anio, cohorte_limpia)][, N := NULL]
}))

estados_comp_cohorte <- merge(estados_obs, estados_sim, by = c("anio", "cohorte_limpia", "estado"))
estados_comp_cohorte[, diferencia := share_sim - share_obs]
setorder(estados_comp_cohorte, cohorte_limpia, anio, estado)
fwrite(estados_comp_cohorte, file.path(out_dir, "46_validacion_por_estado_cohorte.csv"))
log_msg("Desglose por estado, share DENTRO de cada grupo (limpia/contaminada), simulado vs observado:\n%s",
        paste(capture.output(print(estados_comp_cohorte)), collapse = "\n"))

log_msg("\n--- 5) Brecha del motor en 'nunca' por cohorte contaminada, vs cota de mortalidad ---")

exceso_mortalidad <- fread(file.path(out_dir, "25_exceso_acumulado.csv"))
cohortes_informativas <- c(1950, 1955, 1960, 1965, 1970, 1975, 1980, 1985)
exceso_mortalidad <- exceso_mortalidad[cohorte %in% cohortes_informativas]

nunca_obs_cohorte <- P[anio %in% ANIOS_VALIDACION & cohorte %in% cohortes_informativas, .(
  prop_nunca_obs = sum(peso * (estado == 1)) / sum(peso)
), by = .(anio, cohorte)]

nunca_sim_cohorte <- rbindlist(lapply(snapshots, function(dt) {
  dt[cohorte %in% cohortes_informativas, .(prop_nunca_sim = mean(estado == 1)), by = .(anio = anio_snapshot, cohorte)]
}))

nunca_comp_cohorte <- merge(nunca_obs_cohorte, nunca_sim_cohorte, by = c("anio", "cohorte"))
nunca_comp_cohorte[, brecha := prop_nunca_sim - prop_nunca_obs]

brecha_motor_por_cohorte <- nunca_comp_cohorte[, .(
  brecha_motor_media = mean(brecha),
  brecha_motor_2025 = brecha[anio == 2025][1]
), by = cohorte]

comparacion_cota <- merge(brecha_motor_por_cohorte, exceso_mortalidad[, .(cohorte, exceso_acumulado, frac_pares_positivo)], by = "cohorte")
comparacion_cota[, dentro_de_cota := abs(brecha_motor_media) <= abs(exceso_acumulado)]
setorder(comparacion_cota, cohorte)
fwrite(comparacion_cota, file.path(out_dir, "46_comparacion_cota_mortalidad.csv"))
log_msg("Brecha del motor en 'nunca' por cohorte contaminada (informativa) vs. cota de mortalidad (25_exceso_acumulado.csv):\n%s",
        paste(capture.output(print(comparacion_cota)), collapse = "\n"))
log_msg("dentro_de_cota = TRUE quiere decir que la brecha del motor es MENOR O IGUAL en magnitud")
log_msg("al exceso ya cuantificado por la cota de mortalidad -- consistente con deriva de medicion")
log_msg("conocida. FALSE quiere decir que el motor sigue sin explicar una brecha mayor a la cota.")

log_msg("\n--- 6) Recaida: tasa implicita del motor vs. tasa implicita observada (panel ECH) ---")

recaida_motor <- rbindlist(lapply(names(snapshots), function(nm) {
  dt <- snapshots[[nm]]
  sub <- dt[estado %in% c(4, 5)]
  nd <- sub[, .(punto_medio = pmax(anios_desde_dejar, 0.05), sexo)]
  haz <- predict(insumos$modelos$recaida, newdata = nd, type = "response")
  sub[, tasa_motor := haz]
  sub[, .(tasa_motor = mean(tasa_motor), N = .N), by = .(anio = anio_snapshot, sexo)]
}))

obs_panel <- fread(file.path(out_dir, "39_recaida_chequeo_consistencia_panel.csv"))
obs_panel_agg <- obs_panel[anio %in% ANIOS_VALIDACION, .(
  tasa_obs_implicita = median(r_implicito_anual, na.rm = TRUE),
  N_pares = .N
), by = .(anio, sexo)]

comparacion_recaida <- merge(recaida_motor, obs_panel_agg, by = c("anio", "sexo"), all = TRUE)
comparacion_recaida[, diferencia := tasa_motor - tasa_obs_implicita]
setorder(comparacion_recaida, sexo, anio)
fwrite(comparacion_recaida, file.path(out_dir, "48_validacion_recaida.csv"))
log_msg("Recaida -- tasa anual implicita del motor (aplicada a la composicion de duracion")
log_msg("simulada) vs. mediana de la tasa bruta implicita del panel ECH (sin correccion de")
log_msg("mortalidad, misma limitacion que ya declara R/19):\n%s",
        paste(capture.output(print(comparacion_recaida)), collapse = "\n"))

log_msg("\n--- 7) Descomposicion por grupo etario: error TOTAL (2+3) vs. error de COMPOSICION (share diario) ---")

obs_edad <- P[anio %in% ANIOS_VALIDACION, .(
  total_pond = sum(peso),
  fumadores_pond = sum(peso * (estado %in% c(2, 3))),
  diarios_pond = sum(peso * (estado == 3))
), by = .(anio, grupo_etario)]
obs_edad[, `:=`(
  prevalencia_obs = fumadores_pond / total_pond,
  share_diario_obs = diarios_pond / fumadores_pond
)]

sim_edad <- rbindlist(lapply(snapshots, function(dt) {
  dt[, .(
    total = .N,
    fumadores = sum(estado %in% c(2, 3)),
    diarios = sum(estado == 3)
  ), by = .(anio = anio_snapshot, grupo_etario)]
}))
sim_edad[, `:=`(
  prevalencia_sim = fumadores / total,
  share_diario_sim = diarios / fumadores
)]

descomp_edad <- merge(obs_edad[, .(anio, grupo_etario, prevalencia_obs, share_diario_obs)],
                       sim_edad[, .(anio, grupo_etario, prevalencia_sim, share_diario_sim)],
                       by = c("anio", "grupo_etario"))
descomp_edad[, `:=`(
  error_total = prevalencia_sim - prevalencia_obs,
  error_composicion = share_diario_sim - share_diario_obs
)]
setorder(descomp_edad, grupo_etario, anio)
fwrite(descomp_edad, file.path(out_dir, "48_descomposicion_error_edad.csv"))
log_msg("Error TOTAL (prevalencia 2+3, iniciacion/cesacion/mortalidad) vs. error de COMPOSICION")
log_msg("(share diario entre fumadores actuales, escalada/downshift), por grupo etario:\n%s",
        paste(capture.output(print(descomp_edad)), collapse = "\n"))

log_msg("\n--- 8) Validacion por tercil de NSE (nacional, hazard vigente) ---")

obs_tercil <- P[anio %in% ANIOS_VALIDACION & !is.na(tercil), .(
  prevalencia_obs = sum(peso * (estado %in% c(2, 3))) / sum(peso),
  N_pond = sum(peso)
), by = .(anio, tercil = as.integer(as.character(tercil)))]

sim_tercil <- rbindlist(lapply(snapshots, function(dt) {
  dt[!is.na(tercil), .(prevalencia_sim = mean(estado %in% c(2, 3)), N = .N),
     by = .(anio = anio_snapshot, tercil = as.integer(as.character(tercil)))]
}))

comp_tercil <- merge(obs_tercil, sim_tercil, by = c("anio", "tercil"))
comp_tercil[, diferencia := prevalencia_sim - prevalencia_obs]
setorder(comp_tercil, tercil, anio)
fwrite(comp_tercil, file.path(out_dir, "48_validacion_por_tercil.csv"))
log_msg("Prevalencia actual (2+3) por tercil, simulado vs observado:\n%s",
        paste(capture.output(print(comp_tercil)), collapse = "\n"))

gradiente <- dcast(comp_tercil, anio ~ tercil, value.var = c("prevalencia_obs", "prevalencia_sim"))
gradiente[, `:=`(
  gradiente_obs_T1_T3 = prevalencia_obs_1 - prevalencia_obs_3,
  gradiente_sim_T1_T3 = prevalencia_sim_1 - prevalencia_sim_3
)]
gradiente[, dif_gradiente := gradiente_sim_T1_T3 - gradiente_obs_T1_T3]
gradiente <- gradiente[, .(anio, gradiente_obs_T1_T3, gradiente_sim_T1_T3, dif_gradiente)]
fwrite(gradiente, file.path(out_dir, "48_gradiente_tercil.csv"))
log_msg("Gradiente T1-T3, observado vs. simulado (referencia: el panel ECH da")
log_msg("7.1pp en 2011 abriendose a 8.3-9.1pp en 2017-2025 sobre la serie completa; aca solo los")
log_msg("anios de validacion del motor):\n%s",
        paste(capture.output(print(gradiente)), collapse = "\n"))

log_msg("\n--- 9) Cesacion por edad: tasa implicita del motor vs. tasa observada en el panel ECH ---")

cesacion_motor <- rbindlist(lapply(names(snapshots), function(nm) {
  dt <- snapshots[[nm]]
  sub <- dt[estado %in% c(2, 3)]
  nd <- sub[, .(edad, sexo, tercil, anio_c = anio_snapshot - 2018,
                corte_2019 = as.integer(anio_snapshot >= 2019), corte_2021 = as.integer(anio_snapshot >= 2021))]
  haz <- predict(insumos$modelos$cesacion, newdata = nd, type = "response")
  sub[, tasa_motor := haz]
  sub[, edad5 := floor(edad / 5) * 5]
  sub[, .(tasa_motor = mean(tasa_motor), N = .N), by = .(anio = anio_snapshot, edad5)]
}))

E_cesacion <- P[estado %in% c(2, 3, 4) & !is.na(tercil) & anio %in% ANIOS_VALIDACION]
E_cesacion[, evento := as.integer(estado == 4)]
E_cesacion[, edad5 := floor(edad / 5) * 5]
cesacion_obs <- E_cesacion[, .(
  tasa_obs = sum(peso * evento) / sum(peso),
  N_pond = sum(peso), N_obs = .N
), by = .(anio, edad5)]

comparacion_cesacion <- merge(cesacion_motor, cesacion_obs, by = c("anio", "edad5"), all = TRUE)
comparacion_cesacion[, diferencia := tasa_motor - tasa_obs]
setorder(comparacion_cesacion, edad5, anio)
fwrite(comparacion_cesacion, file.path(out_dir, "49_validacion_cesacion_edad.csv"))
log_msg("Cesacion por bin de 5 anios de edad, tasa implicita del motor (sin multiplicador de")
log_msg("precio) vs. tasa observada en el panel ECH, por anio de validacion:\n%s",
        paste(capture.output(print(comparacion_cesacion)), collapse = "\n"))

tendencia_cesacion <- comparacion_cesacion[edad5 >= 45 & !is.na(diferencia) & N_obs >= 10, {
  if (.N >= 3) {
    fit <- lm(diferencia ~ anio)
    .(pendiente_pp_por_anio = unname(coef(fit)[2]) * 100, n_anios = .N, brecha_media_pp = mean(diferencia) * 100)
  } else {
    .(pendiente_pp_por_anio = NA_real_, n_anios = .N, brecha_media_pp = mean(diferencia) * 100)
  }
}, by = edad5]
setorder(tendencia_cesacion, edad5)
fwrite(tendencia_cesacion, file.path(out_dir, "49_validacion_cesacion_tendencia.csv"))
log_msg("Tendencia de la brecha motor-observado (pp/anio) por bin de edad, edad>=45 y N_obs>=10:\n%s",
        paste(capture.output(print(tendencia_cesacion)), collapse = "\n"))
log_msg("Pendiente negativa y creciente en magnitud = sub-cesacion que empeora con los anios de")
log_msg("simulacion transcurridos (confirma la hipotesis dinamica de 18.3). Pendiente ~0 con")
log_msg("brecha_media_pp distinta de 0 = mal ajuste ESTATICO del modelo en esa edad, no un")
log_msg("problema que se acumule con la simulacion.")

log_msg("\n--- 10) Composicion fija del reescalado TwIST de mortalidad: factor_celda simulado vs. pooled (R/23) ---")

sup_estado <- fread(file.path(out_dir, "42_supervivencia_por_estado.csv"))
RR_tabla <- unique(sup_estado[, .(edad, sexo, estado, RR)])

composicion_pooled <- P[, .(N_pond = sum(peso)), by = .(edad, sexo, estado)]
composicion_pooled[, share := N_pond / sum(N_pond), by = .(edad, sexo)]
composicion_pooled <- merge(composicion_pooled, RR_tabla, by = c("edad", "sexo", "estado"))
factor_celda_pooled <- composicion_pooled[, .(factor_celda_pooled = sum(share * RR)), by = .(edad, sexo)]

factor_celda_sim <- rbindlist(lapply(names(snapshots), function(nm) {
  dt <- snapshots[[nm]]
  anio_val <- dt$anio_snapshot[1]
  comp_sim <- dt[, .N, by = .(edad, sexo, estado)]
  comp_sim[, share_sim := N / sum(N), by = .(edad, sexo)]
  comp_sim <- merge(comp_sim, RR_tabla, by = c("edad", "sexo", "estado"))
  out <- comp_sim[, .(factor_celda_sim = sum(share_sim * RR), N_edad = sum(N)), by = .(edad, sexo)]
  out[, anio := anio_val]
  out
}))

comparacion_factor <- merge(factor_celda_sim, factor_celda_pooled, by = c("edad", "sexo"))
comparacion_factor[, ratio_factor := factor_celda_sim / factor_celda_pooled]
setorder(comparacion_factor, sexo, edad, anio)
fwrite(comparacion_factor, file.path(out_dir, "49_validacion_factor_mortalidad_edad.csv"))
log_msg("factor_celda simulado (por snapshot) vs. pooled 2011-2025 (fijo, R/23), edad>=45:\n%s",
        paste(capture.output(print(comparacion_factor[edad >= 45 & edad %% 5 == 0])), collapse = "\n"))
log_msg("ratio_factor<1: motor 'mas sano' que el pooled -> lambda usado mas bajo del necesario ->")
log_msg("sub-mata estados de alto RR -> agravaria el exceso. ratio_factor>1: direccion auto-correctiva.")

tendencia_factor <- comparacion_factor[edad >= 45, {
  if (.N >= 3) {
    fit <- lm(ratio_factor ~ anio)
    .(pendiente_por_anio = unname(coef(fit)[2]), n_anios = .N, ratio_factor_medio = mean(ratio_factor))
  } else {
    .(pendiente_por_anio = NA_real_, n_anios = .N, ratio_factor_medio = mean(ratio_factor))
  }
}, by = .(edad, sexo)]
setorder(tendencia_factor, sexo, edad)
fwrite(tendencia_factor, file.path(out_dir, "49_validacion_factor_mortalidad_tendencia.csv"))
log_msg("Tendencia de ratio_factor (pendiente/anio) por edad>=45 x sexo:\n%s",
        paste(capture.output(print(tendencia_factor[edad %% 5 == 0])), collapse = "\n"))
log_msg("Pendiente ~0 con ratio_factor_medio!=1 = descalce ESTATICO (no se acumula). Pendiente que")
log_msg("aleja ratio_factor de 1 con los anios = descalce que empeora con la simulacion, confirma")
log_msg("la hipotesis dinamica de este candidato.")

capping_diario <- sup_estado[estado == 3 & edad >= 55 & anio %in% ANIOS_VALIDACION]
frac_capado <- capping_diario[, .(frac_capado = mean(prob_sobrevive <= 0), N = .N), by = .(edad_bin = floor(edad / 10) * 10)]
setorder(frac_capado, edad_bin)
log_msg("Chequeo complementario -- fraccion de celdas edad(bin 10)xanio con prob_sobrevive<=0")
log_msg("(tope RR*lambda>=1 activado) para estado diario, edad>=55, anios de validacion:\n%s",
        paste(capture.output(print(frac_capado)), collapse = "\n"))

log_msg("\n--- 11) Fuga de iniciacion capada (edad>=50) en cohortes contaminadas: corrida instrumentada ---")

aplicar_iniciacion_instrumentada <- function(pop, anio_actual, modelo_iniciacion, precio_ratio_tercil) {
  idx <- which(pop$estado == 1)
  if (length(idx) == 0) return(list(pop = pop, registro = data.table()))
  sub <- pop[idx]
  nd <- sub[, .(edad_media_prev = pmin(edad, EDAD_TOPE_INICIACION),
                anio_prev_c = pmin(anio_actual, ANIO_TOPE_TENDENCIA) - 2018,
                sexo, tercil, delta_anios = 1)]
  haz <- predict(modelo_iniciacion, newdata = nd, type = "response")
  mult <- precio_ratio_tercil[as.character(sub$tercil)]^(ELASTICIDAD_EXTENSIVA_TERCIL[as.character(sub$tercil)] *
                                                         FACTOR_TRASLADO_EXTENSIVO_TERCIL[as.character(sub$tercil)])
  haz_ajustado <- pmin(1, pmax(0, haz * mult))
  evento <- runif(nrow(sub)) < haz_ajustado
  pop[idx[evento], estado := 2]
  registro <- sub[, .(evento = evento, edad)][, .(expuestos = .N, eventos = sum(evento)), by = edad]
  registro[, anio := anio_actual]
  list(pop = pop, registro = registro)
}

ciclo_anual_instrumentado <- function(pop, anio_actual, modelos, tabla_superv, precio_lookup, pob_ine, entrada_15_por_anio,
                                       params_escalada, param_downshift) {
  precio_ratio_tercil <- c(`1` = 1, `2` = 1, `3` = 1) * (precio_lookup[anio == anio_actual]$p_reales /
                                                            precio_lookup[anio == anio_actual - 1]$p_reales)
  pop <- aplicar_mortalidad(pop, anio_actual, tabla_superv)
  pop <- aplicar_cesacion(pop, anio_actual, modelos$cesacion, precio_ratio_tercil)
  pop <- aplicar_recaida(pop, modelos$recaida)
  resultado_ini <- aplicar_iniciacion_instrumentada(pop, anio_actual, modelos$iniciacion, precio_ratio_tercil)
  pop <- resultado_ini$pop
  pop <- aplicar_escalada(pop, params_escalada)
  pop <- aplicar_downshift(pop, param_downshift)
  pop <- avanzar_edad(pop)
  entrantes <- generar_entrantes_15(anio_actual + 1, pob_ine, entrada_15_por_anio)
  pop <- rbind(pop, entrantes, fill = TRUE)
  list(pop = pop, registro = resultado_ini$registro)
}

set.seed(20260809)
pop_instr <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"))
pop_instr[, sexo := factor(sexo, levels = c("varon", "mujer"))]
pop_instr[, tercil := factor(tercil, levels = c(1, 2, 3))]

registros <- list()
t0 <- Sys.time()
for (anio_ciclo in 2011:2024) {
  resultado <- ciclo_anual_instrumentado(pop_instr, anio_ciclo, insumos$modelos, insumos$tabla_superv,
                                          insumos$precio_lookup, insumos$pob_ine, insumos$entrada_15_por_anio,
                                          params_escalada, param_downshift)
  pop_instr <- resultado$pop
  registros[[as.character(anio_ciclo)]] <- resultado$registro
}
log_msg("Corrida instrumentada (2011-2024, 14 ciclos) terminada en %.1f segundos", as.numeric(difftime(Sys.time(), t0, units = "secs")))

log_msg("Chequeo de consistencia -- poblacion final de la corrida instrumentada (2025) vs. snapshot")
log_msg("2025 de la seccion 1 (mismo seed/insumos/secuencia, deberian coincidir):")
log_msg("N instrumentada: %d | N seccion 1: %d", nrow(pop_instr), nrow(snapshots[["2025"]]))
log_msg("Distribucion de estados, instrumentada:\n%s",
        paste(capture.output(print(pop_instr[, .N, by = estado][order(estado)])), collapse = "\n"))
log_msg("Distribucion de estados, seccion 1 (2025):\n%s",
        paste(capture.output(print(snapshots[["2025"]][, .N, by = estado][order(estado)])), collapse = "\n"))

registro_iniciacion <- rbindlist(registros)
registro_iniciacion[, cohorte := cohorte_5(anio - edad)]
registro_iniciacion <- merge(registro_iniciacion, criterio_limpias, by = "cohorte", all.x = TRUE)
registro_iniciacion[, edad_capada := edad >= EDAD_TOPE_INICIACION]
setorder(registro_iniciacion, anio, edad)
fwrite(registro_iniciacion, file.path(out_dir, "50_validacion_iniciacion_capada.csv"))
log_msg("Escrito 50_validacion_iniciacion_capada.csv (%d filas: anio x edad, expuestos/eventos de iniciacion)",
        nrow(registro_iniciacion))

deficit_share_2025 <- abs(estados_comp_cohorte[anio == 2025 & cohorte_limpia == FALSE & estado == 1]$diferencia)
N_contaminadas_2025 <- snapshots[["2025"]][cohorte_limpia == FALSE, .N]
deficit_cabezas_2025 <- deficit_share_2025 * N_contaminadas_2025
log_msg("Deficit observado de 'nunca' en cohortes contaminadas, 2025: %.4f (share) x %d (N contaminadas) = %.0f cabezas",
        deficit_share_2025, N_contaminadas_2025, deficit_cabezas_2025)

trayectoria <- registro_iniciacion[edad_capada == TRUE & cohorte_limpia == FALSE, .(eventos_anio = sum(eventos)), by = .(anio_ciclo = anio)]
setorder(trayectoria, anio_ciclo)
trayectoria[, anio_resultado := anio_ciclo + 1]
trayectoria[, eventos_acumulados := cumsum(eventos_anio)]
trayectoria[, deficit_cabezas_2025 := deficit_cabezas_2025]
trayectoria[, eventos_acumulados_vs_deficit_pct := eventos_acumulados / deficit_cabezas_2025 * 100]
fwrite(trayectoria, file.path(out_dir, "50_validacion_iniciacion_capada_resumen.csv"))
log_msg("Eventos de iniciacion capada (edad>=50) en cohortes contaminadas, acumulados por anio, vs.")
log_msg("el deficit de cabezas observado en 2025:\n%s",
        paste(capture.output(print(trayectoria)), collapse = "\n"))
log_msg("eventos_acumulados_vs_deficit_pct = que fraccion del deficit de 'nunca' en 2025 explica la")
log_msg("fuga capada. >30-50%% seria evidencia fuerte de que este candidato es un driver relevante;")
log_msg("<10%% lo descarta como driver PRIMARIO (aunque pueda ser un contribuyente menor).")

log_msg("\n--- 12) Interaccion mortalidad x cesacion: orden alterado (cesacion antes que mortalidad) ---")

ciclo_anual_orden_alterado <- function(pop, anio_actual, modelos, tabla_superv, precio_lookup, pob_ine,
                                        entrada_15_por_anio, params_escalada, param_downshift) {
  precio_ratio_tercil <- c(`1` = 1, `2` = 1, `3` = 1) * (precio_lookup[anio == anio_actual]$p_reales /
                                                            precio_lookup[anio == anio_actual - 1]$p_reales)
  pop <- aplicar_cesacion(pop, anio_actual, modelos$cesacion, precio_ratio_tercil)
  pop <- aplicar_mortalidad(pop, anio_actual, tabla_superv)
  pop <- aplicar_recaida(pop, modelos$recaida)
  pop <- aplicar_iniciacion(pop, anio_actual, modelos$iniciacion, precio_ratio_tercil)
  pop <- aplicar_escalada(pop, params_escalada)
  pop <- aplicar_downshift(pop, param_downshift)
  pop <- avanzar_edad(pop)
  entrantes <- generar_entrantes_15(anio_actual + 1, pob_ine, entrada_15_por_anio)
  rbind(pop, entrantes, fill = TRUE)
}

set.seed(20260809)
pop_orden <- fread(file.path(int_dir, "poblacion_sintetica_2011.csv.gz"))
pop_orden[, sexo := factor(sexo, levels = c("varon", "mujer"))]
pop_orden[, tercil := factor(tercil, levels = c(1, 2, 3))]

t0 <- Sys.time()
for (anio_ciclo in 2011:2024) {
  pop_orden <- ciclo_anual_orden_alterado(pop_orden, anio_ciclo, insumos$modelos, insumos$tabla_superv,
                                           insumos$precio_lookup, insumos$pob_ine, insumos$entrada_15_por_anio,
                                           params_escalada, param_downshift)
}
log_msg("Corrida con orden alterado (2011-2024, 14 ciclos) terminada en %.1f segundos", as.numeric(difftime(Sys.time(), t0, units = "secs")))
log_msg("N final (2025), orden alterado: %d | N seccion 1 (orden real): %d", nrow(pop_orden), nrow(snapshots[["2025"]]))

pop_orden[, cohorte := cohorte_5(2025 - edad)]
pop_orden <- merge(pop_orden, criterio_limpias, by = "cohorte", all.x = TRUE)

share_obs_2025 <- estados_comp_cohorte[anio == 2025 & cohorte_limpia == FALSE, .(estado, share_obs)]
share_original_2025 <- estados_comp_cohorte[anio == 2025 & cohorte_limpia == FALSE, .(estado, share_sim_original = share_sim)]
share_orden_alterado <- pop_orden[cohorte_limpia == FALSE, .N, by = estado]
share_orden_alterado[, share_sim_orden_alterado := N / sum(N)]
share_orden_alterado <- share_orden_alterado[, .(estado, share_sim_orden_alterado)]

comparacion_orden <- merge(share_obs_2025, share_original_2025, by = "estado")
comparacion_orden <- merge(comparacion_orden, share_orden_alterado, by = "estado")
comparacion_orden[, `:=`(
  dif_original = share_sim_original - share_obs,
  dif_orden_alterado = share_sim_orden_alterado - share_obs
)]
comparacion_orden[, cuanto_se_acerca_pp := (abs(dif_original) - abs(dif_orden_alterado)) * 100]
setorder(comparacion_orden, estado)
fwrite(comparacion_orden, file.path(out_dir, "51_validacion_orden_ciclo.csv"))
log_msg("Comparacion por estado, cohortes contaminadas, 2025 -- observado vs. orden real (seccion 1)")
log_msg("vs. orden alterado (cesacion antes que mortalidad):\n%s",
        paste(capture.output(print(comparacion_orden)), collapse = "\n"))
log_msg("cuanto_se_acerca_pp > 0 = el orden alterado ACERCA el simulado al observado (el orden")
log_msg("importa, evidencia a favor de esta forma de interaccion). cuanto_se_acerca_pp ~ 0 = el")
log_msg("orden no importa en esta magnitud, se descarta esta forma especifica de interaccion.")

log_msg("\n=== outputs: 46_validacion_prevalencia_cohortes_limpias.csv, 46_validacion_por_estado_cohorte.csv, 46_comparacion_cota_mortalidad.csv, 48_validacion_recaida.csv, 48_descomposicion_error_edad.csv, 48_validacion_por_tercil.csv, 48_gradiente_tercil.csv, 49_validacion_cesacion_edad.csv, 49_validacion_cesacion_tendencia.csv, 49_validacion_factor_mortalidad_edad.csv, 49_validacion_factor_mortalidad_tendencia.csv, 50_validacion_iniciacion_capada.csv, 50_validacion_iniciacion_capada_resumen.csv, 51_validacion_orden_ciclo.csv ===")
close(log_con)
