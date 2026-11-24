# 48_gradiente_por_margen.R
#
# Split of each tertile's price response between the extensive and intensive margins, based on
# Keeler et al. Three variants: bounded (primary), uniform 50%, and the source estimates taken
# literally.
# Input:  synthetic population 2011, output/tables/43_*, 59_*, 74_*
# Output: output/tables/75_*.csv

library(here)
library(data.table)
library(haven)
library(splines)

here::i_am("R/48_gradiente_por_margen.R")

log_con <- file(here::here("logs", "48_gradiente_por_margen.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 48_gradiente_por_margen.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

source(here::here("R", "24_motor_transiciones.R"))

ANIOS_SNAP <- c(2025L, seq(2030L, 2060L, by = 5L))
ANIO_MAX_PRECIO <- 2070L
ANIO_INICIO_ESCENARIO <- 2027L
HORIZONTE_GRADUAL <- 5L
SHARE_OMS <- 0.75

params_cal <- fread(file.path(out_dir, "43_parametros_calibrados.csv"))
params_escalada <- c(params_cal[parametro == "raw_par1"]$valor, params_cal[parametro == "raw_par2"]$valor)
param_downshift <- params_cal[parametro == "raw_par3"]$valor

log_msg("\n--- 1) Repartos extensivo/intensivo por tercil ---")

pesos_dt <- fread(file.path(out_dir, "74_pesos_consumo_tercil.csv"))[order(tercil)]
w <- setNames(pesos_dt$share_consumo, as.character(pesos_dt$tercil))
stopifnot(abs(sum(w) - 1) < 1e-8)

espec <- fread(file.path(out_dir, "74_especificacion_elasticidad.csv"))[caso == "principal"]
ELAST_PRINCIPAL <- c(`1` = espec$T1, `2` = espec$T2, `3` = espec$T3)
stopifnot(identical(unname(ELAST_PRINCIPAL), unname(ELASTICIDAD_TOTAL_TERCIL[names(ELAST_PRINCIPAL)])))
log_msg("Elasticidad total (principal, R/47): T1=%.4f T2=%.4f T3=%.4f | agregada %.4f",
        ELAST_PRINCIPAL["1"], ELAST_PRINCIPAL["2"], ELAST_PRINCIPAL["3"],
        sum(w * ELAST_PRINCIPAL[names(w)]))

KEELER_PARTICIPACION <- c(pobres = -0.32, bajos = -0.27, medios = -0.24, altos = 0)
KEELER_INTENSIDAD    <- c(pobres = 0, bajos = 0, medios = -0.33, altos = -0.42)
keeler_total  <- KEELER_PARTICIPACION + KEELER_INTENSIDAD
keeler_share  <- KEELER_PARTICIPACION / keeler_total
crudo <- c(`1` = mean(keeler_share[c("pobres", "bajos")]),
           `2` = as.numeric(keeler_share["medios"]),
           `3` = as.numeric(keeler_share["altos"]))
log_msg("Share extensivo crudo de Keeler: T1=%.3f T2=%.3f T3=%.3f (promedio ponderado por consumo: %.3f)",
        crudo["1"], crudo["2"], crudo["3"], sum(w * crudo[names(w)]))

PISO_LOCAL  <- 0.34
TECHO_LOCAL <- 0.68
CONVENCION  <- 0.50
b <- (CONVENCION - PISO_LOCAL) / sum(w * crudo[names(w)])
acotado <- PISO_LOCAL + b * crudo
log_msg("Rango implicado por la evidencia uruguaya: [%.2f; %.2f], convencion Goodchild %.2f",
        PISO_LOCAL, TECHO_LOCAL, CONVENCION)

if (!identical(unname(acotado[c("1","2","3")]), unname(SHARE_EXTENSIVO_TERCIL[c("1","2","3")]))) {
  stop(sprintf(paste0("keeler_acotado computado aca y SHARE_EXTENSIVO_TERCIL de R/24 no son ",
                      "identicos bit a bit (desvio maximo %.3g). Si R/24 cambio a proposito, ",
                      "actualizar sus literales con %%.17g; si no, hay un problema."),
               max(abs(acotado[c("1","2","3")] - SHARE_EXTENSIVO_TERCIL[c("1","2","3")]))))
}
log_msg("Guardarrail OK: keeler_acotado es identico bit a bit al default de R/24 (adoptado el 2026-08-20).")

REPARTOS <- list(
  uniforme_50    = c(`1` = 0.5, `2` = 0.5, `3` = 0.5),
  keeler_literal = crudo,
  keeler_acotado = acotado
)
FUENTE <- c(
  uniforme_50    = "Goodchild et al., convencion 50/50; usada por Ferre et al. (2025). Especificacion vigente.",
  keeler_literal = "Keeler et al. (2025) NTR, TUS-CPS n=337.974, coeficientes n.s. leidos como cero.",
  keeler_acotado = "Forma de Keeler et al. (2025); T3 anclado en el piso 0,34 y promedio ponderado forzado a 0,50, ambos de Gerstenbluth et al. (2026)."
)

tab_rep <- rbindlist(lapply(names(REPARTOS), function(nm) {
  s <- REPARTOS[[nm]]
  data.table(reparto = nm, share_ext_T1 = s["1"], share_ext_T2 = s["2"], share_ext_T3 = s["3"],
             share_ext_promedio_ponderado = sum(w * s[names(w)]),
             elast_ext_T1 = ELAST_PRINCIPAL["1"] * s["1"], elast_ext_T2 = ELAST_PRINCIPAL["2"] * s["2"],
             elast_ext_T3 = ELAST_PRINCIPAL["3"] * s["3"],
             elast_int_T1 = ELAST_PRINCIPAL["1"] * (1 - s["1"]), elast_int_T2 = ELAST_PRINCIPAL["2"] * (1 - s["2"]),
             elast_int_T3 = ELAST_PRINCIPAL["3"] * (1 - s["3"]),
             fuente = FUENTE[[nm]])
}))
stopifnot(all(acotado >= PISO_LOCAL - 1e-12), all(acotado <= TECHO_LOCAL + 1e-12))
fwrite(tab_rep, file.path(out_dir, "75_reparto_margen.csv"))
log_msg("Repartos:\n%s", paste(capture.output(print(
  tab_rep[, .(reparto, share_ext_T1, share_ext_T2, share_ext_T3, share_ext_promedio_ponderado)],
  digits = 4)), collapse = "\n"))

log_msg("\n--- 2) Trayectoria de referencia (conv75_gradual) ---")

insumos <- cargar_insumos_motor(regla_precio = "mantenimiento_politica", anio_max_precio = ANIO_MAX_PRECIO)
precio_base <- insumos$precio_lookup
ultimo_anio_dato <- attr(precio_base, "ultimo_anio_dato")
tray_tc <- fread(file.path(out_dir, "59_trayectorias_impuesto.csv"))[escenario_impuesto == "conv75_gradual"]
stopifnot(nrow(tray_tc) > 0, "factor_imesi" %in% names(tray_tc))
tray_imp <- tray_tc[, .(anio, factor_impuesto_real)]
factor_obj <- max(tray_tc$factor_imesi)
log_msg("factor objetivo sobre tau_ancla = %.4f (+%.1f%% real)", factor_obj, 100 * (factor_obj - 1))

precio_esc <- cargar_insumos_motor(regla_precio = "mantenimiento_politica",
                                    anio_max_precio = ANIO_MAX_PRECIO,
                                    trayectoria_impuesto = tray_imp)$precio_lookup

log_msg("\n--- 3) Corridas (3 repartos x 2 ramas) ---")

correr <- function(precio_lookup, precio_ref = NULL, semilla = 20260809) {
  set.seed(semilla)
  pop <- cargar_poblacion_proyeccion()
  snaps <- list(); a <- ANIO_POB_PROYECCION
  for (obj in ANIOS_SNAP) {
    while (a < obj) {
      ov <- if (is.null(precio_ref)) NULL else {
        r <- precio_lookup[anio == a]$p_reales / precio_ref[anio == a]$p_reales
        if (isTRUE(all.equal(r, 1, tolerance = 1e-12))) NULL else r
      }
      pop <- ciclo_anual(pop, a, insumos$modelos, insumos$tabla_superv, precio_lookup,
                          insumos$pob_ine, insumos$entrada_15_por_anio, params_escalada,
                          param_downshift, precio_ratio_override = ov)
      a <- a + 1L
    }
    snaps[[as.character(obj)]] <- copy(pop)[, anio_snapshot := obj]
  }
  snaps
}

resumir <- function(snaps, precio_lookup, etiqueta, reparto) {
  rbindlist(lapply(snaps, function(dt) {
    a <- dt$anio_snapshot[1]
    d <- copy(dt)
    d[, cig := calcular_cig_dia(d, a, insumos$intensidad, precio_lookup)]
    tot <- sum(d$cig)
    n_tercil <- d[, .(N_tercil = .N), by = tercil]
    out <- merge(d[cig > 0, .(N_fumadores = .N, consumo_mill = sum(cig) * 365 / 1e6,
                               cig_dia_medio = mean(cig), share_consumo = sum(cig) / tot), by = tercil],
                 n_tercil, by = "tercil")
    out[, prevalencia := N_fumadores / N_tercil]
    out[, `:=`(escenario = etiqueta, reparto = reparto, anio = a)]
    out
  }))
}

resultados <- list()
for (nm in names(REPARTOS)) {
  s <- REPARTOS[[nm]]
  assign("ELASTICIDAD_TOTAL_TERCIL", ELAST_PRINCIPAL, envir = .GlobalEnv)
  assign("SHARE_EXTENSIVO_TERCIL", s, envir = .GlobalEnv)
  assign("ELASTICIDAD_EXTENSIVA_TERCIL", ELAST_PRINCIPAL * s, envir = .GlobalEnv)
  assign("ELASTICIDAD_INTENSIVA_TERCIL", ELAST_PRINCIPAL * (1 - s), envir = .GlobalEnv)
  t0 <- Sys.time()
  s_base <- correr(precio_base, NULL)
  s_esc  <- correr(precio_esc, precio_base)
  resultados[[nm]] <- rbind(resumir(s_base, precio_base, "base", nm),
                            resumir(s_esc, precio_esc, "conv75_gradual", nm))
  log_msg("Reparto '%s' (ext T1=%.2f T2=%.2f T3=%.2f) corrido en %.1f min",
          nm, s["1"], s["2"], s["3"], as.numeric(difftime(Sys.time(), t0, units = "mins")))
}
R <- rbindlist(resultados)
fwrite(R, file.path(out_dir, "75_margen_tercil.csv"))

log_msg("\n--- 4) Efecto distributivo: prevalencia contra consumo ---")

largo <- melt(R, id.vars = c("reparto", "escenario", "anio", "tercil"),
              measure.vars = c("prevalencia", "share_consumo", "consumo_mill", "cig_dia_medio"),
              variable.name = "metrica", value.name = "valor")
comp <- dcast(largo, reparto + anio + tercil + metrica ~ escenario, value.var = "valor")
setnames(comp, c("base", "conv75_gradual"), c("valor_base", "valor_esc"))
comp[, `:=`(dif_pp = 100 * (valor_esc - valor_base), dif_pct = 100 * (valor_esc / valor_base - 1))]
setorder(comp, reparto, metrica, tercil, anio)
fwrite(comp, file.path(out_dir, "75_margen_progresividad.csv"))

log_msg("\nCAIDA DE PREVALENCIA por tercil a 2060 (%% contra la base del mismo anio) -- efecto en SALUD:\n%s",
        paste(capture.output(print(dcast(comp[metrica == "prevalencia" & anio == 2060],
                                          reparto ~ tercil, value.var = "dif_pct"), digits = 4)), collapse = "\n"))
log_msg("\nCAIDA DE CONSUMO por tercil a 2060 (%% contra la base del mismo anio):\n%s",
        paste(capture.output(print(dcast(comp[metrica == "consumo_mill" & anio == 2060],
                                          reparto ~ tercil, value.var = "dif_pct"), digits = 4)), collapse = "\n"))
log_msg("\nPARTICIPACION DE T1 en el consumo (pp contra la base) -- metrica distributiva:\n%s",
        paste(capture.output(print(dcast(comp[metrica == "share_consumo" & tercil == 1 & anio %in% c(2030, 2045, 2060)],
                                          reparto ~ anio, value.var = "dif_pp"), digits = 3)), collapse = "\n"))

brecha <- rbindlist(lapply(c("prevalencia", "consumo_mill"), function(m) {
  d <- dcast(comp[metrica == m], reparto + anio ~ tercil, value.var = "dif_pct")
  setnames(d, c("1", "2", "3"), c("T1", "T2", "T3"))
  d[, .(metrica = m, reparto, anio, T1, T3, brecha_T1_menos_T3 = T1 - T3)]
}))
setorder(brecha, metrica, reparto, anio)
fwrite(brecha, file.path(out_dir, "75_margen_brecha.csv"))
log_msg("\nBRECHA T1-T3 (puntos porcentuales de caida; mas negativo = T1 cae mas que T3):\n%s",
        paste(capture.output(print(dcast(brecha[anio %in% c(2030, 2060)], reparto + anio ~ metrica,
                                          value.var = "brecha_T1_menos_T3"), digits = 4)), collapse = "\n"))

agr <- R[, .(consumo_mill = sum(consumo_mill), fumadores = sum(N_fumadores)), by = .(reparto, escenario, anio)]
agr_w <- dcast(agr, reparto + anio ~ escenario, value.var = c("consumo_mill", "fumadores"))
agr_w[, `:=`(caida_consumo_pct = 100 * (consumo_mill_conv75_gradual / consumo_mill_base - 1),
             caida_fumadores_pct = 100 * (fumadores_conv75_gradual / fumadores_base - 1))]
fwrite(agr_w, file.path(out_dir, "75_margen_agregado.csv"))
log_msg("\nAGREGADO a 2060 (el reparto no deberia mover mucho el consumo, si la prevalencia):\n%s",
        paste(capture.output(print(agr_w[anio == 2060, .(reparto, caida_consumo_pct, caida_fumadores_pct)],
                                    digits = 4)), collapse = "\n"))

log_msg("\n=== outputs: 75_reparto_margen.csv, 75_margen_tercil.csv, 75_margen_progresividad.csv,")
log_msg("    75_margen_brecha.csv, 75_margen_agregado.csv ===")
close(log_con)
