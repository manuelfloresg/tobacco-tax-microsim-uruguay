# 52_razon_prevalencia_tercil.R
#
# Distributional metrics in levels: prevalence by tertile, the T1-T3 gap and the T1/T3 ratio across
# branches, gradient variants and margin splits. Does not run the model.
# Input:  output/tables/59_*, 72_*, 75_*
# Output: output/tables/79_*.csv

library(here)
library(data.table)

here::i_am("R/52_razon_prevalencia_tercil.R")

log_con <- file(here::here("logs", "52_razon_prevalencia_tercil.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 52_razon_prevalencia_tercil.R -- %s ===", format(Sys.time()))

out_dir <- here::here("output", "tables")

razon_t1_t3 <- function(d, col_prev, por) {
  w <- dcast(d, as.formula(paste(paste(por, collapse = " + "), "~ tercil")),
             value.var = col_prev)
  setnames(w, c("1", "2", "3"), c("prev_T1", "prev_T2", "prev_T3"), skip_absent = TRUE)
  w[, razon_T1_T3 := prev_T1 / prev_T3]
  w[, brecha_T1_T3_pp := 100 * (prev_T1 - prev_T3)]
  w[]
}

log_msg("\n--- 1) Razon T1/T3 en niveles, las cuatro ramas (R/38) ---")

esc <- fread(file.path(out_dir, "59_espacio_escenarios_tercil.csv"))
if (!"N_tercil" %in% names(esc))
  stop("59_espacio_escenarios_tercil.csv no trae N_tercil: re-correr R/38 con la correccion ",
       "de resumir_tercil(). Sin denominador no hay prevalencia en niveles.")

esc[, prevalencia := N_fumadores / N_tercil]
r38 <- razon_t1_t3(esc, "prevalencia", c("escenario", "anio"))
setorder(r38, escenario, anio)
fwrite(r38, file.path(out_dir, "79_razon_prevalencia_ramas.csv"))
log_msg("Razon de prevalencias T1/T3 en niveles, por rama y anio:\n%s",
        paste(capture.output(print(r38[, .(escenario, anio, prev_T1, prev_T3, razon_T1_T3,
                                            brecha_T1_T3_pp)], digits = 4)), collapse = "\n"))

est <- r38[, .(razon_min = min(razon_T1_T3), razon_max = max(razon_T1_T3),
               amplitud = max(razon_T1_T3) - min(razon_T1_T3)), by = escenario]
log_msg("\nRango de la razon a lo largo del horizonte, por rama:\n%s",
        paste(capture.output(print(est, digits = 4)), collapse = "\n"))

base_r <- r38[escenario == "base", .(anio, razon_base = razon_T1_T3)]
mov <- merge(r38[escenario != "base"], base_r, by = "anio")
mov[, dif_contra_base := razon_T1_T3 - razon_base]
log_msg("\nEfecto de la politica sobre la razon (escenario menos base):\n%s",
        paste(capture.output(print(mov[, .(escenario, anio, razon_base, razon_T1_T3,
                                            dif_contra_base)], digits = 4)), collapse = "\n"))

log_msg("\n--- 2) Estabilidad de la razon a traves de la envolvente de gradiente (R/45) ---")

arch_45 <- file.path(out_dir, "72_sensibilidad_t1_tercil.csv")
if (file.exists(arch_45)) {
  sens <- fread(arch_45)
  sens_c <- sens[grepl("^C_", variante)]
  if (nrow(sens_c) == 0) {
    log_msg("AVISO: 72_sensibilidad_t1_tercil.csv no tiene variantes del grupo C. Saltado.")
  } else {
    r45 <- razon_t1_t3(sens_c, "prevalencia_tercil", c("variante", "escenario", "anio"))
    setorder(r45, escenario, anio, variante)
    fwrite(r45, file.path(out_dir, "79_razon_prevalencia_envolvente.csv"))

    env <- r45[, .(n_variantes = .N, razon_min = min(razon_T1_T3), razon_max = max(razon_T1_T3),
                   amplitud = max(razon_T1_T3) - min(razon_T1_T3)), by = .(escenario, anio)]
    env[, amplitud_rel_pct := 100 * amplitud / ((razon_min + razon_max) / 2)]
    setorder(env, escenario, anio)
    fwrite(env, file.path(out_dir, "79_razon_envolvente_amplitud.csv"))
    log_msg("Amplitud de la razon T1/T3 a traves de las %d variantes del grupo C:\n%s",
            uniqueN(sens_c$variante),
            paste(capture.output(print(env, digits = 4)), collapse = "\n"))

    par <- dcast(r45[, .(variante, escenario, anio, razon_T1_T3)],
                 variante + anio ~ escenario, value.var = "razon_T1_T3")
    setnames(par, c("base", "conv75_gradual"), c("razon_base", "razon_esc"), skip_absent = TRUE)
    par[, efecto := razon_esc - razon_base]
    ef <- par[, .(n = .N, efecto_min = min(efecto), efecto_max = max(efecto),
                  signo_determinado = min(efecto) * max(efecto) > 0), by = anio]
    setorder(ef, anio)
    fwrite(par, file.path(out_dir, "79_razon_efecto_politica_envolvente.csv"))
    log_msg("\nEfecto de la politica sobre la razon, APAREADO dentro de cada variante:\n%s",
            paste(capture.output(print(ef, digits = 4)), collapse = "\n"))
    log_msg("Signo del efecto determinado en todas las variantes? %s",
            paste(ef[, sprintf("%d:%s", anio, fifelse(signo_determinado, "SI", "no"))], collapse = "  "))

    sh <- sens_c[tercil == 1, .(variante, escenario, anio, share_consumo)]
    sh_b <- sh[escenario == "base", .(variante, anio, share_base = share_consumo)]
    sh_e <- merge(sh[escenario != "base"], sh_b, by = c("variante", "anio"))
    sh_e[, cambio_share_pp := 100 * (share_consumo - share_base)]
    cmp <- sh_e[, .(cambio_min_pp = min(cambio_share_pp), cambio_max_pp = max(cambio_share_pp),
                    amplitud_pp = max(cambio_share_pp) - min(cambio_share_pp)), by = anio]
    log_msg("\nContraste. La metrica de CAMBIO (participacion de T1 en el consumo, pp) a traves")
    log_msg("de la misma envolvente:\n%s",
            paste(capture.output(print(cmp, digits = 4)), collapse = "\n"))
    log_msg("El signo del cambio esta determinado en cada anio? %s",
            paste(cmp[, sprintf("%d:%s", anio, fifelse(cambio_min_pp * cambio_max_pp > 0, "SI", "NO"))],
                  collapse = "  "))
  }
} else {
  log_msg("AVISO: falta 72_sensibilidad_t1_tercil.csv (R/45 grupo C). Seccion 2 saltada.")
}

log_msg("\n--- 3) Estabilidad de la razon a traves del reparto por margen (R/48) ---")

arch_48 <- file.path(out_dir, "75_margen_tercil.csv")
if (file.exists(arch_48)) {
  marg <- fread(arch_48)
  r48 <- razon_t1_t3(marg, "prevalencia", c("reparto", "escenario", "anio"))
  setorder(r48, escenario, anio, reparto)
  fwrite(r48, file.path(out_dir, "79_razon_prevalencia_reparto.csv"))
  amp48 <- r48[, .(razon_min = min(razon_T1_T3), razon_max = max(razon_T1_T3),
                   amplitud = max(razon_T1_T3) - min(razon_T1_T3)), by = .(escenario, anio)]
  setorder(amp48, escenario, anio)
  log_msg("Amplitud de la razon a traves de los tres repartos:\n%s",
          paste(capture.output(print(amp48, digits = 4)), collapse = "\n"))
} else {
  log_msg("AVISO: falta 75_margen_tercil.csv (R/48). Seccion 3 saltada.")
}

log_msg("\n--- 4) Contra la referencia de TwIST ---")
log_msg("TwIST (Mills et al. 2026) reporta 2,1 a 2,3 veces, sostenido 2020-2050, para")
log_msg("pobreza contra no-pobreza. Nuestro objeto NO es el mismo (terciles de ingreso per")
log_msg("capita del hogar contra linea de pobreza), asi que el NIVEL no es comparable; lo")
log_msg("comparable es la ESTABILIDAD en el tiempo, que es lo que hace la metrica reportable.")
r_ini <- r38[escenario == "base"][which.min(anio)]
r_fin <- r38[escenario == "base"][which.max(anio)]
log_msg("Rama base: razon %.3f en %d -> %.3f en %d (variacion %.1f%%).",
        r_ini$razon_T1_T3, r_ini$anio, r_fin$razon_T1_T3, r_fin$anio,
        100 * (r_fin$razon_T1_T3 / r_ini$razon_T1_T3 - 1))

log_msg("\n=== outputs: 79_razon_prevalencia_ramas.csv, 79_razon_prevalencia_envolvente.csv,")
log_msg("    79_razon_envolvente_amplitud.csv, 79_razon_prevalencia_reparto.csv ===")
close(log_con)
