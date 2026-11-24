# 40_bracket_informante.R
#
# Measurement bracket for ECH prevalence. Floor: the estimate as recorded. Ceiling: the estimate if
# proxy-reported individuals had the smoking distribution of self-reporting individuals in the same
# age, sex and tertile cell. Positions the model inside or outside the bracket.
# Input:  panel from R/04, output/tables/46_*, 48_*, 52_*
# Output: output/tables/67_*.csv

library(here)
library(data.table)

here::i_am("R/40_bracket_informante.R")

log_con <- file(here::here("logs", "40_bracket_informante.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 40_bracket_informante.R -- %s ===", format(Sys.time()))

int_dir <- here::here("data", "interim")
out_dir <- here::here("output", "tables")

COHORTES_LIMPIAS <- c(1985, 1990, 1995, 2000, 2005)
ESTADOS <- 1:5
ETIQ_ESTADO <- c("1 nunca fumo", "2 ocasional", "3 diario", "4 ex reciente", "5 ex consolidado")

log_msg("\n--- 1) Panel con informante ---")

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))
log_msg("Panel: %d filas, anios %s", nrow(P), paste(sort(unique(P$anio)), collapse = ", "))
log_msg("Anios sin variable de informante (e557 no existe): %s",
        paste(sort(unique(P[is.na(directo)]$anio)), collapse = ", "))

P <- P[!is.na(directo) & !is.na(edad) & !is.na(estado)]
P[, directo := as.integer(directo)]
P[, grupo_etario := fcase(edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+")]
P[, edad5 := pmin(80, 5 * (edad %/% 5))]
P[, edad10 := pmin(80, 10 * (edad %/% 10))]
P[, grupo_cohorte := fifelse(cohorte %in% COHORTES_LIMPIAS, "limpia", "contaminada")]

share_directo <- P[, .(share_directo = sum(peso * directo) / sum(peso), n = .N), by = anio][order(anio)]
log_msg("Participacion de respuesta directa por anio:\n%s",
        paste(capture.output(print(share_directo, digits = 4)), collapse = "\n"))

bracket <- function(dt, indicador, grupos, estrato) {
  d <- copy(dt)
  d[, ind := indicador(d)]
  cel <- d[, .(w = sum(peso), wd = sum(peso * directo),
               y = sum(peso * ind), yd = sum(peso * ind * directo)),
           by = c(grupos, estrato)]
  cel[, cubierta := wd > 0]
  cel[, tasa_directo := fifelse(cubierta, yd / wd, NA_real_)]
  cel[, .(obs_total = sum(y) / sum(w),
          obs = sum(y[cubierta]) / sum(w[cubierta]),
          std = sum(w[cubierta] * tasa_directo[cubierta]) / sum(w[cubierta]),
          cobertura = sum(w[cubierta]) / sum(w),
          n_celdas = .N, n_celdas_sin_directo = sum(!cubierta),
          n_muestral = sum(w) * 0),
      by = grupos]
}

ind_prev  <- function(d) as.integer(d$estado %in% c(2, 3))
ind_nunca <- function(d) as.integer(d$estado == 1)
ind_estado <- function(k) function(d) as.integer(d$estado == k)

EST_BASE <- c("edad5", "sexo", "tercil")
EST_ALT1 <- c("edad10", "sexo", "tercil")
EST_ALT2 <- c("edad5", "sexo")

chk <- P[, .(w = sum(peso), wd = sum(peso * directo), y = sum(peso * ind_prev(.SD)),
             yd = sum(peso * ind_prev(.SD) * directo)),
         by = c("anio", EST_BASE), .SDcols = c("estado")]
chk_full <- chk[abs(wd - w) < 1e-9]
log_msg("\nChequeo de identidad: %d celdas con 100%% de respuesta directa; desvio maximo |obs - std| en ellas: %.2e",
        nrow(chk_full), if (nrow(chk_full) > 0) max(abs(chk_full$y / chk_full$w - chk_full$yd / chk_full$wd)) else NA_real_)

log_msg("\n--- 3) Brecha directo - proxy, estandarizada ---")

brecha_estandarizada <- function(dt, indicador, estrato, etiqueta) {
  d <- copy(dt)
  d[, ind := indicador(d)]
  cel <- d[, .(w = sum(peso), wd = sum(peso * directo), wp = sum(peso * (1 - directo)),
               yd = sum(peso * ind * directo), yp = sum(peso * ind * (1 - directo))),
           by = c("anio", estrato)]
  cel <- cel[wd > 0 & wp > 0]
  cel[, gap := yd / wd - yp / wp]
  out <- cel[, .(brecha_estandarizada = sum(w * gap) / sum(w),
                 brecha_cruda = sum(yd) / sum(wd) - sum(yp) / sum(wp),
                 cobertura_celdas = .N), by = anio]
  out[, esquema := etiqueta]
  out
}

brechas <- rbindlist(lapply(ESTADOS, function(k) {
  rbindlist(list(
    brecha_estandarizada(P, ind_estado(k), EST_BASE, "edad5 x sexo x tercil"),
    brecha_estandarizada(P, ind_estado(k), EST_ALT1, "edad10 x sexo x tercil"),
    brecha_estandarizada(P, ind_estado(k), EST_ALT2, "edad5 x sexo")
  ))[, estado := k]
}))
brechas[, estado_lab := ETIQ_ESTADO[estado]]
setcolorder(brechas, c("estado", "estado_lab", "esquema", "anio"))
setorder(brechas, estado, esquema, anio)
fwrite(brechas, file.path(out_dir, "67_brecha_informante_por_estado.csv"))

log_msg("Brecha estandarizada directo - proxy, estado 1 (nunca fumo), por esquema:\n%s",
        paste(capture.output(print(dcast(brechas[estado == 1], anio ~ esquema,
                                          value.var = "brecha_estandarizada"), digits = 3)), collapse = "\n"))
log_msg("\nBrecha estandarizada por estado (esquema base), promedio 2014-2025:\n%s",
        paste(capture.output(print(brechas[esquema == "edad5 x sexo x tercil",
                                            .(brecha_media = mean(brecha_estandarizada),
                                              brecha_media_cruda = mean(brecha_cruda)),
                                            by = .(estado, estado_lab)], digits = 3)), collapse = "\n"))

log_msg("\n--- 4) Bracket [ECH cruda, todos-directos] ---")

armar <- function(grupos, etiqueta) {
  rbindlist(list(
    cbind(bracket(P, ind_prev, c("anio", grupos), EST_BASE), metrica = "prevalencia_actual"),
    cbind(bracket(P, ind_nunca, c("anio", grupos), EST_BASE), metrica = "nunca_fumo")
  ))[, corte := etiqueta][]
}

br_nacional <- armar(character(0), "nacional")
br_edad     <- armar("grupo_etario", "grupo_etario")
br_cohgrupo <- armar("grupo_cohorte", "grupo_cohorte")
br_tercil   <- armar("tercil", "tercil")
br_cohorte  <- armar("cohorte", "cohorte")

for (x in list(br_nacional, br_edad, br_cohgrupo, br_tercil, br_cohorte)) {
  x[, `:=`(bracket_ancho = abs(std - obs))]
}

fwrite(br_nacional, file.path(out_dir, "67_bracket_nacional.csv"))
fwrite(br_edad,     file.path(out_dir, "67_bracket_grupo_etario.csv"))
fwrite(br_cohgrupo, file.path(out_dir, "67_bracket_grupo_cohorte.csv"))
fwrite(br_tercil,   file.path(out_dir, "67_bracket_tercil.csv"))
fwrite(br_cohorte,  file.path(out_dir, "67_bracket_cohorte.csv"))

log_msg("Cobertura del contrafactico (fraccion del peso en celdas con al menos un directo): minimo %.4f, mediana %.4f",
        min(br_edad$cobertura), median(br_edad$cobertura))
log_msg("\nBracket nacional, prevalencia actual:\n%s",
        paste(capture.output(print(br_nacional[metrica == "prevalencia_actual",
                                                .(anio, obs, std, ancho_pp = 100 * (std - obs))], digits = 4)), collapse = "\n"))

log_msg("\n--- 5) Posicion del motor dentro / fuera del bracket ---")

sim_edad <- fread(file.path(out_dir, "48_descomposicion_error_edad.csv"))[
  , .(anio, grupo_etario, sim = prevalencia_sim, obs_r28 = prevalencia_obs)]
sim_edad[, metrica := "prevalencia_actual"]

pos_edad <- merge(br_edad[metrica == "prevalencia_actual"], sim_edad, by = c("anio", "grupo_etario", "metrica"))
pos_edad[, `:=`(
  piso = pmin(obs, std), techo = pmax(obs, std),
  dif_contra_obs_pp = 100 * (sim - obs),
  dif_contra_std_pp = 100 * (sim - std)
)]
pos_edad[, dentro := sim >= piso & sim <= techo]
pos_edad[, exceso_sobre_techo_pp := 100 * pmax(0, sim - techo)]
pos_edad[, chequeo_obs_r28_pp := 100 * (obs_r28 - obs_total)]
setorder(pos_edad, grupo_etario, anio)
fwrite(pos_edad, file.path(out_dir, "67_posicion_motor_grupo_etario.csv"))
log_msg("Chequeo de consistencia del lado observado (este script vs. R/28): desvio maximo %.4f pp",
        max(abs(pos_edad$chequeo_obs_r28_pp)))
log_msg("\nPosicion del motor por grupo etario:\n%s",
        paste(capture.output(print(pos_edad[, .(anio, grupo_etario, obs, std, sim, dentro,
                                                 exceso_sobre_techo_pp)], digits = 4)), collapse = "\n"))

sim_limpias <- fread(file.path(out_dir, "46_validacion_prevalencia_cohortes_limpias.csv"))[
  , .(anio, sim = prevalencia_sim)]
pos_limpias <- merge(br_cohgrupo[metrica == "prevalencia_actual" & grupo_cohorte == "limpia"],
                     sim_limpias, by = "anio")
pos_limpias[, `:=`(piso = pmin(obs, std), techo = pmax(obs, std))]
pos_limpias[, dentro := sim >= piso & sim <= techo]
pos_limpias[, `:=`(dif_contra_obs_pp = 100 * (sim - obs), dif_contra_std_pp = 100 * (sim - std))]
setorder(pos_limpias, anio)
fwrite(pos_limpias, file.path(out_dir, "67_posicion_motor_cohortes_limpias.csv"))
log_msg("\nPosicion del motor en cohortes limpias (prevalencia actual):\n%s",
        paste(capture.output(print(pos_limpias[, .(anio, obs, std, sim, dentro,
                                                    dif_contra_obs_pp, dif_contra_std_pp)], digits = 4)), collapse = "\n"))

tray <- fread(file.path(out_dir, "52_trayectoria_nunca_cohorte.csv"))
sim_coh <- rbind(
  tray[, .(cohorte, anio, metrica = "nunca_fumo", sim = nunca_sim)],
  tray[, .(cohorte, anio, metrica = "prevalencia_actual", sim = prev_sim)]
)
pos_coh <- merge(br_cohorte, sim_coh, by = c("cohorte", "anio", "metrica"))
pos_coh[, `:=`(piso = pmin(obs, std), techo = pmax(obs, std))]
pos_coh[, dentro := sim >= piso & sim <= techo]
pos_coh[, `:=`(dif_contra_obs_pp = 100 * (sim - obs), dif_contra_std_pp = 100 * (sim - std))]
pos_coh[, cohorte_limpia := cohorte %in% COHORTES_LIMPIAS]
setorder(pos_coh, metrica, cohorte, anio)
fwrite(pos_coh, file.path(out_dir, "67_posicion_motor_cohorte.csv"))

resumen_coh <- pos_coh[metrica == "prevalencia_actual" & anio >= 2014,
                       .(anios = .N, anios_dentro = sum(dentro),
                         dif_media_contra_obs_pp = mean(dif_contra_obs_pp),
                         dif_media_contra_std_pp = mean(dif_contra_std_pp)),
                       by = .(cohorte, cohorte_limpia)][order(cohorte)]
fwrite(resumen_coh, file.path(out_dir, "67_posicion_motor_cohorte_resumen.csv"))
log_msg("\nResumen por cohorte (prevalencia actual, 2014-2025): en cuantos anios el motor cae dentro del bracket:\n%s",
        paste(capture.output(print(resumen_coh, digits = 3)), collapse = "\n"))

log_msg("\n--- 6) Diagnostico: mis-reporte del proxy vs. seleccion no observable ---")

base_estado <- P[, .(share_pob = sum(peso * (estado == .BY$k)) / sum(peso)), by = .(k = estado)]
rel <- merge(brechas[esquema == "edad5 x sexo x tercil", .(brecha_media = mean(brecha_estandarizada)),
                     by = .(estado, estado_lab)],
             P[, .(share_pob = sum(peso) / P[, sum(peso)]), by = .(estado)], by = "estado")
rel[, brecha_relativa := brecha_media / share_pob]
setorder(rel, estado)
fwrite(rel, file.path(out_dir, "67_brecha_relativa_por_estado.csv"))
log_msg("Brecha absoluta y relativa al tamano del estado (esquema base, promedio 2014-2025):\n%s",
        paste(capture.output(print(rel, digits = 3)), collapse = "\n"))

ancho <- dcast(br_edad[metrica == "prevalencia_actual"], anio ~ grupo_etario,
               value.var = "bracket_ancho")
log_msg("\nAncho del bracket (pp) en prevalencia actual, por grupo etario:\n%s",
        paste(capture.output(print(ancho[, lapply(.SD, function(x) round(100 * x, 2)), .SDcols = !"anio", by = anio])), collapse = "\n"))

log_msg("\n=== outputs: 67_brecha_informante_por_estado.csv, 67_bracket_{nacional,grupo_etario,grupo_cohorte,tercil,cohorte}.csv,")
log_msg("    67_posicion_motor_{grupo_etario,cohortes_limpias,cohorte,cohorte_resumen}.csv, 67_brecha_relativa_por_estado.csv ===")
close(log_con)
