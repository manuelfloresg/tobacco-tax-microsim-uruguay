# 05_correccion_2_4.R
#
# Cumulative excess of the never-smoked share over its mortality bound, by birth cohort, with and
# without proxy responses. Used by the cohort analysis of the backcast (R/28).
# Input:  output/tables/24_*.csv
# Output: output/tables/25_*.csv

library(here)
library(data.table)

here::i_am("R/05_correccion_2_4.R")

log_con <- file(here::here("logs", "05_correccion_2_4.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 05_correccion_2_4.R -- %s ===", format(Sys.time()))

out_dir <- here::here("output", "tables")
int_dir <- here::here("data", "interim")

UMBRAL_ATRICION <- 0.025

log_msg("\n--- A.1: poder de la cota por cohorte ---")

poder_y_exceso <- function(path_csv, tag) {
  tab <- fread(path_csv)
  tab[, atricion_rate := atricion / N_prev]
  tab[, saturado := cota_proporcion >= 0.9999]

  poder <- tab[, .(n_pares = .N, atricion_media = round(mean(atricion_rate), 4),
                    frac_saturado = round(mean(saturado), 4)), by = cohorte]
  poder[, informativa := atricion_media < UMBRAL_ATRICION]
  setorder(poder, cohorte)
  fwrite(poder, file.path(out_dir, sprintf("25_cota_poder_por_cohorte%s.csv", tag)))

  log_msg("  [%s] distribucion de atricion_media por cohorte:", tag)
  for (i in seq_len(nrow(poder))) {
    log_msg("    cohorte %d: atricion_media=%.1f%% frac_saturado=%.0f%% informativa=%s",
            poder$cohorte[i], 100 * poder$atricion_media[i], 100 * poder$frac_saturado[i],
            poder$informativa[i])
  }

  cohortes_inf <- poder[informativa == TRUE]$cohorte
  acum <- tab[cohorte %in% cohortes_inf, .(
    n_pares = .N,
    exceso_acumulado = round(sum(exceso), 4),
    frac_pares_positivo = round(mean(exceso > 0), 4)
  ), by = cohorte]
  acum <- merge(acum, poder[, .(cohorte, atricion_media, frac_saturado)], by = "cohorte")
  setorder(acum, cohorte)
  fwrite(acum, file.path(out_dir, sprintf("25_exceso_acumulado%s.csv", tag)))
  log_msg("  [%s] cohortes informativas (atricion media < %.1f%%): %s",
          tag, 100 * UMBRAL_ATRICION, paste(cohortes_inf, collapse = ", "))
  list(poder = poder, acum = acum)
}

res_todos   <- poder_y_exceso(file.path(out_dir, "24_cota_mortalidad_cohortes.csv"), "")
res_directo <- poder_y_exceso(file.path(out_dir, "24_cota_mortalidad_directo.csv"), "_directo")

log_msg("\n--- A.2: identidad de exfumadores por cohorte ---")

P <- fread(file.path(int_dir, "panel_personas_15mas_con_informante.csv.gz"))

pares <- list(c(2017, 2018), c(2018, 2019), c(2021, 2022), c(2022, 2023), c(2023, 2024), c(2024, 2025))

stock_cohorte_anio <- P[, .(
  E1 = sum(peso * (estado == 4)), E2 = sum(peso * (estado == 5)),
  n_E1 = sum(estado == 4), n_E2 = sum(estado == 5)
), by = .(cohorte, anio)]

id_cohorte <- list()
for (p in pares) {
  t0 <- p[1]; t1 <- p[2]
  s0 <- stock_cohorte_anio[anio == t0]
  s1 <- stock_cohorte_anio[anio == t1, .(cohorte, E2_t1 = E2, n_E2_t1 = n_E2)]
  m <- merge(s0, s1, by = "cohorte")
  m[, `:=`(anio_t0 = t0, anio_t1 = t1, edad_t0 = t0 - cohorte)]
  m[, E2_esperado := E1 + E2]
  m[, atricion_implicita := round(1 - E2_t1 / E2_esperado, 4)]
  id_cohorte[[length(id_cohorte) + 1]] <- m[, .(cohorte, edad_t0, anio_t0, anio_t1,
                                                 E1_t0 = round(E1, 1), n_E1_t0 = n_E1,
                                                 E2_t0 = round(E2, 1), n_E2_t0 = n_E2,
                                                 E2_esperado = round(E2_esperado, 1),
                                                 E2_observado = round(E2_t1, 1), n_E2_t1,
                                                 atricion_implicita)]
}
id_cohorte_dt <- rbindlist(id_cohorte)
setorder(id_cohorte_dt, cohorte, anio_t0)
fwrite(id_cohorte_dt, file.path(out_dir, "25_identidad_exfumadores_cohorte.csv"))

base_ok <- id_cohorte_dt[n_E1_t0 + n_E2_t0 >= 30]
cor_edad <- base_ok[is.finite(atricion_implicita), cor(edad_t0, atricion_implicita, use = "complete.obs")]
log_msg("Correlacion (atricion_implicita) x (edad_t0), pares con base>=30: %.3f (n=%d pares)",
        cor_edad, nrow(base_ok[is.finite(atricion_implicita)]))

log_msg("\n=== outputs: 25_cota_poder_por_cohorte(_directo).csv, 25_exceso_acumulado(_directo).csv, 25_identidad_exfumadores_cohorte.csv ===")
close(log_con)

print(res_todos$acum)
print(base_ok[, .(cohorte, edad_t0, anio_t0, n_E1_t0, n_E2_t0, atricion_implicita)])
