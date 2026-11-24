# 20_escalada_downshift.R
#
# Daily-to-occasional transitions observable in GATS, by age and sex. Used as the observed
# reference for the calibrated downshift rate (R/25).
# Input:  data/raw/GATS/
# Output: output/tables/40_*.csv

library(here)
library(data.table)
library(haven)

here::i_am("R/20_escalada_downshift.R")

log_con <- file(here::here("logs", "20_escalada_downshift.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 20_escalada_downshift.R -- %s ===", format(Sys.time()))

out_dir <- here::here("output", "tables")
raw_gats <- here::here("data", "raw", "GATS")

log_msg("\n--- 1) Carga GATS 2009/2017: downshift (B01==2 actual, B02==1 fue diario antes) ---")

cargar_gats_downshift <- function(path, anio_encuesta) {
  nms0 <- names(haven::read_dta(path, n_max = 1))
  agevar <- nms0[match("age", tolower(nms0))]
  d <- haven::read_dta(path, col_select = dplyr::all_of(c("A01", agevar, "gatsweight", "B01", "B02")))
  setDT(d)
  setnames(d, agevar, "edad")
  setnames(d, "gatsweight", "peso")
  d[, edad := as.numeric(edad)]
  d[, peso := as.numeric(peso)]
  d[, sexo := fifelse(as.numeric(A01) == 1, "varon", fifelse(as.numeric(A01) == 2, "mujer", NA_character_))]
  d[, B01n := as.numeric(B01)]
  d[, B02n := as.numeric(B02)]
  d[, anio_encuesta := anio_encuesta]
  d[B01n == 2 & !is.na(sexo) & !is.na(edad), .(anio_encuesta, sexo, edad, fue_diario_antes = B02n == 1, peso)]
}
g09 <- cargar_gats_downshift(file.path(raw_gats, "gats_2009.dta"), 2009)
g17 <- cargar_gats_downshift(file.path(raw_gats, "gats_2017.dta"), 2017)
G <- rbind(g09, g17)
log_msg("Ocasionales actuales (B01==2) con dato de B02: 2009 = %d, 2017 = %d, total = %d", nrow(g09), nrow(g17), nrow(G))

log_msg("\n--- 2) Share de downshift por grupo etario x sexo ---")

G[, grupo_etario := fcase(
  edad < 25, "15-24", edad < 45, "25-44", edad < 65, "45-64", default = "65+"
)]

por_grupo_ola <- G[, .(n = .N, n_pond = sum(peso), share_downshift = sum(peso * fue_diario_antes) / sum(peso)),
                    by = .(anio_encuesta, grupo_etario, sexo)]
setorder(por_grupo_ola, sexo, grupo_etario, anio_encuesta)
log_msg("Share de downshift por ola (celdas con n>=10 son las minimamente confiables):\n%s",
        paste(capture.output(print(por_grupo_ola)), collapse = "\n"))

por_grupo_comb <- G[, .(n = .N, n_pond = sum(peso), share_downshift = sum(peso * fue_diario_antes) / sum(peso)),
                     by = .(grupo_etario, sexo)]
setorder(por_grupo_comb, sexo, grupo_etario)
fwrite(por_grupo_ola, file.path(out_dir, "40_downshift_diario_ocasional_edad_sexo.csv"))
fwrite(por_grupo_comb, file.path(out_dir, "40_downshift_diario_ocasional_edad_sexo_combinado.csv"))
log_msg("Share de downshift combinando ambas olas (2009+2017), por grupo etario x sexo:\n%s",
        paste(capture.output(print(por_grupo_comb)), collapse = "\n"))

w_ola <- dcast(por_grupo_ola[n >= 10], grupo_etario + sexo ~ anio_encuesta,
               value.var = c("n", "share_downshift"))
if (all(c("share_downshift_2009", "share_downshift_2017") %in% names(w_ola))) {
  w_ola[, diferencia_2017_menos_2009 := share_downshift_2017 - share_downshift_2009]
  log_msg("Diferencia 2017 vs 2009 en celdas con n>=10 en ambas olas (estabilidad del indicador entre olas):\n%s",
          paste(capture.output(print(w_ola[!is.na(diferencia_2017_menos_2009)])), collapse = "\n"))
}

log_msg("Share de downshift agregado (todos los grupos, ambas olas, por sexo): varon = %.3f, mujer = %.3f",
        G[sexo == "varon", sum(peso * fue_diario_antes) / sum(peso)],
        G[sexo == "mujer", sum(peso * fue_diario_antes) / sum(peso)])

log_msg("\n--- 3) Upshift (ocasional -> diario): sin proxy directo en las fuentes del proyecto ---")
log_msg("Ni la ECH ni GATS preguntan, a quien hoy es fumador diario, si paso antes por un periodo")
log_msg("ocasional -- confirma la conclusion ya escrita en R/12 Parte C.4. La edad de inicio a")
log_msg("diario (B04/B08/B11 de GATS, ya usada en R/08 -> 28_hazard_por_cohorte_sexo_combinado.csv)")
log_msg("es lo mas cercano disponible, pero mezcla nunca->diario directo con ocasional->diario sin")
log_msg("separarlos. Se deja como ancla PROVISORIA para el motor hasta la etapa de calibracion por")
log_msg("ajuste de momentos que R/12 ya identifico como el camino")
log_msg("correcto para esta transicion especifica -- no se inventa una tasa aqui.")

referencia_upshift <- fread(file.path(out_dir, "28_hazard_por_cohorte_sexo_combinado.csv"))
log_msg("Tabla de referencia (NO especifica de ocasional->diario, mezcla de entrada a diario en general): 28_hazard_por_cohorte_sexo_combinado.csv, %d filas, ya construida en R/08.",
        nrow(referencia_upshift))

log_msg("\n=== outputs: 40_downshift_diario_ocasional_edad_sexo.csv, 40_downshift_diario_ocasional_edad_sexo_combinado.csv ===")
close(log_con)
