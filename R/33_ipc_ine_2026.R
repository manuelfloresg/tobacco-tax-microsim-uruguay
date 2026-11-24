# 33_ipc_ine_2026.R
#
# Real cigarette price 2022-2026 from the INE consumer price index (tobacco component relative to
# the general index), spliced onto the annual fiscal panel. Aborts if any existing value would change.
# Input:  data/raw/INE_IPC_2026/, output/tables/41_panel_fiscal_anual.csv
# Output: output/tables/41_panel_fiscal_anual.csv (extended), 55_*.csv

library(here)
library(data.table)
library(readxl)

here::i_am("R/33_ipc_ine_2026.R")

log_con <- file(here::here("logs", "33_ipc_ine_2026.log"), open = "wt", encoding = "UTF-8")
log_msg <- function(...) { msg <- sprintf(...); cat(msg, "\n"); cat(msg, "\n", file = log_con) }
log_msg("=== 33_ipc_ine_2026.R -- %s ===", format(Sys.time()))

raw_ipc <- here::here("data", "raw", "INE_IPC_2026")
out_dir <- here::here("output", "tables")

f_nuevo <- file.path(raw_ipc, "IPC_detallado_TotalPais_base_Oct2022.xlsx")
f_viejo <- file.path(raw_ipc, "IPC_divgrupoclase_TotalPais_base_Dic2010.xls")
f_gral  <- file.path(raw_ipc, "IPC_general_empalmado_base_Oct2022.xlsx")

MESES_TXT <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
               "julio", "agosto", "setiembre", "septiembre", "octubre", "noviembre", "diciembre")
MESES_NUM <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 11, 12)

a_fecha <- function(x) {
  chr <- trimws(as.character(x))
  v <- suppressWarnings(as.numeric(chr))
  out <- as.Date(rep(NA_real_, length(chr)), origin = "1970-01-01")
  es_seg <- !is.na(v) & v > 1e6
  es_ser <- !is.na(v) & v > 1e4 & v <= 1e6
  out[es_seg] <- as.Date(as.POSIXct(v[es_seg], origin = "1970-01-01", tz = "UTC"))
  out[es_ser] <- as.Date(v[es_ser], origin = "1899-12-30")
  falta <- is.na(out) & !is.na(chr) & nzchar(chr)
  if (any(falta)) {
    txt <- tolower(chr[falta])
    m <- sapply(txt, function(s) {
      hit <- which(sapply(MESES_TXT, function(mm) grepl(mm, s, fixed = TRUE)))
      if (length(hit) == 0) NA_integer_ else MESES_NUM[hit[1]]
    })
    a <- suppressWarnings(as.integer(sub(".*?(\\d{4}).*", "\\1", txt)))
    ok <- !is.na(m) & !is.na(a)
    res <- as.Date(rep(NA_real_, length(txt)), origin = "1970-01-01")
    res[ok] <- as.Date(sprintf("%d-%02d-01", a[ok], m[ok]))
    out[falta] <- res
  }
  as.Date(format(out, "%Y-%m-01"))
}

log_msg("\n--- 1) Base Octubre 2022=100 (serie detallada, Total Pais) ---")

NU <- setDT(suppressMessages(read_excel(f_nuevo, sheet = "Cuadro 2.0", col_names = FALSE, skip = 10)))
setnames(NU, 1:7, c("division", "grupo", "clase", "subclase", "producto", "descripcion", "ponderacion"))
cols_fecha <- 8:ncol(NU)
fechas_nu <- a_fecha(unlist(NU[1, ..cols_fecha]))
log_msg("Columnas de fecha: %d, de %s a %s", length(fechas_nu),
        format(min(fechas_nu), "%Y-%m"), format(max(fechas_nu), "%Y-%m"))
stopifnot(!anyNA(fechas_nu), min(fechas_nu) >= as.Date("2022-09-01"))

fila_de <- function(dt, cond, etiqueta) {
  i <- which(cond)
  if (length(i) != 1) stop(sprintf("33: no se identifico univocamente la fila '%s' (%d coincidencias)",
                                   etiqueta, length(i)))
  log_msg("  fila '%s': %d (%s, ponderacion %.5f)", etiqueta, i, dt$descripcion[i],
          as.numeric(dt$ponderacion[i]))
  i
}
i_gral_nu <- fila_de(NU, is.na(NU$division) & toupper(trimws(NU$descripcion)) == "GENERAL", "GENERAL")
i_tab_nu  <- fila_de(NU, NU$grupo == "023" & NU$clase == "0230" & is.na(NU$subclase) &
                       !is.na(NU$grupo) & !is.na(NU$clase), "Tabaco (clase 0230)")
i_cig_nu  <- fila_de(NU, !is.na(NU$subclase) & NU$subclase == "02301" & is.na(NU$producto), "Cigarrillos (subclase 02301)")

serie_nu <- function(i) as.numeric(unlist(NU[i, ..cols_fecha]))
NUEVO <- data.table(fecha = fechas_nu, gral = serie_nu(i_gral_nu),
                    tabaco = serie_nu(i_tab_nu), cigarrillos = serie_nu(i_cig_nu))
NUEVO <- NUEVO[!is.na(gral) & !is.na(tabaco)]
log_msg("Meses con dato: %d (%s a %s)", nrow(NUEVO),
        format(min(NUEVO$fecha), "%Y-%m"), format(max(NUEVO$fecha), "%Y-%m"))

log_msg("\n--- 2) Base Diciembre 2010=100 (Div/Gr/Cl, Total Pais) ---")

leer_hoja_vieja <- function(hoja) {
  todo <- setDT(suppressMessages(read_excel(f_viejo, sheet = hoja, col_names = FALSE,
                                            skip = 6, col_types = "text")))
  etiquetas <- unlist(todo[1, ])
  fechas_todas <- a_fecha(etiquetas)
  pos <- which(!is.na(fechas_todas))
  fechas <- fechas_todas[pos]

  col1 <- trimws(as.character(todo[[1]]))
  col2 <- trimws(as.character(todo[[2]]))
  i_g <- which(col2 == "Índice General")[1]
  i_t <- which(col1 == "0220")[1]
  if (is.na(i_g) || is.na(i_t)) stop("33: no se ubicaron 'Índice General' o el codigo 0220 en ", hoja)

  z <- data.table(fecha = fechas,
                  gral   = as.numeric(unlist(todo[i_g, ..pos])),
                  tabaco = as.numeric(unlist(todo[i_t, ..pos])))
  if (median(z$gral, na.rm = TRUE) < 50)
    stop(sprintf("33: la hoja '%s' devuelve gral con mediana %.2f -- se estan leyendo columnas de variacion, no de indice",
                 hoja, median(z$gral, na.rm = TRUE)))
  z
}
hojas <- excel_sheets(f_viejo)
VIEJO <- rbindlist(lapply(hojas, function(h) {
  z <- leer_hoja_vieja(h)
  log_msg("  hoja '%s': %d meses, %s a %s", h, nrow(z),
          format(min(z$fecha), "%Y-%m"), format(max(z$fecha), "%Y-%m"))
  z
}))
VIEJO <- unique(VIEJO[!is.na(gral) & !is.na(tabaco)], by = "fecha")
setorder(VIEJO, fecha)
log_msg("Total base vieja: %d meses, %s a %s", nrow(VIEJO),
        format(min(VIEJO$fecha), "%Y-%m"), format(max(VIEJO$fecha), "%Y-%m"))

log_msg("\n--- 3) Empalme de bases ---")

VIEJO[, ratio := tabaco / gral]
NUEVO[, ratio := tabaco / gral]
NUEVO[, ratio_cig := cigarrillos / gral]

mes_empalme <- as.Date("2022-10-01")
r_v <- VIEJO[format(fecha, "%Y-%m") == format(mes_empalme, "%Y-%m")]$ratio
r_n <- NUEVO[format(fecha, "%Y-%m") == format(mes_empalme, "%Y-%m")]$ratio
if (length(r_v) != 1 || length(r_n) != 1) stop("33: octubre 2022 no esta en ambas bases; no se puede empalmar")
log_msg("Mes de empalme %s: ratio base vieja = %.6f, ratio base nueva = %.6f",
        format(mes_empalme, "%Y-%m"), r_v, r_n)

IPC <- rbind(
  VIEJO[fecha < mes_empalme, .(fecha, base = "Dic2010", ratio_emp = ratio / r_v, ratio_cig = NA_real_)],
  NUEVO[fecha >= mes_empalme, .(fecha, base = "Oct2022", ratio_emp = ratio / r_n, ratio_cig = ratio_cig / NUEVO[fecha == mes_empalme]$ratio_cig)]
)
setorder(IPC, fecha)
log_msg("Serie empalmada: %d meses, %s a %s (ratio tabaco/general, normalizado a 1 en oct-2022)",
        nrow(IPC), format(min(IPC$fecha), "%Y-%m"), format(max(IPC$fecha), "%Y-%m"))
fwrite(IPC, file.path(out_dir, "55_ipc_mensual_empalmado.csv"))

IPC[, `:=`(anio = as.integer(format(fecha, "%Y")), mes = as.integer(format(fecha, "%m")))]
ANUAL <- IPC[, .(ratio_emp = mean(ratio_emp), ratio_cig = mean(ratio_cig), n_meses = .N), by = anio]

log_msg("\n--- 4) Calibracion y validacion contra p_reales ---")

panel <- fread(file.path(out_dir, "41_panel_fiscal_anual.csv"))
solape <- merge(panel[!is.na(p_reales) & n_meses_precio == 12, .(anio, p_reales)],
                ANUAL[n_meses == 12, .(anio, ratio_emp)], by = "anio")
log_msg("Anios de solape con doce meses en ambas fuentes: %d (%d-%d)",
        nrow(solape), min(solape$anio), max(solape$anio))
solape[, k := p_reales / ratio_emp]
K <- median(solape$k)
solape[, p_reales_ine := K * ratio_emp]
solape[, dif_pp := 100 * (p_reales_ine / p_reales - 1)]
log_msg("Constante de nivel k = %.4f (dispersion del cociente: CV = %.3f%%)",
        K, 100 * sd(solape$k) / mean(solape$k))
log_msg("Contraste anio a anio (p_reales reconstruido desde el IPC vs. el existente):\n%s",
        paste(capture.output(print(solape[, .(anio, p_reales = round(p_reales, 2),
                                               p_reales_ine = round(p_reales_ine, 2),
                                               dif_pct = round(dif_pp, 2))])), collapse = "\n"))
log_msg("Desvio maximo: %.2f%% | desvio medio absoluto: %.2f%%",
        max(abs(solape$dif_pp)), mean(abs(solape$dif_pp)))
fwrite(solape, file.path(out_dir, "55_validacion_ipc_vs_precios.csv"))

if (max(abs(solape$dif_pp)) > 5) {
  log_msg("ATENCION: el desvio supera el 5%%. El concepto 'Tabaco' del INE y el IPCcigar de")
  log_msg("'Precios e impuestos.dta' no son el mismo agregado. Revisar antes de extender.")
}

sens <- ANUAL[n_meses == 12 & !is.na(ratio_cig), .(anio, ratio_emp, ratio_cig)]
sens[, dif_pct := 100 * (ratio_cig / ratio_emp - 1)]
log_msg("Sensibilidad concepto Cigarrillos vs. Tabaco (solo base nueva):\n%s",
        paste(capture.output(print(sens[, .(anio, dif_pct = round(dif_pct, 2))])), collapse = "\n"))

log_msg("\n--- 5) Extension de p_reales en 41_panel_fiscal_anual.csv ---")

nuevos <- ANUAL[n_meses == 12, .(anio, p_reales_ine = K * ratio_emp)]
panel_viejo <- copy(panel)
panel <- merge(panel, nuevos, by = "anio", all.x = TRUE)

ext <- panel[is.na(p_reales) & !is.na(p_reales_ine)]$anio
panel[anio %in% ext, `:=`(p_reales = p_reales_ine, n_meses_precio = 12L)]
log_msg("p_reales extendido en %d anios: %s", length(ext), paste(ext, collapse = ", "))
log_msg("NOTA: p_reales de esos anios viene del IPC del INE (concepto 'Tabaco'), no de")
log_msg("'Precios e impuestos.dta'. La columna origen_p_reales lo deja explicito.")
panel[, origen_p_reales := fifelse(is.na(p_reales), NA_character_,
                            fifelse(anio %in% ext, "INE IPC (tabaco/general, empalmado)",
                                    "Precios e impuestos.dta"))]

if (2022 %in% panel$anio && !is.na(panel[anio == 2022]$p_reales_ine)) {
  log_msg("2022 (seis meses en la fuente vieja): p_reales existente = %.2f, reconstruido desde IPC = %.2f (%.2f%% de diferencia).",
          panel[anio == 2022]$p_reales, panel[anio == 2022]$p_reales_ine,
          100 * (panel[anio == 2022]$p_reales_ine / panel[anio == 2022]$p_reales - 1))
  log_msg("NO se pisa: rellenar un anio vacio y corregir uno con dato son decisiones distintas.")
}

comunes <- setdiff(intersect(names(panel_viejo), names(panel)), "anio")
viol <- rbindlist(lapply(comunes, function(v) {
  cmp <- merge(panel_viejo[, .(anio, viejo = get(v))], panel[, .(anio, nuevo = get(v))], by = "anio")
  if (is.numeric(cmp$viejo)) cmp <- cmp[!is.na(viejo) & (is.na(nuevo) | abs(nuevo - viejo) > 1e-9)]
  else cmp <- cmp[!is.na(viejo) & (is.na(nuevo) | nuevo != viejo)]
  if (nrow(cmp) == 0) NULL else cmp[, columna := v][]
}), fill = TRUE)
if (nrow(viol) > 0) {
  log_msg("Valores preexistentes que cambian (no deberia haber ninguno):\n%s",
          paste(capture.output(print(viol)), collapse = "\n"))
  stop("33: cambios inesperados en el panel, no se reescribe nada")
}
log_msg("Guardarrail: ningun valor preexistente cambia.")

panel[, p_reales_ine := NULL]
setorder(panel, anio)
fwrite(panel, file.path(out_dir, "41_panel_fiscal_anual.csv"))
log_msg("Panel reescrito. Cobertura de p_reales:\n%s",
        paste(capture.output(print(panel[anio >= 2018, .(anio, p_reales = round(p_reales, 2),
                                                          n_meses_precio, origen_p_reales)])), collapse = "\n"))

log_msg("\nPENDIENTE: porcentaje_tax_p (participacion del impuesto en el precio) sigue cortando")
log_msg("en 2022 -- el IPC no la contiene. La regla de politica de R/24 la usa solo en el anio de")
log_msg("ANCLAJE, asi que hay que asegurarse de que el anclaje siga cayendo en un anio que la tenga.")

log_msg("\n=== outputs: 55_ipc_mensual_empalmado.csv, 55_validacion_ipc_vs_precios.csv, 41_panel_fiscal_anual.csv (reescrito) ===")
close(log_con)
