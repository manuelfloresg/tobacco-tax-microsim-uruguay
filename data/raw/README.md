# data/raw — source data (not included)

Only needed for **Level 3** (rebuilding the model inputs from the microdata). Levels 1 and 2 run
with what is already in the repository.

The scripts expect the files below at exactly these paths. Survey microdata are not
redistributed here; each is public or available on request from the institution that produces it.
The one file included, `Proyecciones_poblacion/poblacion_1950_2070_empalmada.dta`, holds INE
population projections by single year of age and sex (public aggregate data), as a spliced
1950–2070 series.

| Path under `data/raw/` | Source | Producer | Used by |
|---|---|---|---|
| `ECH/ech6_2006.dta`, `ech11_2011.dta`, `ech14_2014.dta`, `ech17_2017.dta`, `ech18_2018.dta`, `ech19_2019.dta`, `ech21_2021.dta`, `ech22_2022.dta`, `ech23_impl_2023.dta`, `ech24_2024.dta`, `ECH_2025_implantacion.csv` | Encuesta Continua de Hogares, annual microdata (implantation form from 2023) | Instituto Nacional de Estadística (INE) | 02–04 (see note below) |
| `GATS/gats_2009.dta`, `gats_2017.dta` | Global Adult Tobacco Survey, Uruguay | Ministerio de Salud Pública, WHO, CDC | 06, 08, 12, 14–16, 19, 20, 34 |
| `JND_Hogares/jnd_1998.dta`, `jnd_2001_2018.dta`, `jnd_2024.dta` | Encuesta Nacional sobre Consumo de Drogas en Población General | Junta Nacional de Drogas, Observatorio Uruguayo de Drogas | 06, 29 |
| `JND_Ensenanza_Media/jovenes_2003_2021.dta` | Encuesta Nacional sobre Consumo de Drogas en Estudiantes de Enseñanza Media | Junta Nacional de Drogas | 12, 29 |
| `ENS/ens_2014.sav` | Encuesta Nacional de Salud 2014 | Ministerio de Salud Pública | 15 |
| `STEPS/steps_2006.dta`, `steps_2013.dta` | WHO STEPwise survey of NCD risk factors, Uruguay (2006 has no sampling weights) | Ministerio de Salud Pública, WHO | 15, 16, 19 |
| `DGI_series_2026/IMESI_vol_fisico_series_mensuales_2026-07.csv` | "Volúmenes físicos de bienes gravados por el IMESI – Series mensuales", downloaded July 2026 from https://www.gub.uy/direccion-general-impositiva | Dirección General Impositiva (DGI) | 32 |
| `DGI_series_2026/IMESI_recaudacion_por_rubros_series_mensuales_2026-07.csv` | "Recaudación de IMESI por rubros – Series mensuales", same site | DGI | 32 |
| `INE_IPC_2026/IPC_detallado_TotalPais_base_Oct2022.xlsx`, `IPC_divgrupoclase_TotalPais_base_Dic2010.xls`, `IPC_general_empalmado_base_Oct2022.xlsx` | Consumer price index series (base Dec 2010 and Oct 2022), https://www5.ine.gub.uy | INE | 33 |
| `Impuestos/*.xlsx`, `Impuestos/*.xlsm`, `Precios/*.dta`, `Precios/IPC_Tabaco_1997-2022.xlsx` | Monthly IMESI rates, legal volumes and revenue (DGI), cigarette prices and the tobacco CPI component (INE), compiled by the research group in earlier work | DGI, INE; compilation by the authors | 21 |

**Note on the ECH person panel.** `R/03` and `R/04` build a harmonised person-level panel from
the ECH (`data/interim/panel_personas_15mas*.csv.gz`). It is microdata and is not included. The
scripts that estimate transitions from it (`17`, `19`, `20`, `22`, `23`), the calibration (`25`)
and the backcast validation (`26`, `28`, `30`, `40`, `53`, `62`, `63`, `74`, `77`) therefore
belong to Level 3. Their results are included in `data/interim/` and `output/tables/`.

The annual fiscal panel that the model actually reads (`output/tables/41_panel_fiscal_anual.csv`)
is included, so the price and tax sources are only needed to rebuild it.

Household income tertiles are computed from the ECH on a constant, deflated per-capita concept.
Two traps in the ECH that `R/02`–`R/04` handle: in 2014 and 2022 household variables are loaded
only on the household head's record; and the 2025 wave has no `HT11` (the scripts use
`YDA_SVL / HT19`).
