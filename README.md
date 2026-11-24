# The timing of tobacco tax effects — replication package

*A dynamic microsimulation of an excise increase in Uruguay, 2025–2060.*

**Manuel Flores¹, Mariana Gerstenblüth¹, Luca Pruzzo²**
¹ Departamento de Economía, Facultad de Ciencias Sociales, Universidad de la República, Montevideo, Uruguay
² Escuela de Negocios, Universidad Adolfo Ibáñez, Santiago, Chile
Contact: manuel.flores@cienciassociales.edu.uy

This repository contains the code, the model inputs and the model outputs behind:

- Flores M, Gerstenblüth M, Pruzzo L. *The timing of tobacco tax effects: a dynamic microsimulation
  of an excise increase in Uruguay, 2025–2060.* Manuscript submitted for publication.
- Flores M, Gerstenblüth M, Pruzzo L. *Distribución temporal de los efectos de un
  impuesto al tabaco. Microsimulación dinámica de un aumento impositivo en Uruguay, 2025–2060.*
  Working paper, Tabaconomía, forthcoming.

Archived version: https://doi.org/10.5281/zenodo.24516151 (all versions). Funding: Universidad Adolfo Ibáñez and Bloomberg Philanthropies (grant 2022-110829).
See `CITATION.cff`.

*Resumen en español al final.*

---

## What the model does

A synthetic population of Uruguayan residents aged 15 and over (one simulated individual per
inhabitant, about 2.65 million in 2011) is followed in annual cycles from 2011 to 2060. Each
individual carries age, sex, household income tertile, smoking state (never, occasional, daily,
quit less than one year ago, quit one year ago or more), the smoking pattern before quitting and
time since quitting. Cessation, initiation and relapse rates are estimated from Uruguayan surveys
(eleven waves of the *Encuesta Continua de Hogares*, 2011–2025; the national drug surveys; the
2009 and 2017 Global Adult Tobacco Surveys). The two transitions between occasional and daily
smoking are calibrated to observed prevalence (three parameters). Mortality is applied by smoking
state, with relative risks rescaled to the survival implied in the national population
projections (INE, Revision 2025). Policy scenarios move the total tax share of the retail price
(excise plus value added tax) to 75% (WHO) or 81.55% (EU average), and transitions respond to the resulting price level through
tertile-specific elasticities split between an extensive and an intensive margin.

## Three levels of reproduction

| Level | What it reproduces | Needs | Time (8 cores) |
|---|---|---|---|
| 1 | Every figure and table of the working paper and the article | this repository | ~5 minutes |
| 2 | The simulation itself: every scenario, sensitivity analysis, uncertainty band and seed-noise measure | this repository | ~2 days |
| 3 | The model inputs from the survey microdata | the microdata, downloaded separately (`data/raw/README.md`) | several hours, plus the download |

### Level 1 — figures and tables (minutes)

```bash
bash reproducir_figuras_tablas.sh
```

Reads `output/tables/` and writes the figures and tables of the article (`paper/TC/figures/`,
`paper/TC/tables/`, `paper/TC/supp/tables/`) and of the working paper, in Spanish
(`informe/figuras/`, `informe/tablas/`). Does not run the model. The outputs in this package are
those of the article. The working paper uses an earlier run, before the fiscal anchor was updated
to the 2024 WHO tax profile, so the Spanish figures and tables regenerated here differ from its
numbers (for example, the 2060 fall in consumption is 15.5% there and 14.4% here).

### Level 2 — the simulation (about two days)

```bash
bash reproducir_simulacion.sh
```

Re-runs, from the model inputs in `data/interim/` and `output/tables/`, every model run that
produces a reported number, in dependency order, and then Level 1:

| Step | Script | What |
|---|---|---|
| 1 | `R/26`, `R/40`, `R/53`, `R/68` | backcast 2011–2025, measurement bracket (proxy reporting and sampling error), check of the extensive-margin correction factor |
| 2 | `R/38`, `R/80`, `R/79` | status quo and the three tax trajectories; check of the consumption decomposition; prevalence by tertile standardised for age |
| 3 | `R/45 C` | envelope over elasticity gradients (3 h) |
| 4 | `R/48`, `R/52` | split of the price response by margin; distributional metrics |
| 5 | `R/64` | cessation vs. avoided initiation; deaths averted |
| 6 | `R/69` | alternative calibration point and graded former-smoking mortality risk (3 seeds) |
| 7 | `R/72`, `R/73`, `R/77` | cessation trend capped in 2030; relapse at its lower bound; denominator bias of the cessation rate |
| 8 | `R/50`, `R/67`, `R/78` | Gaussian-process emulator and credible intervals; 100 seeds × 6 branches (13 h); sensitivity analyses of the article: timing, equal response across tertiles, pass-through and illicit diversion, roll-your-own tobacco, delayed start, graded former-smoking risk and starts from the 2025 survey, annual 2025–2060, 100 or 20 seeds (9 h) |
| 9 | Level 1 | |

**Reported numbers come from one run (seed 20260809) for every scenario.** All branches share the
seed, so differences between scenarios are paired comparisons, and quantities linked by
definition (consumption = number of people who smoke × intensity, the split of the extensive
margin) add up exactly. The 100 seeds of `R/67` measure the simulation noise of each reported
number; deaths averted, which a single run cannot resolve, are reported as the mean over the 100
seeds. The full noise summary is `output/tables/91_semillas_resumen.csv`.

### Level 3 — from the microdata

Download the sources listed in `data/raw/README.md` into `data/raw/`, then run the estimation
scripts (stages A–D in the script map below) in numerical order, recalibrate with `R/25`, and
then run Level 2. This level was run in the original project; it has not been re-executed end to
end from inside this package.

## Reproducibility

- **Software.** R 4.5.3 (Windows, ucrt) with here 1.0.1, data.table 1.17.6, haven 2.5.5,
  dplyr 1.1.4, survey 4.4.2, splines, survival 3.8.6, readxl 1.4.5, ggplot2 3.5.2,
  patchwork 1.3.1, svglite 2.2.2. Run every script from the repository root; paths are resolved
  with `here` against the `.here` file.
- **Seeds.** Every random procedure uses a fixed, explicit seed.
- **Bitwise reproducibility.** The model is chaotically sensitive: a difference of 1e-7 in a
  parameter flips individual random draws and produces a visibly different trajectory with the
  same statistical properties. That is why parameters are stored with 15–17 significant digits,
  and why the numbers in `output/tables/` are reproduced exactly only with the same R and package
  versions. With other versions, results should differ by amounts of the order of the simulation
  noise reported in `91_semillas_resumen.csv` (a standard deviation of 0.19 percentage points on
  the 2060 consumption effect, against a credible interval 6.7 points wide).
- **Guardrails.** `R/67` aborts unless seed 20260809 reproduces the published scenario runs;
  several other scripts abort when an input they depend on changes silently.
- **Logs.** Each script writes a log to `logs/`.

## What is included and what is not

Included:

- `R/` — the scripts that build the model inputs, run the model and produce every reported number,
  together with the diagnostics on which the validation section rests (see the map).
- `data/interim/` — the fitted transition models (`modelo_*.rds`), **stripped of their
  estimation data**: only coefficients, terms and what `predict()` needs; predictions are
  bit-identical to the original objects. And the synthetic initial population
  (`poblacion_sintetica_2011.csv.gz`), derived from the public ECH 2011, and the two 2025
  populations used by the sensitivity analyses that start in 2025 (`poblacion_sintetica_2025.csv.gz`,
  as recorded, and `poblacion_sintetica_2025_corr.csv.gz`, corrected for proxy reporting), derived
  from the public ECH 2025.
- `data/raw/Proyecciones_poblacion/` — INE population projections (public aggregate data).
- `output/tables/` — the intermediate and final tables produced by the scripts, as CSV.

Not included:

- Survey microdata (ECH, GATS, JND, ENAJ, ENS, STEPS). All are public or available on request
  from the producing institution; see `data/raw/README.md`.
- The text of the working paper and the article.
- Exploratory scripts for alternatives that were tested and not adopted.

## Script map

Scripts are numbered in the order they were written, which is close to, but not exactly, the
order of execution; gaps in the numbering correspond to scripts not included. Each script opens
with a header stating what it does, what it reads and what it writes. Variable, column and file
names are in Spanish.

**A. Harmonisation of the microdata** — `02`–`16`. Valid response counts, person panel and
synthetic cohorts, measurement validity, test of the 2019 questionnaire change, initiation and
entry states from GATS and the ECH, drift of the never-smoked share, intensity by survey.

**B. Transition estimates and model inputs** — `17` cessation; `18`, `29` initiation (`29`
replaces the level of `18` with the drug surveys); `19` relapse (pooled up to age 44,
age-conditional from 45); `20` occasional↔daily transitions; `22` synthetic population; `23`
survival by smoking state; `34` cigarettes-per-day distribution.

**C. Prices and taxes** — `21`, `32`, `33` annual fiscal panel (prices, excise, legal volumes,
revenue; DGI and INE series).

**D. Model core and calibration** — `24` transition engine (sourced by every model run, not run
on its own); `25` calibration of the occasional↔daily transitions.

**E. Validation and diagnostics** — `05`, `26`, `28`, `30` retrospective run 2011–2025 and its
breakdown by cohort and age; `40`, `53` measurement bracket (proxy reporting and sampling error);
`62`, `63`, `74` the excess prevalence above age 45 and the relapse specification; `37` the
2003–2010 tax episode; `68` equilibrium response of prevalence; `77` bias in the denominator of
the cessation rate.

**F. Price response** — `47`, `48`, `58`: elasticity specification, split by margin, evidence
on the income gradient.

**G. Scenarios and results** — `38` scenario space; `45` gradient envelope; `50` credible
intervals; `52` distributional metrics; `64` extensive-margin decomposition and deaths averted;
`67` simulation noise with 100 seeds; `69`, `72`, `73`, `78` sensitivity analyses; `79` prevalence
by tertile standardised for age; `80` decomposition of consumption by number of people who smoke.

**H. Deliverables** — `55` figures, `56`, `57` tables, `60` parameter tables, `61` state diagram.
Each of `55`, `56`, `60` and `61` takes `es` as an argument for the Spanish working-paper version.

## Licence

Code: MIT (`LICENSE`). Data files in `data/` and `output/`: CC BY 4.0
(https://creativecommons.org/licenses/by/4.0/). The underlying survey microdata remain subject
to the terms of their producers.

---

## Resumen en español

Paquete de replicación del artículo *The timing of tobacco tax effects* y del documento de
trabajo *Distribución temporal de los efectos de un impuesto al tabaco. Microsimulación dinámica
de un aumento impositivo en Uruguay, 2025–2060* (Tabaconomía, en prensa).

- **Nivel 1** (`bash reproducir_figuras_tablas.sh`, minutos): regenera todas las figuras y tablas
  del documento y del artículo a partir de `output/tables/`, sin correr el modelo.
- **Nivel 2** (`bash reproducir_simulacion.sh`, alrededor de dos días en 8 núcleos): vuelve
  a correr todas las simulaciones que producen cifras reportadas, incluidas las sensibilidades,
  las bandas de incertidumbre y las cien semillas, y después el nivel 1.
- **Nivel 3**: reconstruye los insumos del modelo desde los microdatos, que no se incluyen y se
  descargan según `data/raw/README.md`.

Las cifras reportadas provienen de una corrida única (semilla 20260809) en todos los escenarios;
las cien semillas miden el ruido de simulación de cada cifra. Los modelos de transición se
incluyen sin sus datos de estimación y predicen de manera idéntica a los originales. Cada
script abre con un encabezado en inglés; los nombres de variables y archivos están en español.
