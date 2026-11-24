#!/usr/bin/env bash
# Level 2: re-runs every simulation that produces a reported number, in dependency order, and
# then Level 1. About two days with 8 cores.
#
#   bash reproducir_simulacion.sh
#   N_NUCLEOS=4 RSCRIPT=/path/to/Rscript bash reproducir_simulacion.sh
#
# Starts from the transition models in data/interim/ and the calibrated parameters in
# output/tables/43_parametros_calibrados.csv (to recalibrate: Rscript R/25_calibracion_escalada.R).
# Parallel runs use N_NUCLEOS processes; do not exceed the number of PHYSICAL cores. Each script
# writes its log to logs/.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
R="${RSCRIPT:-Rscript}"
N="${N_NUCLEOS:-8}"
N50=$(( N < 4 ? N : 4 ))     # R/50 has a 21-point design: more than 4 parts does not help
SEMILLAS="20260809 20260810 20260811"
etapa() { echo "=== [$(date +%H:%M:%S)] $*"; }
corre() { echo "--- [$(date +%H:%M:%S)] $*"; "$R" "$@" > /dev/null; }
paralelo() {   # paralelo <script> <n_parts>
  local s="$1" n="$2"
  for K in $(seq 1 "$n"); do ( "$R" "$s" parte "$K" "$n" > /dev/null ) & done
  wait
}

etapa "1/9 validation: retrospective run 2011-2025 and measurement bracket"
corre R/26_validacion_backcast.R
corre R/40_bracket_informante.R
corre R/53_bracket_error_muestral.R
corre R/68_equilibrio_margen_extensivo.R      # checks the extensive-margin correction factor

etapa "2/9 scenario space"                     ; corre R/38_espacio_escenarios.R
corre R/80_descomposicion_numero_fumadores.R   # checks the decomposition identity (no change if R/38 is current)
corre R/79_prevalencia_tercil_edad.R           # prevalence by tertile standardised for age (~20 min)
etapa "3/9 envelope over elasticity gradients (3 h)" ; corre R/45_sensibilidad_elasticidad_t1.R C
etapa "4/9 split by margin"                    ; corre R/48_gradiente_por_margen.R
                                                 corre R/52_razon_prevalencia_tercil.R
etapa "5/9 extensive-margin decomposition and deaths averted" ; corre R/64_margen_extensivo_y_muertes.R

etapa "6/9 sensitivity: alternative calibration point and graded former-smoker risk (9 runs)"
for V in principal punto_alt rr_graduado; do
  for S in $SEMILLAS; do ( "$R" R/69_sensibilidades_calibracion.R "$V" "$S" > /dev/null ) & done
  wait
done
corre R/69_sensibilidades_calibracion.R fit

etapa "7/9 sensitivity: cessation trend and relapse at its lower bound"
for S in $SEMILLAS; do
  for V in principal tope_2030; do corre R/72_tope_cesacion_2030.R "$V" "$S"; done
  for V in principal cota_65 cota_45; do corre R/73_recaida_cota_inferior.R "$V" "$S"; done
done
corre R/72_tope_cesacion_2030.R resumir
corre R/73_recaida_cota_inferior.R resumir
corre R/77_sesgo_denominador_cesacion.R

etapa "8/9 uncertainty and article sensitivity analyses: emulator ($N50 parts), 100 seeds (~13 h) and R/78 (~9 h)"
paralelo R/50_bandas_incertidumbre_mc.R "$N50"
paralelo R/67_semillas_incertidumbre.R "$N"
corre R/67_semillas_incertidumbre.R fusion
# Sensitivity analyses of the article (timing, equal response by tertile, pass-through and
# diversion, roll-your-own, delayed start, graded mortality risk, starts in 2025): resumable.
paralelo R/78_sensibilidades_tc.R "$N"
corre R/78_sensibilidades_tc.R fit
corre R/50_bandas_incertidumbre_mc.R fit         # after R/67: reads its noise floor

etapa "9/9 Level 1"                              ; bash reproducir_figuras_tablas.sh
