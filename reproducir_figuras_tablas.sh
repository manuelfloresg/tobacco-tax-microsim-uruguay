#!/usr/bin/env bash
# Level 1: regenerates every figure and table of the article (English) and of the working paper
# (Spanish) from output/tables/. Does not run the model. A few minutes.
#
#   bash reproducir_figuras_tablas.sh
#   RSCRIPT=/path/to/Rscript bash reproducir_figuras_tablas.sh
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
R="${RSCRIPT:-Rscript}"
corre() { echo "--- $*"; "$R" "$@" > /dev/null; }

corre R/55_figuras_articulo.R           # paper/TC/figures/
corre R/55_figuras_articulo.R es        # informe/figuras/
corre R/61_diagrama_estados.R
corre R/61_diagrama_estados.R es
corre R/56_tablas_articulo.R            # paper/TC/tables/
corre R/56_tablas_articulo.R es         # informe/tablas/
corre R/60_parametros_modelo.R
corre R/60_parametros_modelo.R es
corre R/57_tablas_suplemento.R          # paper/TC/supp/tables/ (LaTeX); reads the outputs of R/56 and R/60
echo "Done. Figures in paper/TC/figures and informe/figuras; tables in paper/TC/tables, paper/TC/supp/tables and informe/tablas."
