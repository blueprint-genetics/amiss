#!/bin/bash

set -e

cd /amiss
Rscript $1/R/01_parse_vcf.R
Rscript $1/R/02_preprocess_data.R
Rscript $1/R/03_descriptive_stats.R
Rscript $1/R/04_run_impute_and_train.R
Rscript $1/R/05_run_prediction.R
Rscript $1/R/06_generate_simulated_data.R
Rscript $1/R/07_run_simulations.R
Rscript $1/R/08_analyze_results.R
Rscript $1/R/09_analyze_simulation_results.R

