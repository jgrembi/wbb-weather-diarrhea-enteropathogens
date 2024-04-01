#!/bin/bash

#SBATCH --job-name=run-manuscript-stats
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=10
#SBATCH --mem=5G
#SBATCH --output=01-run_manuscript_stats_log.out
#SBATCH --time=2:00:00
#SBATCH --partition=relman

module --force purge

module load math
module load devel

# load gcc, a C++ compiler (required for certain packages)
module load gcc/10
# module load system
# module load readline/7.0

# load R version 4.2.0 (required for certain packages)
module load R/4.2.0
#module load readline/7.0

# load software required for spatial analyses in R
ml physics gdal udunits proj geos
# module load system
module load readline/7.0

ml pandoc/2.7.3

#######################################################################
## Run manuscript tables
# These produce all main and supplemental tables for the manuscript
# which will be saved in the directory 6-tables
#######################################################################
cd /oak/stanford/groups/relman/users/jgrembi/wash/wbb-weather-diarrhea-enteropathogens/5-table-scripts/
R CMD BATCH --no-save 1-table_demographics_script.R
R CMD BATCH --no-save S1-table-bacteria-temp-outcomes.R
R CMD BATCH --no-save S2-aic-comparison-temp-ppt-interaction-models.R
R CMD BATCH --no-save S3-table-negative-control-outcomes.R