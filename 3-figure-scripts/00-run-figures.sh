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

# load software required for spatial analyses in R
ml physics gdal udunits proj geos
# module load system
module load readline/7.0

ml pandoc/2.7.3


#######################################################################
## Run manuscript figures
# These produce all main and supplemental figures for the manuscript
# which will be saved in the directory 4-figures
#######################################################################
cd /oak/stanford/groups/relman/users/jgrembi/wash/wbb-weather-diarrhea-enteropathogens/3-figure-scripts/
R CMD BATCH --no-save 1-map-diar-prev.R

cd /oak/stanford/groups/relman/users/jgrembi/wash/wbb-weather-diarrhea-enteropathogens/3-figure-scripts/2-riskfactor-biweekly-figure/
R CMD BATCH --no-save 1-biweekly_dates.R
R CMD BATCH --no-save 2a-process-temp-alldays.R
R CMD BATCH --no-save 2b-process-ppt-alldays.R
R CMD BATCH --no-save 2c-process-surface-water-alldays.R
R CMD BATCH --no-save 2d-process-vpd-alldays.R
R CMD BATCH --no-save 3-fig-diarrhea-risk-factors-over-time-control.R

cd /oak/stanford/groups/relman/users/jgrembi/wash/wbb-weather-diarrhea-enteropathogens/3-figure-scripts/
R CMD BATCH --no-save 3-plot-gam-temperature-maintext.R
R CMD BATCH --no-save 4-plot-risk-factor-prev-ratios-rain.R
R CMD BATCH --no-save S1-plot-env-correlations.R
R CMD BATCH --no-save S2-S3-S4-S5-plot-gam-temperature-diar-path-unadj.R
R CMD BATCH --no-save S6-plot-risk-factor-prev-ratios-rain-75-90-percentiles.R
R CMD BATCH --no-save S7-plot-risk-factor-prev-ratios-rain-heavy.R
R CMD BATCH --no-save S8-plot-distance-to-surface-water-adjusted.R
R CMD BATCH --no-save S9-plot-proportion-household-adjusted.R
R CMD BATCH --no-save S10-plot-gam-vpd.R
R CMD BATCH --no-save S11-diarrhea-risk-factors-age-interaction.R
R CMD BATCH --no-save S12-plot-gam-negc-temp.R
