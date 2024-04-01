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
## Run manuscript stats
# These are files whose interactive output (on the command line of R/RStudio)
# provide the statistics reported in the results text of the manuscript
#######################################################################
cd /oak/stanford/groups/relman/users/jgrembi/wash/wbb-weather-diarrhea-enteropathogens/2b-analysis/

R CMD BATCH --no-save 4-manuscript-stats-diarrhea-prev.R 4-manuscript-stats-diarrhea-prev.Rout
R CMD BATCH --no-save 5-manuscript-stats-temperature.R 5-manuscript-stats-temperature.Rout
R CMD BATCH --no-save 6-manuscript-stats-precipitation.R 6-manuscript-stats-precipitation.Rout
R CMD BATCH --no-save 7-manuscript-stats-surface-water.R 7-manuscript-stats-surface-water.Rout
R CMD BATCH --no-save 8-manuscript-stats-humidity-vpd.R 8-manuscript-stats-humidity-vpd.Rout
