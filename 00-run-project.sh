#!/bin/bash

#SBATCH --job-name=run-fit-models
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=10
#SBATCH --mem=5G
#SBATCH --output=00-run_fit_models_log.out
#SBATCH --time=00:00:60
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
ml physics gdal udunits proj geos cmake
# module load system
module load readline/7.0

ml pandoc/2.7.3

#######################################################################
## Install packages needed
#######################################################################
R CMD BATCH --no-save 00-install.packages.R 0-install.packages.Rout



#######################################################################
## Run pre-processing scripts to make all clean data.frames for analysis
#######################################################################

#######################################################################
## Run model fits
#######################################################################


#######################################################################
## Run manuscript statistics from results section
#######################################################################
sbatch 2b-analysis/00-run-analysis-manuscript-stats.sh

#######################################################################
## Run manuscript figures
#######################################################################
