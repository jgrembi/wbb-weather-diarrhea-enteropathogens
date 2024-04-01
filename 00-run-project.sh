#!/bin/bash

#SBATCH --job-name=run-fit-models
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=10
#SBATCH --mem=5G
#SBATCH --output=00-run_fit_models_log.out
#SBATCH --time=00:00:60
#SBATCH --partition=relman



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
