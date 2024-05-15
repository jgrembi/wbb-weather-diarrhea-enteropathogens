#!/bin/bash

#SBATCH --job-name=run-project
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=10
#SBATCH --mem=5G
#SBATCH --output=00-run_project_log.out
#SBATCH --time=00:00:60
#SBATCH --partition=relman




#######################################################################
## Run pre-processing scripts 
## These make all clean data.frames for analysis, including :
## outcome data from the trial, 
## pre-processing of hydrometeorological data,
## and generating exposure variables from hydrometeorological data.
#######################################################################
sbatch --wait 1-dm/00-1-process-raw-data.sh

#######################################################################
## Run model fits
#######################################################################
wait
sbatch --wait 2a-fit-models/00-2-run-fit-models.sh

#######################################################################
## Run manuscript statistics from results section
#######################################################################
wait
sbatch --wait 2b-analysis/00-3-run-analysis-manuscript-stats.sh

#######################################################################
## Run manuscript figures
#######################################################################
wait
sbatch --wait 3-figure-scripts/00-4-run-figures.sh

#######################################################################
## Run manuscript tables
#######################################################################
wait
sbatch 5-figure-scripts/00-5-run-tables.sh