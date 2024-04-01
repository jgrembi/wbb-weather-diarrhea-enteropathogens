#!/bin/bash

#SBATCH --job-name=run-diarrhea-interaction-ppt-adj-0-1
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=24
#SBATCH --mem=64G
#SBATCH --output=00-run_inc_log.out
#SBATCH --time=40:00:00
#SBATCH --partition=relman

module purge

module load math
module load devel

# load gcc, a C++ compiler (required for certain packages)
module load gcc/10

# load R version 4.0.2 (required for certain packages)
module load R/4.0.2


# load software required for spatial analyses in R
ml physics gdal udunits proj geos
ml system

ml pandoc/2.7.3

cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis/0e-diarrhea-adjusted-interaction-ppt-temp-0/

R CMD BATCH --no-save diarrhea-interaction-ppt-adj-0-1.R 
