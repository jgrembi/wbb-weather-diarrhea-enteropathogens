#!/bin/bash

#SBATCH --job-name=run-pathogens-unadjusted
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --output=00-run_inc_log.out
#SBATCH --time=15:00:00
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

cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis/0g-pathogens-unadjusted/

R CMD BATCH --no-save pathogens-unadjusted.R 
