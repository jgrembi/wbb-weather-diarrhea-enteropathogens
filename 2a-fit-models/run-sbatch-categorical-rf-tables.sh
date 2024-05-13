#!/bin/bash

#SBATCH --job-name=run-rf-tables
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --output=1-run_categorical_rf_tables_log.out
#SBATCH --time=24:00:00
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

#cd 2a-fit-models/

R CMD BATCH --no-save 1-categorical-rf-tables.R