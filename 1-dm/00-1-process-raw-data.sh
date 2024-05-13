#!/bin/bash

#SBATCH --job-name=run-fit-models
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=10
#SBATCH --mem=5G
#SBATCH --output=01-run_process_raw_data_log.out
#SBATCH --time=00:00:60
#SBATCH --partition=relman

# module --force purge
# 
# module load math
# module load devel
# # load gcc, a C++ compiler (required for certain packages)
# module load gcc/10
# 
# # load R version 4.2.0 (required for certain packages)
# module load R/4.2.0
# 
# # load software required for spatial analyses in R
# ml physics gdal udunits proj geos cmake
# module load readline/7.0
# 
# ml pandoc/2.7.3

# cd $OAK/users/jgrembi/wash/wbb-weather-diarrhea-enteropathogens


#######################################################################
## Clean diarrhea and pathogen datasets
#######################################################################
R CMD BATCH --no-save 1-dm/1-clean-diarr-data.R 1-dm/1-clean-diarr-data.Rout
R CMD BATCH --no-save 1-dm/2-clean-pathogen-data.R 1-dm/2-clean-pathogen-data.Rout

#######################################################################
## Prepare temperature dataset
#######################################################################
R CMD BATCH --no-save 1-dm/3a-extract-daily-temp-data.R 1-dm/3a-extract-daily-temp-data.Rout
R CMD BATCH --no-save 1-dm/3b-calculate-temp-cutoffs.R 1-dm/3b-calculate-temp-cutoffs.Rout
R CMD BATCH --no-save 1-dm/3c-construct-temp-variables.R 1-dm/3c-construct-temp-variables.Rout

#######################################################################
## Prepare precipitation dataset
#######################################################################
python 1-dm/4a-subset-raw-ppt-data.py >> 1-dm/4a-subset-raw-ppt-data.txt
## The next 2 scripts are to empirically define the rainy season
python 1-dm/4b-calculate-ppt-daily-averages.py >> 1-dm/4b-calculate-ppt-daily-averages.txt
R CMD BATCH --no-save 1-dm/4c-define-rainy-season.R 1-dm/4c-define-rainy-season.Rout

R CMD BATCH --no-save 1-dm/4d-extract-daily-ppt-data.R 1-dm/4d-extract-daily-ppt-data.Rout
R CMD BATCH --no-save 1-dm/4e-calculate-ppt-cutoffs.R 1-dm/4e-calculate-ppt-cutoffs.Rout
R CMD BATCH --no-save 1-dm/4f-construct-ppt-variables.R 1-dm/4f-construct-ppt-variables.Rout

#######################################################################
## Prepare surface water dataset
#######################################################################
R CMD BATCH --no-save 1-dm/5-import-surface-water-data.R 1-dm/5-import-surface-water-data.Rout

#######################################################################
## Prepare humidity/vpd dataset
#######################################################################
R CMD BATCH --no-save 1-dm/6-extract-raster-terraclimate-vpd.R 1-dm/6-extract-raster-terraclimate-vpd.Rout

#######################################################################
## Make final combined dataset
#######################################################################
R CMD BATCH --no-save 1-dm/7-merge-wbb-data.R 1-dm/7-merge-wbb-data.Rout


