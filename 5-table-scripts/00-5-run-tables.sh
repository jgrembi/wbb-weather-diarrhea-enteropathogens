#!/bin/bash

#SBATCH --job-name=run-tables
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=10
#SBATCH --mem=5G
#SBATCH --output=00-5-run_tables_log.out
#SBATCH --time=2:00:00
#SBATCH --partition=relman

if [ "$LMOD_SYSHOST" == "sherlock" ]
then 
  module --force purge
  
  module load math
  module load devel
  
  # load gcc, a C++ compiler (required for certain packages)
  module load gcc
  # module load system
  # module load readline/7.0
  
  # load R version 4.2.0 (required for certain packages)
  module load R/4.2.0
  #module load readline/7.0
  
  # load software required for spatial analyses in R
  ml physics gdal/2.2.1 udunits proj/4.9.3 geos cmake fribidi python sqlite/3.18.0 netcdf/4.4.1.1 hdf5/1.10.0p1
  # module load system
  module load readline/7.0
  
  ml pandoc/2.7.3
  
  ml physics gdal/3.5.2 udunits proj/9.1.0 geos/3.11.0 cmake fribidi python sqlite/3.44.2 netcdf/4.8.1 readline/7.0
  #######################################################################
  ## Run manuscript stats
  # These are files whose interactive output (on the command line of R/RStudio)
  # provide the statistics reported in the results text of the manuscript
  #######################################################################
  cd /oak/stanford/groups/relman/users/jgrembi/wash/wbb-weather-diarrhea-enteropathogens/5-table-scripts/
else
  cd /Users/JGrembi/Dropbox/08_JadeCollaboration/wbb-weather-diarrhea-enteropathogens/5-table-scripts/
fi


#######################################################################
## Run manuscript tables
# These produce all main and supplemental tables for the manuscript
# which will be saved in the directory 6-tables
#######################################################################
R CMD BATCH --no-save 1-table_demographics_script.R
R CMD BATCH --no-save S1-table-bacteria-temp-outcomes.R
R CMD BATCH --no-save S2-aic-comparison-temp-ppt-interaction-models.R
R CMD BATCH --no-save S3-table-negative-control-outcomes.R