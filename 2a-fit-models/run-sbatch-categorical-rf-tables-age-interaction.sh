#!/bin/bash

#SBATCH --job-name=run-rf-tables-age-interaction
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --output=1-run_categorical_rf_tables_age_interaction_log.out
#SBATCH --time=24:00:00
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

## move into the server repo directory
  cd /oak/stanford/groups/relman/users/jgrembi/wash/wbb-weather-diarrhea-enteropathogens/2a-fit-models/
else
## move into the local repo directory
  cd /Users/JGrembi/Dropbox/08_JadeCollaboration/wbb-weather-diarrhea-enteropathogens/2a-fit-models/
fi

# cd 2-analysis/

R CMD BATCH --no-save 2-categorical-rf-tables-age-interaction-diarrhea.R