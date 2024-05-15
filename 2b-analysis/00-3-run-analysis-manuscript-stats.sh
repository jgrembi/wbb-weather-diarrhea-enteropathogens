#!/bin/bash

#SBATCH --job-name=run-manuscript-stats
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=10
#SBATCH --mem=5G
#SBATCH --output=01-run_manuscript_stats_log.out
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
 
  cd /oak/stanford/groups/relman/users/jgrembi/wash/wbb-weather-diarrhea-enteropathogens/2b-analysis/
else
  cd /Users/JGrembi/Dropbox/08_JadeCollaboration/wbb-weather-diarrhea-enteropathogens/2b-analysis/
fi

#######################################################################
## Make categorical risk factors table of Prevalence Ratios - for all results
#######################################################################
# # This makes the table for diarrhea and pathogen results
# R CMD BATCH --no-save 1-categorical-rf-tables.R 1-categorical-rf-tables.Rout
# 
# ## This makes a separate table for age effect modification for diarrhea
# ## (not run for pathogens because of the narrow age range)
# R CMD BATCH --no-save 2-categorical-rf-tables-age-interaction-diarrhea.R 2-categorical-rf-tables-age-interaction-diarrhea.Rout
# 
# ## This cleans up a few results that had unrealistically small standard errors,
# ## which was caused by data sparsity in the gps coordinates when including the spatial term.
# R CMD BATCH --no-save 3-correct-CI-tables-nointeraction-diarrhea-pathogens.R 3-correct-CI-tables-nointeraction-diarrhea-pathogens.Routsbatch run-sbatch-categorical-rf-tables.sh
# 
# 
# #######################################################################
# ## Run manuscript stats
# # These are files whose interactive output (on the command line of R/RStudio)
# # provide the statistics reported in the results text of the manuscript
# #######################################################################
# R CMD BATCH --no-save 4-manuscript-stats-diarrhea-prev.R 4-manuscript-stats-diarrhea-prev.Rout
# R CMD BATCH --no-save 5-manuscript-stats-temperature.R 5-manuscript-stats-temperature.Rout
R CMD BATCH --no-save 6-manuscript-stats-precipitation.R 6-manuscript-stats-precipitation.Rout
R CMD BATCH --no-save 7-manuscript-stats-surface-water.R 7-manuscript-stats-surface-water.Rout
R CMD BATCH --no-save 8-manuscript-stats-humidity-vpd.R 8-manuscript-stats-humidity-vpd.Rout
