#!/bin/bash

#SBATCH --job-name=run-figures
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=10
#SBATCH --mem=5G
#SBATCH --output=01-run_figures_log.out
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
  cd /oak/stanford/groups/relman/users/jgrembi/wash/wbb-weather-diarrhea-enteropathogens/3-figure-scripts/
else
  cd /Users/JGrembi/Dropbox/08_JadeCollaboration/wbb-weather-diarrhea-enteropathogens/3-figure-scripts/
fi

#######################################################################
## Run manuscript figures
# These produce all main and supplemental figures for the manuscript
# which will be saved in the directory 4-figures
#######################################################################
# cd /oak/stanford/groups/relman/users/jgrembi/wash/wbb-weather-diarrhea-enteropathogens/3-figure-scripts/
R CMD BATCH --no-save 1-map-diar-prev.R 1-map-diar-prev.Rout 

##Note the interim files associated with these scripts contain PII and are therefore not included in the repo.  
## These scripts are also not able to be run given the public dataset because these datasets do not contain the required variables (e.g. latitude and longitude)
R CMD BATCH --no-save 2-riskfactor-biweekly-figure/1-biweekly_dates.R 2-riskfactor-biweekly-figure/1-biweekly_dates.Rout
R CMD BATCH --no-save 2-riskfactor-biweekly-figure/2a-process-temp-alldays.R 2-riskfactor-biweekly-figure/2a-process-temp-alldays.Rout
R CMD BATCH --no-save 2-riskfactor-biweekly-figure/2b-process-ppt-alldays.R 2-riskfactor-biweekly-figure/2b-process-ppt-alldays.Rout
R CMD BATCH --no-save 2-riskfactor-biweekly-figure/2c-process-surface-water-alldays.R 2-riskfactor-biweekly-figure/2c-process-surface-water-alldays.Rout
R CMD BATCH --no-save 2-riskfactor-biweekly-figure/2d-process-vpd-alldays.R 2-riskfactor-biweekly-figure/2d-process-vpd-alldays.Rout
R CMD BATCH --no-save 2-riskfactor-biweekly-figure/3-fig-diarrhea-risk-factors-over-time-control.R 2-riskfactor-biweekly-figure/3-fig-diarrhea-risk-factors-over-time-control.Rout

R CMD BATCH --no-save 3-plot-gam-temperature-maintext.R 3-plot-gam-temperature-maintext.Rout
R CMD BATCH --no-save 4-plot-risk-factor-prev-ratios-rain.R 4-plot-risk-factor-prev-ratios-rain.Rout
R CMD BATCH --no-save S1-plot-env-correlations.R S1-plot-env-correlations.Rout
R CMD BATCH --no-save S2-S3-S4-S5-plot-gam-temperature-diar-path-unadj.R S2-S3-S4-S5-plot-gam-temperature-diar-path-unadj.Rout
R CMD BATCH --no-save S6-plot-risk-factor-prev-ratios-rain-75-90-percentiles.R S6-plot-risk-factor-prev-ratios-rain-75-90-percentiles.Rout
R CMD BATCH --no-save S7-plot-risk-factor-prev-ratios-rain-heavy.R S7-plot-risk-factor-prev-ratios-rain-heavy.Rout
R CMD BATCH --no-save S8-plot-distance-to-surface-water-adjusted.R S8-plot-distance-to-surface-water-adjusted.Rout
R CMD BATCH --no-save S9-plot-proportion-household-adjusted.R S9-plot-proportion-household-adjusted.Rout
R CMD BATCH --no-save S10-plot-gam-vpd.R S10-plot-gam-vpd.Rout
R CMD BATCH --no-save S11-diarrhea-risk-factors-age-interaction.R S11-diarrhea-risk-factors-age-interaction.Rout
R CMD BATCH --no-save S12-plot-gam-negc-temp.R S12-plot-gam-negc-temp.Rout
