#!/bin/bash

#SBATCH --job-name=run-fit-models
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=10
#SBATCH --mem=5G
#SBATCH --output=00-run_fit_models_log.out
#SBATCH --time=00:00:20
#SBATCH --partition=relman

if [ "$LMOD_SYSHOST" == "sherlock" ]
then 
 ## move into the server repo directory
  cd /oak/stanford/groups/relman/users/jgrembi/wash/wbb-weather-diarrhea-enteropathogens/2a-fit-models/
else
## move into the local repo directory
  cd /Users/JGrembi/Dropbox/08_JadeCollaboration/wbb-weather-diarrhea-enteropathogens/2a-fit-models/
fi


######################################################################
# Fit diarrhea models - for diarrhea cohort, control arm only
######################################################################
# run diarrhea-unadjusted-0
cd /0a-diarrhea-unadjusted-0
sbatch run-diarrhea-unadjusted-0-1-sbatch.sh
sbatch run-diarrhea-unadjusted-0-2-sbatch.sh
sbatch run-diarrhea-unadjusted-0-3-sbatch.sh

## run diarrhea-adjusted-0
cd ../0b-diarrhea-adjusted-0
sbatch run-diarrhea-adjusted-0-1-sbatch.sh
sbatch run-diarrhea-adjusted-0-2-sbatch.sh
sbatch run-diarrhea-adjusted-0-3-sbatch.sh

## run diarrhea-unadjusted-0-negc
cd ../0c-diarrhea-unadjusted-0-negc
sbatch run-diarrhea-unadjusted-0-negc-1-sbatch.sh
sbatch run-diarrhea-unadjusted-0-negc-2-sbatch.sh

## run diarrhea-adjusted-0-negc
cd ../0d-diarrhea-adjusted-0-negc
sbatch run-diarrhea-adjusted-0-negc-1-sbatch.sh
sbatch run-diarrhea-adjusted-0-negc-2-sbatch.sh

## run diarrhea-adjusted-interaction-age-0
cd ../0e-diarrhea-adjusted-interaction-age-0
sbatch run-diarrhea-interaction-age-adj-0-1-sbatch.sh
sbatch run-diarrhea-interaction-age-adj-0-2-sbatch.sh

# run diarrhea-adjusted-interaction-ppt-temp-0
cd ../0f-diarrhea-adjusted-interaction-ppt-temp-0
sbatch run-diarrhea-interaction-ppt-adj-0-1-sbatch.sh

## run diarrhea-adjusted-nointeraction-ppt-temp-0
cd ../0g-diarrhea-adjusted-nointeraction-ppt-temp-0
sbatch run-diarrhea-nointeraction-ppt-temp-adj-0-1-sbatch.sh

#######################################################################
## Fit pathogen models - for pathogen cohort only
#######################################################################
## run pathogens-unadjusted
cd ../0h-pathogens-unadjusted
sbatch run-pathogens-unadjusted-sbatch.sh

## run pathogens-adjusted
cd ../0i-pathogens-adjusted
sbatch run-pathogens-adjusted-sbatch-1.sh
sbatch run-pathogens-adjusted-sbatch-2.sh
sbatch run-pathogens-adjusted-sbatch-3.sh
sbatch run-pathogens-adjusted-sbatch-4.sh

## run pathogens-adjusted-interaction-ppt-temp
cd ../0j-pathogens-adjusted-interaction-ppt-temp
sbatch run-pathogens-adjusted-interaction-ppt-sbatch-1.sh
sbatch run-pathogens-adjusted-interaction-ppt-sbatch-2.sh
sbatch run-pathogens-adjusted-interaction-ppt-sbatch-3.sh
sbatch run-pathogens-adjusted-interaction-ppt-sbatch-4.sh

## run pathogens-adjusted-nointeraction-ppt-temp
cd ../0k-pathogens-adjusted-nointeraction-ppt-temp
sbatch run-pathogens-adjusted-nointeraction-ppt-temp-sbatch-1.sh
sbatch run-pathogens-adjusted-nointeraction-ppt-temp-sbatch-2.sh
sbatch run-pathogens-adjusted-nointeraction-ppt-temp-sbatch-3.sh
sbatch run-pathogens-adjusted-nointeraction-ppt-temp-sbatch-4.sh

