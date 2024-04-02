#!/bin/bash

#SBATCH --job-name=run-fit-models
#SBATCH --begin=now
#SBATCH --mail-type=ALL
#SBATCH --cpus-per-task=10
#SBATCH --mem=5G
#SBATCH --output=00-run_fit_models_log.out
#SBATCH --time=00:00:20
#SBATCH --partition=relman


#######################################################################
## Fit diarrhea models - for diarrhea cohort, control arm only
#######################################################################
## run diarrhea-unadjusted-0 
# cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis/0a-diarrhea-unadjusted-0
sbatch 0a-diarrhea-unadjusted-0/run-diarrhea-unadjusted-0-1-sbatch.sh            
sbatch 0a-diarrhea-unadjusted-0/run-diarrhea-unadjusted-0-2-sbatch.sh       
sbatch 0a-diarrhea-unadjusted-0/run-diarrhea-unadjusted-0-3-sbatch.sh  

## run diarrhea-adjusted-0 
# cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis/0b-diarrhea-adjusted-0
sbatch 0b-diarrhea-adjusted-0/run-diarrhea-adjusted-0-1-sbatch.sh            
sbatch 0b-diarrhea-adjusted-0/run-diarrhea-adjusted-0-2-sbatch.sh       
sbatch 0b-diarrhea-adjusted-0/run-diarrhea-adjusted-0-3-sbatch.sh            

## run diarrhea-unadjusted-0-negc 
# cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis/0c-diarrhea-unadjusted-0-negc
sbatch 0c-diarrhea-unadjusted-0-negc/run-diarrhea-unadjusted-0-negc-1-sbatch.sh            
sbatch 0c-diarrhea-unadjusted-0-negc/run-diarrhea-unadjusted-0-negc-2-sbatch.sh            

## run diarrhea-adjusted-0-negc 
# cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis/0c-diarrhea-adjusted-0-negc
sbatch 0c-diarrhea-adjusted-0-negc/run-diarrhea-adjusted-0-negc-1-sbatch.sh            
sbatch 0c-diarrhea-adjusted-0-negc/run-diarrhea-adjusted-0-negc-2-sbatch.sh            

## run diarrhea-adjusted-interaction-age-0 
# cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis/0d-diarrhea-adjusted-interaction-age-0
sbatch 0d-diarrhea-adjusted-interaction-age-0/run-diarrhea-interaction-age-adj-0-1-sbatch.sh          
sbatch 0d-diarrhea-adjusted-interaction-age-0/run-diarrhea-interaction-age-adj-0-2-sbatch.sh    

# run diarrhea-adjusted-interaction-ppt-temp-0 
# cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis/0e-diarrhea-adjusted-interaction-ppt-temp-0
sbatch 0e-diarrhea-adjusted-interaction-ppt-temp-0/run-diarrhea-interaction-ppt-adj-0-1-sbatch.sh

## run diarrhea-adjusted-nointeraction-ppt-temp-0 
cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis/0f-diarrhea-adjusted-nointeraction-ppt-temp-0 
sbatch 0f-diarrhea-adjusted-nointeraction-ppt-temp-0/run-diarrhea-nointeraction-ppt-temp-adj-0-1-sbatch.sh

#######################################################################
## Fit pathogen models - for pathogen cohort only
#######################################################################
## run pathogens-unadjusted 
cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis/0g-pathogens-unadjusted 
sbatch run-pathogens-unadjusted-sbatch.sh    

## run pathogens-adjusted 
cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis/0h-pathogens-adjusted
sbatch run-pathogens-adjusted-sbatch-1.sh    
sbatch run-pathogens-adjusted-sbatch-2.sh    
sbatch run-pathogens-adjusted-sbatch-3.sh    
sbatch run-pathogens-adjusted-sbatch-4.sh    

## run pathogens-adjusted-interaction-ppt-temp
cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis/0i-pathogens-adjusted-interaction-ppt-temp
sbatch run-pathogens-adjusted-interaction-ppt-sbatch-1.sh 
sbatch run-pathogens-adjusted-interaction-ppt-sbatch-2.sh 
sbatch run-pathogens-adjusted-interaction-ppt-sbatch-3.sh 
sbatch run-pathogens-adjusted-interaction-ppt-sbatch-4.sh 

## run pathogens-adjusted-nointeraction-ppt-temp
cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis/0j-pathogens-adjusted-nointeraction-ppt-temp
sbatch run-pathogens-adjusted-nointeraction-ppt-temp-sbatch-1.sh 
sbatch run-pathogens-adjusted-nointeraction-ppt-temp-sbatch-2.sh 
sbatch run-pathogens-adjusted-nointeraction-ppt-temp-sbatch-3.sh 
sbatch run-pathogens-adjusted-nointeraction-ppt-temp-sbatch-4.sh 

#######################################################################
## Make categorical risk factors table of Prevalence Ratios - for all results
#######################################################################
cd /oak/stanford/groups/relman/users/jgrembi/wash/WBB-weather-diarrhea-pathogens/2-analysis
## This makes the table for diarrhea and pathogen results
sbatch run-sbatch-categorical-rf-tables.sh 
## This makes a separate table for age effect modification for diarrhea 
## (not run for pathogens because of the narrow age range)
sbatch run-sbatch-categorical-rf-tables-age-interaction.sh 
## This cleans up a few results that had unrealistically small standard errors, 
## qhich was caused by data sparsity in the gps coordinates when including the spatial term
sbatch run-sbatch-correct-CI-tables-nointeraction.sh