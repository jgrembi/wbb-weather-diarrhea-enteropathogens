#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Script containing functions used in other scripts - mostly for plotting
#######################################



##############################################
##############################################
# Documentation: prep_gam_plot
# Usage: prep_gam_plot_temp(results_directory, yname, risk_factor, scale)
# Description: Prepare gam continuous risk factor output for plotting with
# x-axis as risk factor and y-axis as prevalence. Run on local with Box mounted. 

# Args/Options:
# results_directory:  directory path containing the gam model outputs that will be plotted, as a string
# risk_factor:        continuous risk factor being plotted, as a string
# yname:              binary outcome being plotted, as a string
# scale:              scale for the model fitting

# Returns: a png saved to a directory given by figures_dir
# Output: none
prep_gam_plot <- function(results_directory, yname, risk_factor, scale=100){
  
  # load results 
  file_name <- paste0(results_directory, "gam_", yname, risk_factor, ".RDS")
  print(file.info(file_name)$ctime)
  print(paste0("Opening: ", file_name))
  x = readRDS(file_name)
  data = x$model_input_data
  risk_factor = x$a
  w_included = x$w_included

  #------------------------------------------------------
  ## Outcomes with no spatial autocorrelation will use a separate gam fit object
  #------------------------------------------------------
  if (all(!is.na(x$spatial_gam_fit))) {
    gamfit = x$spatial_gam_fit$gam
  } else {
    gamfit = x$gam_fit$gam
  }
  
 
  if (!is.null(w_included)) {
    w_included = w_included[!grepl("qgps", w_included)]
    print(w_included)
    for (w in w_included) {
      w = sym(w)
      if (is.numeric(data %>% pull({{w}}))) {
        median = median(data %>% pull({{w}}))
        data = data %>%
          mutate(!!w := median)
      } else if (is.factor(data %>% pull({{w}}))) {
        if (w == "wsh") {
          data = data %>% 
            mutate(!!w := "WSH")
        } else if (w == "nutrition") {
          data = data %>% 
            mutate(!!w := "Nutrition")
        } else if (w == "HHwealth_quart") {
          data = data %>%
            mutate(!!w := "Wealth Q1")
        } else {
          print(w)
          level_1 = levels(data %>% pull({{w}}))[1]
          data = data %>%
            mutate(!!w := level_1)
        }
      } else if (grepl("ppt_30daysum_", w)) {
        data = data %>%
          mutate(!!w := "at or below median")
      } else if (w == "sex") {
        data = data %>%
          mutate(!!w := "male")
      }
    }
  }
  
  terms_included = attr(terms(x$formula), "term.labels")
  if (any(grepl("qgpslong", terms_included))) {
    exclude_terms = "s(qgpslong,qgpslat)"
  } else {
    exclude_terms = NULL
  }

  #------------------------------------------------------
  ## Obtain simultaneous CIs
  #------------------------------------------------------
  fitp_ci <- gamCI(m=gamfit,newdata=data,exclude_terms = exclude_terms, nreps=10000) #"s(qgpslong,qgpslat)"
  
  #------------------------------------------------------
  ## Obtain model family
  #------------------------------------------------------
  link <- gamfit$family[[2]]
  
  #------------------------------------------------------
  ## Calculate y-value transformations and y-axis labels based on model family
  #------------------------------------------------------
  if(link == "logit") {
    fitp_ci <- fitp_ci %>% 
      mutate(fit = exp(fit)/(1+exp(fit)) * scale,
             lwrS = exp(lwrS)/(1+exp(lwrS)) * scale,
             uprS = exp(uprS)/(1+exp(uprS)) * scale)
    ylab_name <- "Prevalence (%)"
  }
  
  if(link == "log") {
    fitp_ci <- fitp_ci %>% 
      mutate(fit = exp(fit) * scale,
             lwrS = exp(lwrS) * scale,
             uprS = exp(uprS) * scale)
    ylab_name <- "Prevalence (%)"
  }
  
  #------------------------------------------------------
  ## Add means values back so centered risk factors to put back on their original scale
  #------------------------------------------------------
  
  rf_mean = paste0(clean_offset_data_dir, "risk_factor_mean_values.RDS") %>% 
    readRDS() %>%
    filter(variable == gsub("_C", "", risk_factor)) %>%
    pull(mean_value)
  fitp_ci <- fitp_ci %>% 
    mutate(!!sym(risk_factor) := !!sym(risk_factor) + rf_mean) #%>%

  fitp_ci <- fitp_ci %>% mutate(risk_factor = risk_factor) %>% 
    dplyr::select(risk_factor, everything()) %>% 
    rename(risk_factor_value = !!sym(risk_factor))
  
  return(fitp_ci)
}

##############################################
##############################################
# Documentation: clean_pathogen_names
# Usage: clean_pathogen_names(pathogen_variable)
# Description: Recode pathogen variable names for plotting

# Args/Options:
# pathogen_variable:  pathogen_variable, as a character vector
# Returns:            recoded pathogen variable for plotting
# Output:             none

clean_pathogen_names <- function(pathogen_variable){
  newvar = str_replace(pathogen_variable, "pos_","")
  newvar = str_replace(newvar, "_","")
  newvar = str_replace(newvar, "_","")
  
  newvar = ifelse(newvar=="Adenovirus4041", "Adenovirus 40/41", newvar)
  newvar = ifelse(newvar=="Bfragilis", "B. fragilis", newvar)
  newvar = ifelse(newvar=="Cdifficile", "C. difficile", newvar)
  newvar = ifelse(newvar=="Ebieneusi", "E. bieneusi", newvar)
  newvar = ifelse(newvar=="LTETEC", "LT-ETEC", newvar)
  newvar = ifelse(newvar=="ShigellaEIEC", "Shigella/EIEC", newvar)
  newvar = ifelse(newvar=="STETEC", "ST-ETEC", newvar)
  newvar = ifelse(newvar=="parasite", "Any parasite", newvar)
  newvar = ifelse(newvar=="virus", "Any virus", newvar)
  newvar = ifelse(newvar=="ETECany", "ETEC", newvar)
  newvar = ifelse(newvar=="EPECany", "EPEC", newvar)
  return(newvar)
}

##############################################
##############################################
# Documentation: add_pathogen_category
# Usage: add_pathogen_category(data, pathogen_varname)
# Description: Add type of pathogen variable to data frame 

# Args/Options:
# data:               data frame to add variable to 
# pathogen_variable:  pathogen variable name, as a string 
# Returns:            data frame with new pathogen category variable 
# Output:             none

add_pathogen_category <- function(data, pathogen_varname){
  data = data %>% mutate(pathogen_category = case_when(
    !!sym(pathogen_varname) == "pos_Adenovirus40_41_" ~ "Virus",
    !!sym(pathogen_varname) == "pos_aEPEC_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_Aeromonas_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_B_fragilis_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_C_difficile_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_Campylobacter_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_Cryptosporidium_" ~ "Parasite",
    !!sym(pathogen_varname) == "pos_E_bieneusi_" ~ "Parasite",
    !!sym(pathogen_varname) == "pos_EAEC_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_ETEC_any_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_EPEC_any_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_Giardia_" ~ "Parasite",
    !!sym(pathogen_varname) == "pos_LT_ETEC_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_Norovirus_" ~ "Virus",
    !!sym(pathogen_varname) == "pos_parasite_" ~ "Parasite",
    !!sym(pathogen_varname) == "pos_Plesiomonas_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_Sapovirus_" ~ "Virus",
    !!sym(pathogen_varname) == "pos_Shigella_EIEC_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_ST_ETEC_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_STEC_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_tEPEC_" ~ "Bacteria",
    !!sym(pathogen_varname) == "pos_virus_" ~ "Virus"
  ))
  return(data)
}


# # --------------------------------------------------
# # function to obtain the point estimate and confidence intervals for each 
# # results object using the model fit 
# # --------------------------------------------------
# ##############################################
# ##############################################
# # Documentation: get_mer_CI
# # Usage: get_mer_CI(fit)
# # Description: Gets the prevalence ratio and 95% CI for categorical risk factors against outcome 
# 
# # Args/Options:
# # fit:               the model fit output by by the gamm4 call used in the fit_gam function for this project 
# # Returns:           a data.frame with the prevalence ratio (PR), standard error (SE) and lower (lb) and upper (ub) bounds of the 95% CI 
# # Output:             none
# 
get_mer_CI <- function(fit){
  summary_mer = summary(fit$mer)

  estimate = summary_mer$coefficients[[2]]
  SE <- summary_mer$coefficients[ , 2][[2]]
  PR <- exp(estimate)
  PR_lower <- exp(estimate - qnorm(0.975)*SE)
  PR_upper <- exp(estimate + qnorm(0.975)*SE)
  risk_factor_model <-

  data.frame(
    PR = PR,
    SE = SE,
    lb = PR_lower,
    ub = PR_upper
  )
}

