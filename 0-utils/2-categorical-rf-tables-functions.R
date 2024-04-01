#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Functions to obtain necessary info from model output .RDS files
#######################################
#------------------------------------------------------
## DEFINE TABLE FUNCTION ##
# For use on Sherlock Virtual Machine
# @param outcome_subdir outcome subdirectories (diarrhea-0, diarrhea-1, pathogens-)
# @param dirs_df dataframe of the un-adjusted and the adjusted subdirectories in the gam_outputs folder
# @param risk_factor environmental risk factor 
# @param risk_factor_label label for given risk factor
# @param outcome specific binary outcome variable
# @param outcome_colname column name of specific outcome
#------------------------------------------------------

# --------------------------------------------------
# function to obtain the point estimate and confidence intervals for each 
# results object using the model fit 
# @param i indicates adjusted or unadjusted model
# @param prev_level exposure level that has already been included in table
# --------------------------------------------------
get_mer_CI <- function(adj_unadj, fit, risk_factor, prev_level, data){
  
  group = ifelse(adj_unadj == 1, "Unadj.", "Adj.")
  
  summary_mer = summary(fit$mer)
  rownames <- summary_mer$coefficients %>% rownames()
  X_rf <- paste0("X", risk_factor)

  
  # Initialize other exposure level if prev_level exists
  if (!is.null(prev_level)) {
    rownumber = which(grepl(X_rf, rownames) & !grepl(prev_level, rownames))
    estimate = summary_mer$coefficients[[rownumber]]
    SE <- summary_mer$coefficients[ , 2][[rownumber]]
    exposure_level <- gsub(X_rf, "", rownames[[rownumber]])
  } else {
    # Binary risk factor or first level in 3-level risk factor
    estimate = summary_mer$coefficients[[2]]
    SE <- summary_mer$coefficients[ , 2][[2]]
    exposure_level <- gsub(X_rf, "", rownames[[2]])
  }
  # Dataframe for given exposure level
  PR <- exp(estimate)
  PR_lower <- exp(estimate - qnorm(0.975)*SE)
  PR_upper <- exp(estimate + qnorm(0.975)*SE)
  
  analysis <- case_when(
    (PR < 0.9 & !between(1, PR_lower, PR_upper)) ~ "Protective; significant",
    (PR < 0.9 & between(1, PR_lower, PR_upper)) ~ "Protective; not significant",
    (PR >= 0.9 & PR <= 1.1 & between(1, PR_lower, PR_upper)) ~ "No association", 
    (PR > 1.1 & between(1, PR_lower, PR_upper)) ~ "Increased risk; not significant",
    (PR > 1.1 & !between(1, PR_lower, PR_upper)) ~ "Increased risk; significant",
    (PR_upper == PR_lower) ~ "equal bounds",
    TRUE ~ "narrow CI")
  
  df <- data.frame(
    PR = PR,
    SE = SE,
    PR.Lower = PR_lower,
    PR.Upper = PR_upper,
    RF_Type = exposure_level,
    Group = group,
    Analysis = analysis,
    N = n <- formatC(nrow(data), big.mark=","),
    risk_factor = risk_factor
  )
  return(df)
}



clean_title <- function(title) {
  case_when(
   title == "close water" ~ "Close vs. Far",
    title == "medium distance water" ~ "Medium vs. Far",
    TRUE ~ as.character(title)
  )
}





make_table_row <- function(outcome_subdir, 
                           dirs_df,
                           risk_factor, risk_factor_label,
                           outcome, outcome_colname, outcome_label) {
  
  #------------------------------------------------------
  # Create table row to house pertinent values
  #------------------------------------------------------
  
  table_row <- data.frame(matrix(ncol = 12, nrow = 0))
  
  colnames(table_row) <-  c("Outcome", "RF", "PR.Lower", "PR.Upper",
                            "PR", "RF_Type", "Group", "Analysis", "N", "risk_factor", "filename", "SE")
  
  
  #------------------------------------------------------
  # Cycle through unadjusted then adjusted model fit
  #------------------------------------------------------
  for(i in 1:2) {
    
    if(Sys.getenv("LMOD_SYSHOST")=="sherlock"){
      # Identify which sub-folder to choose based on outcome & adjusted or unadjusted
      len <- nchar(outcome_subdir)
      
      # Splice outcome_subdir at hyphen
      last_dash_index = unlist(regexpr('-', outcome_subdir))
      
      hyphen <- ifelse(len - last_dash_index == 0, "", "-")
      
      # Paste together the subfolder using the first part of outcome_subdir (ex. diarrhea), 
      # the given adjusted/unadjusted model, and specific characteristics of subfolder (ex. 0 or 1)
      subfolder_expr <- paste0(substr(outcome_subdir, 1, last_dash_index), names(dirs_df[i]), hyphen,
                               substr(outcome_subdir, last_dash_index + 1, len))
      
      subfolder_path = paste0(sherlock_results_dir, "gam_outputs/", subfolder_expr)
      file_to_identify = paste0(outcome,"_",risk_factor,".RDS")
      file_path = paste0(subfolder_path, "/", file_to_identify)
      # if (i == 2 && outcome == "gam_bruise7d_0") {
      #     break
      # }
    }
    print(paste0("Filepath: ", file_path))
    x <- readRDS(file_path)
    data = x$model_input_data
    
    #------------------------------------------------------
    # Outcomes with no spatial autocorrelation will use a separate gam fit object
    # Error: land_use_type has no gam fit or spatial gam fit object
    #------------------------------------------------------
    
    if (!any(is.na(x$spatial_gam_fit))) {
      fit <- x$spatial_gam_fit
    } else {
      fit <- x$gam_fit
    }
    
    #------------------------------------------------------
    # Define exposed and unexposed levels
    #------------------------------------------------------
    exposed <- c("heavy rain", "close water", "higher than median proportion", "above median")  # Add all 'exposed' level names across all risk factors here
    exposed2 <- c("medium distance water")
    unexposed <- c("not heavy rain", "far water", "less than or median proportion", "at or below median")
    
    #------------------------------------------------------
    # Acquire PR and 95% CI manually for unadjusted model fit
    #------------------------------------------------------
    if (is.atomic(fit) && is.na(fit)) {
      print(x$message)
      break
    }
    df_1 <- get_mer_CI(adj_unadj = i, 
                       fit = fit, 
                       risk_factor = risk_factor, 
                       prev_level = NULL, data) %>% 
      mutate(Outcome = outcome_label,
             RF = risk_factor_label,
             filename = file_path)
    
    exp_title_uncleaned <- df_1$RF_Type
    df_1$RF_Type <- clean_title(exp_title_uncleaned)
    #------------------------------------------------------
    # Special acquisition for 3-level variables, 2nd exposed level
    #------------------------------------------------------
    if(risk_factor %in% c("distance_from_any_surface_water_tertile",
                          "distance_from_seasonal_surface_water_tertile",     
                          "distance_from_ephemeral_surface_water_tertile")) {
      df_2 <- get_mer_CI(adj_unadj = i, 
                         fit = fit, 
                         risk_factor = risk_factor, 
                         prev_level = exp_title_uncleaned, data) %>% 
        mutate(Outcome = outcome_label,
               RF = risk_factor_label, 
               filename = file_path)
      
      df_2$RF_Type <- clean_title(df_2$RF_Type)    
    }
    
    #------------------------------------------------------
    # Assemble the table row for regular, 2-level categorical variables
    #------------------------------------------------------
    if(!risk_factor %in% c("distance_from_any_surface_water_tertile",
                           "distance_from_seasonal_surface_water_tertile",     
                           "distance_from_ephemeral_surface_water_tertile")) {
      table_row <- rbind(table_row, df_1)
    } else {
      #------------------------------------------------------
      # Assemble the table rows for risk factors with 3 levels
      #------------------------------------------------------
      table_row <- rbind(table_row, df_1)
      table_row <- rbind(table_row, df_2)
    }
  }
  # path_type <- add_pathogen_category(table_row, paste0(outcome_colname, "_"))
  # table_row <- cbind(table_row, path_type)
  
  return(table_row)
}

# # --------------------------------------------------
# # function to obtain the point estimate and confidence intervals for categorical risk factors  
# # results object using the glm model fit (when gamm4 results had tiny SEs) 
# # --------------------------------------------------
# ##############################################
# ##############################################
# # Documentation: get_pr_ci
# # Usage: get_pr_ci(estimate, SE, model_family)
# # Description: Gets the prevalence ratio and 95% CI for categorical risk factors against outcome 
# #             This function was specifically written to get the PR and CI from the output by the glm call used in the fit_clustered_glm function 
# #             which was used when there were issues with the gamm4 model fit having tiny SE due to likely convergence issues
# 
# # Args/Options:
# # estimate:       the coefficient for the appropriate variable from the model fit output by the glm call 
# # SE:             the se for the appropriate variable from the model fit output by the glm call 
# # model_family:   the family used for the model, so estimates and SEs can be appropriately transformed
# #
# # Returns:        A data.frame with the estimate and standard error (SE) formatted as a text object, 
# #                 prevalence ratio (PR), and lower (PR_lower) and upper (PR_upper) bounds of the 95% CI 
# # Output:             none
# 
get_pr_ci <- function(estimate, SE, model_family) {
  if(model_family$link == "log"){
    out <- data.frame(
      estimate_se = paste0(round(estimate, 2), " (", round(SE, 2), ")"),
      SE = SE,
      PR = exp(estimate),
      PR_lower = exp(estimate - qnorm(0.975) * SE),
      PR_upper = exp(estimate + qnorm(0.975) * SE)
    )
  } else if (model_family$link == "identity") {
    out <- data.frame(
      estimate_se = paste0(round(estimate, 2), " (", round(SE, 2), ")"),
      PR = estimate,
      SE = SE,
      PR_lower = estimate - qnorm(0.975) * SE,
      PR_upper = estimate + qnorm(0.975) * SE
    )
  } else if (model_family$link == "logit") {
    out <- data.frame(
      estimate_se = paste0(round(estimate, 2), " (", round(SE, 2), ")"),
      SE = SE,
      PR = exp(estimate)/(1+exp(estimate)),
      PR_lower = exp(estimate - qnorm(0.975) * SE)/(1+exp(estimate - qnorm(0.975) * SE)),
      PR_upper = exp(estimate + qnorm(0.975) * SE)/(1+exp(estimate + qnorm(0.975) * SE))
    )
  }
}



###############################################
# makes a table with the PR and 95% CI for risk_factor1, stratified by the interaction term (risk_factor2),
###############################################
# Documentation:    fit_clustered_glm
# Usage:            fit_clustered_glm()
# Description:      This function calculates the PR & 95% CI for risk_factor1 stratified by risk_factor2 (the interaction term) from 
#                   output for models containing an interaction term and a categorical risk_factor1. 
#                   If there is poor model fit due to data sparsity (insufficient spatial variation) the model is 
#                   rerun as a glm without terms for the spatial component (latitude and longitude).  
# Args/Options: 
#   y:                Character argument that specifies the outcome variable
#   a:                Character argument that specifies the exposure variable
#   coefname:         The name of the third level of a risk factor with more than 2 levels should be provided here (risk factors with >3 levels are not currently handled)
#   data:             The data set to calculate the glm on (typically the model_input_data returned from the fit_gam function which has been screened for sparsity, has missing observations removed, etc) 
#   family:           The family object specifying the distribution and link to use in fitting the gam.  Acceptable responses  
#                       are currently "binomial", "poisson", and "gaussian", although in practice any allowed by family.mgcv could be possible.
#   idvar:            This indicates the variable used for clustering ("dataid" for diar7d and bruise7d outcomes and "clusterid" for pathogen outcomes)
#   interaction_term: Character argument that specifies the interaction term. Default: NULL
#   adjusted:         Boolean whether the model is adjusted or unadjusted. Default: FALSE
#   formula:          Optional argument to provide the formula directly, otherwise formula will be generated as y ~ a. 
#                     If y and a are also provided, they are ignored and this formula is used. This argument is useful for adjusted models.
#                     Default: NULL
#   output:           Optional argument. If set to "raw", will receive the estimate and SE as the output (not transformed to PR and 95% CI). Default: NULL
# Returns:          A data frame which includes the PR and 95% CI. For risk factors that have 3 levels, there will be two observations (one each comparing the 2nd and 3rd levels to the reference group) 
#                   For interaction models, will be stratified by the levels of interaction_term, with one observation per group.
# Output:           none

fit_clustered_glm <- function(y, a, coefname=NULL, data, family, idvar, interaction_term = NULL, adjusted = FALSE, formula = NULL, output = NULL){
  
  if (is.factor(data[[y]]))   data[[y]] <- as.numeric(as.character(data[[y]]))
  
  rf1_levels = levels(data %>% pull({{a}}))
  # rf2_levels = levels(data %>% pull({{interaction_term}}))
  
  if (!is.null(formula)) {
    glm_formula <- formula
  } else if (is.null(interaction_term)) {
    glm_formula <- as.formula(paste0(y, "~", a))
  } else {
    glm_formula <- as.formula(paste0(y, "~", a, " + ", a, "*", interaction_term ))
  }
  
  # fit glm model 
  fit <- glm(glm_formula, family = family, data = data)
  
  # obtain clustered confidence interval 
  data[[idvar]] = droplevels(data[[idvar]])
  vcovCL <- sandwichSE(fm = fit, cluster = data[[idvar]])
  rfit <- coeftest(fit, vcovCL)
  
  # for categorical variables: 
  # if an argument is entered for coefname, 
  # search the model coefficient names in order to
  # find the correct row of results
  if(!is.null(coefname)){
    if(length(grep(coefname, rownames(rfit)))==0){
      print("Coefficient name not found")
    }
    rownumber = grep(coefname, rownames(rfit))
    estimate = rfit[rownumber,1]
    SE <- rfit[rownumber,2]
    
    out = get_pr_ci(estimate = estimate, SE = SE, model_family = family)
    
    # if an argument is NOT entered for coefname, 
    # use the second model coefficient assuming the 
    # risk factor is binary
  } else {
    ###########################################
    #  This is for the regular models
    ###########################################
    if(is.null(interaction_term)) {
      estimate = rfit[2,1]
      SE <- rfit[2,2]
      out = get_pr_ci(estimate = estimate, SE = SE, model_family = family)
      ###########################################
      #  This is for the models with interaction terms
      ###########################################
    } else if (length(rf1_levels) == 2) {  ## all risk factors except distance from surface water, which is in tertiles
      if (output == "raw") {
        out = map(2:4, function(i) {
          term = ifelse(i == 1, "risk_factor", ifelse(i == 2, "interaction_term", "interaction"))
          estimate = rfit[i,1]
          SE = rfit[i,2]
          out = data.frame(estimate = estimate,
                           SE = SE,
                           term = term,
                           p.val = rfit[i,4])
                    
          return(out)
        }) %>%
          list_rbind()
      } else {
        out = map(2:4, function(i) {
          term = ifelse(i == 1, "risk_factor", ifelse(i == 2, "interaction_term", "interaction"))
          if (i != 3) {
            estimate = rfit[i,1]
            SE = rfit[i,2]
            out = get_pr_ci(estimate = estimate, SE = SE, model_family = family) %>%
              mutate(estimate = estimate,
                     term = term,
                     p.val = rfit[i,4])
            return(out)
          } else {
            estimate = sum(rfit[2,1], rfit[i,1])
            SE = sqrt(vcovCL[2,2] + vcovCL[i,i] + 2*vcovCL[2,i])
            out = get_pr_ci(estimate = estimate, SE = SE, model_family = family) %>%
              mutate(estimate = estimate,
                     term = term,
                     p.val = pnorm(-abs(estimate)/SE) * 2)
            return(out)
          }
        }) %>%
          list_rbind()
      }
    } else if (length(rf1_levels) == 3) {
      if (output == "raw") {
        out = map(0:2, function(i) {
          term = ifelse(i == 0, "risk_factor", "interaction")
          if (i == 0) {
            out = data.frame(estimate = rfit[2:3,1],
                             SE = rfit[2:3,2], 
                             term = term,
                             p.val = rfit[2:3,4]) %>%
              rownames_to_column("var") %>%
              select(estimate, SE, term, p.val, var)
            return(out)
          } else {
            out = data.frame(estimate = rfit[i+4,1],
                             SE = rfit[i+4,2], 
                             term = term,
                             p.val = rfit[i+4,4],
                             var = rownames(rfit)[i+4])
            return(out)
          }
        }) %>%
          list_rbind()
      } else {
        out = map(0:2, function(i) {
          term = ifelse(i == 0, "risk_factor", "interaction")
          if (i == 0) {
            out1 = get_pr_ci(estimate = rfit[2,1], SE = rfit[2,2], model_family = family) %>%
              mutate(estimate = rfit[2,1],
                     term = term,
                     p.val = rfit[2,4],
                     var = rownames(rfit)[2])
            out2 = get_pr_ci(estimate = rfit[3,1], SE = rfit[3,2], model_family = family) %>%
              mutate(estimate = rfit[3,1],
                     term = term,
                     p.val = rfit[3,4],
                     var = rownames(rfit)[3])
            return(rbind(out1, out2))
          } else {
            estimate = sum(rfit[i+1,1], rfit[i+4,1])
            SE = sqrt(vcovCL[i+1, i+1] + vcovCL[i+4, i+4] + 2*vcovCL[i+1, i+4])
            out = get_pr_ci(estimate = estimate, SE = SE, model_family = family) %>%
              mutate(estimate = estimate,
                     term = term,
                     p.val = pnorm(-abs(estimate)/SE) * 2,
                     var = rownames(rfit)[i+4])
            return(out)
          }
        }) %>%
          list_rbind()
      }
    }
  }
  out = out %>% mutate(
    Outcome = as.character(y),
    RF = as.character(a),
    Group = case_when(
      !is.null(interaction_term) ~ "Interaction", 
      adjusted == FALSE ~ "Unadj.",
      adjusted == TRUE ~ "Adj.")) 
  
  if (!is.null(coefname)) {
    out = out %>% mutate(RF_Type = coefname) 
  } else {
    out = out %>% mutate(RF_Type = as.character(a)) 
  }
  
  out = out %>% dplyr::select(Outcome, RF_Type, RF, everything()) %>% 
    mutate(Model = case_when(
      !is.null(interaction_term) ~ "Interaction", 
      adjusted == FALSE ~ "Unadjusted",
      adjusted == TRUE ~ "Adjusted"))
  
  print(out)
  return(out)
}


sandwichSE=function (fm, cluster) 
{
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(estfun(fm), 2, function(x) tapply(x, cluster, 
                                                sum))
  vcovCL <- dfc * sandwich(fm, meat = crossprod(uj)/N)
  return(vcovCL)
}


###############################################
# makes a table with the PR and 95% CI for risk_factor1, stratified by the interaction term (risk_factor2),
###############################################
# Documentation:    make_interaction_table
# Usage:            make_interaction_table()
# Description:    This function calculates the PR & 95% CI for risk_factor1 stratified by risk_factor2 (the interaction term) from 
#                 output for models containing an interaction term and a categorical risk_factor1. 
#                 If there is poor model fit due to data sparsity (insufficient spatial variation) the model is 
#                 rerun as a glm without terms for the spatial component (latitude and longitude).  
# Args/Options: 
#   file_path:     complete filepath to directory containing file_name 
#   file_name:     name of file which contains model output
#   outcome:       outcome variable name
#   risk_factor1:  risk factor variable name 
#   risk_factor2:  interaction variable name
# Returns:         a data frame which includes the PR and 95% CI for each level of risk_factor2.
# Output:          none

make_interaction_table <- function(file_path, file_name, outcome, risk_factor1, risk_factor2){
  print(file_name)
  #------------------------------------------------------
  # load results
  #------------------------------------------------------
  x = readRDS(paste0(file_path, file_name))
  
  #------------------------------------------------------
  # Outcomes with no spatial autocorrelation will use a separate gam fit object
  #------------------------------------------------------
  # check that any model was fit
  if(!all(is.na(x$spatial_gam_fit), is.na(x$gam_fit))) {
    # print(paste0("Model was fit in ", file_name))
    
    if (!all(is.na(x$spatial_gam_fit))) {
      fit <- x$spatial_gam_fit
    } else {
      fit <- x$gam_fit
    }
    
    # save coefficients from mer
    coefs <- as.data.frame(summary(fit$mer)$coefficients) 
    coefs$name = rownames(coefs)
    # get variance-covariane matrix from mer
    coefs_var <- vcov(fit$mer)
    
    
    ## If the model failed to converge, indicated by very tiny SEs, we will rerun as GLM
    if (coefs[1,2] < 0.02 && coefs[2,2] < 0.02) {
      
      model_family = x$family
      
      df = x$model_input_data
      # the glm can not handle the spline terms (used here for lat, long) so we grab the non-spatial formula which excludes those
      glm_formula = x$gam_fit$gam$formula
      fit_glm = glm(glm_formula, family = model_family, data = df)
      
      coefs <- as.data.frame(summary(fit_glm)$coefficients) 
      coefs$name = rownames(coefs)
      
      idvar = ifelse(as.character(x$y) %in% c("diar7d","bruise7d"), "dataid", "clusterid")
      
      # obtain clustered CIs using robust SEs, obtained with sandwichSE()
      df[[idvar]] = droplevels(df[[idvar]])
      coefs_var <- sandwichSE(fm = fit_glm, cluster = df[[idvar]])
      
    }
    
    # get risk factor 2 levels 
    rf2_levels = levels(x$model_input_data[[risk_factor2]])
    
    # get PRs and CIs stratified by risk factor 2
    # stratified_PR_list = matrix(NA, length(rf2_levels), 5)
    stratified_PR_list = list()
    
    for(j in 1:length(rf2_levels)){
      
      coef_names_rf1 = coefs$name[grep(risk_factor1, coefs$name)]
      coef_names_ref = coef_names_rf1[-grep(":", coef_names_rf1)]
      # reference level of risk factor 2
      if(j == 1){
        PR_log = coefs[which(coefs$name %in% coef_names_ref),"Estimate"]
        SE = coefs[which(coefs$name %in% coef_names_ref),"Std. Error"]
        pval = coefs[which(coefs$name %in% coef_names_ref),"Pr(>|z|)"]
        ## Jess added this term_name variable for the case where we have more than 2 levels for rf1 
        term_name = gsub(risk_factor1, "", gsub("X", "", coef_names_ref))
        out <- data.frame(
          risk_factor1 = risk_factor1,
          risk_factor2 = risk_factor2,
          rf2_stratum = paste0(rf2_levels[j]),
          PR_log = PR_log,
          SE = SE, 
          p.val = pval,
          term_name = term_name)
        # other levels of risk factor 
      } else {
        coef_names = coefs$name[grep(paste0(risk_factor2, rf2_levels[j]), coefs$name)]
        coef_names_rf2 = coef_names[grep(risk_factor1, coef_names)]
        
        out = NULL
        for (l in 1:length(coef_names_rf2)) {
          # get prevalence ratios for risk factor 1 within stratum of risk factor 2
          # assumes that level 1 of the risk factor 1 is the reference
          PR_log = sum(coefs[which(coefs$name %in% c(coef_names_ref[l],coef_names_rf2[l])),"Estimate"])
          
          # get SE for risk factor 1 within stratum of risk factor 2
          vcov_indices = which(rownames(coefs_var) %in% c(coef_names_ref[l],coef_names_rf2[l]))
          ind1 = vcov_indices[1]
          ind2 = vcov_indices[2]
          
          SE = sqrt(coefs_var[ind1, ind1]+
                      coefs_var[ind2, ind2]+
                      2*coefs_var[ind1, ind2])
          
          p.val = pnorm(-abs(PR_log)/SE) * 2
          term_name = gsub(risk_factor1, "", gsub("X", "", coef_names_rf2[l]))
          
          res <- data.frame(
            risk_factor1 = risk_factor1,
            risk_factor2 = risk_factor2,
            rf2_stratum = paste0(rf2_levels[j]),
            PR_log = PR_log,
            SE = SE, 
            p.val = pval,
            term_name = term_name)
          
          out = bind_rows(out, res)
        }  
      }
      
      out = out %>%
        mutate(Outcome = outcome,
               PR = exp(PR_log),
               lb = exp(PR_log - qnorm(0.975)*SE),
               ub = exp(PR_log + qnorm(0.975)*SE))
      
      stratified_PR_list[[j]] = out
      
    }
    
    stratified_PRs = bind_rows(stratified_PR_list)
    
    return(stratified_PRs)
    
  }else{
    print("No model was fit")
    return(NULL)
  }
}
