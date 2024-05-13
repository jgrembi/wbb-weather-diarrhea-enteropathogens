#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Base functions for analysis
#######################################


###############################################
# Make asset-based PCA wealth index
###############################################
# Usage: 
# Description: Takes in a list of assets and:
#   1) replaces blank characters with NA & creates a factor level for missing data
#   2) drops rows (observations/subjects) with no asset data
#   3) drops assets with great missingness or almost no variance 
#   4) computes a PCA and takes loadings from 1st component as wealth index
#   5) computes quartiles of wealth index (HHwealth_quart)
#
# Args/Options: 
#   dfull: dataframe with all assets to be included
#   varlist: character vector of variable names from dfull to indicate which assets to include
#   reorder: T/F to indicate if wealth quartiles should be re-ordered 
# 
# Returns: A data.frame with dataid, union, and HHwealth_quart
# Output: none

assetPCA<-function(dfull, varlist, reorder=F){
  
  varlist<-c("dataid", "union", varlist)
  
  #Subset to only needed variables for subgroup analysis
  ret <- dfull %>%
    subset(select = c(varlist)) 
  
  
  #Select assets
  ret<-as.data.frame(ret) 
  id<-subset(ret, select=c("dataid", "union")) #drop subjectid
  ret<-ret[,which(!(colnames(ret) %in% c("dataid", "union")))]
  
  #Replace character blank with NA
  for(i in 1:ncol(ret)){
    ret[,i]<-ifelse(ret[,i]=="",NA,ret[,i])
  } 
  
  #drop rows with no asset data
  id<-id[rowSums(is.na(ret[,3:ncol(ret)])) != ncol(ret)-2,]  
  ret<-ret[rowSums(is.na(ret[,3:ncol(ret)])) != ncol(ret)-2,]  
  
  
  #Drop assets with great missingness
  for(i in 1:ncol(ret)){
    cat(colnames(ret)[i],"\n")
    print(table(is.na(ret[,i])))
    print(class((ret[,i])))
  }
  
  #create level for for missing factor levels
  table(is.na(ret))
  for(i in 1:ncol(ret)){
    ret[,i]<-as.character(ret[,i])
    ret[is.na(ret[,i]),i]<-"miss"
    ret[,i]<-as.factor(ret[,i])
    
  }
  table(is.na(ret))
  
  #Convert factors into indicators
  ret<-droplevels(ret)
  ret<-design_matrix(ret)
  
  
  #Remove columns with almost no variance
  if(length(nearZeroVar(ret))>0){
    ret<-ret[,-nearZeroVar(ret)]
  }
  
  ## Convert the data into matrix ##
  ret<-as.matrix(ret)
  
  ##Computing the principal component using eigenvalue decomposition ##
  princ.return <- princomp(ret) 
  
  ## To get the first principal component in a variable ##
  load <- loadings(princ.return)[,1]   
  
  pr.cp <- ret %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form. 
  
  HHwealth <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.
  
  #Create 4-level household wealth index
  quartiles<-quantile(HHwealth, probs=seq(0, 1, 0.25))
  print(quartiles)
  ret<-as.data.frame(ret)
  ret$HHwealth_quart<-rep(1, nrow(ret))
  ret$HHwealth_quart[HHwealth>=quartiles[2]]<-2
  ret$HHwealth_quart[HHwealth>=quartiles[3]]<-3
  ret$HHwealth_quart[HHwealth>=quartiles[4]]<-4
  table(ret$HHwealth_quart)
  ret$HHwealth_quart<-factor(ret$HHwealth_quart)
  
  if(reorder==T){
    levels(ret$HHwealth_quart)<-c("Wealth Q4","Wealth Q3","Wealth Q2","Wealth Q1")
    ret$HHwealth_quart<-factor(ret$HHwealth_quart, levels=c("Wealth Q1", "Wealth Q2","Wealth Q3","Wealth Q4"))
  }else{
    levels(ret$HHwealth_quart)<-c("Wealth Q1", "Wealth Q2","Wealth Q3","Wealth Q4")
  }
  
  #Table assets by pca quartile to identify wealth/poverty levels
  d<-data.frame(id, ret)
  wealth.tab <- d %>% subset(., select=-c(dataid, union)) %>%
    group_by(HHwealth_quart) %>%
    summarise_all(list(mean = mean)) %>% as.data.frame()
  print(wealth.tab)
  
  #Save just the wealth data
  pca.wealth<-d %>% subset(select=c(dataid, union, HHwealth_quart))
  
  # pca.wealth$dataid<-as.character(pca.wealth$dataid)
  
  d <-dfull %>% subset(., select=c("dataid","union"))
  # d$dataid<-as.numeric(d$dataid)
  d<-left_join(d, pca.wealth, by=c("dataid","union"))
  return(d)
}


###############################################
# sparsity check
###############################################
# Check whether the number of cases per variable is sufficient to perform analysis
# y:                    outcome variable name, as numeric or factor
# a:                    risk factor variable name, as numeric or factor
# w:                    covariate variable name(s), as a data frame
# y_per_variable:       Default 10. Number of cases per variable required to perform analysis 

# returns T if number of cases per variable is sufficient to perform analysis 
# returns F if not 

check_sparsity = function(y, a, w, y_per_variable = 10){
  
  assert_that(length(y) == length(a))
  assert_that(length(y) == nrow(w))
  assert_that(length(a) == nrow(w))
  
  df = data.frame(y = y, a = a)
  
  df = bind_cols(df, w)
  
  # subset to complete cases
  df_complete = df[complete.cases(df),]
  
  # subset to categorical factor variables 
  df_numeric = df_complete %>% dplyr::select_if(is.factor)
  
  # count number of observations within each stratum
  dfN = df_numeric %>% group_by_all() %>% count()
  
  # check whether cases per variable meets required number
  check = ifelse(min(dfN$n) >= y_per_variable, T, F) 
  
  return(check)
}


###############################################
# check for residual spatial autocorrelation using Moran's coefficients
###############################################
# Usage:        check_autocorr(gam_fit$gam, lldata = d[,c("qgpslong", "qgpslat")], nbc = 8)
# Description:  checks for residual spatial autocorrelation using Moran's coefficients
# Args/Options: 
#   fit:    The gam model fit, from which residuals will be extracted
#   lldata: A 2-column data.frame of spatial coordinates
#   nbc:    Default 10, number of bins passed to nbclass of the pgirmess::correlog function

# Returns: A list with two elements 1) a logical value if there is residual spatial autocorrelation and 
#           2) the correlogram
# Output: A statement whether residual spatial autocorrelation was detected and whether non-spatial model 
#           is saved or proceeding with spatial model.

check_autocorr <- function(fit, lldata, nbc=10){
  # Compute correlogram of the residuals
  lldata = lldata %>% 
    mutate(res = residuals(fit)) %>% 
    filter(!is.na(res)) %>%
    select(-res)
  
  cor_r <- correlog(coords=lldata,
                    z=residuals(fit)[!is.na(residuals(fit))],
                    method="Moran", nbclass=nbc) #na.action = na.omit
  
  correlograms <- as.data.frame(cor_r)

  # define boolean for whether any spatial autocorrelation is present 
  spatial_autocorr <- any(correlograms$p.value <= 0.05)
  
  if(spatial_autocorr) print("Residual spatial autocorrelation is present. Proceeding with spatial model.")
  if(!spatial_autocorr) print("No residual spatial autocorrelation. Saving non-spatial model.")
  
  return(list(spatial_autocorr = spatial_autocorr,
              correlograms = correlograms))
}


###############################################
# get list of adjustment covariates based on risk factor and outcome
###############################################
# Documentation:    get_covariate_list
# Usage:            get_covariate_list(outcome_cat = "diarrhea", risk_factor = "vpd_C")
# Description:      Will determine the list of covariates and confounders to include in adjusted analyses 
#                     for the supplied risk factor.  These were defined in the analysis plan.
# Args/Options: 
#   outcome:      The pathogen outcome being evaluated 
#   risk_factor:  The meteorological or environmental risk factor being evaluated
# Returns: A list of adjustment covariates and confounders.  These will be pre-screened with a Likelihood ratio test
# Output: A statement whether residual spatial autocorrelation was detected and whether non-spatial model 
#           is saved or proceeding with spatial model.

get_covariate_list = function(outcome_cat = c("pathogens","diarrhea"), risk_factor) {

  cov_list = c("sex", "aged_C", "HHwealth_quart") #"any_holiday", "season" 
  
  if (outcome_cat == "pathogens") {
    cov_list = c(cov_list, "wsh", "nutrition", "abx_7d")
  }
  
  if (risk_factor == "vpd_C") {
    if (outcome_cat == "pathogens") {
      cov_list = c(cov_list, "ppt_30daysum_0weeklag")
    } else {
      cov_list = c(cov_list, "ppt_30daysum_1weeklag")
    }
  } else if (risk_factor == "flow_accum_median") {
    cov_list = c(cov_list, "ncow_c_C", "ngoat_c_C", "nchicken_c_C", ifelse(outcome_cat == "pathogens", "ppt_30daysum_0weeklag", "ppt_30daysum_1weeklag"))
  } else if (risk_factor %in% c("distance_from_any_surface_water_C", "distance_from_seasonal_surface_water_C", "distance_from_ephemeral_surface_water_C")) {
    cov_list = c(cov_list, "flow_accum_median", "ncow_c_C", "ngoat_c_C", "nchicken_c_C") 
  } else if (grepl("prop_detected", risk_factor) | grepl("max_months_water", risk_factor)) {
    cov_list = c(cov_list, ifelse(outcome_cat == "pathogens", "ppt_30daysum_0weeklag", "ppt_30daysum_1weeklag"))
  } else if (risk_factor == "land_use_type") {
    cov_list = c(cov_list, "momeduy_C", "dadeduy_C")
  } else if (risk_factor == "evi_C") {
    cov_list = c(cov_list, "momeduy_C", "dadeduy_C", "ncow_c_C", "ngoat_c_C", "nchicken_c_C", "vpd_C", "flow_accum_median") 
  } else if (risk_factor == "pop_density_C") {
    cov_list = c(cov_list, "momeduy_C", "dadeduy_C", "Nhh_median")
  }
  
  return(cov_list)
}


###############################################
# check for collinearity between risk factor and covariate
###############################################
# Documentation:    check_collinearity
# Usage:            check_collinearity(risk_factor = "avgtemp_7_days", covariate = "aged", df = d_diarrhea)
# Description:    Depending on whether the risk_factor and covariate are continuous or categorical, will
#                 evaluate collinearity with a Pearson correlation, ANOVA, or chi-squared test. 
# Args/Options: 
#   risk_factor:  The meterological or environmental risk factor being evaluated
#   covariate:    The covariate being screened for collinearity
#   df:           The data.frame in which collinearity will be evaluated
# Returns: A list of correlation result and a plot of the association between risk_factor and covariate.
# Output: 

check_collinearity <- function(risk_factor, covariate, df) {

  if (is.numeric(df[,risk_factor]) & is.numeric(df[,covariate])) {
    res = cor.test(df[,risk_factor], df[,covariate], method = "pearson", alternative = "two.sided")
    plot = ggplot(df, aes_string(x = covariate, y = risk_factor)) + 
      geom_point(shape = 1, alpha = 0.5)
    out = list(res = res, 
               plot = plot) 
  } else if (is.numeric(df[,risk_factor]) & !is.numeric(df[,covariate])) {
    fm <- as.formula(paste0(risk_factor, "~", covariate))
    res = aov(fm, df)
    plot = ggplot(df, aes_string(x = covariate, y = risk_factor)) + 
      geom_boxplot(outlier.shape = NA) + 
      geom_jitter(shape = 1, width = 0.25, alpha = 0.5)
    out = list(res = summary(res), 
               plot = plot)
  } else if (is.numeric(df[,covariate]) & !is.numeric(df[,risk_factor])) {
    fm <- as.formula(paste0(covariate, "~", risk_factor))
    res = aov(fm, df)    
    plot = ggplot(df, aes_string(x = risk_factor, y = covariate)) + 
      geom_boxplot(outlier.shape = NA) + 
      geom_jitter(shape = 1, width = 0.25, alpha = 0.5)
    out = list(res = summary(res), 
               plot = plot)
  } else if (!is.numeric(df[,risk_factor]) & !is.numeric(df[,covariate])) {
    res = chisq.test(df[,risk_factor], df[,covariate])
    plot = NA
    out = list(res = res, 
               plot = plot)
  }
  return(out)
}


###############################################
# a modified version of the washb_prescreen function to ensure output is an ordered list,
# provided in order of increasing p-value with the smallest first and the largest last.   
###############################################
# Documentation:    washb_prescreen_mod
# Usage:            washb_prescreen_mod()
# Description:    This function modifies the wasb_prescreen function to return the list of covariates in
#                 order of increasing p-value from the likelihood ratio test. This is needed when sparsity 
#                 is a problem, so that covariates can be systematically dropped based on those with
#                 the least strong association with the outcome.  
# Args/Options: 
#   Y:      Outcome variable (continuous, such as LAZ, or binary, such as diarrhea)
#   Ws:     data frame that includes candidate adjustment covariates to screen
#   family: GLM model family (gaussian, binomial, poisson, or negative binomial). Use "neg.binom" 
#             for Negative binomial.
#   pval:   The p-value threshold: any variables with a p-value from the lielihood ratio test 
#             below this threshold will be returned. Defaults to 0.2
#   print:  Logical for whether to print function output, defaults to TRUE.
# Returns: A list which includes covariates that passed the LR test ranked in order of increasing p-value.
# Output: 
#   1) Result of likelihood ratio test for pre-screening covariates
#   2) List of covariates passing LR screening, in ranked order of increasing p-value

washb_prescreen_mod = function (Y, Ws, family = c("gaussian", "binomial", "neg.binom", "poisson"), pval = 0.2, print = TRUE) {

  require(lmtest)
  if (family[[1]] == "neg.binom") {
    require(MASS)
  }
  if (pval > 1 | pval < 0) {
    stop("P-value threshold not set between 0 and 1.")
  }
  Ws <- as.data.frame(Ws)
  dat <- data.frame(Ws, Y)
  dat <- dat[complete.cases(dat), ]
  nW <- ncol(Ws)
  LRp <- matrix(rep(NA, nW), nrow = nW, ncol = 1)
  rownames(LRp) <- names(Ws)
  colnames(LRp) <- "P-value"
  if (family[[1]] != "neg.binom") {
    for (i in 1:nW) {
       # i = 3
      dat$W <- dat[, i]
      if (class(dat$W) == "factor" & dim(table(dat$W)) ==
          1) {
        fit1 <- fit0 <- glm(Y ~ 1, data = dat, family = family)
      }
      else {
        fit1 <- glm(Y ~ W, data = dat, family = family)
        fit0 <- glm(Y ~ 1, data = dat, family = family)
      }
      LRp[i] <- lrtest(fit1, fit0)[2, 5]
    }
  }
  else {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("Pkg needed for this function to work. Please install it.",
           call. = FALSE)
    }
    else {
      for (i in 1:nW) {
        dat$W <- dat[, i]
        if (class(dat$W) == "factor" & dim(table(dat$W)) ==
            1) {
          fit1 <- fit0 <- glm(Y ~ 1, data = dat, family = family)
        }
        else {
          fit1 <- glm.nb(Y ~ W, data = dat, family = family)
          fit0 <- glm.nb(Y ~ 1, data = dat, family = family)
        }
        LRp[i] <- lrtest(fit1, fit0)[2, 5]
      }
    }
  }
  p20 <- ifelse(LRp < pval, 1, 0)
  if (print == TRUE) {
    cat("\nLikelihood Ratio Test P-values:\n")
    print(round(LRp, 5))
    if (sum(p20) > 0) {
      LRps <- matrix(LRp[p20 == 1, ], ncol = 1)
      rownames(LRps) <- names(Ws)[p20 == 1]
      colnames(LRps) <- "P-value"
      cat(paste("\n\nCovariates selected (P<", pval, "):\n",
                sep = ""))
      print(LRps)
    }
    else {
      cat(paste("\nNo covariates were associated with the outcome at P<",
                pval))
    }
  }
  out = data.frame(LRp) %>%
    rownames_to_column("covariate") %>%
    filter(P.value < pval) %>%
    arrange(P.value)
  # return(names(Ws)[p20 == 1])
  return(out$covariate)
}

###############################################
# fit_gam function
###############################################
# Documentation:    fit_gam
# Usage:            fit_gam(df = d_sth, 
#                           y = as.character(sth_tbl_row[["outcome"]]), 
#                           a = as.character(sth_tbl_row[["risk_factor"]]), 
#                           w = c("sex", "aged"), 
#                           family = "binomial")
# Description:      Function that 1) checks there are sufficient observations for analysis, 
#                                 2) screens covariates for association with the outcome, 
#                                 3) subsets the data to include complete observations,
#                                 4) fits a GAM ignoring spatial variation,
#                                 5) verifies that the model converged and throws an error if not,
#                                 6) checks for residual spatial autocorrelation,
#                                 7) fits a spatial GAM if residual spatial autocorrelation is present,
#                                 8) verifies the spatial GAM converged and throws and error if not.
#
# Args/Options: 
#   df:               Data.frame containing all variables needed for analysis
#   y:                Character argument that specifies the outcome variable
#   a:                Character argument that specifies the exposure variable
#   w:                Default NULL. A list or single character vector that specifies the names of covariates or other 
#                       variables to adjust for in the gam function. This list will be screened with a likelihood ratio 
#                       test for association with the outcome before being included in the model.
#   y_per_variable:   Defaut 10. Number of cases per variable required to perform analysis, passed to check_sparsity function.
#   random_intercept: Character argument that specifies the variable to be used as a random intercept. Default: "dataid" 
#                       (for all main cohort analyses - diarrhea, sth, giardia) and "clusterid" for EE cohort diaarrhea and pathogen analysis.
#   family:           The family object specifying the distribution and link to use in fitting the gam.  Acceptable responses  
#                       are currently "binomial", "poisson", and "gaussian", although in practice any allowed by family.mgcv could be possible.
# 
# Returns: A list with the following elements:
#   gam_fit:                          gam model fit object 
#   spatial_gam_fit:                  spatial gam model fit if spatial autocorrelation was detected, else NA
#   spatial_autocorr:                 True/False if spatial autocorrelation was detected in the gam_fit
#   spatial_autocorr_coeff:           the correlogram of the Moran's coefficients from the spatial autocorrelation check on the gam_fit model
#   residual_spatial_autocorr:		    True/False if spatial autocorrelation was detected still in the spatial_gam_fit 
#   residual_spatial_autocorr_coeff:  the correlogram of the Moran's coefficients from the spatial autocorrelation check on the spatial_gam_fit model
#   y:                                the outcome variable specified
#   a:                                the exposure variable specified
#   w_included:                       the list of covariates included in the model (i.e. those that passed screening with the likelihood ratio test)
#   formula:                          the model formula used in the gam.
#   family:                           the family specified by the user indicating the distribution used to fit the gam. 
#   model_input_data:                 the data.frame used for model fitting (with observations dropped that had missing variables)
#   drop_outcome:				              the number of observations dropped due to missing outcome
#   drop_predictors:				          the number of observations dropped due to missing predictors
#   message:                          One of 5 messages which indicate the outcome of the attempted gam model fit. Possible messages include:
#                                       -"Adjusted model not run because no covariates were associated with the outcome at P< 0.1" [no model was fit]
#                                       -"Adjusted/interaction model could not be run due to data sparsity" [no model was fit]
#                                       -"Non-spatial GAM failed to converge" [no model was fit]
#                                       -"Non-spatial gam fit successfully, no spatial autocorrelation detected so no spatial gam fit" [a non-spatial model was fit]
#                                       -"Spatial gam fit successfully" [both a non-spatial and a spatial model were fit]
#
# Output: 
#   1) Result of likelihood ratio test for pre-screening covariates 
#   2) Error message if sparsity check fails
#   3) Error message if gam fails to converge 
#   4) Message reporting if any residual spatial autocorrelation remains
#   5) If residual spatial autocorrelation remains, output of gam model fit including spatial autocorrelation
#   6) Error message if spatial gam fails to converge 
#   7) Message reporting if spatial GAM has larger AIC than non-spatial GAM
#
fit_gam = function(df, y, a, w = NULL, y_per_variable = 10, random_intercept = c("dataid", "clusterid"), family = c("binomial", "gaussian", "poisson")) {
#family = c(binomial(link = "logit"),gaussian(link = "identity"), gaussian, poisson(link = "log"), binomial(link = "log")
  #---------------------------------------------
  # define new variable for message and set to null (this will trigger function to exit at certain spots when no longer null)
  #---------------------------------------------# 
  message = NULL
  #---------------------------------------------
  # check that the family is one of the allowed responses (no typos, no Poisson, etc)
  #---------------------------------------------
  random_intercept <- match.arg(random_intercept)
  family <- match.arg(family)
  y = sym(y)
  #---------------------------------------------
  # drop rows with missing outcome
  #---------------------------------------------
  d = df %>% filter(!is.na({{y}}))
  drop_outcome = dim(df)[1] - dim(d)[1]
  message(paste0("Observations lost due to missing outcome: ", drop_outcome))
  
  #---------------------------------------------
  # covariate screening with likelihood ratio test, keeping p<=0.1
  #---------------------------------------------
  if (!is.null(w)) {
    if (family[1] == "poisson" & length(unique(d %>% pull({{y}}))) == 2) {
      prescreen_family = "binomial"  ## Although we are using poisson to model Prevalence Ratio, we care about the relationship between 0 v 1
    } else {
      prescreen_family = family
    }
    keep_w = washb_prescreen_mod(d %>% pull(y), Ws = d %>% select(all_of(w)), family = prescreen_family, pval = 0.1, print = T)
    if (length(keep_w) == 0) message = "Adjusted model not run because no covariates were associated with the outcome at P< 0.1"
  } else {
    keep_w = NULL
  }
  
  #---------------------------------------------
  # check there are sufficient observations for analysis
  #---------------------------------------------
  sparsity_check = try(validate_that(check_sparsity(y = d %>% pull(y),
                                                  a = d[, a],
                                                  w = d %>% select(all_of(keep_w)),
                                                  y_per_variable = y_per_variable),
                                   msg = "Data sparsity check failed. Choose new covariate set."))
  
  if (sparsity_check != TRUE & "season" %in% keep_w) {
    keep_w = keep_w[!keep_w %in% "season"] 
    sparsity_check = try(validate_that(check_sparsity(y = d %>% pull(y),
                                                    a = d[, a],
                                                    w = data.frame(d[,all_of(keep_w)]),
                                                    y_per_variable = y_per_variable),
                                     msg = "Data sparsity check failed. Choose new covariate set."))
  }
  while (sparsity_check != TRUE & length(keep_w) > 1) {
    keep_w = head(keep_w, -1)
    sparsity_check = try(validate_that(check_sparsity(y = d %>% pull(y),
                                                    a = d[, a],
                                                    w = data.frame(d[,all_of(keep_w)]),
                                                    y_per_variable = y_per_variable),
                                     msg = "Data sparsity check failed. Choose new covariate set."))
  }
  if (sparsity_check != TRUE & length(keep_w) == 1) {
    message = "Adjusted/interaction model could not be run due to data sparsity"
  }
  
  if (!is.null(message)) {
    return(list(gam_fit = NA,
                spatial_gam_fit = NA,
                spatial_autocorr = NA,
                spatial_autocorr_coeff = NA,
                residual_spatial_autocorr = NA,
                residual_spatial_autocorr_coeff = NA,
                y = y, 
                a = a, 
                w_included = keep_w,
                formula = NA,
                family = family,
                model_input_data = d, 
                drop_outcome = drop_outcome, 
                drop_predictors = NA,
                message = message))
  } 
  #---------------------------------------------
  # build formula
  #---------------------------------------------
  if (is.numeric(d[,a])) {
    formula_start = paste(y, paste0("s(", a, ", bs=\"cs\")"), sep = " ~ ")
  } else {
    formula_start = paste(y, a, sep = " ~ ")
  }
  keep_w_terms = NULL
  if (!is.null(keep_w)) {
    ## function to write s() for numeric variables
    keep_w_terms = map_chr(keep_w, function(x) {
      ifelse(is.numeric(d[,x]), paste0("s(", x, ", bs=\"cs\")"), x)
    })
  } 
  if (!is.null(keep_w_terms)) {
    formula = as.formula(paste(formula_start,
                               paste(keep_w_terms, collapse = " + "),
                               # "s(clusterid, bs = \"re\", by = dummy)",
                               sep = " + "))
    
    formula_spatial =  as.formula(paste(formula_start,
                                        paste(keep_w_terms, collapse = " + "),    
                                        # thin plate smoother for lat & long
                                        "s(qgpslong, qgpslat, bs = \"tp\")",
                                        # "s(clusterid, bs = \"re\", by = dummy)",
                                        sep = " + "))
  } else {
    formula = as.formula(formula_start)
    
    formula_spatial = as.formula(paste(formula_start,
                                       # thin plate smoother for lat & long
                                       "s(qgpslong, qgpslat, bs = \"tp\")", 
                                       # "s(clusterid, bs = \"re\", by = dummy)",
                                       sep = " + "))
  }
  
  
  #---------------------------------------------
  # drop rows with missing risk factor or covariates
  #---------------------------------------------
  d = d %>% 
    select(all_of(y), all_of(a), all_of(keep_w), qgpslat, qgpslong, all_of(random_intercept)) %>%
    na.omit() 
  
  drop_predictors = dim(df)[1] - drop_outcome - dim(d)[1]
  message(paste0("Observations lost due to missing predictor or covariates: ", drop_predictors))
  
  random = as.formula(paste0("~(1|", random_intercept, ")"))
  
  
  #---------------------------------------------
  # fit GAM ignoring spatial variation
  #---------------------------------------------
  gam_fit = try(gamm4(formula, 
                      family = family,
                      data = d,
                      REML = T,
                      na.action = na.exclude, 
                      random = random
  ))

  #---------------------------------------------
  # confirm the model converged
  #---------------------------------------------
  print(paste0("Non-spatial gam fit: ", y, " ~ ", a))
  if(length(gam_fit) > 1 & !class(gam_fit) %in% c("try-error", "try-warning", "try-message")){
    print(summary(gam_fit$gam))
  } else {
    print(gam_fit)
    message = "Non-spatial GAM failed to converge"
    return(list(gam_fit = NA,
                spatial_gam_fit = NA,
                spatial_autocorr = NA,
                spatial_autocorr_coeff = NA,
                residual_spatial_autocorr = NA,
                residual_spatial_autocorr_coeff = NA,
                y = y, 
                a = a, 
                w_included = keep_w,
                formula = formula,
                family = family,
                model_input_data = d, 
                drop_outcome = drop_outcome, 
                drop_predictors = drop_predictors,
                message = message))
  }
  #---------------------------------------------
  # check for residual spatial autocorrelation
  #---------------------------------------------
  check_spatial_autocorr <- check_autocorr(gam_fit$gam, lldata = d[,c("qgpslong", "qgpslat")], nbc = 8)
  
  #---------------------------------------------
  # if residual spatial autocorrelation is present,
  # proceed with spatial GAM
  #---------------------------------------------
  if(!check_spatial_autocorr$spatial_autocorr){
    return(list(gam_fit = gam_fit,
                spatial_gam_fit = NA,
                spatial_autocorr = check_spatial_autocorr$spatial_autocorr,
                spatial_autocorr_coeff = check_spatial_autocorr$correlograms,
                residual_spatial_autocorr = NA,
                residual_spatial_autocorr_coeff = NA,
                y = y, 
                a = a, 
                w_included = keep_w,
                formula = formula,
                family = family,
                model_input_data = d, 
                drop_outcome = drop_outcome, 
                drop_predictors = drop_predictors,
                message = "Non-spatial gam fit successfully, no spatial autocorrelation detected so no spatial gam fit"))
  }else{
    
    #---------------------------------------------
    # If g is a factor then s(g,bs="re") produces a
    # random coefficient for each level of g, with
    # the random coefficients all modelled as i.i.d. normal.
    #---------------------------------------------
    
    
    spatial_gam_fit = try(gamm4(formula_spatial, 
                                family = family,
                                data = d,
                                REML = T,
                                na.action = na.exclude, 
                                random = random
    ))
    
    #---------------------------------------------
    # confirm the model converged
    #---------------------------------------------
    print("Spatial gam fit")
    # summary(spatial_gam_fit$gam)
    if(length(spatial_gam_fit) > 1 & !class(spatial_gam_fit) %in% c("try-error", "try-warning", "try-message")){
      print(summary(spatial_gam_fit$gam))
    } else {
     # print(spatial_gam_fit)
      stop("Spatial GAM failed to converge")
    }
    
    # compare AIC for model with and without spatial smoother
    if (AIC(gam_fit$mer) < AIC(spatial_gam_fit$mer)) {
      print("Even though residual spatial autocorrelation was present, spatial GAM has larger AIC than non-spatial GAM. Check model.")
    }
    
    #---------------------------------------------
    # check for residual spatial autocorrelation 
    #   **after** adjusting for lat and long
    #---------------------------------------------
    check_residual_spatial_autocorr <- check_autocorr(spatial_gam_fit$gam, lldata = d[,c("qgpslong", "qgpslat")], nbc = 8)
    
    
    return(list(gam_fit = gam_fit,
                spatial_gam_fit = spatial_gam_fit,
                spatial_autocorr = check_spatial_autocorr$spatial_autocorr,
                spatial_autocorr_coeff = check_spatial_autocorr$correlograms,
                residual_spatial_autocorr = check_residual_spatial_autocorr$spatial_autocorr,
                residual_spatial_autocorr_coeff = check_residual_spatial_autocorr$correlograms,
                y = y, 
                a = a, 
                w_included = c(keep_w, "qgpslong", "qgpslat"),
                formula = formula_spatial,
                family = family,
                model_input_data = d, 
                drop_outcome = drop_outcome, 
                drop_predictors = drop_predictors,
                message = "Spatial gam fit successfully"))
    # end of spatial autocorrelation if statement
  }
  
}

###############################################
# fit_gam function for models with interaction terms
# Should eventually be merged with the above after testing to ensure it 
# doesn't break any of the above code.
#   Main differences include that 
#   1) the interaction term is added to the sparsity check
#   2) an interaction term is added to the formula, 
#   3) the function output includes a list item for the interaction term.  
###############################################
fit_gam_interaction = function(df, y, a, w = NULL, interaction_term = NULL, interaction = TRUE, y_per_variable = 10, random_intercept = c("dataid", "clusterid"), family = c("binomial", "gaussian", "poisson")) {

  #---------------------------------------------
  # define new variable for message and set to null (this will trigger function to exit at certain spots when no longer null)
  #---------------------------------------------# 
  message = NULL
  
  #---------------------------------------------
  # check that the family is one of the allowed responses (no typos, no Poisson, etc)
  #---------------------------------------------
  random_intercept <- match.arg(random_intercept)
  family <- match.arg(family)
  y = sym(y)
  #---------------------------------------------
  # drop rows with missing outcome
  #---------------------------------------------
  d = df %>% filter(!is.na({{y}}))
  drop_outcome = dim(df)[1] - dim(d)[1]
  message(paste0("Observations lost due to missing outcome: ", drop_outcome))
  
  #---------------------------------------------
  # covariate screening with likelihood ratio test, keeping p<=0.1
  #---------------------------------------------
  if (!is.null(w)) {
    if (family[1] == "poisson" & length(unique(d$y)) == 2) {
      prescreen_family = "binomial" 
    } else {
      prescreen_family = family
    }
    keep_w = washb_prescreen_mod(d %>% pull(y), Ws = d %>% select(all_of(w)), family = prescreen_family, pval = 0.1, print = T)
    if (length(keep_w) == 0) message = "Adjusted model not run because no covariates were associated with the outcome at P< 0.1"
  } else {
    keep_w = NULL
  }
  
  
  #---------------------------------------------
  # check there are sufficient observations for analysis
  #---------------------------------------------
  if (is.null(interaction_term)) {
    check_w = keep_w
  } else {
    check_w = c(keep_w, interaction_term)
  }
  
  sparsity_check = try(validate_that(check_sparsity(y = d %>% pull(y),
                                                  a = d[, a],
                                                  w = data.frame(d[,all_of(check_w)]),
                                                  y_per_variable = y_per_variable),
                                   msg = "Data sparsity check failed. Choose new covariate set."))
  
  if (sparsity_check != TRUE & "season" %in% check_w) {
    check_w = check_w[!check_w %in% "season"] 
    sparsity_check = try(validate_that(check_sparsity(y = d %>% pull(y),
                                                    a = d[, a],
                                                    w = data.frame(d[,all_of(check_w)]),
                                                    y_per_variable = y_per_variable),
                                     msg = "Data sparsity check failed. Choose new covariate set."))
  }
  while (sparsity_check != TRUE & length(check_w) > 1) {
    check_w = head(check_w, -1)
    sparsity_check = try(validate_that(check_sparsity(y = d %>% pull(y),
                                                    a = d[, a],
                                                    w = data.frame(d[,all_of(check_w)]),
                                                    y_per_variable = y_per_variable),
                                     msg = "Data sparsity check failed. Choose new covariate set."))
  }
  if (sparsity_check != TRUE & length(check_w) == 1) {
    message = "Adjusted/interaction model could not be run due to data sparsity"
    message("Adjusted/interaction model could not be run due to data sparsity")
  }
  
  if (!is.null(message)) {
    return(list(gam_fit = NA,
                spatial_gam_fit = NA,
                spatial_autocorr = NA,
                spatial_autocorr_coeff = NA,
                residual_spatial_autocorr = NA,
                residual_spatial_autocorr_coeff = NA,
                y = y, 
                a = a, 
                w_included = keep_w,
                interaction_term = interaction_term,
                formula = NA,
                family = family,
                model_input_data = d, 
                drop_outcome = drop_outcome, 
                drop_predictors = NA,
                message = message))
  } 
  
  #---------------------------------------------
  # build formula
  #---------------------------------------------
  ## Formula start with outcome and risk factor
  if (is.numeric(d[,a])) {
    # formula_start = paste(y, paste0("s(",a,", bs=\"cs\")"), sep = " ~ ")
    formula_start = paste(y, "", sep = " ~ ") #, paste0("s(",a,")")
  } else {
    # formula_start = paste(y, a, sep = " ~ ")
    formula_start = paste(y, "", sep = " ~ ") #a,
  }
  
  ## Format adjustment covariates
  keep_w_terms = NULL
  if (!is.null(keep_w)) {
    ## function to write s() for numeric variables
    keep_w_terms = map_chr(keep_w, function(x) {
      ifelse(is.numeric(d[,x]), paste0("s(", x, ", bs=\"cs\")"), x)
    })
  } 
  
  ## Format interaction, if applicable
  interaction_formula = NULL
  if (!is.null(interaction_term) & interaction == T) {
    if (is.numeric(d[,a]) & is.factor(d[,interaction_term])) {
      interaction_formula = paste0("s(", a, ", by = ", interaction_term, ", bs=\"cs\", m = 1)", " + ", interaction_term, " + s(", a,", bs=\"cs\")")
    } else if (is.numeric(d[,a]) & is.numeric(d[,interaction_term])) {
      interaction_formula = paste0("t2(", a, ",", interaction_term, ", bs=\"cs\")")
    } else if (is.factor(d[,a]) & is.numeric(d[,interaction_term])) {
      print("There should be no factor risk_factors with continuous interaction_terms")
    } else if (is.factor(d[,a]) & is.factor(d[,interaction_term])) {
      interaction_formula = paste0(a, "*", interaction_term)
    } 
  } else if (!is.null(interaction_term) & interaction == FALSE) {
    if (is.numeric(d[,a]) & is.factor(d[,interaction_term])) {
      interaction_formula = paste0("s(", a,", bs=\"cs\")", " + ", interaction_term)
    } else if (is.numeric(d[,a]) & is.numeric(d[,interaction_term])) {
      interaction_formula = paste0("s(", a, ", bs=\"cs\")", " + s(", interaction_term, ", bs=\"cs\")")
    } else if (is.factor(d[,a]) & is.numeric(d[,interaction_term])) {
      print("There should be no factor risk_factors with continuous interaction_terms")
    } else if (is.factor(d[,a]) & is.factor(d[,interaction_term])) {
      interaction_formula = paste0(a, " + ", interaction_term)
    } 
  }
  ## Make complete formula
  if (!is.null(keep_w_terms) & is.null(interaction_formula)) {
    print("here1")
    formula = as.formula(paste(formula_start,
                               paste0(ifelse(is.numeric(d[,a]), paste0("s(",a,", bs=\"cs\") +"), paste0(a, " + ")), ifelse(length(keep_w_terms) > 1, paste(keep_w_terms, collapse = " + "), keep_w_terms)),
                               sep = ""))

    formula_spatial =  as.formula(paste(paste0(formula_start,
                                               paste0(ifelse(is.numeric(d[,a]), paste0("s(",a,", bs=\"cs\") +"), paste0(a, " + ")), ifelse(length(keep_w_terms) > 1, paste(keep_w_terms, collapse = " + "), keep_w_terms))),
                                        # thin plate smoother for lat & long
                                        "s(qgpslong, qgpslat, bs = \"tp\")",
                                        sep = " + "))
  } else if (!is.null(keep_w_terms) & !is.null(interaction_formula)) {
    print("here2")
    formula = as.formula(paste(paste0(formula_start, interaction_formula), 
                                paste(ifelse(length(keep_w_terms) > 1, paste(keep_w_terms, collapse = " + "), keep_w_terms)),
                                sep = " + "))
    
    formula_spatial =  as.formula(paste(paste0(formula_start, interaction_formula), 
                                         paste(ifelse(length(keep_w_terms) > 1, paste(keep_w_terms, collapse = " + "), keep_w_terms)),
                                         # thin plate smoother for lat & long
                                         "s(qgpslong, qgpslat, bs = \"tp\")",
                                        sep = " + "))
  } else if (is.null(interaction_formula)) {
    print("here3")
    formula = as.formula(paste(formula_start,
                               paste0(ifelse(is.numeric(d[,a]), paste0("s(",a,", bs=\"cs\")"), a))))
    
    formula_spatial = as.formula(paste(formula_start,
                                       paste0(ifelse(is.numeric(d[,a]), paste0("s(",a,", bs=\"cs\") +"), paste0(a, " + ")),
                                              # thin plate smoother for lat & long
                                              "s(qgpslong, qgpslat, bs = \"tp\")"),
                                       # "s(clusterid, bs = \"re\", by = dummy)",
                                       sep = ""))
  } else {
    print("here4")
    formula = as.formula(paste(formula_start, 
                               interaction_formula,
                               sep = ""))
    
    formula_spatial = as.formula(paste(paste0(formula_start,
                                              interaction_formula), 
                                       # thin plate smoother for lat & long
                                       "s(qgpslong, qgpslat, bs = \"tp\")", 
                                       # "s(clusterid, bs = \"re\", by = dummy)",
                                       sep = " + "))
  }
  
  #---------------------------------------------
  # drop rows with missing risk factor or covariates
  #---------------------------------------------
  d = d %>% 
    select(all_of(y), all_of(a), all_of(check_w), qgpslat, qgpslong, all_of(random_intercept)) %>%
    na.omit() 
  
  drop_predictors = dim(df)[1] - drop_outcome - dim(d)[1]
  message(paste0("Observations lost due to missing predictor or covariates: ", drop_predictors))
  
  random = as.formula(paste0("~(1|", random_intercept, ")"))
  #---------------------------------------------
  # fit GAM ignoring spatial variation
  #---------------------------------------------
  print("gamfit")
  gam_fit = try(gamm4(formula, 
                      family = family,
                      data = d,
                      REML = T,
                      na.action = na.exclude, 
                      random = random
  ))
  
  
  #---------------------------------------------
  # confirm the model converged
  #---------------------------------------------
  print(paste0("Non-spatial gam fit: ", formula[3]))
  if(length(gam_fit) > 1 & !class(gam_fit) %in% c("try-error", "try-warning", "try-message")){
    print(summary(gam_fit$gam))
  } else {
    print("here2")
    print(gam_fit)
    #stop("Non-spatial GAM failed to converge")
    message = "Non-spatial GAM failed to converge"
    return(list(gam_fit = NA,
                spatial_gam_fit = NA,
                spatial_autocorr = NA,
                spatial_autocorr_coeff = NA,
                residual_spatial_autocorr = NA,
                residual_spatial_autocorr_coeff = NA,
                y = y, 
                a = a, 
                w_included = keep_w,
                interaction_term = interaction_term,
                formula = formula,
                family = family,
                model_input_data = d, 
                drop_outcome = drop_outcome, 
                drop_predictors = drop_predictors,
                message = message))
  }
  #---------------------------------------------
  # check for residual spatial autocorrelation
  #---------------------------------------------
  # print(gam_fit$gam)
  check_spatial_autocorr <- check_autocorr(gam_fit$gam, lldata = d[,c("qgpslong", "qgpslat")], nbc = 4)
  
  #---------------------------------------------
  # if residual spatial autocorrelation is present,
  # proceed with spatial GAM
  #---------------------------------------------
  if(!check_spatial_autocorr$spatial_autocorr){
    return(list(gam_fit = gam_fit,
                spatial_gam_fit = NA,
                spatial_autocorr = check_spatial_autocorr$spatial_autocorr,
                spatial_autocorr_coeff = check_spatial_autocorr$correlograms,
                residual_spatial_autocorr = NA,
                residual_spatial_autocorr_coeff = NA,
                y = y, 
                a = a, 
                w_included = keep_w,
                interaction_term = interaction_term,
                formula = formula,
                family = family,
                model_input_data = d, 
                drop_outcome = drop_outcome, 
                drop_predictors = drop_predictors,
                message = "Non-spatial gam fit successfully, no spatial autocorrelation detected so no spatial gam fit"))
  }else{
    
    #---------------------------------------------
    # If g is a factor then s(g,bs="re") produces a
    # random coefficient for each level of g, with
    # the random coefficients all modelled as i.i.d. normal.
    #---------------------------------------------
    
    
    spatial_gam_fit = try(gamm4(formula_spatial, 
                                family = family,
                                data = d,
                                REML = T,
                                na.action = na.exclude, 
                                random = random
    ))
    
   
    #---------------------------------------------
    # confirm the model converged
    #---------------------------------------------
    print("Spatial gam fit")
    # summary(spatial_gam_fit$gam)
    if(length(spatial_gam_fit) > 1 & !class(spatial_gam_fit) %in% c("try-error", "try-warning", "try-message")){
      print(summary(spatial_gam_fit$gam))
    } else {
      print(spatial_gam_fit)
      stop("Spatial GAM failed to converge")
    }
    
    # compare AIC for model with and without spatial smoother
    
    if (AIC(gam_fit$mer) < AIC(spatial_gam_fit$mer)) {
      print("Even though residual spatial autocorrelation was present, spatial GAM has larger AIC than non-spatial GAM. Check model.")
    }
    
    #---------------------------------------------
    # check for residual spatial autocorrelation 
    #   **after** adjusting for lat and long
    #---------------------------------------------
    check_residual_spatial_autocorr <- check_autocorr(spatial_gam_fit$gam, lldata = d[,c("qgpslong", "qgpslat")], nbc = 4)
    
    return(list(gam_fit = gam_fit,
                spatial_gam_fit = spatial_gam_fit,
                spatial_autocorr = check_spatial_autocorr$spatial_autocorr,
                spatial_autocorr_coeff = check_spatial_autocorr$correlograms,                
                residual_spatial_autocorr = check_residual_spatial_autocorr$spatial_autocorr,
                residual_spatial_autocorr_coeff = check_residual_spatial_autocorr$correlograms,
                y = y, 
                a = a, 
                w_included = c(keep_w, "qgpslong", "qgpslat"),
                interaction_term = interaction_term,
                formula = formula_spatial,
                family = family,
                model_input_data = d, 
                drop_outcome = drop_outcome, 
                drop_predictors = drop_predictors,
                message = "Spatial gam fit successfully"))
    # end of spatial autocorrelation if statement
  }
  
}

###############################################
# check_gam_fit
###############################################

# Documentation:    check_gam_fit
# Usage:            check_gam_fit(outcome, risk_factor, output_type, analysis, plot, results_dir)
# Description:      check model fit for a given outcome and risk factor pair, 
#                   provides output as either a .pdf file or inline plots/summary 
# Args/Options: 
#    outcome          character argument specifying the outcome
#    risk_factor      character argument specifying the risk factor
#    interaction_term character argument specifying the interaction term
#    output_type      character argument specifying the output type, accepted values are "pdf" (Default) or "in-line"
#    analysis         character argument specifying the outcome category (sth, diarrhea, giardia, pathogens), 
#                       adjusted/unadjusted, and model type (binary, continuous, count) with each element separated by a  
#                       hyphen (e.g. "sth-adjusted-binary"). NOTE: the diarrhea dataset can have -0 or -1 appended 
#                       to the end to signify which dataset, control (0) or intervention (1), and effect modification 
#                       analyses can also have -EM appended to the end.
#    plot             character argument specifying whether to plot only the risk factor (Default) or 
#                       risk factor and all adjustment covariates.
#    results_dir      character argument specifying the output directory, only used if outcome_type == "pdf"
# Returns:          The gam model fit summary, plot, and gam.check results, either as a pdf file or in-line
# Prints:           The gam model fit summary, plot, and gam.check results; nothing if output_type == "pdf"

check_gam_fit <- function(outcome, 
                          risk_factor, 
                          interaction_term = NULL,
                          output_type = c("pdf", "in-line"), 
                          analysis = NULL,
                          plot = c("only risk factor", "all"),
                          results_dir = results_path) {
  output_dir = paste0(results_dir, "gam_check/", analysis)
  if (output_type == "pdf") {
    render(input = gam_check_template_rmd,
           params = list(outcome = outcome, risk_factor = risk_factor, interaction_term = interaction_term, analysis = analysis, plot = plot),
           output_file = ifelse(is.null(interaction_term), paste0("gam_check_", outcome, "_", risk_factor, ".pdf"), paste0("gam_check_", outcome, "_", risk_factor, "_by_", interaction_term, ".pdf")),
           output_dir = output_dir)
  } else {
    if (is.null(interaction_term)) {
      x = readRDS(paste0(here::here(), 
                         "/results/gam_outputs/", analysis, "/gam_",
                         outcome, 
                         "_",
                         risk_factor,
                         ".RDS"))
    } else {
      x = readRDS(paste0(here::here(), 
                         "/results/gam_outputs/", analysis, "/gam_",
                         outcome, 
                         "_",
                         risk_factor,
                         "_by_", 
                         interaction_term,
                         ".RDS"))
    }
    
    message(paste0("Observations lost due to missing outcome: ", x$drop_outcome))
    message(paste0("Observations lost due to missing predictor or covariates: ", x$drop_predictors))
    if (is.na(x$spatial_gam_fit)) {
      fit.gam = x$gam_fit$gam
      fit.mer = x$gam_fit$mer
      message("Spatial GAM not fit, plotting non-spatial GAM results")
    } else {
      fit.gam = x$spatial_gam_fit$gam
      fit.mer = x$spatial_gam_fit$mer
    }
    print(summary(fit.mer))
    print(summary(fit.gam))
    simulationOut = simulateResiduals(fittedModel = fit.mer)
    plot(simulationOut)
    
    res2 = recalculateResiduals(simulationOut, group = x$model_input_data$dataid)
    plot(res2)

    
    if(plot == "only risk factor") {
      plot(x = fit.gam, trans = plogis, shift = coef(fit.gam)[1],
           select = 1,
           seWithMean = TRUE, residuals = T, pch = 1, cex = 1)#,
      # ylab = paste("Prevalence of", outcome))
    } else {
      plot(x = fit.gam, trans = plogis, shift = coef(fit.gam)[1],
           seWithMean = TRUE, residuals = TRUE, pch = 1, cex = 1)#,
      # ylab = paste("Prevalence of", outcome))
    }
    if (x$spatial_autocorr) { # Only run this chunk if spatial gam was used
      vis.gam(x = fit.gam,
              view = c("qgpslong", "qgpslat"),
              plot.type = "persp",
              se = 2)
      vis.gam(x = fit.gam,
              view = c("qgpslong", "qgpslat"),
              plot.type = "contour")
    }
 
  }
}



###############################################
# predict_gam
###############################################

# Documentation:     predict_gam
# Usage:             predict_gam(fit, data, riskfactorName)
# Description:       get marginal predicted probabilities from 
#                    logistic model for the observed range 
#                    of a given risk factor 
# Args/Options: 
#    fit             glm or gam model fit object
#    data            dataset used to fit model
#    riskfactorName  name of risk factor of interest, as character string
# Returns:           data frame with a column for the risk factor values
#                    over which the prediction was done and the predicted
#                    probabilities from the input model
# Prints:            nothing
predict_gam = function(fit, data, riskfactorName){
  
  
  wrapr::let(
    alias=list(riskfactor = riskfactorName),
    expr={
      
      # create vector of observed unique values
      riskfactor_vals = seq(min(data$riskfactor, na.rm = T),
                            max(data$riskfactor, na.rm = T),
                            0.01)
      
      pred_wrapper = function(x){
        mean(predict(fit, 
                     newdata = data %>% mutate(riskfactor = x), 
                     type= "response"))
      }
      
      preds = map_dbl(riskfactor_vals, 
                      pred_wrapper)
      
      df = data.frame(riskfactor = riskfactor_vals,
                      pred = preds)
      
    })
  
  return(df)
}




#----------------------------------
# adapted from: https://github.com/ben-arnold/mbita-schisto

# simultaneous CIs for GAMs
# estimated by resampling the 
# Bayesian posterior estimates of
# the variance-covariance matrix
# assuming that it is multivariate normal
# the function below also estimates 
# the unconditional variance-covariance
# matrix, Vb=vcov(x,unconditional=TRUE), 
# which allows for undertainty in the actual
# estimated mean as well 
# (Marra & Wood 2012 Scandinavian Journal of Statistics, 
#  Vol. 39: 53-74, 2012, doi: 10.1111/j.1467-9469.2011.00760.x )
# simultaneous CIs provide much better coverage than pointwise CIs
# see: http://www.fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/
#
# @param       m : GAM model fit object from mgcv gam()
# @param newdata : data.frame on which to make predictions from the model fit.
#                  Must include all variables used to fit the model m
# @param  nreps  : number of replications to sample from the Bayesian posterior
#
# @param exclude_terms : model terms to exclude from prediction. Term names should be given 
#                        as they appear in the model summary (for example, "s(x0,x1)").
# @returns : gamCI returns a data.frame identical to newdata with 6 new variables:
#            NOTE: ALL PREDICTIONS ARE ON THE SCALE OF LINEAR PREDICTIONS FROM THE MODEL 
#                 (i.e., log-odds for logit model)
#            fit    : marginal spline prediction for each observation
#            se_fit : approximate standard error of the spline prediction
#            uprP   : upper pointwise 95% confidence interval
#            lwrP   : lower pointwise 95% confidence interval
#            uprS   : upper simultaneous 95% confidence interval
#            lwrS   : lower simultaneous 95% confidence interval
#----------------------------------
gamCI <- function(m,newdata,exclude_terms=NULL, nreps=10000) {
  require(mgcv)
  require(dplyr)
  Vb <- vcov(m,unconditional = TRUE)
  if(is.null(exclude_terms)) pred <- predict(m, newdata, se.fit = TRUE)
  if(!is.null(exclude_terms)) pred <- predict(m, newdata, exclude = exclude_terms, se.fit = TRUE)
  
  fit <- pred$fit
  se.fit <- pred$se.fit
  BUdiff <- MASS::mvrnorm(n=nreps, mu = rep(0, nrow(Vb)), Sigma = Vb)
  Cg <- predict(m, newdata, exclude = exclude_terms, type = "lpmatrix")
  simDev <- Cg %*% t(BUdiff)
  absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  masd <- apply(absDev, 2L, max)
  crit <- quantile(masd, prob = 0.95, type = 8)
  pred <- data.frame(newdata,fit=pred$fit,se_fit=pred$se.fit)
  pred <- mutate(pred,
                 uprP = fit + (2 * se.fit),
                 lwrP = fit - (2 * se.fit),
                 uprS = fit + (crit * se.fit),
                 lwrS = fit - (crit * se.fit)
  )
  return(pred)
}



##############################################
##############################################
# Documentation: plot_gam_int_hist
# Usage: plot_gam_int_hist(file_path, file_name, model_subdir, outcome_subdir, 
# risk_factor1, risk_factor1_label, 
# risk_factor2, risk_factor2_label, outcome, outcome_label,
# scale, x_lower, x_upper)
# Description: Make a plot with the smooth gam fits for interactions for 
# continuous x categorical risk factor levels and include a histogram
# for the continuous risk factor 

# Args/Options:
# file_path:          file path for interaction output, as a string
# file_name:          file name with interaction output, as a string
# model_subdir:       model subdirectory for saved figures, as a string 
# risk_factor1:       variable name for continuous risk factor, as a string 
# risk_factor1_label: label for continuous risk factor, as a string 
# risk_factor2:       variable name for categorical risk factor, as a string 
# risk_factor2_label: label for categorical risk factor, as a string 
# outcome:            variable name for outcome, as a string 
# outcome_label:      label for outcome, as a string 
# scale:              number to multiply outcome by (e.g., 100 for prevalence)
# x_lower:
# x_upper:

# Returns: saves png with figure in appropriate directory 
# Output: none

plot_gam_int_hist <- function(file_path, file_name, model_subdir,
                              risk_factor1, risk_factor1_label, 
                              risk_factor2, risk_factor2_label, outcome, outcome_label,
                              scale, x_lower, x_upper, histogram = TRUE) {
  #------------------------------------------------------
  # load in output
  #------------------------------------------------------
  x = readRDS(paste0(file_path, file_name))
  data = x$model_input_data
  w_included = x$w_included
  #------------------------------------------------------
  # check whether model was fit 
  # if not, exit function
  #------------------------------------------------------
  if(!(all(is.na(x$gam_fit)) & all(is.na(x$spatial_gam_fit)))){
    
    
    #------------------------------------------------------
    # Outcomes with no spatial autocorrelation will use a separate gam fit object
    #------------------------------------------------------
    if (!all(is.na(x$spatial_gam_fit))) {
      gamfit = x$spatial_gam_fit$gam
    } else {
      gamfit = x$gam_fit$gam
    }
    
    
    #------------------------------------------------------
    # Create data frame with all unique observed values of risk factors
    #------------------------------------------------------
    ## Add means values back so centered risk factors to put back on their original scale
    rf_mean = readRDS(paste0(clean_offset_data_dir, "risk_factor_mean_values.RDS")) %>%
      filter(variable == gsub("_C", "", risk_factor1)) %>%
      pull(mean_value)
    
    # number of levels of second risk factor 
    rf2_levels <- levels(data %>% pull(!!sym(risk_factor2)))
    
    newd_list <- list()
    fitp_ci_list <-list()
    
    # for each level of risk factor 2, create dataset filtered to those
    # values, then fit CIs
    for(i in 1:length(rf2_levels)){
      # filter to risk factor 2 level i
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
      newd_list[[i]] <- data %>% 
        filter(!sym(risk_factor2) == rf2_levels[i])
      } else {
        newd_list[[i]] <- data %>% 
          filter(!sym(risk_factor2) == rf2_levels[i])
      }
      
      terms_included = attr(terms(x$formula), "term.labels")
      if (any(grepl("qgpslong", terms_included))) {
        exclude_terms = "s(qgpslong,qgpslat)"
      } else {
        exclude_terms = NULL
      }
      
      # Obtain simultaneous CIs
      fitp_ci_list[[i]] <- gamCI(m=gamfit,newdata=newd_list[[i]],exclude_terms = exclude_terms, nreps=10000) #"s(qgpslong,qgpslat)"
      
      #------------------------------------------------------
      # Obtain model family
      #------------------------------------------------------
      link <- gamfit$family[[2]]
      
      #------------------------------------------------------
      # Calculate y-value transformations and y-axis labels based on model family
      #------------------------------------------------------
      if(link == "logit") {
        fitp_ci_list[[i]] <- fitp_ci_list[[i]] %>% 
          mutate(fit = exp(fit)/(1+exp(fit)) * scale,
                 lwrS = exp(lwrS)/(1+exp(lwrS)) * scale,
                 uprS = exp(uprS)/(1+exp(uprS)) * scale)
        ylab_name <- "Prevalence (%)"
      } else if(link == "log") {
        fitp_ci_list[[i]] <- fitp_ci_list[[i]] %>% 
          mutate(fit = exp(fit) * scale,
                 lwrS = exp(lwrS) * scale,
                 uprS = exp(uprS) * scale)
        ylab_name <- "Prevalence (%)"
      }  
      
      
    }
    
    fitp_ci <- bind_rows(fitp_ci_list)
    
    if (risk_factor2 == "age_cat") {
      fitp_ci <- fitp_ci %>%
        mutate(age_cat = factor(age_cat, levels = c("<1.5yr", "1.5-5yr"), labels = c("6m-1.5yr", "1.5-5yr")))
    }
    
    fitp_ci <- fitp_ci %>%
      mutate(!!sym(risk_factor1) := !!sym(risk_factor1) + rf_mean) 
    
    #------------------------------------------------------
    # Make plots
    #------------------------------------------------------
    
    plot_smooth <- ggplot(fitp_ci, aes(x = !!sym(risk_factor1), y = fit, 
                                       group = !!sym(risk_factor2))) + 
      geom_ribbon(aes(ymin = lwrS, ymax= uprS, fill = !!sym(risk_factor2)), alpha =0.25) +
      geom_line(aes(col = !!sym(risk_factor2))) +
      scale_color_discrete(name=risk_factor2_label) + 
      scale_fill_discrete(name=risk_factor2_label) + 
      ylab(ylab_name) +
      theme_bw() +
      theme(
        axis.title.x = element_blank(),
        legend.position = "bottom"
      )
    
    plot_histogram <- ggplot(data, aes(x = !!sym(risk_factor1))) +
      geom_histogram(bins = 100) +
      xlab(risk_factor1_label) + 
      ylab("Count") + 
      theme_bw() +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
      )
    
    if (histogram) {
      combined_plot <- grid.arrange(plot_smooth, plot_histogram, heights = c(4,2),
                                    top = textGrob(paste0(outcome_label,"\n",
                                                          "(", risk_factor1_label, " x\n",
                                                          risk_factor2_label,")"), gp=gpar(fontsize=14)))
    } else {
      combined_plot = plot_smooth
    }
     
    
    #------------------------------------------------------
    # Save plots
    #------------------------------------------------------
    figure_directory_box <- paste0(local_root_path, local_box_path)
    
    save_name <- paste0(figure_directory_box, model_subdir, 
                        outcome,"_",risk_factor1, "_", risk_factor2, ".tiff")
    ggsave(save_name, combined_plot, bg = "white",
           width = 14, height = 11.5, units = "cm")
    
    return(combined_plot)
    
  } else{
    # if model wasn't fit
    print(paste0("No model output for ", outcome, 
                 " - ", risk_factor1, " - ", risk_factor2))
  }
}

