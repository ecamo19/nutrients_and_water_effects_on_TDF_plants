# Objetive ---------------------------------------------------------------------
# This script contains all the models used to analyze the data from the shade 
# house experiment 

# Load packages ----------------------------------------------------------------
library(nlme)

# Models -----------------------------------------------------------------------

## Objetive --------------------------------------------------------------------

# This function was created for fitting a model with the fixed effects, treatment,
# nfixer, their interaction and the covariate initial height (which takes into 
# account that each seedling had a diferent height at the beginning of the 
# experiment). This function was created for using it in combination with 
# purrr::map to save time. 


## Model 1: Base model, random effect (1|spcode) -------------------------------

mixed_model_1 <-  function(response, data) {    
    #this function takes each response variable and join it to the formula
    fixed_effects <-  paste(response, " ~ nfixer*treatment + init_height")
    
    # Fit the model
    lme(as.formula(fixed_effects),
        random = ~ 1|spcode,
        method = "REML",
        data = data)
}


## Model 2: VarIdent model -----------------------------------------------------

# This function fit a mixed effect models with only random intercepts for the 
# data set data_complete
# The argument varident_variable is for comparing models with different weights

mixed_model_2_varident <- function(response, data, varident_variable = c("nfixer,
                                                                   treatment,
                                                                   nfixer_treatment")){
    # Specify fixed effects
    fixed_effects <-  paste(response, " ~ nfixer*treatment + init_height")
    
    # Add option for change the weights parameter
    if (!hasArg(varident_variable)) { 
        print("Please select one of the options: nfixer, treatment or nfixer_treatment")
        
        } else if(varident_variable == "nfixer") 
        
        {# Fit the model
        lme(as.formula(fixed_effects),
                random = ~ 1|spcode,
                method = "REML",
                weights = varIdent(form = ~ 1 | nfixer),
                data = data)

        } else if (varident_variable == "treatment") {

        # Fit the model
        lme(as.formula(fixed_effects),
                random = ~ 1|spcode,
                method = "REML",
                weights = varIdent(form = ~ 1 | treatment),
                data = data)


        }  else if (varident_variable == "nfixer_treatment") {

        # Fit the model
        lme(as.formula(fixed_effects),
                random = ~ 1|spcode,
                method = "REML",
                weights = varIdent(form = ~ 1 | treatment*nfixer),
                data = data)
        }

}


