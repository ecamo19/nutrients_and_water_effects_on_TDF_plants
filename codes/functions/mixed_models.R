# Objetive ---------------------------------------------------------------------
# This script contains all the models used to analyze the data from the shade 
# house experiment 

# Load packages ----------------------------------------------------------------
library(nlme)


# Models -----------------------------------------------------------------------


## Model 1: random effect 1|spcode ---------------------------------------------

mixed_model_1_spcode_random_lmer <-  function(response, data) {    
    #this function takes each response variable and join it to the formula
    formula <-  paste(response, " ~ nfixer*treatment + init_height + (1 | spcode)")
    
    # Fit the model
    lmer(as.formula(formula), 
        #method = "REML",
        data = data)
}


## Model 1.1 VarIdent model -----------------------------------------------------

model_1_1_varident <- function(response, data){
            
        #this function takes each response variable and join it to the formula
        formula <-  paste(response, " ~ nfixer*treatment + init_height")
        
        # Fit the model
        lme(as.formula(formula),random = ~ 1|spcode, method = "REML",
            weights = varIdent(form = ~ 1 | treatment*nfixer),
            data = data)
}

## Model 2: random effect treatment|spcode -------------------------------------

# mixed_model_2_treatment_spcode_random_lmer <-  function(response, data) {
#     
#     #this function takes each response variable and join it to the formula
#     formula <-  
#         paste(response, " ~ nfixer*treatment + init_height + (1 + spcode |treatment)")
#     
#     # Fit the model
#     lmer(as.formula(formula), data = data)
# }
# 

## Model 2.1 varIdent ----------------------------------------------------------

# model_2_1_varident <- function(response, data){
#     
#     #this function takes each response variable and join it to the formula
#     formula <-  paste(response, " ~ nfixer*treatment + init_height")
#     
#     # Fit the model
#     lme(as.formula(formula),
#         random = ~ treatment|spcode, 
#         method = "REML",
#         weights = varIdent(form = ~ 1 | treatment*nfixer),
#         data = data)
# }
# 


