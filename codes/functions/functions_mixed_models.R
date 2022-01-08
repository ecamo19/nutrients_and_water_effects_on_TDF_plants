# Objetive ---------------------------------------------------------------------
# This script contains all the models used to analyze the data from the shade 
# house experiment 

# Load packages ----------------------------------------------------------------
library(nlme)
library(cowplot)
library(lme4)


## Objetive --------------------------------------------------------------------

# These functions were created for fitting a mixed model with the fixed effects, 
# treatment, nfixer, their interaction and the covariate initial height 
# (which takes into account that each seedling had a different height at the 
# beginning of the experiment). These functions were created for using tehm in 
# combination with purrr::map to save time. 

# Model fitting ----------------------------------------------------------------

## Model 1: Base model, random effect (1|spcode) -------------------------------

mixed_model_1 <-  function(response, data) {    
    
    # Take each response variable and join it to the model formula
    formula = paste(response, " ~ treatment * nfixer + init_height + (1|spcode)")
    #fixed_effects <-  paste0(response, " ~ nfixer*treatment + init_height")
    
    # Fit the model
    model_1_base <- lmer(as.formula(formula),
                            #random = ~ 1|spcode,
                            #method = "REML",
                            data = data) 
    return(model_1_base)
    
}


## Model 2: VarIdent model -----------------------------------------------------

# This function fit a mixed effect models with only random intercepts for the 
# data set data_complete
# The argument varident_variable is for fitting models with different weights

mixed_model_2_varident <- function(response, data, 
                                   varident_variable = c("nfixer,
                                                         treatment,
                                                         nfixer_treatment,
                                                         spcode")){
    
    # Take each response variable and join it to the model formula
    fixed_effects <-  paste(response, " ~ nfixer*treatment + init_height")
    
    # Add option for change the weights parameter
    if (!hasArg(varident_variable)) { 
        print("Please select one of the options: nfixer, treatment or nfixer_treatment")
        
        } else if(varident_variable == "nfixer") 
        
        # Model with varIdent =  ~ 1 | nfixer
        {model_varident_1 <- lme(as.formula(fixed_effects),
                                random = ~ 1|spcode,
                                method = "REML",
                                weights = varIdent(form = ~ 1 | nfixer),
                                data = data)
        return(model_varident_1)
        
        # Model with varIdent =  ~ 1 | treatment
        } else if (varident_variable == "treatment") {

        # Fit the model
        model_varident_2 <- lme(as.formula(fixed_effects),
                                random = ~ 1|spcode,
                                method = "REML",
                                weights = varIdent(form = ~ 1 | treatment),
                                data = data)
        return(model_varident_2)
        
        # Model with varIdent =  ~ 1 | nfixer*treatment
        }  else if (varident_variable == "nfixer_treatment") {

        # Fit the model
        model_varident_3 <- lme(as.formula(fixed_effects),
                                    random = ~ 1|spcode,
                                    method = "REML",
                                    weights = varIdent(form = ~ 1 | treatment*nfixer),
                                    data = data)
        return(model_varident_3)
        
        # Model with varIdent =  ~ 1 | spcode
        } else if (varident_variable == "spcode") {
        
        # Fit the model
        model_varident_4 <- lme(as.formula(fixed_effects),
                                random = ~ 1|spcode,
                                method = "REML",
                                weights = varIdent(form = ~ 1 | spcode),
                                data = data)
        return(model_varident_4)
    }

}


# Model Validation plot varIdent model -----------------------------------------

# The coplot generated here if for checking variance hetereogeneity

validation_plot_varident_models <-  function(response, data, 
                                              varident_variable = c("nfixer,
                                                                    treatment,
                                                                    nfixer_treatment,
                                                                    spcode")){
    
    # Take each response variable and join it to the model formula
    fixed_effects <-  paste(response, " ~ nfixer*treatment + init_height")
    
    # Add option for change the weights parameter
    if (!hasArg(varident_variable)) { 
        print("Please select one of the options: nfixer,treatment or 
              nfixer_treatment")
        
    } else if(varident_variable == "nfixer")  
     
        # Model with varIdent =  ~ 1 | nfixer
        {model_varident_1 <- lme(as.formula(fixed_effects),
                                    random = ~ 1|spcode,
                                    method = "REML",
                                    weights = varIdent(form = ~ 1 | nfixer),
                                    data = data)
        # Validation plot
        return(
        coplot(resid(model_varident_1,type = "normailized") ~ treatment | nfixer, 
               data = data, 
               ylab = "Normalised residuals" ))
        
    } else if (varident_variable == "treatment") {
        
        # Fit the model
        model_varident_2 <- lme(as.formula(fixed_effects),
                                random = ~ 1|spcode,
                                method = "REML",
                                weights = varIdent(form = ~ 1 | treatment),
                                data = data)
        
        # Validation plot
        return(coplot(resid(model_varident_2,
                         type = "normailized") ~ treatment | nfixer,
                   data = data, 
                   ylab = "Normalised residuals" ))
        
    }  else if (varident_variable == "nfixer_treatment") {
        
        # Fit the model
        model_varident_3 <- lme(as.formula(fixed_effects),
                                random = ~ 1|spcode,
                                method = "REML",
                                weights = varIdent(form = ~ 1 | treatment*nfixer),
                                data = data)
        
        # Validation plot
        return(coplot(resid(model_varident_3,
                            type = "normailized") ~ treatment | nfixer,
                      data = data, 
                      ylab = "Normalised residuals" ))
    
    } else if (varident_variable == "spcode") {
        
        # Fit the model
        model_varident_4 <- lme(as.formula(fixed_effects),
                                random = ~ 1|spcode,
                                method = "REML",
                                weights = varIdent(form = ~ 1 | spcode),
                                data = data)
        
        # Validation plot
        return(coplot(resid(model_varident_4,
                            type = "normailized") ~ treatment | nfixer,
                      data = data, 
                      ylab = "Normalised residuals"))
        
        
    }
}


