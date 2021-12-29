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
                                   varident_variable = c("nfixer,treatment,
                                                         nfixer_treatment")){
    
    # Take each response variable and join it to the model formula
    fixed_effects <-  paste(response, " ~ nfixer*treatment + init_height")
    
    # Add option for change the weights parameter
    if (!hasArg(varident_variable)) { 
        print("Please select one of the options: nfixer, treatment or nfixer_treatment")
        
        } else if(varident_variable == "nfixer") 
        
        {# Fit the model
        model_varident_1 <- lme(as.formula(fixed_effects),
                                random = ~ 1|spcode,
                                method = "REML",
                                weights = varIdent(form = ~ 1 | nfixer),
                                data = data)
        return(model_varident_1)

        } else if (varident_variable == "treatment") {

        # Fit the model
        model_varident_2 <- lme(as.formula(fixed_effects),
                                random = ~ 1|spcode,
                                method = "REML",
                                weights = varIdent(form = ~ 1 | treatment),
                                data = data)
        return(model_varident_2)

        }  else if (varident_variable == "nfixer_treatment") {

        # Fit the model
        model_varident_3 <- lme(as.formula(fixed_effects),
                                    random = ~ 1|spcode,
                                    method = "REML",
                                    weights = varIdent(form = ~ 1 | treatment*nfixer),
                                    data = data)
        return(model_varident_3)
        }

}


# Model Validation -------------------------------------------------------------

## Validation plots for model 1 ------------------------------------------------

validation_plots <-  function(response, data) {    
    
    # This function takes each response variable and join it to the formula
    fixed_effects <-  paste0(response, " ~ nfixer*treatment + init_height")
    
    # Fit the model
    model_1_base <- lme(as.formula(fixed_effects),
                        random = ~ 1|spcode,
                        method = "REML",
                        data = data) 
    
    p1 <- plot(model_1_base,which = c(1), col = data$treatment,add.smooth = T,
               main = paste0( "Model residuals of ",response, 
                              " by Treatment") )
    
    
    p2 <- plot(model_1_base,which = c(1), col = data$nfixer,
               add.smooth = T, main = paste0("Model residuals of ",response, 
                                             " by Nfixer"))
    
    p3 <- qqnorm(model_1_base, ~ resid(., type = "p"), abline = c(0, 1),
                 main = paste0("Model ",response))
    
    #p4 <- plot(model_1_base, resid(model_1_base, type = "p") ~ fitted(model_1_base),
    #           type = c("p", "smooth"),main = paste0("Model ",response))
    
    return(cowplot::plot_grid(p1,p2,p3, ncol = 2))
}


