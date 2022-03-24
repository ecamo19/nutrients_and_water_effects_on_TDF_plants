# Objetive ---------------------------------------------------------------------
# This script contains all the models used to analyze the data from the shade 
# house experiment 

# Load packages ----------------------------------------------------------------
library(lme4)

## Objective --------------------------------------------------------------------

# These functions were created for fitting a mixed model with treatment, nfixer 
#  as fixed effects and their interaction and the covariate initial height 
# (which takes into account that each seedling had a different height at the 
# beginning of the experiment), spcode as random effects

# These functions were created for using them in combination with purrr::map 
# to save time. 

# Model fitting ----------------------------------------------------------------

## Model 1 ---------------------------------------------------------------------

# This is the base model. I created this models using lme4 mainly because the 
# performance package don't work that good with nlme objects. 

mixed_model_1 <-  function(response, data) {    
    
    # Take each response variable and join it to the model formula
    formula = paste(response, " ~ treatment * nfixer + init_height + (1|spcode)")
    
    # Fit the model
    model_1_base <- lmer(as.formula(formula),
                            data = data) 
    return(model_1_base)
    
}

## Model 2: Three way interaction ----------------------------------------------

## This funtion takes two columns a create a model formula with all the possible 
## combinations

model_combinations_formulas <- function(y_var, x_var){
    
    variables <- crossing(y_var, x_var)
    pattern <- c('\\+.*|nfixer|treatment|[[:punct:]]| ')
    
    
    # Model
    models <- paste0(variables$y_var,"~treatment*nfixer*",
                     variables$x_var,"+init_height+(1|spcode)")
    
    formulas <- map(models, as.formula)
    
    # Add name to the list 
    names(formulas) <- stringr::str_replace_all(formulas, pattern, replacement = '')
    return(formulas)
}



mixed_model_2 <-  function(response, trait, data) {    
    
    # Take each response variable and join it to the model formula
    formula = paste0(response, " ~ treatment * nfixer * ", trait, " + init_height + (1|spcode)")
    
    # Fit the model
    model_1_base <- lmer(as.formula(formula),
                         data = data,
                         control = lmerControl(optCtrl = list(maxfun = 2e10))) 
    return(model_1_base)
    
}

