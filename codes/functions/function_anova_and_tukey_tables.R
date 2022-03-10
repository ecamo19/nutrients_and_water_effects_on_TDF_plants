# Original Code developer https://gist.github.com/dsparks/4332698

# Load packages ----------------------------------------------------------------
library(ggplot2)
library(tibble)
library(reactablefmtr)
library(emmeans)

# Model inference --------------------------------------------------------------

## Anova Table -----------------------------------------------------------------
## For lme4 models
# First, get and customize anova table from model 
anova_table_df <- function(model){
    
    # get the response variable name from the model
    terms <- terms(model)
    response_var <- as.character(attr(terms, "variables"))[2] 
    trait_var <- as.character(attr(terms, "variables"))[5]
    
    # Generate anova table
    car::Anova(model, type = "III", test.statistic="F") %>% 
        
        data.frame() %>%
        rownames_to_column("fixed_effects") %>% 
        
        # create column with the name of the response variable
        add_column(response_variable = paste0(response_var, "-", trait_var)) %>% 
        clean_names() %>% 
        
        # Round p and f values
        mutate(f = round(f,3),
               pr_f = round(pr_f,6),
               df_res = round(pr_f,5)) %>% 
        select(response_variable, fixed_effects, everything())
}

# Second, generate final table 
anova_table_tidy <- function(model, single_model = FALSE, model_list = FALSE){
    
    if(single_model == FALSE & model_list == FALSE |
       single_model == TRUE & model_list == TRUE) {
        stop("Error: single_model OR model_list should be set to TRUE")
    }
    
    else if(model_list == TRUE) {
        
        reactable(map_df(model, anova_table_df),
                  
                  groupBy = "response_variable",
                  
                  rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee', 
                        borderLeft: '2px solid #ffa62d' }} 
                        else {return { borderLeft: '2px solid transparent' }}}"),
                  
                  columns = list(
                      
                      # Adjust columns width
                      response_variable = colDef(minWidth = 165),
                      fixed_effects = colDef(minWidth = 125),
                      f = colDef(minWidth = 65),
                      df = colDef(minWidth = 70),
                      df_res = colDef(minWidth = 70),
                      
                      
                      # Color p_value if it is less than 0.05
                      pr_f = colDef(style = function(value) {
                          if (value >= 0.05) {color <- "black"}
                          else {color <- "#008000"} 
                          list(color = color)})))
    } 
    else 
        reactable(anova_table_df(model),
                  
                  groupBy = "response_variable",
                  
                  rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee', 
                        borderLeft: '2px solid #ffa62d' }} 
                        else {return { borderLeft: '2px solid transparent' }}}"),
                  
                  columns = list(
                      
                      # Adjust columns width
                      response_variable = colDef(minWidth = 165),
                      fixed_effects = colDef(minWidth = 125),
                      f = colDef(minWidth = 70),
                      df = colDef(minWidth = 70),
                      df_res = colDef(minWidth = 65),
                      
                      
                      # Color p_value if it is less than 0.05
                      pr_f = colDef(style = function(value) {
                          if (value >= 0.05) {color <- "black"}
                          else {color <- "#008000"} 
                          list(color = color)})))
    
}


## Multiple comparisons table --------------------------------------------------

# First, get multiple comparisons as data frame
tukey_table_df <- function(model,formula){
  
  # get the response variable name from the model
    terms <- terms(model)
    response_var <- as.character(attr(terms, "variables"))[2] 
    
    if(missing(formula)){
        stop("Error: At least one var should be specified! Example A, B or A|B")
    } 
    else
        formula <- formula(paste0("pairwise ~ ", formula))
    
    
    # Get contrasts
    # estimate: of the effect size, that is the difference 
    # between the two emmeans (estimated marginal means)
        as.data.frame(emmeans(model,
                              formula,
                              adjust ="tukey")$contrast) %>%
        clean_names() %>% 
        
        # Multiply estimate by -1 to improve interpretation
        mutate(estimate = abs(estimate)) %>% 
          
        # create column with the name of the response variable
        add_column(response_variable = response_var) %>%
          
        mutate(response_variable = factor(response_variable),
               contrast = factor(contrast)) %>%
          
        select(response_variable, everything(),-c(df, t_ratio)) %>% 
        
        mutate_if(is.numeric, round, 4)
    
} 


# Second, tidy table 
tukey_table_tidy <- function(model,single_model = FALSE, model_list = FALSE, 
                             formula = NULL){
    
    if(single_model == FALSE & model_list == FALSE |
       single_model == TRUE & model_list == TRUE) {
        stop("Error: single_model OR model_list arguments should be set to TRUE")
    }
    
    if(missing(formula)){
        stop("Error: At least one var should be specified! Example A, B or A|B")
    } 
  
    if(model_list == TRUE) {
        
        reactable(map_df(model, ~ tukey_table_df(.x, formula)),
                  
                  groupBy = "response_variable",
                  
                  rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee', 
                        borderLeft: '2px solid #ffa62d' }} 
                        else {return { borderLeft: '2px solid transparent' }}}"),
                  
                  columns = list(
                      
                      # Adjust columns width
                      response_variable = colDef(minWidth = 165),
                      contrast = colDef(minWidth = 185),
                      estimate = colDef(minWidth = 75),
                      se = colDef(minWidth = 55),
                      
                      # Color p_value if it is less than 0.05
                      p_value = colDef(minWidth = 60,
                          style = function(value) {
                            if (value >= 0.05) {color <- "black"}
                            else {color <- "#008000"} 
                            list(color = color)})))
    }
    
    else if (single_model == TRUE) {
      
    
        reactable(tukey_table_df(model, formula),
                  
                  groupBy = "response_variable",
                  
                  rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee', 
                        borderLeft: '2px solid #ffa62d' }} 
                        else {return { borderLeft: '2px solid transparent' }}}"),
                  
                  columns = list(
                      
                      # Adjust columns width
                      response_variable = colDef(minWidth = 165),
                      contrast = colDef(minWidth = 185),
                      estimate = colDef(minWidth = 75),
                      se = colDef(minWidth = 55),
                      
                      # Color p_value if it is less than 0.05
                      p_value = colDef(minWidth = 60,
                          style = function(value) {
                            if (value >= 0.05) {color <- "black"}
                            else {color <- "#008000"} 
                            list(color = color)})))
    } 
}


# Get emmeans ------------------------------------------------------------------

emmeans_df <- function(model,formula, grouping_var = NULL){
  
  if (is_empty(grouping_var) == TRUE) {
    var = NULL
  } else (
    var <- ensym(grouping_var) 
  )
  #var <- sym(ifelse(is_empty(grouping_var) == TRUE, NA, grouping_var))
  
  # get the response variable name from the model
  terms <- terms(model)
  response_var <- as.character(attr(terms, "variables"))[2] 
  
  if(missing(formula)){
    stop("Error: At least one var should be specified! Example A, B or A|B")
  } 
  else
    formula <- formula(paste0("pairwise ~ ", formula))
  
  # Get contrasts tukey
  # estimate: of the effect size, that is the difference 
  # between the two emmeans (estimated marginal means)
  as.data.frame(emmeans(model,
                        formula,
                        adjust = "tukey")$emmeans) %>% 
    
    add_column(response = response_var) %>% 
    clean_names() %>% 
    dplyr::select(response, everything(),
                  -c(df,lower_cl, upper_cl,se)) %>%
    
    
    group_by(!!(var)) %>%
    
    mutate(difference = ((emmean - first(emmean))),
           perc_difference =((emmean - first(emmean) )/first(emmean))*100) %>% 
    mutate_if(is.numeric, round, 3)
  
}

emmeans_table_tidy <- function(model, grouping_var = NULL, single_model = FALSE, 
                               model_list = FALSE, formula = NULL){
  
  if(single_model == FALSE & model_list == FALSE |
     single_model == TRUE & model_list == TRUE) {
    stop("Error: single_model OR model_list arguments should be set to TRUE")
  }
  
  if(missing(formula)){
    stop("Error: At least one var should be specified! Example A, B or A|B")
  } 
  
  if(model_list == TRUE) {
    
    reactable(map_df(model, ~ emmeans_df(.x, formula, !!grouping_var)),
              
              groupBy = "response",
              
              rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee', 
                        borderLeft: '2px solid #ffa62d' }} 
                        else {return { borderLeft: '2px solid transparent' }}}"),
              
              columns = list(
                
                # Adjust columns width
                response = colDef(minWidth = 140),
                treatment = colDef(minWidth = 185),
                emmean = colDef(minWidth = 120),
                difference = colDef(minWidth = 120),
                
                # Color p_value if it is less than 0.05
                perc_difference = colDef(minWidth = 175,
                                         style = function(value) {
                                           if (value <= 0 ) {color <- "red"}
                                           else {color <- "#008000"} 
                                           list(color = color)})))
  }
  
  else if (single_model == TRUE) {
    
    reactable(emmeans_df(model, formula, grouping_var),
              
              groupBy = "response",
              
              rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee', 
                        borderLeft: '2px solid #ffa62d' }} 
                        else {return { borderLeft: '2px solid transparent' }}}"),
              
              columns = list(
                
                # Adjust columns width
                response = colDef(minWidth = 140),
                treatment = colDef(minWidth = 185),
                emmean = colDef(minWidth = 120),
                difference = colDef(minWidth = 120),
                
                # Color p_value if it is less than 0.05
                perc_difference = colDef(minWidth = 175,
                                 style = function(value) {
                                   if (value <= 0 ) {color <- "red"}
                                   else {color <- "#008000"} 
                                   list(color = color)})))
  } 
}
 
    
