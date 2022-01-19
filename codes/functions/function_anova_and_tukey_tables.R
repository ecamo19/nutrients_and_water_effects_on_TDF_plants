# Original Code developer https://gist.github.com/dsparks/4332698


# Load packages ----------------------------------------------------------------
library(ggplot2)
library(tibble)
library(reactablefmtr)
library(emmeans)

# Model inference --------------------------------------------------------------

## Anova Table -----------------------------------------------------------------

# First, get and customize anova table from model 
anova_table_df <- function(model){
    
    # get the response variable name from the model
    terms <- terms(model)
    response_var <- as.character(attr(terms, "variables"))[2] 
    
    # Generate anova table
    anova.lme(model, type = "marginal") %>% 
        
        data.frame() %>%
        rownames_to_column("fixed_effects") %>% 
        
        # create column with the name of the response variable
        add_column(response_variable = response_var) %>% 
        clean_names() %>%
        
        # Round p and f values
        mutate(f_value = round(f_value,3),
               p_value = round(p_value,6)) %>% 
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
                  num_df = colDef(minWidth = 70),
                  den_df = colDef(minWidth = 65),
                  f_value = colDef(minWidth = 70),
                
                  
                  # Color p_value if it is less than 0.05
                  p_value = colDef(style = function(value) {
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
                      num_df = colDef(minWidth = 70),
                      den_df = colDef(minWidth = 65),
                      f_value = colDef(minWidth = 70),
                      
                      
                      # Color p_value if it is less than 0.05
                      p_value = colDef(style = function(value) {
                          if (value >= 0.05) {color <- "black"}
                          else {color <- "#008000"} 
                          list(color = color)})))
    
}

## Multiple comparisons table --------------------------------------------------

# First, get multiple comparisons
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
    #table_df <- 
        as.data.frame(emmeans(model,
                              formula,
                              adjust ="tukey")$contrast) %>%
        clean_names() %>% 
        
        # create column with the name of the response variable
        add_column(response_variable = response_var) %>%
        #select(-c(df, t_ratio)) %>% 
        
        select(response_variable, everything(),-c(df, t_ratio)) %>% 
        
        mutate_if(is.numeric, round, 4)
    
    #return(table_df)
} 


# Second, generate final table 
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

    
    
