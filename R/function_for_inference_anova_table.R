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
    
    #trait_var <- as.character(attr(terms, "variables"))[5]
    
    # Generate anova table
    car::Anova(model, type = "III", test.statistic=c("F")) %>% 
        
        data.frame() %>%
        rownames_to_column("fixed_effects") %>% 
        
        # create column with the name of the response variable
        add_column(response_variable = response_var) %>% 
        clean_names()  %>% 
 
        # Round p and f values
        mutate(f = round(f,4),
               pr_f = round(pr_f,20)) %>% 
        dplyr::select(response_variable, fixed_effects, everything())
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
                      f = colDef(minWidth = 70),
                      df = colDef(minWidth = 70),
                      
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
                      
                      # Color p_value if it is less than 0.05
                      pr_f = colDef(style = function(value) {
                          if (value >= 0.05) {color <- "black"}
                          else {color <- "#008000"} 
                          list(color = color)})))
    
}


