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

    
    
# Get tidy table
    
## Visualize multiple comparison -----------------------------------------------

## Create single data frame with all coefficients ------------------------------
#crear una clase?
    
#La gracia es que una function acepte una lista y me devuelva un plot

# 
# ## Get coefficients ------------------------------------------------------------
# ## 
# models_1
# create_data_frame <- function(model_list){
#     
#     # get the response variable name from the model
#     terms <- terms(model_list)
#     vars <- as.character(attr(terms, "variables"))[-1] 
#     response <- attr(terms, "response") 
#     
#     # Create the data frame
#     data.frame(#variable = rownames(summary(model_list)$tTable),
#         
#         # Get coefs from summary
#         coefficient = summary(model_list)$tTable[, 1],
#         
#         # Get standard error from model
#         SE = summary(model_list)$tTable[, 2], 
#         
#         # Set model name
#         model_name = paste0("model_", vars[response])) %>% 
#         rownames_to_column("variable") 
# } 
# 
# map_df(models_1, create_data_frame)
# model1Frame <- data.frame(variable = rownames(summary(models_1$rmf)$tTable),
# 
#                           # Get coefs from summary
#                           coefficient = summary(models_1$rmf)$tTable[, 1],
# 
#                           # Get standard error from model
#                           SE = summary(models_1$rmf)$tTable[, 2],
#                           modelName = "South Indicator")
# 
# # 
# model2Frame <- data.frame(variable = rownames(summary(models_1$lmf)$tTable),
# 
#                           coefficient = summary(models_1$lmf)$tTable[, 1],
#                           SE = summary(models_1$lmf)$tTable[, 2],
#                           modelName= "Age Interaction")
# # 
# # model3Frame <- data.frame(variable = rownames(summary(model3)$tTable),
# #                           coefficient = summary(model3)$tTable[, 1],
# #                           SE = summary(model3)$tTable[, 2],
# #                           modelName = "Univariate")
# 
# ## Join the coefficients into single data frame --------------------------------
# emmip(models_1$lmf)
# emm1_1 <- emmeans(models_1$total_biomass, specs = pairwise ~ treatment|nfixer,adjust = "bonf", )$contrast
# 
# 
# 
# allModelFrame <- data.frame(rbind(model1Frame, model2Frame)) 
# 
# model1Frame
# 
# 
# #data <- map_df(models_2_varident_treat_nfixer,create_data_frame)
# 
# # Specify the width of your confidence intervals -------------------------------
# 
# ## 90% multiplier -------------------------------------------------------------- 
# interval1 <- -qnorm((1-0.9)/2)  
# 
# ## 95% multiplier --------------------------------------------------------------
# interval2 <- -qnorm((1-0.95)/2)  
# 
# # Plot the coefficients --------------------------------------------------------
# 
# ## Base plot -------------------------------------------------------------------
# #zp1 <- ggplot(allModelFrame, aes(colour = modelName))
# 
# zp1 <- ggplot(data = allModelFrame,aes(colour = modelName))
# 
# ## Add line at x = 0 -----------------------------------------------------------
# zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
# 
# 
# zp1 <- zp1 + geom_linerange(aes(x = variable, 
#                                 ymin = coefficient - SE*interval1,
#                                 ymax = coefficient + SE*interval1),
#                             lwd = 1, position = position_dodge(width = 1/2))
# 
# 
# 
# zp1 <- zp1 + geom_pointrange(aes(x = variable, y = coefficient, 
#                                  ymin = coefficient - SE*interval2,
#                                  ymax = coefficient + SE*interval2),
#                              lwd = 1/2, position = position_dodge(width = 1/2),
#                              shape = 21, fill = "WHITE")
# 
# ## Flip the plot ---------------------------------------------------------------
# zp1 <- zp1 + coord_flip() + theme_bw()
# zp1 <- zp1 + ggtitle("Comparing several models")
# 
# # The trick to these is position_dodge().
# print(zp1)  
