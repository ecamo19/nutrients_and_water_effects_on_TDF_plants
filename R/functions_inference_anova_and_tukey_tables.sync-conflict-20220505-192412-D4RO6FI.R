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
    car::Anova(model, type = "III") %>% 
        
        data.frame() %>%
        rownames_to_column("fixed_effects") %>% 
        
        # create column with the name of the response variable
        add_column(response_variable = response_var) %>% 
        clean_names()  %>% 
 
        # Round p and f values
        mutate(chisq = round(chisq,3),
               pr_chisq = round(pr_chisq,6)) %>% 
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
                      chisq = colDef(minWidth = 70),
                      df = colDef(minWidth = 70),
                      
                      # Color p_value if it is less than 0.05
                      pr_chisq = colDef(style = function(value) {
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
                      chisq = colDef(minWidth = 70),
                      df = colDef(minWidth = 70), 
                      
                      # Color p_value if it is less than 0.05
                      pr_chisq = colDef(style = function(value) {
                          if (value >= 0.05) {color <- "black"}
                          else {color <- "#008000"} 
                          list(color = color)})))
    
}


## Multiple comparisons table --------------------------------------------------

# First, get multiple comparisons as data frame
tukey_table_df <- function(model, formula){
  
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
                              type = "response", 
                              adjust ="tukey")$contrast) %>%
        clean_names() %>% 
        
        # Multiply estimate by -1 to improve interpretation
        #mutate(estimate = abs(estimate)) %>% 
          
        # create column with the name of the response variable
        add_column(response_variable = response_var) %>%
          
        mutate(response_variable = factor(response_variable),
               contrast = factor(contrast)) %>%
          
        dplyr::select(response_variable, everything(),-c(df)) %>% 
        
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
                          type = "response",
                          adjust = "tukey")$emmeans) %>% 
        
        add_column(resp_var = response_var) %>% 
        clean_names() %>% 
        dplyr::select(resp_var, everything(),
                      -c(df,lower_cl, upper_cl,se)) %>%
        
        # Rename response to emmean, this is done when models is log  
        rename_all(funs(stringr::str_replace_all(., "response", "emmean"))) %>%
        
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
        
        reactable(map_df(model, ~ emmeans_df(.x, formula, grouping_var)),
                  
                  groupBy = "resp_var",
                  
                  rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee', 
                        borderLeft: '2px solid #ffa62d' }} 
                        else {return { borderLeft: '2px solid transparent' }}}"),
                  
                  columns = list(
                      
                      # Adjust columns width
                      resp_var = colDef(minWidth = 140),
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
                  
                  groupBy = "resp_var",
                  
                  rowStyle = JS("function(rowInfo) {
                        if (rowInfo.level > 0) {
                        return { background: '#eee', 
                        borderLeft: '2px solid #ffa62d' }} 
                        else {return { borderLeft: '2px solid transparent' }}}"),
                  
                  columns = list(
                      
                      # Adjust columns width
                      resp_var = colDef(minWidth = 140),
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


# Bootstrap function -----------------------------------------------------------
bootstrap_model_df <- function(model, iter = 100, category = "norm"){
    
    
    cli::cli_alert("Available options for category are norm, basic, perc or all" )
    
    # get the response variable name from the model
    terms <- terms(model)
    response_var <- as.character(attr(terms, "variables"))[2] 
    #trait_var <- as.character(attr(terms, "variables"))[5] 
    colmn <- paste0("col_", 1:3)
    
    # Get 95% CIs
    
    # Bootstrap fixed effects
    # 
    bootstrap_nlme <- lmeresampler::bootstrap(model,.f = fixef, 
                                              type = "parametric",
                                              B = iter)
    confidence_intervals_nlme_params <- 
        stats::confint(bootstrap_nlme,type = category) %>%  
        
        as.data.frame(.) %>%
        
        # create column with the name of the response variable
        tibble::add_column(response_var = response_var) %>%
        
        janitor::clean_names()  %>% 
        
        filter(!term %in% c("(Intercept)","init_height")) %>% 
        
        tidyr::separate(data = ., col = term, sep = ":",into = colmn,
                        remove = T) %>% 
        
        mutate(col_1 = case_when(
            col_1 == "treatmentno_additions" ~ "no_additions",
            col_1 == "treatmentplus_nutrients" ~ "plus_nutrients",
            col_1 == "treatmentplus_water" ~ "plus_water",
            col_1 == "treatmentplus_water_nutrients" ~ "plus_water_nutrients",
            
            TRUE ~ col_1)) %>% 
        
        mutate(col_2 = case_when(
            col_2 == "nfixerfixer" ~ "fixer",
            col_2 == "nfixernonfixer" ~ "nonfixer",
            
            TRUE ~ col_2)) %>% 
        
        filter(!col_1 %in% "no_additions") %>%
        arrange(col_1, col_2) %>%
        unite("term", 1:3, sep = ":",remove = TRUE) %>%
        mutate(term = forcats::fct_inorder(term)) %>%
        
        # Create significance column
        dplyr::mutate(significance = if_else((lower > 0 & upper > 0 | 
                                                  lower < 0 & upper < 0),
                                             TRUE, FALSE)) 
    
    results <- list(bootstrap_nlme,confidence_intervals_nlme_params)
    return(results)
}
