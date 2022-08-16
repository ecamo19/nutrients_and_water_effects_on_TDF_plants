# Load packages ----------------------------------------------------------------
library(dplyr)
# For Model fitting
library(nlme)
# For adding new columns
library(tibble)
library(lmeresampler)

# Load data --------------------------------------------------------------------
sys.source("./scripts/code_join_data_full_dataset.R", envir = knitr::knit_global())


# Load functions ---------------------------------------------------------------
# Inference
sys.source("./R/functions_inference_anova_and_tukey_tables.R", envir = knitr::knit_global())

sys.source("./R/functions_models.R", envir = knitr::knit_global())



# Clean data -------------------------------------------------------------------

data_for_models <-
    data_for_models %>%
    
    # Select variables for analysis
    dplyr::select(c(1:7,9,11:ncol(data_for_models)))


data_for_models_scaled <-
    data_for_models %>% 
    mutate(across(c(4,9:14),scale)) 


# Models -----------------------------------------------------------------------


## Aboveground Biomass ---------------------------------------------------------


### Test A ---------------------------------------------------------------------
test_above_a <- lme(above_biomass ~   treatment:nfixer:amax +
                                        treatment:nfixer:gs  +
                                        treatment:nfixer:wue +
                                        treatment:nfixer:pnue +
                                        treatment:nfixer:d13c +
                                        init_height ,
                    random = ~1 | spcode, 
                    data = data_for_models_scaled)

#### Check assumptions ---------------------------------------------------------


plot(test_above_a, resid(., scaled = TRUE) ~ fitted(.),
  abline = 0, pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
)

plot(test_above_a, resid(., scaled = TRUE) ~ fitted(.) | spcode, abline = 0,
     pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
)


plot(test_above_a, as.factor(spcode) ~ resid(., scaled = TRUE),
  abline = 0, pch = 16, xlab = "Standardised residuals", ylab = "Spcode"
)

qqnorm(resid(test_above_a), pch = 16)
qqline(resid(test_above_a))

# Nothing wrong with the model 
AIC(test_above_a)


#### Results -------------------------------------------------------------------
data.frame(intervals(test_above_a)$fixed) %>% 
    tibble::rownames_to_column("names") %>% 
    filter(!names %in% c("(Intercept)","init_height")) %>% 
    mutate(significance = ifelse((lower > 0 & upper > 0 | lower < 0 & upper < 0 ), 
                                 TRUE, FALSE)) %>% 
    filter(significance == TRUE)

boots_test_model_above_biom <- 
    bootstrap_model_ci_df(model = test_above_a, category = "all", 
                          iter = 9999) %>% 
    filter(significance == TRUE)

saveRDS(boots_test_model_above_biom, 
        file = "./processed_data/bootstrap_ci_aboveground_biomass.RData")


### Test B ---------------------------------------------------------------------
# test_above_b <- lme(above_biomass ~   treatment:nfixer:amax +
#                                         treatment:nfixer:gs  +
#                                         treatment:nfixer:wue +
#                                         treatment:nfixer:pnue +
#                                         treatment:nfixer:d13c +
#                                         init_height ,
#                    random = ~1 | spcode, 
#                    weights = varIdent(form = ~1 |spcode), 
#                    data = data_for_models_scaled)

#### Check assumptions ---------------------------------------------------------

# plot(test_above_b, resid(., scaled = TRUE) ~ fitted(.),
#      abline = 0, pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
# )
# 
# plot(test_above_b, resid(., scaled = TRUE) ~ fitted(.),
#      abline = 0, pch = 16,
#      col = data_for_models_scaled$species, xlab = "Fitted values", 
#      ylab = "Standardised residuals"
# )
# 
# plot(test_above_b, resid(., scaled = TRUE) ~ fitted(.) | spcode, abline = 0,
#      pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
# )
# 
# plot(test_above_b, as.factor(spcode) ~ resid(., scaled = TRUE),
#      abline = 0, pch = 16, xlab = "Standardised residuals", ylab = "Regions"
# )
# 
# qqnorm(resid(test_above_b), pch = 16)
# qqline(resid(test_above_b))

#### Results -------------------------------------------------------------------
# data.frame(intervals(test_above_b)$fixed) %>% 
#     tibble::rownames_to_column("names") %>% 
#     filter(!names %in% c("(Intercept)","init_height")) %>% 
#     mutate(significance = ifelse((lower > 0 & upper > 0 | lower < 0 & upper < 0 ), 
#                                  TRUE, FALSE)) %>% 
#     filter(significance == TRUE)
# 

# bootstrap_ci_results <- bootstrap_model_ci_df(model = test_again, 
#                                                category = "all", 
#                                                iter = 9999)

## Belowground Biomass ---------------------------------------------------------

### Test A ---------------------------------------------------------------------
test_below_a <- lme(below_biomass ~   treatment:nfixer:amax +
                                        treatment:nfixer:gs  +
                                        treatment:nfixer:wue +
                                        treatment:nfixer:pnue +
                                        treatment:nfixer:d13c +
                                        init_height ,
                    random = ~1 | spcode, 
                    data = data_for_models_scaled)

#### Check assumptions ---------------------------------------------------------

plot(test_below_a, resid(., scaled = TRUE) ~ fitted(.),
     abline = 0, pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
)

plot(test_below_a, resid(., scaled = TRUE) ~ fitted(.),
     abline = 0, pch = 16,
     col = data_for_models_scaled$spcode, xlab = "Fitted values", 
     ylab = "Standardised residuals"
)

plot(test_below_a, resid(., scaled = TRUE) ~ fitted(.) | spcode, abline = 0,
     pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
)


plot(test_below_a, as.factor(spcode) ~ resid(., scaled = TRUE),
     abline = 0, pch = 16, xlab = "Standardised residuals", ylab = "Regions"
)

qqnorm(resid(test_below_a), pch = 16)
qqline(resid(test_below_a))

# Nothing wrong with model

#### Results -------------------------------------------------------------------

data.frame(intervals(test_below_a)$fixed) %>% 
    tibble::rownames_to_column("names") %>% 
    filter(!names %in% c("(Intercept)","init_height")) %>% 
    mutate(significance = ifelse((lower > 0 & upper > 0 | lower < 0 & upper < 0 ), 
                                 TRUE, FALSE)) %>% 
    filter(significance == TRUE)


boots_test_model_below_biom <- 
    bootstrap_model_ci_df(model = test_below_a, category = "all", 
                          iter = 9999) %>%
    filter(significance == TRUE)

saveRDS(boots_test_model_below_biom, 
        file = "./processed_data/bootstrap_ci_belowground_biomass.RData")

### Test B ---------------------------------------------------------------------
# test_below_b <- lme(below_biomass ~   treatment:nfixer:amax +
#                                         treatment:nfixer:gs  +
#                                         treatment:nfixer:wue +
#                                         treatment:nfixer:pnue +
#                                         treatment:nfixer:d13c +
#                                         init_height ,
#                   random = ~1 | spcode, 
#                   weights = varIdent(form = ~1 |spcode), 
#                   data = data_for_models_scaled)
# 
# #### Check assumptions -------------------------------------------------------
# 
# plot(test_below_b, resid(., scaled = TRUE) ~ fitted(.),
#      abline = 0, pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
# )
# 
# plot(test_below_b, resid(., scaled = TRUE) ~ fitted(.),
#      abline = 0, pch = 16,
#      col = data_for_models_scaled$spcode, xlab = "Fitted values", 
#      ylab = "Standardised residuals"
# )
# 
# plot(test_below_b, resid(., scaled = TRUE) ~ fitted(.) | spcode, abline = 0,
#      pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
# )
# 
# 
# plot(test_below_b, as.factor(spcode) ~ resid(., scaled = TRUE),
#      abline = 0, pch = 16, xlab = "Standardised residuals", ylab = "Regions"
# )
# 
# qqnorm(resid(test_below_b), pch = 16)
# qqline(resid(test_below_b))
# 
# 
# #### Results -----------------------------------------------------------------
# data.frame(intervals(test_below_b)$fixed) %>% 
#     tibble::rownames_to_column("names") %>% 
#     filter(!names %in% c("(Intercept)","init_height")) %>% 
#     mutate(significance = ifelse((lower > 0 & upper > 0 | lower < 0 & upper < 0 ), 
#                                  TRUE, FALSE)) %>% 
#     filter(significance == TRUE)
# 
# 
# AIC(test_below_a,test_below_b)


# bootstrap_ci_results <- bootstrap_model_ci_df(model = test_again, 
#                                               category = "all", 
#                                               iter = 9999)

## RGR  ------------------------------------------------------------------------

### Test A ---------------------------------------------------------------------
test_rgr_a <- lme(rgr ~   treatment:nfixer:amax +
                            treatment:nfixer:gs  +
                            treatment:nfixer:wue +
                            treatment:nfixer:pnue +
                            treatment:nfixer:d13c +
                            init_height ,
                  random = ~1 | spcode, 
                  data = data_for_models_scaled)

#### Check assumptions ---------------------------------------------------------

plot(test_rgr_a, resid(., scaled = TRUE) ~ fitted(.),
     abline = 0, pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
)

plot(test_rgr_a, resid(., scaled = TRUE) ~ fitted(.),
     abline = 0, pch = 16,
     col = data_for_models_scaled$spcode, xlab = "Fitted values", 
     ylab = "Standardised residuals"
)

plot(test_rgr_a, resid(., scaled = TRUE) ~ fitted(.) | spcode, abline = 0,
     pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
)


plot(test_rgr_a, as.factor(spcode) ~ resid(., scaled = TRUE),
     abline = 0, pch = 16, xlab = "Standardised residuals", ylab = "Regions"
)

qqnorm(resid(test_rgr_a), pch = 16)
qqline(resid(test_rgr_a))

# Nothing wrong with this model

#### Results -------------------------------------------------------------------
data.frame(intervals(test_rgr_a)$fixed) %>% 
    tibble::rownames_to_column("names") %>% 
    filter(!names %in% c("(Intercept)","init_height")) %>% 
    mutate(significance = ifelse((lower > 0 & upper > 0 | lower < 0 & upper < 0 ), 
                                 TRUE, FALSE)) %>% 
    filter(significance == TRUE)


boots_test_model_rgr <- 
    bootstrap_model_ci_df(model = test_rgr_a, category = "all", 
                          iter = 9999) %>%
    filter(significance == TRUE)


saveRDS(boots_test_model_rgr, 
        file = "./processed_data/bootstrap_ci_rgr.RData")

### Test B ---------------------------------------------------------------------
# test_rgr_b <- lme(rgr ~   treatment:nfixer:amax +
#                             treatment:nfixer:gs  +
#                             treatment:nfixer:wue +
#                             treatment:nfixer:pnue +
#                             treatment:nfixer:d13c +
#                             init_height ,
#                   random = ~1 | spcode, 
#                   weights = varIdent(form = ~1 |spcode), 
#                   data = data_for_models_scaled)
# 
# #### Check assumptions ---------------------------------------------------------
# 
# plot(model, resid(., scaled = TRUE) ~ fitted(.),
#      abline = 0, pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
# )
# 
# plot(model, resid(., scaled = TRUE) ~ fitted(.),
#      abline = 0, pch = 16,
#      col = data_for_models_scaled$species, xlab = "Fitted values", 
#      ylab = "Standardised residuals"
# )
# 
# plot(model, resid(., scaled = TRUE) ~ fitted(.) | spcode, abline = 0,
#      pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
# )
# 
# par(mfrow = c(2, 4))
# 
# for (i in 1:length(unique(data_for_models_scaled$species))) {
#     plot(resid(model, type = "pearson")[data_for_models_scaled$species == i] ~ 
#              predict(model)[data_for_models_scaled$species == i],
#          pch = 16, ylim = c(-4, 4), main = paste("Region", i), 
#          xlab = "Fitted values", ylab = "Standardised residuals"
#     )
#     lines(x = c(-1000, 1000), y = c(0, 0))
# }
# 
# 
# plot(model, as.factor(spcode) ~ resid(., scaled = TRUE),
#      abline = 0, pch = 16, xlab = "Standardised residuals", ylab = "Regions"
# )
# 
# qqnorm(resid(model), pch = 16)
# qqline(resid(model))
# 
# # Calculate leverage
# lev <- hat(model.matrix(model))
# 
# # Plot leverage against standardised residuals
# plot(resid(model, type = "pearson") ~ lev,
#      las = 1, ylab = "Standardised residuals", xlab = "Leverage"
# )
# 
# # Calculate Cook's Distance
# cd <- cooks.distance(model)
# # N.B. If Cook's distance is greater than 1 this highlights problematic datapoints
# 
# # Plot leverage and Cook's distance together
# par(mfrow = c(1, 1))
# plot(lev, pch = 16, col = "red", ylim = c(0, 1.2), las = 1, 
#      ylab = "Leverage/Cook's distance value")
# 
# points(cd, pch = 17, col = "blue")
# points(x = 150, y = 1.1, pch = 16, col = "red")
# points(x = 150, y = 0.9, pch = 17, col = "blue")
# text(x = 155, y = 1.1, "Leverage", adj = c(0, 0.5))
# text(x = 155, y = 0.9, "Cook's distance", adj = c(0, 0.5))
# 


# #### Results -----------------------------------------------------------------
# 
# 
# data.frame(intervals(test_rgr_a)$fixed) %>% 
#     tibble::rownames_to_column("names") %>% 
#     filter(!names %in% c("(Intercept)","init_height")) %>% 
#     mutate(significance = ifelse((lower > 0 & upper > 0 | lower < 0 & upper < 0 ), 
#                                  TRUE, FALSE)) %>% 
#     filter(significance == TRUE)
# 
# 


# bootstrap_ci_results <- bootstrap_model_ci_df(model = test_again, 
#                                               category = "all", 
#                                               iter = 9999)

 
## Total Biomass ---------------------------------------------------------------

### Test A ---------------------------------------------------------------------
test_total_biomass_a <- lme(total_biomass ~   treatment:nfixer:amax +
                                                treatment:nfixer:gs  +
                                                treatment:nfixer:wue +
                                                treatment:nfixer:pnue +
                                                treatment:nfixer:d13c +
                                                init_height,
                            random = ~1 | spcode, 
                            data = data_for_models_scaled)


#### Check assumptions ---------------------------------------------------------

plot(test_total_biomass_a, resid(., scaled = TRUE) ~ fitted(.),
     abline = 0, pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
)

plot(test_total_biomass_a, resid(., scaled = TRUE) ~ fitted(.),
     abline = 0, pch = 16,
     col = data_for_models_scaled$spcode, xlab = "Fitted values", 
     ylab = "Standardised residuals"
)

plot(test_total_biomass_a, resid(., scaled = TRUE) ~ fitted(.) | spcode, abline = 0,
     pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
)


plot(test_total_biomass_a, as.factor(spcode) ~ resid(., scaled = TRUE),
     abline = 0, pch = 16, xlab = "Standardised residuals", ylab = "Regions"
)

qqnorm(resid(model), pch = 16)
qqline(resid(model))


# Nothing wrong with the model

#### Results -------------------------------------------------------------------

data.frame(intervals(test_total_biomass_a)$fixed) %>% 
    tibble::rownames_to_column("names") %>% 
    filter(!names %in% c("(Intercept)","init_height")) %>% 
    mutate(significance = ifelse((lower > 0 & upper > 0 | lower < 0 & upper < 0 ), 
                                 TRUE, FALSE)) %>% 
    filter(significance == TRUE)

boots_test_model_total_biom <- 
    bootstrap_model_ci_df(model = test_total_biomass_a, category = "all", 
                          iter = 9999) %>%
    filter(significance == TRUE)


saveRDS(boots_test_model_total_biom, 
        file = "./processed_data/bootstrap_ci_total_biomass.RData")


# ### Test B ---------------------------------------------------------------------
# test_total_biomass_b <- lme(total_biomass ~   treatment:nfixer:amax +
#                                                 treatment:nfixer:gs  +
#                                                 treatment:nfixer:wue +
#                                                 treatment:nfixer:pnue +
#                                                 treatment:nfixer:d13c +
#                                                 init_height,
#                   random = ~1 | spcode, 
#                   weights = varIdent(form = ~1 |spcode), 
#                   data = data_for_models_scaled)
# 
# 
# #### Check assumptions ---------------------------------------------------------
# 
# plot(model, resid(., scaled = TRUE) ~ fitted(.),
#      abline = 0, pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
# )
# 
# plot(model, resid(., scaled = TRUE) ~ fitted(.),
#      abline = 0, pch = 16,
#      col = data_for_models_scaled$species, xlab = "Fitted values", 
#      ylab = "Standardised residuals"
# )
# 
# plot(model, resid(., scaled = TRUE) ~ fitted(.) | spcode, abline = 0,
#      pch = 16, xlab = "Fitted values", ylab = "Standardised residuals"
# )
# 
# par(mfrow = c(2, 4))
# 
# for (i in 1:length(unique(data_for_models_scaled$species))) {
#     plot(resid(model, type = "pearson")[data_for_models_scaled$species == i] ~ 
#              predict(model)[data_for_models_scaled$species == i],
#          pch = 16, ylim = c(-4, 4), main = paste("Region", i), 
#          xlab = "Fitted values", ylab = "Standardised residuals"
#     )
#     lines(x = c(-1000, 1000), y = c(0, 0))
# }
# 
# 
# plot(model, as.factor(spcode) ~ resid(., scaled = TRUE),
#      abline = 0, pch = 16, xlab = "Standardised residuals", ylab = "Regions"
# )
# 
# qqnorm(resid(model), pch = 16)
# qqline(resid(model))
# 
# # Calculate leverage
# lev <- hat(model.matrix(model))
# 
# # Plot leverage against standardised residuals
# plot(resid(model, type = "pearson") ~ lev,
#      las = 1, ylab = "Standardised residuals", xlab = "Leverage"
# )
# 
# # Calculate Cook's Distance
# cd <- cooks.distance(model)
# # N.B. If Cook's distance is greater than 1 this highlights problematic datapoints
# 
# # Plot leverage and Cook's distance together
# par(mfrow = c(1, 1))
# plot(lev, pch = 16, col = "red", ylim = c(0, 1.2), las = 1, 
#      ylab = "Leverage/Cook's distance value")
# 
# points(cd, pch = 17, col = "blue")
# points(x = 150, y = 1.1, pch = 16, col = "red")
# points(x = 150, y = 0.9, pch = 17, col = "blue")
# text(x = 155, y = 1.1, "Leverage", adj = c(0, 0.5))
# text(x = 155, y = 0.9, "Cook's distance", adj = c(0, 0.5))
# 
# 
# 
# #### Results -------------------------------------------------------------------
# 
# 
# data.frame(intervals(test_total_biomass_b)$fixed) %>% 
#     tibble::rownames_to_column("names") %>% 
#     filter(!names %in% c("(Intercept)","init_height")) %>% 
#     mutate(significance = ifelse((lower > 0 & upper > 0 | lower < 0 & upper < 0 ), 
#                                  TRUE, FALSE)) %>% 
#     filter(significance == TRUE)
# 
# 
# 
# AIC(test_total_biomass_a,test_total_biomass_b)
# 
# # bootstrap_ci_results <- bootstrap_model_ci_df(model = test_again, 
# #                                               category = "all", 
# #                                               iter = 9999)
# 
# 
# 
# 
