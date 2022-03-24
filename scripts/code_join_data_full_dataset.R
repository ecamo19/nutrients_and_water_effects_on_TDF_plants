# Objective --------------------------------------------------------------------

# This script join all trait and biomass data for posterior analysis

# Packages ---------------------------------------------------------------------
library(dplyr)
library(janitor)
library(tidyr)
library(tibble)

# Load data --------------------------------------------------------------------

## RGR and AGR data cleaned ----------------------------------------------------
source("./scripts/code_clean_and_calculate_growth_measurements.R")

## Biomass data ---------------------------------------------------------------- 
raw_data_biomass <- 
    read.csv("./raw_data/6_plant_dry_weights_data.csv",header = T) %>% 
	clean_names()

## Ecophys data ----------------------------------------------------------------
raw_data_ecophys <- 
    read.csv("./raw_data/3_physiology_data.csv",header = T) %>%
    clean_names() %>% 
    dplyr::select(- c(family))

## Leaf trait data ------------------------------------------------------------- 
raw_data_traits <- 
    read.csv("./raw_data/2_leaf_trait_data.csv",header = T) %>%
    clean_names()

## Isotopes data ---------------------------------------------------------------
raw_data_isotopes <- 
    read.csv("./raw_data/4_isotopes_data.csv",header = T) %>%
    clean_names()

## Initial height data ---------------------------------------------------------
raw_data_initheight <- 
    read.csv("./raw_data/data_heights.csv",header = T) %>%
    clean_names()

## RGR and AGR -----------------------------------------------------------------
# No raw data file loaded here because this data set was cleaned in the file 
# called code_clean_and_calculate_growth_measurements.R 
 

# Cleaned data sets ------------------------------------------------------------

## Biomass data----------------------------------------------------------------- 
data_biomass_cleaned <- 
	raw_data_biomass %>%
        
        # Delete Harvested at the beginning treatment 
        filter(!treatment %in% 'Harvestatthebegging') %>% 
        
        # Calculate biomass related variables
        mutate(total_biomass = root_dry_weight + stem_dry_weight + 
                               whole_leaf_dry_weight,
               
            # Above ground biomass	
            above_biomass = (stem_dry_weight + whole_leaf_dry_weight),
            
            # Below ground biomass
            below_biomass = (root_dry_weight),
            # Mass fractions
            # # % Root Mass Fraction
            rmf = (root_dry_weight / total_biomass)*100,
            
            # % Stem Mass Fractions
            smf = (stem_dry_weight / total_biomass)*100,
            
            # % Leaf Mass Fraction
            lmf = (whole_leaf_dry_weight / total_biomass)*100) %>%
    
        # Order Columns
        dplyr::select(id, spcode, treatment, rmf, smf, lmf,
                      whole_leaf_dry_weight, everything()) %>% 
        
        # Convert character columns to factor
        mutate(across(where(is.character), as.factor))


## Ecophys data ----------------------------------------------------------------
data_ecophys_cleaned <- 

    raw_data_ecophys %>%
    
            # Recode treatment levels 
            mutate(treatment = dplyr::recode(treatment,
                                      `ambientrain` = "ambientrain",			   
                                      `ambientrain+nutrients`= "ambientrain_nutrients",
                                      `ambientrain+water` = "ambientrain_water",
                                      `ambientrain+water+nutrients` = "ambientrain_water_nutrients")) %>%   
        
            # Convert character columns to factor
            mutate(across(where(is.character), as.factor))


## Leaf traits data ------------------------------------------------------------

data_leaftraits_cleaned <- 
	raw_data_traits %>%
    
    	# Delete Harvested at the beginning treatment 
    	filter(!treatment %in% 'Harvestatthebeginning') %>%
    	dplyr::select(-c ()) %>% 
        
        # Convert traits to the right units
        # Transform sla to cm2
        mutate(sla_cm2_g = sla*10000) %>% 
               
        # Remove traits not used in the analysis
        dplyr::select(-c(family, leaf_fresh_weight,leaf_density,lt, lma, sla,
                         ldmc))  %>%
    
        # Convert character columns to factor
        mutate(across(where(is.character), as.factor))

## Isotopes data ---------------------------------------------------------------

data_isotopes_cleaned <-
    raw_data_isotopes %>% 
	
	# Delete Harvested at the beginning treatment 
	filter(!treatment %in% 'Harvestatthebeginning') %>% 
    dplyr::select(-family) %>% 
    
    # Convert character columns to factor
    mutate(across(where(is.character), as.factor))

## Initial height data ---------------------------------------------------------

data_initheight_cleaned <- 
	raw_data_initheight	%>%
        
        # Select columns
        dplyr::select(1:5) %>%
        
        # Rename column
        rename(init_height = x20150831) %>%
        
        #Delete Harvested at the beginning treatment 
        #filter(!treatment %in% 'Harvestatthebegging') %>% 
        dplyr::select(-family) %>% 
        
        # Delete Harvested at the beginning treatment 
        filter(!treatment %in% 'Harvestatthebegging') %>%     
    
        # Convert character columns to factor
        mutate(across(where(is.character), as.factor))

# Join data sets ---------------------------------------------------------------

data_complete <- 
	
	# Join biomass and leaf traits   
    data_biomass_cleaned %>% 
        
        # Add ecophys data
        inner_join(data_ecophys_cleaned, by = c("id", "spcode", "treatment")) %>% 
    
        # Add leaf traits
	    inner_join(data_leaftraits_cleaned, by = c("id", "spcode", "treatment")) %>% 
	    
        #Add isotope data
        inner_join(data_isotopes_cleaned, by = c("id", "spcode", "treatment")) %>% 
        
        # Add plant initial height
        inner_join(data_initheight_cleaned, by = c("id", "spcode", "treatment")) %>%
    
        # Add plant RGR and AGR
        inner_join(data_rgr_agr_cleaned, by = c("id", "spcode", "treatment")) %>%
    
        clean_names()  
        

# Complete dataset -------------------------------------------------------------

data_complete <-
    data_complete %>%
        
        # Create Nfixer category
        mutate(nfixer = factor(ifelse(spcode == "ec" |
                               spcode == "dr" |
                               spcode == "gs" ,"fixer", "nonfixer")),
               
               # Transform Nitrogen to grams
                N_g = leaf_dry_weight*(perc_n/100),
               
                # Transform Nitrogen to mg
                N_mg = (leaf_dry_weight*(perc_n/100))*1000,
               
                # Calculate Nmass and Narea 
                Narea_g_m2 = N_g/la,
                Nmass_mg_g = N_mg/leaf_dry_weight,
                
                # Rename the factor levels
               treatment = factor(recode(data_complete$treatment,ambientrain  = "no_additions",
                                        ambientrain_water_nutrients = "plus_water_nutrients",
                                        ambientrain_nutrients = "plus_nutrients",
                                        ambientrain_water      = "plus_water"))) %>% 
               
        # Order data set columns
        dplyr::select(id, spcode, nfixer, treatment,init_height, everything()) %>%  
        
        # remove unused columns
        dplyr::select(-c(la,leaf_dry_weight,N_mg,N_g, whole_leaf_dry_weight,
                         root_dry_weight,stem_dry_weight))


# Data set use for fitting the models ------------------------------------------

data_for_models <- 
    data_complete %>%
    
    # Calculate nitrogen use efficiency column
    # I followed Leaf traits explaining the growth of tree 
    # species planted in a Central Amazonian disturbed area
    mutate(pnue = (amax*sla_cm2_g*0.1)/perc_n) %>% 
    
    # select variables that are going to be used in the models
    dplyr::select(id, spcode, treatment,nfixer, init_height, 
                  
                  # Plant preformance
                  total_biomass, above_biomass, below_biomass, 
                  agr, rgr, rgr_slope,
                  
                  # Traits
                  amax, gs, wue, d13c, d15n, pnue) %>%
    
    # add id to rownames for keep track of the rows
    column_to_rownames("id") %>% 
    mutate(nfixer = factor(nfixer, levels = c("nonfixer","fixer")))


# Remove all unused data -------------------------------------------------------
items <- c("data_biomass_cleaned", "data_ecophys_cleaned", "data_initheight_cleaned",
           "data_isotopes_cleaned", "data_leaftraits_cleaned", "data_rgr_agr_cleaned",
            "raw_data_biomass", "raw_data_ecophys","raw_data_initheight",
           "raw_data_isotopes", "raw_data_traits")

remove(items, list = items)
ls()
