# Load packages ----------------------------------------------------------------
library(dplyr)


# Load Biomass data ------------------------------------------------------------

data_mycorrhizal_colonization <- 
	read.csv("./raw_data/2_micorryzhal_colonization_data.csv", header = T)

# Clean data -------------------------------------------------------------------

data_mycorrhizal_colonization_cleaned <- 

	data_mycorrhizal_colonization %>% 
	dplyr::select(-family) %>% 
	
	# There are some values greater that 100%. Values greater than 100% were
	# treated as 100%
	mutate(percentage_upto_100 = if_else(percentage > 100, 100, percentage)) %>% 
	
	# Create nfixer column
	mutate(nfixer = ifelse(spcode == "ec" |
						   	spcode == "dr" |
						   	spcode == "gs","fixer", "nonfixer")) %>% 
	
	# Order the columns
	dplyr::select(id,spcode,treatment,nfixer, everything()) %>% 
    
    # Transform to factor class spcode,family and treatment
    mutate(treatment = factor(treatment,
                              #Order factors      
                              levels = c("ambientrain", 
                                         "ambientrain_nutrients",
                                         "ambientrain_water",
                                         "ambientrain_water_nutrients")),
           nfixer = factor(nfixer),
           spcode = factor(spcode)) %>% 

	# Group data this is done because there are 2 sub samples per 
	# plant so what I am doing is get the mean of the sub-samples
	group_by(id,spcode,treatment,nfixer) %>%
	 
	# log only the values greater than 0
	mutate(log_perc = if_else(percentage > 0, log(percentage),0),
	       log_perc_upto_100 = 
	        if_else(percentage_upto_100  > 0, log(percentage_upto_100),0)) %>% 
	
	# Create a mean value for every single plant
	summarise_if(is.numeric, funs(mean))  %>% 
	arrange(nfixer)
  

# Remove raw data set ----------------------------------------------------------
rm(data_mycorrhizal_colonization)
