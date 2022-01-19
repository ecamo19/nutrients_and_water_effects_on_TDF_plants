# Load packages -----------------------------------------------------------
library(ggplot2)

# Function: Plot with lines ----------------------------------------------------
## color scheme for the levels (treatments)  -----------------------------------
# Harvestatthebegging = Black ("#000000")
# ambientrain = Yellow ("#F0E442")
# ambientrain_nutrients = Green ("#009E73") 
# ambientrain_water = Light blue ("#56B4E9")
# ambientrain_water_nutrients = Dark blue ("#0072B2")


masomenos_plot_lines <- function(data,xvar,yvar,tvar,lcl,ucl,color,n_treat){
	xvar <- enquo(xvar)
	yvar <- enquo(yvar)
	tvar <- enquo(tvar)
	lcl  <- enquo(lcl)
	ucl  <- enquo(ucl)
	color <- enquo(color)
		
	if (n_treat == 4 ){	
	plot_lines <- ggplot(aes(x = !! xvar, y = !! yvar,
					   group = !! tvar, colour = !! color),
					   data = data,
					   addDot = TRUE, dotSize = 1) +
	    
		    geom_point(position = position_dodge(width=0.5))+
		    geom_line(linetype = "dashed", position=position_dodge(width=0.5))+

		    geom_errorbar(aes( ymin = !! lcl, ymax = !! ucl), width=.2,
		                  position=position_dodge(0.5)) +
		    
	        xlab("Levels of nfixer") +
	        ylab("Linear prediction") +
		    
	        scale_colour_manual(values = c("#F0E442","#009E73","#56B4E9", "#0072B2")) +
		    
	        theme_classic() +
	
			theme(axis.text.y   = element_text(size= 14),
			  	  axis.text.x   = element_text(size= 14),
			  	  axis.title.y  = element_text(size= 14),
			  	  axis.title.x  = element_text(size= 14),
			  	  panel.background = element_blank(),
			  	  panel.grid.major = element_blank(), 
			  	  panel.grid.minor = element_blank(),
			  	  axis.line = element_line(size = .4,colour = "black"),
			  	  panel.border = element_rect(colour = "black", fill= NA,size = 1.3)) + 
		    
	        guides(col = guide_legend(ncol = 1,title.position = "top",))
		
		return(plot_lines)}}
	


# Function: Plot no lines ------------------------------------------------------
masomenos_plot_no_lines <- 
   	function(data,xvar,yvar,tvar,lcl,ucl,color, n_treat){
	xvar <- enquo(xvar)
	yvar <- enquo(yvar)
	tvar <- enquo(tvar)
	lcl  <- enquo(lcl)
	ucl  <- enquo(ucl)
	color <- enquo(color)
	
	if (n_treat == 4 ){
		plot_nolines_4_treatments <- 
		    
			ggplot(aes(x = !! xvar, y = !! yvar, group = !! tvar, colour = !! color ),
			       data = data) +
		    
		    geom_point(position = position_dodge(width=0.5))+
		    geom_errorbar(aes( ymin = !! lcl, ymax = !! ucl), width=.2,
			position=position_dodge(0.5)) +
		    xlab("Levels of nfixer") + 
		    ylab("Linear prediction") +
		
		scale_colour_manual(values = c("#F0E442","#009E73", "#56B4E9", "#0072B2"))+
		theme_classic() +
		theme(legend.position = "right") +
		guides(col = guide_legend(ncol = 1,title.position = "top",))
	
	return(plot_nolines_4_treatments)} else {	
		
		plot_nolines_more_than_4 <- ggplot(aes(x = !! xvar, y = !! yvar,
								   group = !! tvar, colour = !! color ), 
														data = data) +
		geom_point(position = position_dodge(width=0.5))+
		#geom_line(linetype = "dashed", position=position_dodge(width=0.5))+
		geom_errorbar(aes(
			ymin = !! lcl,
			ymax = !! ucl),
			width=.2,
			position=position_dodge(0.5)) +
		xlab("Levels of nfixer") + 
		ylab("Linear prediction") +

		scale_colour_manual(values = c("#000000","#F0E442","#009E73","#56B4E9",
		                               "#0072B2"))+
		theme_classic() +
		theme(legend.position = "right") +
		guides(col = guide_legend(ncol = 1,title.position = "top",))
	
	return(plot_nolines_more_than_4)}
}
		

	
# Boxplots ---------------------------------------------------------------------

masomenos_boxplot <- function(data,xvar,yvar,color, n_treat){
	xvar <- enquo(xvar)
	yvar <- enquo(yvar)
	color <- enquo(color)
	
	if (n_treat == 4 ) {
		boxplot <- ggplot(aes(x = !! xvar, y = !! yvar, 
							  #group = !! tvar, 
							  colour = !! color ), data = data)+
			geom_boxplot(position = position_dodge(width = .93 ))+
			
			
			xlab("Levels of nfixer") + 
			ylab("") +
		    
			scale_colour_manual(values = c("#F0E442","#009E73",
										   "#56B4E9","#0072B2"))+
			theme_classic() +
			theme(legend.position = "right") + 
			guides(col = guide_legend(ncol = 1 ,title.position = "top",))
		
		return(boxplot)} else {
		
		boxplot <- ggplot(aes(x = !! xvar, y = !! yvar,colour = !! color ), 
						  data = data) +
			geom_boxplot(position = position_dodge(width = .93 )) +
		
			xlab("Levels of nfixer") +
			ylab("") +
				
		
			scale_colour_manual(values = c("#000000","#F0E442",
										   "#009E73","#56B4E9","#0072B2"))+
			theme_classic() +
			theme(legend.position = "right") + 
			guides(col = guide_legend(ncol = 1 ,title.position = "top",))
			
			return(boxplot)} 
	
}	

## Boxplot for using it with pmap ----------------------------------------------

boxplot_plot_pmap <-  function(x, y, fill, data) {
    
    xvar <- enquo(x)
    yvar <- enquo(y)
    fill <- enquo(fill)
    
    ggplot(data, aes(fill = !!fill, x = !!xvar, y = !!yvar )) +
        geom_boxplot() +
        scale_fill_manual(values = c("#F0E442","#009E73",
                                       "#56B4E9","#0072B2")) +
        
        theme_classic() +
        theme(legend.position = "bottom") +
        theme(axis.text=element_text(size = 21),
              axis.title=element_text(size = 21,face = "bold")) 
}
	

# Multiple comparisons plot ----------------------------------------------------

# ## Get coefficients ----------------------------------------------------------
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




 



