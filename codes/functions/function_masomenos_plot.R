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
	

