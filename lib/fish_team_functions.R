# reshape library inclues the cast() function used below
library(reshape2)
library(ggplot2) ## to create the diver vs diver graphs

## Calculate mean length of entire assemblage
Calc_Site_MeanLength<-function(x, prop_size=0.3,min_size=10){  
  # function assumes that x is a data frame with at least the columns/elements listed in base_cols, plus the field_of_interest, in this case CommonFamily
  # prop_size is proportion of max size, min_size is minimum size included in mean size calculation, set at 10 cm
  
  #Base unit will be the entire survey
  base_cols=c("SITEVISITID", "METHOD") 
  pool_cols<-c(base_cols, "SIZE_")                          
  
  #set count to zero for all sizes smaller than min size to exclude recruits
  x[x$SIZE_< (prop_size*x$LMAX),]$COUNT<-0
  
  
  # set count to zero for all sizes smaller than 15 cm
  x[x$SIZE_< min_size,]$COUNT<-0
  
  #sum total number offishes per SIZE_
  y<-aggregate(x$COUNT,by=x[,pool_cols], sum)
  names(y)<-c(pool_cols, "COUNT")
  y$CS<-y$COUNT*y$SIZE_
  
  #now format this more or less as a crosstab, with field of interest as column variable
  y<-aggregate(y[,c("COUNT", "CS")],by=y[,base_cols], sum)
  y$MEAN_SIZE<-y$CS/y$COUNT
  
  return(y[,c(base_cols, "MEAN_SIZE")])
  
} # end Calc_Site_MeanLength

Calc_Site_MeanLengthByGroup<-function(x, grouping_field, min_size=10){  
	
	#Base unit will be the entire survey
	base_cols=c("SITEVISITID", "METHOD", grouping_field) 
	pool_cols<-c(base_cols, "SIZE_")                          
	
	#set count to zero for all sizes smaller than min size
	#x[is.na(x$SIZE_),]$SIZE_<-0
	x[x$SIZE_< (min_size),]$COUNT<-0
	
	#sum total number offishes per SIZE_
	y<-aggregate(x$COUNT,by=x[,pool_cols], sum)
	names(y)<-c(pool_cols, "COUNT")
	y$CS<-y$COUNT*y$SIZE_
	
	#now format this more or less as a crosstab, with field of interest as column variable
	y<-aggregate(y[,c("COUNT", "CS")],by=y[,base_cols], sum)
	y$MEAN_SIZE<-y$CS/y$COUNT
				
	y$GROUP_FIELD<-y[,grouping_field]
	z<-cast(y, SITEVISITID + METHOD ~ GROUP_FIELD, value="MEAN_SIZE", fill=NaN)	
	return(z)
	
} # end Calc_Site_MeanLengthByGroup


##calculate the biomass per m2 per record
Calc_Biomassgm2<-function(x){
	Biomassperfish<-x$LW_A*((x$SIZE_*x$LENGTH_CONVERSION_FACTOR)^x$LW_B)
	Biomassperrecord<-Biomassperfish*x$COUNT
	Area<-ifelse(x$METHOD %in% c("nSPC", "nSPC-CCR"), pi*(7.5^2), ifelse(x$SIZE_ < 20, 50, 100))
	return(Biomassperrecord/Area)
	
} #end Calc_Biomassgm2


##calculate the abundance per m2 per record
Calc_Abundm2<-function(x){
	Area<-ifelse(x$METHOD %in% c("nSPC", "nSPC-CCR"), pi*(7.5^2), ifelse(x$SIZE_ < 20, 50, 100))
	return(x$COUNT/Area)
	
} #end Calc_Abundm2


Calc_Site_Bio_By_SizeClass<-function(x, size_classes = c(0,10,20,30,40,50, Inf)){  
  # I would prefer this to be a function that can work with any value (species, family, trophic group etc.., am not there yet) 
	# function assumes that x is a data frame with at least the columns/elements listed in base_cols, plus the field_of_interest, in this case Trophic_MonRep
	# function returns a data frame with Site_VisitID, Method, and mean site biomass(gm2) per each value of the field_of_interest (hard wired as Trophic_MonRep for now!)
	
	#add a Biomassgm2 field to x
	x$Bio_gm2<-Calc_Biomassgm2(x)
  	##add a size class field to x
  	x$sizeclass<-cut(x$SIZE_, breaks = size_classes, include.lowest=TRUE)
 
	#Replicate ID is the base unit .. so pool up biomass at ReplicateID level, for the field of interest
	field_of_interest<-c("sizeclass") # this can later be a function parameter
	base_cols=c("SITEVISITID","METHOD","REP", "REPLICATEID") # minimum set of fields to build up from
	pool_cols<-c(base_cols,field_of_interest)                # minimum set, plus the one we are interested in
	
	#first calculate total biomass per rep for all values of this field
	y<-aggregate(x$Bio_gm2,by=x[,pool_cols], sum)
	names(y)<-c(pool_cols, "Bio_gm2")
	#now format this more or less as a crosstab, with field of interest as column variable
	y<-cast(y, SITEVISITID + METHOD + REP + REPLICATEID ~ sizeclass, fun.aggregate=sum, value="Bio_gm2", fill=0)
	
	#pool by Rep ("A","B","C" generally), then by site-survey (i.e. by SiteVisitID and Method)
	num_row_cols=length(base_cols)
	pool_cols<-c("SITEVISITID","METHOD","REP")
	y<-aggregate(y[,(num_row_cols+1):dim(y)[2]],by=y[,pool_cols], mean)
	num_row_cols=length(pool_cols) #working data now has fewer columns
	pool_cols<-c("SITEVISITID", "METHOD")
	y<-aggregate(y[,(num_row_cols+1):dim(y)[2]],by=y[,pool_cols], mean)
	
	return(y)
	
} # end Calc_Site_Bio_By_SizeClass

Calc_Site_Abund_By_SizeClass<-function(x, size_classes = c(0,10,20,30,40,50,Inf)){  
	#add an abundance field to x
	x$Abund_m2<-Calc_Abundm2(x)
  	##add a size class field to x
  	x$sizeclass<-cut(x$SIZE_, breaks = size_classes, include.lowest=TRUE)
 
	#Replicate ID is the base unit .. so pool up biomass at ReplicateID level, for the field of interest
	field_of_interest<-c("sizeclass") # this can later be a function parameter
	base_cols=c("SITEVISITID","METHOD","REP", "REPLICATEID") # minimum set of fields to build up from
	pool_cols<-c(base_cols,field_of_interest)                # minimum set, plus the one we are interested in
	
	#first calculate total biomass per rep for all values of this field
	y<-aggregate(x$Abund_m2,by=x[,pool_cols], sum)
	names(y)<-c(pool_cols, "Abund_m2")
	#now format this more or less as a crosstab, with field of interest as column variable
	y<-cast(y, SITEVISITID + METHOD + REP + REPLICATEID ~ sizeclass, fun.aggregate=sum, value="Abund_m2", fill=0)
	
	#pool by Rep ("A","B","C" generally), then by site-survey (i.e. by SiteVisitID and Method)
	num_row_cols=length(base_cols)
	pool_cols<-c("SITEVISITID","METHOD","REP")
	y<-aggregate(y[,(num_row_cols+1):dim(y)[2]],by=y[,pool_cols], mean)
	num_row_cols=length(pool_cols) #working data now has fewer columns
	pool_cols<-c("SITEVISITID", "METHOD")
	y<-aggregate(y[,(num_row_cols+1):dim(y)[2]],by=y[,pool_cols], mean)
	
	return(y)
	
} # end Calc_Site_Abund_By_SizeClass



# #********************
 Mode<- function(x) {
	ux<- unique(x)
	ux[which.max(tabulate(match(x, ux)))]
}

# #********************


# # #*****************************************************
# #*****************************************************
# ######## IDW - weights SE's equally ## this is short term temp func ##
# tmpPooledSE<- function(x) {
	# x<-na.omit(x)
	# sum.var<-sum(x^2)
	# return (sqrt(sum.var)/length(x))
# }
# #*****************************************************

##############################################################################################################################################
##### FINALLY ... generic abundance and biomass formulas .. these can do the jobs of the various ones above ef Calc_Site_Bio_By_Species, Calc_Site_Bio_By_Family
##############################################################################################################################################
Calc_Site_Abund<-function(x, grouping_field){  
	# function assumes that x is a data frame with at least the columns/elements listed in base_cols, plus the field_of_interest, in this case CommonFamily
	# function returns a data frame with Site_VisitID, Method, and mean site biomass(gm2) per each value of the field_of_interest (hard wired as CommonFamily for now!)
	
	#add an Abundance m2 field to x
	x$Abund_m2<-Calc_Abundm2(x)
	
	#create pooling field to use in cast functions below
	x$GROUP<-x[,grouping_field]
	
	#Replicate ID is the base unit .. so pool up biomass at ReplicateID level, for the grouping field passed in
	base_cols=c("SITEVISITID", "METHOD", "REP", "REPLICATEID") # minimum set of fields to build up from
	pool_cols<-c(base_cols, "GROUP")                    # minimum set, plus the one we are interested in
	
	#first calculate total abundance per rep for all values of this field
	y<-aggregate(x$Abund_m2,by=x[,pool_cols], sum)
	names(y)<-c(pool_cols, "Abund_m2")
	#now format this more or less as a crosstab, with field of interest as column variable
	y<-cast(y, SITEVISITID + METHOD + REP + REPLICATEID ~ GROUP, fun.aggregate=sum, value="Abund_m2", fill=0)
	
	#pool by Rep ("A","B","C" generally), then by site-survey (i.e. by SiteVisitID and Method)
	num_row_cols=length(base_cols)
	pool_cols<-c("SITEVISITID","METHOD","REP")
	y<-aggregate(y[,(num_row_cols+1):dim(y)[2]],by=y[,pool_cols], mean)
	num_row_cols=length(pool_cols) #working data now has fewer columns
	pool_cols<-c("SITEVISITID", "METHOD")
	y<-aggregate(y[,(num_row_cols+1):dim(y)[2]],by=y[,pool_cols], mean)
	
	return(y)
	
} # end Calc_Site_Abund

Calc_Site_Bio<-function(x, grouping_field){  
	# function assumes that x is a data frame with at least the columns/elements listed in base_cols, plus the field_of_interest, in this case CommonFamily
	# function returns a data frame with Site_VisitID, Method, and mean site biomass(gm2) per each value of the field_of_interest (hard wired as CommonFamily for now!)

	#add an Abundance m2 field to x
	x$Bio_gm2<-Calc_Biomassgm2(x)

	x$GROUP<-x[,grouping_field]
	
	#Replicate ID is the base unit .. so pool up biomass at ReplicateID level, for the field of interest
	base_cols=c("SITEVISITID","METHOD","REP", "REPLICATEID") # minimum set of fields to build up from
	pool_cols<-c(base_cols,"GROUP")                # minimum set, plus the one we are interested in
	
	#first calculate total biomass per rep for all values of this field
	y<-aggregate(x$Bio_gm2,by=x[,pool_cols], sum)
	names(y)<-c(pool_cols, "Bio_gm2")
	#now format this more or less as a crosstab, with field of interest as column variable
	y<-cast(y, SITEVISITID + METHOD + REP + REPLICATEID ~ GROUP, fun.aggregate=sum, value="Bio_gm2", fill=0)
	
	#pool by Rep ("A","B","C" generally), then by site-survey (i.e. by SiteVisitID and Method)
	num_row_cols=length(base_cols)
	pool_cols<-c("SITEVISITID","METHOD","REP")
	y<-aggregate(y[,(num_row_cols+1):dim(y)[2]],by=y[,pool_cols], mean)
	num_row_cols=length(pool_cols) #working data now has fewer columns
	pool_cols<-c("SITEVISITID", "METHOD")
	y<-aggregate(y[,(num_row_cols+1):dim(y)[2]],by=y[,pool_cols], mean)
	
	return(y)
} # end Calc_Site_Bio


#######################################################################################
# FUNCTION: CalcMeanSEMeanSHDiff
# Uses SUBSTRATE_HEIGHT_0 through SUBSTRATE_HEIGHT_150, plus MAX_HEIGHT variables in xx (passed in df) to calculate MEAN_SH [the weighted mean height], MEAN_SH_DIFF [the weighted mean deviance from that mean substrate height] and SD_SH [standard deviation of substrate heights around the mean height. NB I beleive that corrections should be applied for SD of binned data, but I am not sure how to to them. Hence this is an approximation that should be useful for now, but should be improved].
# MEAN_SH is the weighted mean of the substrate height categories .. SUBSTRATE_HEIGHTS_xx used to weight the mean of each height class (lowest class: 0->20cm mean:10cm; next:0-50cm mean:35cm; for largest class (>150cm), mean is average of 150cm and MAX_HEIGHT)
# MEAN_SH_DIFF is a measure of variability in height. In this case it is a weighted mean of the difference between the height class averages and the MEAN_SH weighted in the same way as above 
#.. thus if mean height is 0.43m, then substrate in the 0-20cm category (mean height of this class is 10cm) is 0.33m below the mean, substrate in the 20-50cm height class is 0.08m below the mean (mean is 0.35m), etc.. 
# .. average substrate height diff (MEAN_SH_DIFF) is the weighted means of the absolute values of thsoe differences, again weighted by the amount of substrate in each substrate height class
# Together with MAX_HEIGHT, those two values will give much information about the complexity of the replicate... and teh variaiblity in heights within the rep
#
# input should be a standard REA data frame, with the SUBSTRATE_HEIGHT_xx fields and MAX_HEIGHT
# values returned as list(MAX_SH, MAX_SH_DIFF, SD<SH)
#######################################################################################
CalcMeanSHMeanSHDiff<-function(xx) {

	#temp function to convert a string to a numeric and return 0 where it was NA
	clean_vals <- function(vals) {
		vals<-as.double(vals)
		vals[is.na(vals)]<-0
		return(vals)
	} #end clean_vals


	#first create a field to determine whether there is substrate hight info
	xx$SH_DATA_EXISTS<-TRUE
	xx$TOT_SH<-rowSums(xx[,c("SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150")])

	if(length(xx[is.na(xx$TOT_SH),]$SH_DATA_EXISTS)>0)
		xx[is.na(xx$TOT_SH),]$SH_DATA_EXISTS<-FALSE


	xx$MAX_HEIGHT <-clean_vals(xx$MAX_HEIGHT)
	xx$SUBSTRATE_HEIGHT_0<-clean_vals(xx$SUBSTRATE_HEIGHT_0)
	xx$SUBSTRATE_HEIGHT_20<-clean_vals(xx$SUBSTRATE_HEIGHT_20)
	xx$SUBSTRATE_HEIGHT_50<-clean_vals(xx$SUBSTRATE_HEIGHT_50)
	xx$SUBSTRATE_HEIGHT_100<-clean_vals(xx$SUBSTRATE_HEIGHT_100)
	xx$SUBSTRATE_HEIGHT_150<-clean_vals(xx$SUBSTRATE_HEIGHT_150)

	#create simpler substrate_height df to work with
	sh<-xx[,c("SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150")]

	#SUBSTRATE_HEIGHTS_XX should total to either 0 or 100 - next line checks that
	sh.tot<-rowSums(sh)
	num.bad<-length(sh[!sh.tot %in% c(0,100)])
	if (num.bad>0) 	
		print("Records with substrate not adding to 0 or 100 is", num.bad)

	n_records<-length(sh.tot)

	#create temp array to save weighting values for the substrate height totals
	mh<-sh
	mh$SUBSTRATE_HEIGHT_0<-0.1
	mh$SUBSTRATE_HEIGHT_20<-0.35
	mh$SUBSTRATE_HEIGHT_50<-0.75
	mh$SUBSTRATE_HEIGHT_100<-1.25
	mh$SUBSTRATE_HEIGHT_150<-apply(data.frame(rep(1.5,n_records), xx$MAX_HEIGHT),1,mean)

	#calculate the mean by multiplying the weighting (sh) by they height mid-points
	MEAN_SH<-rowSums(sh*mh)/100
	#NOW CALCULATE THE MEAN DIFF FROM THE MEAN
	sh_diff<- abs(mh - MEAN_SH)
	MEAN_SH_DIFF<-rowSums(sh_diff*sh)/100
	
	SD_SH_DIFF<-sqrt(rowSums((sh_diff^2)*(sh/100)))

	MEAN_SH[!xx$SH_DATA_EXISTS]<-NaN
	MEAN_SH_DIFF[!xx$SH_DATA_EXISTS]<-NaN
	SD_SH_DIFF[!xx$SH_DATA_EXISTS]<-NaN

	return(list(MEAN_SH, MEAN_SH_DIFF, SD_SH_DIFF))

} # CalcMeanSHMeanSHDiff

Calc_Site_Species_Richness<-function(x){  
  # I would prefer this to be a function that can work with any value (species, family, genera.., am not there yet) 
  # function returns a data frame with Site_VisitID, Method, and mean species_richness per Rep at the sute (Rep being an nSPC cylinder or a transect)	
  
  y<-aggregate(x$COUNT,by=x[,c("SITEVISITID", "METHOD", "REP", "SPECIES")], sum)	#convert to count per species per rep
  z<-aggregate(y$SPECIES,by=y[,c("SITEVISITID", "METHOD", "REP")], length)  		# count number of entries per rep	
  xx<-aggregate(z$x,by=z[,c("SITEVISITID", "METHOD")], mean)				  		# count number of entries per rep	
  dimnames(xx)[[2]]<-c("SITEVISITID", "METHOD", "SPECIESRICHNESS")
  
  return(xx)
  
}
# end Calc_Site_Species_Richness

Modified_Site_Species_Richness<-function(x){  
  # Modification fos tandard Calc_Site_Species_Richness to not count species with zero counts (as they can be left in data file to ensure that the site has data records at all) 
  y<-aggregate(x$COUNT,by=x[,c("SITEVISITID", "METHOD", "REP", "SPECIES")], sum)	#convert to count per species per rep
  y[y$x>1,]$x<-1																	#convert any non-zero count to 1, so we can sum those to get total number of species with count>0 
  z<-aggregate(y$x,by=y[,c("SITEVISITID", "METHOD", "REP")], sum)  		            # count number of species with non-zero counts this REP	
  xx<-aggregate(z$x,by=z[,c("SITEVISITID", "METHOD")], mean)				  		# count number of entries per rep	
  dimnames(xx)[[2]]<-c("SITEVISITID", "METHOD", "SPECIESRICHNESS")
  
  return(xx)
  
}
# end Modified_Site_Species_Richness

Calc_RepDiversity<-function(x){
#Returns H (SW Diversity), R (Species Richness) and R (Jaccard Evenness) per REPLICATEID
#requires package vegan
	a<-cast(x, REPLICATEID ~ SPECIES, value="COUNT", sum)
	ncols<-dim(a)[2]
	H<-diversity(a[,2:ncols])
	R<-specnumber(a[,2:ncols], MARGIN = 1)
	E<-H/log(R)
	
	return(data.frame(REPLICATEID=a$REPLICATEID, H=H, R=R, E=E))
} #end CalcRepDiversity

Calc_REP_Species_Richness<-function(x){  
  # I would prefer this to be a function that can work with any value (species, family, genera.., am not there yet) 
  # function returns a data frame with Site_VisitID, Method, and mean species_richness per Rep at the sute (Rep being an nSPC cylinder or a transect)	
  
  y<-aggregate(x$COUNT,by=x[,c("SITEVISITID", "METHOD", "SITE", "REP", "DIVER", "REPLICATEID", "SPECIES")], sum)	#convert to count per species per rep
  z<-aggregate(y$SPECIES,by=y[,c("SITEVISITID", "METHOD", "SITE", "REP", "DIVER", "REPLICATEID")], length)  		# count number of entries per rep	
  dimnames(z)[[2]]<-c("SITEVISITID", "METHOD", "SITE", "REP","DIVER", "REPLICATEID", "SPECIESRICHNESS")
  
  return(z)
  
}   # end Calc_REP_Species_Richness

Calc_REP_Bio<-function(x, grouping_field){  
  # function assumes that x is a data frame with at least the columns/elements listed in base_cols, plus the field_of_interest, in this case CommonFamily
  # function returns a data frame with Site_VisitID, Method, and mean site biomass(gm2) per each value of the field_of_interest (hard wired as CommonFamily for now!)
  #add an Abundance m2 field to x
  x$Bio_gm2<-Calc_Biomassgm2(x)
  
  x$GROUP<-x[,grouping_field]
  
  #Replicate ID is the base unit .. so pool up biomass at ReplicateID level, for the field of interest
  base_cols=c("SITEVISITID","METHOD","SITE", "REP", "DIVER", "REPLICATEID") # minimum set of fields to build up from
  pool_cols<-c(base_cols,"GROUP")                # minimum set, plus the one we are interested in
  
  #first calculate total biomass per rep for all values of this field
  y<-aggregate(x$Bio_gm2,by=x[,pool_cols], sum)
  names(y)<-c(pool_cols, "Bio_gm2")
  #now format this more or less as a crosstab, with field of interest as column variable
  y<-cast(y, SITEVISITID + METHOD + SITE + REP + DIVER + REPLICATEID ~ GROUP, fun.aggregate=sum, value="Bio_gm2", fill=0)
  
  return(y)
  
} # end Calc_REP_Bio


# REMAINING FUNCTIONS ARE FOR FISH QC. I AM NOT CERTAIN THAT THESE ARE THE MOST RECENT VERSION - WHICH I THINK ARE MORE LIEKLY TO BE IN THE MOST REcNET QC SCRIPTS THEMSELVES (IDW Nov 14th 2019)

# divervs
# divervsdiver ------------------------------------------------------------
###################################
# to compare the biomass estimates of one diver versus their buddy 
# required: working.data generated from fishbase, then specify arguments, year and region
# e.g. divervsdiver(working.data, 2012, "MHI")
# it produces two graphs in the working directory - one anonymous, one with diver initals
# 
# currently for each diver it substracts the difference of their estimate compared to their buddy at each site and boxplots the data
# Adel to return and improve to include a if else statement for 
# 1) anon or not
# 2) biomass ratio vs biomass difference (currently it calculates biomass difference)
# 
###################################

divervsdiver<-function(data, year, region){
  
  data$OBS_YEAR<-as.factor(data$OBS_YEAR)
  size_comp<-data[data$OBS_YEAR == year,] ## select year
  size_comp<-size_comp[size_comp$REGION == region,] ## select region
  size_comp<-droplevels(size_comp)
  
  divers<-levels(size_comp$DIVER)
  
  scr<-size_comp[,c("SITE", "DIVER", "BIOMASS_G_M2")] ### stripped down
  msd<-data.frame(with(scr, tapply(BIOMASS_G_M2, list(SITE, DIVER), sum))) ## mean biomass estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean because at some sites there are more than 2 divers)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% c(paste(i))), drop=FALSE], na.rm = TRUE)
    
    ## for diver 1 create a site level biomass ratio, the biomass of diver 1 / the biomass of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("biomass_ratio", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  p <- ggplot(toplot, aes(factor(diver), biomass_ratio))
  p + geom_boxplot() + coord_flip() + geom_jitter(colour = "red", size =1, alpha = 0.4) + labs(y = expression(paste("Difference in biomass estimate (g ", m^-2,") relative to buddy")), x = "Diver")# + scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename ="appendix_qc_divervsdiver_anon.pdf", width=13, height = 12.0, units = c("cm")))
  
  p <- ggplot(toplot, aes(factor(diver), biomass_ratio))
  p + geom_boxplot() + coord_flip() + geom_jitter(colour = "red", size =1, alpha = 0.4) + labs(y = expression(paste("Difference in biomass estimate (g ", m^-2,") relative to buddy")), x = "Diver") + scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename ="appendix_qc_divervsdiver.pdf", width=13, height = 12.0, units = c("cm")))
  
}   ## divervsdiver                                                                                                          

###################################to compare the biomass estimates of one diver versus their buddy 
# required: from working data in fishbase run Calc_Rep_Cover, Calc_Rep_Bio and Calc_Rep_Richness then merge region and years together with this
# currently this is set up for the status specify data, year and region e.g. divervsdiver(working.data, 2012, "MHI")
# it saves one png containing 3 graphs in the working directory - currently the one anonymous version

# note: best to come in and tweak the margins of graphs manually, same for anon vs diver named versions
###################################
divervsdiver4<-function(data, year, region, x_range){
  #data<-compdata
  #year<-"2014"
  #region<-"N.MARIAN"
  #x_range<-250
  data$OBS_YEAR<-as.factor(data$OBS_YEAR)
  size_comp<-data[data$OBS_YEAR == year,] ## select year
  size_comp<-size_comp[size_comp$REGION == region,] ## select region
  size_comp<-droplevels(size_comp)
  
  divers<-levels(size_comp$DIVER)
  
  scr<-size_comp[,c("SITE", "DIVER", "TotFish")] ### stripped down
  msd<-data.frame(with(scr, tapply(TotFish, list(SITE, DIVER), mean))) ## mean biomass estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean because at some sites there are more than 2 divers)
    #otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% c(paste(i))), drop=FALSE], na.rm = TRUE)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
    
    ## for diver 1 create a site level biomass ratio, the biomass of diver 1 / the biomass of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("biomass_ratio", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  bio_anon <- ggplot(toplot, aes(factor(diver), biomass_ratio))
  bio_anon <- bio_anon + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in biomass estimate (g ", m^-2,") relative to buddy")), x = "Diver")# + scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename = paste("divervsdiver_biomass",region,"_",year, "_anon.png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
  bio <- ggplot(toplot, aes(factor(diver), biomass_ratio))
  bio <- bio + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in biomass estimate (g ", m^-2,") relative to buddy")), x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_biomass",region,"_",year, "_.png", sep = ""), width=13, height = 14.0, units = c("cm")))     
  
  ######## diver vs diver for species richness (here number of species)
  
  scr<-size_comp[,c("SITE", "DIVER", "SPECIESRICHNESS")] ### stripped down
  msd<-data.frame(with(scr, tapply(SPECIESRICHNESS, list(SITE, DIVER), mean))) ## total number of species per diver at each site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean because at some sites there are more than 2 divers)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% c(paste(i))), drop=FALSE], na.rm = TRUE)
    
    ## for diver 1 create a site level biomass ratio, the biomass of diver 1 / the biomass of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("species", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  species_anon <- ggplot(toplot, aes(factor(diver), species))
  species_anon <- species_anon + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    labs(y = "Difference in species richness relative to buddy", x = "Diver")# + scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename = paste("divervsdiver_species",region,"_",year, "_anon.png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
  species <- ggplot(toplot, aes(factor(diver), species))
  species <- species + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    labs(y = "Difference in species richness relative to buddy", x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  
  
  ######## diver vs diver for coral estimates
  
  scr<-size_comp[,c("SITE", "DIVER", "HARD_CORAL")] ### stripped down
  msd<-data.frame(with(scr, tapply(HARD_CORAL, list(SITE, DIVER), mean))) ## mean biomass estimate per diver per site
  
  thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
  
  
  toplot<-list()
  
  for(i in 1:length(divers)){
    ##i<-(divers)[7]
    diver<-which(thenas[,i] == FALSE)
    
    ##select out the sites for diver from the dataframe
    diverdata<-msd[diver,i]
    
    ## get the other diver estimates per site (take mean because at some sites there are more than 2 divers)
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% c(paste(i))), drop=FALSE], na.rm = TRUE)
    
    ## for diver 1 create a site level biomass ratio, the biomass of diver 1 / the biomass of the other divers
    x<-as.vector(diverdata - otherdiver)
    toplot[[i]]<-x
    
  }
  
  toplot<-(melt(toplot))
  
  names(toplot)<-c("hard_coral", "diver")
  toplot$diver<-as.factor(toplot$diver)
  
  coral_anon <- ggplot(toplot, aes(factor(diver), hard_coral))
  coral_anon <- coral_anon + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    labs(y = "Difference in coral cover estimate (%) relative to buddy", x = "Diver")# + scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename = paste("divervsdiver_coral",region,"_",year, "_anon.png", sep = ""), width=13, height = 14.0, units = c("cm")))
  
  coral <- ggplot(toplot, aes(factor(diver), hard_coral))
  coral <- coral + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    labs(y = "Difference in coral cover estimate (%) relative to buddy", x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_coral",region,"_",year, "_.png", sep = ""), width=13, height = 12.0, units = c("cm")))
  
  png(filename = paste("QC_",region,"_",year,"_.png", sep = ""), width = 5, height = 9, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  multiplot(bio, species, coral)
  dev.off()
  
  png(filename = paste("QC_anon_",region,"_",year,"_.png", sep = ""), width = 5, height = 9, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  multiplot(bio_anon, species_anon, coral_anon)
  dev.off()
  
  
}        ## divervsdiver4 

### ## # Multiple plot function -------------------------------------------------------------
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}






