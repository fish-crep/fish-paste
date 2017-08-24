# Functions for data provided NOAA Ecosystem Sciences Division
# Stationary point count survey data collected for the NOAA Pacific Reef Assessment and Monitoring Program
# 2010-2016 only
# anonymous diver and cleaned data to be shared publically

# To cite data: 
# insert citation once we have it

### -------------------------------------------------------------
### GENERATE SITE SURVEY METADATA FROM REP LEVEL DATA
# To use: Aggregate_InputTable(datafile, list of fields to be included)
# Returns dataframe of site level survey data
### -------------------------------------------------------------

Aggregate_InputTable<-function(x, field_list){  
	# function assumes that x is a data frame looking like our standard input
	# field_list is the list of fields to include (could be verything relating to each survey, or everything relating to a fish species)
	# function returns a data frame 
	
	y<-aggregate(x$COUNT,by=x[,field_list], sum)  # aggregate sums total count of all fishes per record, using field_list 
	y<-y[,field_list]                             # drop the count - was just using that to generate a summary table
	
	return(y)
	
} # End Aggregate_InputTables


### -------------------------------------------------------------
### FILL MISSING DATA FIELDS (NAs) TO AVERAGE OF NON- NA values
# When a diver failed to record benthic or survey data we set it to the mean value from the other diver in the REP
# Support function for Calc_Site_nSurveysArea functions 
# Will not be called directly by user
### -------------------------------------------------------------

fill.NA.count<- function(x, rep_fields, survey_data_fields) {
	
	all.reps<-aggregate(x, by=x[,rep_fields], length)
	#go through rep by rep, setting NAs to average of non-NA values
	for(i in 1:dim(all.reps)[1]){
		good.data<-array(NA,dim=length(survey_data_fields))
		yy<-x
		for(j in 1:length(rep_fields)){
			yy<-subset(yy, yy[,rep_fields[j]]==all.reps[i,rep_fields[j]])
		}
		#determine the means of values with data
		good.data<-colMeans(yy[,survey_data_fields], na.rm=TRUE)
		n.survey.fields<-length(survey_data_fields)
		for(k in 1:n.survey.fields){
			yy[is.na(yy[,survey_data_fields[k]]), survey_data_fields[k]]<-good.data[k]
		}
		#now write yy back into x
		n.y.rows<-dim(yy)[1]
		for(m in 1:n.y.rows){
			x[x$REPLICATEID == yy$REPLICATEID[m],survey_data_fields]<-yy[m ,survey_data_fields]
		}
	}
	return(x)
}




### -------------------------------------------------------------
### GENERATE REP LEVEL COMPLEXITY FROM HEIGHT BIN ESTIMATES
## To use: CalcMeanSEMeanSHDiff(datafile)
## returns a list with 3 elements MEAN_SH, MEAN_SH_DIFF, SD_SH_DIFF
## MEAN_SH is the weighted mean of the substrate height categories .. SUBSTRATE_HEIGHTS_xx used to weight the mean of each height class (lowest class: 0->20cm mean:10cm; next:0-50cm mean:35cm; for largest class (>150cm), mean is average of 150cm and MAX_HEIGHT)
## MEAN_SH_DIFF is a measure of variability in height. In this case it is a weighted mean of the difference between the height class averages and the MEAN_SH weighted in the same way as above thus if mean height is 0.43m, then substrate in the 0-20cm category (mean height of this class is 10cm) is 0.33m below the mean, substrate in the 20-50cm height class is 0.08m below the mean (mean is 0.35m), etc..
## MEAN_SH_DIFF is the weighted means of the absolute values of the differences in average substrate heights, again weighted by the amount of substrate in each substrate height class
### -------------------------------------------------------------

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

	list1<-list(MEAN_SH, MEAN_SH_DIFF, SD_SH_DIFF)
	names(list1)<-c("MEAN_SH", "MEAN_SH_DIFF", "SD_SH_DIFF")
		return(list1)

} # End CalcMeanSHMeanSHDiff

### -------------------------------------------------------------
### GENERATE SITE DEPTH AND BENTHIC INFO FROM REP LEVEL DATA
# To use: Calc_Site_nSurveysArea(datafile, survey_id_fields, rep_fields, count_fields, survey_data_fields)
# survey_data_fields generally will be depth, coral cover, complexity etc...)
# returns dataframe with SITEVISITID, METHOD and SITE survey data fields
# nCounts = number of individual cylinder surveys per site nREPS = number paired cylinders per site
### -------------------------------------------------------------

Calc_Site_nSurveysArea<-function(x, survey_id_fields, rep_fields, count_fields, survey_data_fields){  
	# function assumes that x is a data frame with at least the fields mentioned in parameters survey_fields to survey_data_fields
	# survey_id_fields are fields that identify unique site-survey (now .. SiteVisitId and Method)
	# rep_fields are fields that identify the replicate BLT transect or nSPC pair (generally survey_id_fields plus "A","B","C")
	# count_fields are fields needed to sum/mean by the specific count (e.g. nSPC cont as part of a SPC-pair, or one side of a single transect)
	# survey_data_fields are the fields that we want to get mean oer survey eventually (after pooling up in this function)
	# function returns a data frame with survey_id_fields, and means of all the data columns
 
	RETURN_FIELDS<-c("nCounts", "nReps")

	#first average survey_data_fields for all replicate-counts
	y<-aggregate(x[,survey_data_fields],by=x[,count_fields], mean)
	
	#idw if a rep has NAs in the data fields, then try to set them to the average of other counts in that rep
	y<-fill.NA.count(y, rep_fields, survey_data_fields)

	y$nCounts<-1
	y$nReps<-1
	names(y)<-c(count_fields, survey_data_fields, RETURN_FIELDS)
	
	#pool by Rep ("A","B","C" generally), then by survey (i.e. by SiteVisitID and Method)
	idx_first_data_field<-length(count_fields)+1
	z<-aggregate(y[,idx_first_data_field:dim(y)[2]],by=y[,rep_fields], sum)
	y<-aggregate(y[,idx_first_data_field:dim(y)[2]],by=y[,rep_fields], mean)
	y$nCounts<-z$nCounts

	idx_first_data_field<-length(rep_fields)+1
	z<-aggregate(y[,idx_first_data_field:dim(y)[2]],by=y[,survey_id_fields], sum)
	y<-aggregate(y[,idx_first_data_field:dim(y)[2]],by=y[,survey_id_fields], mean)

	y$nReps<-z$nReps
	y$nCounts<-z$nCounts
	
	return(y)
	
} # End Calc_Site_nSurveysArea

### -------------------------------------------------------------
### GENERATE SITE FISH BIOMASS
# To use: Calc_Site_Bio(datafile, grouping_field)
# grouping_field could be CONSUMER_GROUP, SPECIES, COMMON_FAMILY must be in datafile
# returns dataframe with SITEVISITID, METHOD and MEAN SITE biomass (g per meter2 per grouping_field)
### -------------------------------------------------------------

Calc_Site_Bio<-function(x, grouping_field){  

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
	
} # End Calc_Site_Bio

### -------------------------------------------------------------
### CALCULATE BIOMASS PER M2 PER RECORD
# Support function for Calc_Site_Bio functions 
# Will not be called directly by user
### -------------------------------------------------------------

Calc_Biomassgm2<-function(x){
	# IDW return y .. do not modify x inside the function .. just pass out biomassgm2
	# do this elsewhere - keep this function doing one thing - calculating biomassgm2 y$Srvy.Yr<-as.factor(y$Srvy.Yr)
	Biomassperfish<-x$LW_A*((x$SIZE_*x$LENGTH_CONVERSION_FACTOR)^x$LW_B)
	Biomassperrecord<-Biomassperfish*x$COUNT
	Area<-ifelse(x$METHOD %in% c("nSPC", "nSPC-CCR"), pi*(7.5^2), ifelse(x$SIZE_ < 20, 50, 100))
	return(Biomassperrecord/Area)
	
} # End Calc_Biomassgm2


### -------------------------------------------------------------
### CALCULATE SITE FISH ABUNDANCE
# To use: Calc_Site_Abund(datafile, grouping_field)
# grouping_field could be CONSUMER_GROUP, SPECIES, COMMON_FAMILY must be in datafile
# Returns dataframe with SITEVISITID, METHOD and MEAN SITE abundance (g per meter2 per grouping_field)
### -------------------------------------------------------------

Calc_Site_Abund<-function(x, grouping_field){  
	
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
	
} # End Calc_Site_Abund

### -------------------------------------------------------------
### CALCULATE ABUNDANCE PER M2 PER RECORD
# Suport function for Calc_Site_Abund functions
# Will not be called directly by user
### -------------------------------------------------------------

Calc_Abundm2<-function(x){
	# IDW return y .. do not modify x inside the function .. just pass out biomassgm2
	# do this elsewhere - keep this function doing one thing - calculating biomassgm2 y$Srvy.Yr<-as.factor(y$Srvy.Yr)
	Area<-ifelse(x$METHOD %in% c("nSPC", "nSPC-CCR"), pi*(7.5^2), ifelse(x$SIZE_ < 20, 50, 100))
	return(x$COUNT/Area)
	
} # End Calc_Abundm2

### -------------------------------------------------------------
### CALCULATE BIOMASS PER M2 PER SIZE CLASS
# To use: Calc_Site_Bio_By_SizeClass(datafile, size_classes)
# size_classes is a vector of number cut offs
# returns dataframe with SITEVISITID mean estimates
### -------------------------------------------------------------

Calc_Site_Bio_By_SizeClass<-function(x, size_classes = c(0,10,20,30,40,50, Inf)){  
 	
	#add a Biomassgm2 field to x
	x$Bio_gm2<-Calc_Biomassgm2(x)
  	##add a size class field to x
  	x$sizeclass<-cut(x$SIZE_TL_CM, breaks = size_classes, include.lowest=TRUE)
 
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
	
} # End Calc_Site_Bio_By_SizeClass

### -------------------------------------------------------------
### CALCULATE ABUNDANCE PER M2 PER SIZE CLASS
# To use: Calc_Site_Abund_By_SizeClass(datafile, size_classes)
# datafile contains base_cols ("SITEVISITID","METHOD","REP", "REPLICATEID") and pool cols 
# size_classes is a vector to create size classes
# returns dataframe with SITEVISITID mean estimates
### -------------------------------------------------------------

Calc_Site_Abund_By_SizeClass<-function(x, size_classes = c(0,10,20,30,40,50,Inf)){  
   
	#add an abundance field to x
	x$Abund_m2<-Calc_Abundm2(x)
  	##add a size class field to x
  	x$sizeclass<-cut(x$SIZE_TL_CM, breaks = size_classes, include.lowest=TRUE)
 
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
	
} # End Calc_Site_Abund_By_SizeClass

### -------------------------------------------------------------
### CALCULATE MEAN LENGTH OF FISH 
# To use: Calc_Site_Abund_By_SizeClass(datafile, minimum size)
# datafile contains base_cols (SITEVISITID) and pool cols SIZE_TL_CM
# fish smaller than minimum size are not included i.e removes small recruits
# returns dataframe with SITEVISITID mean estimates
### -------------------------------------------------------------

Calc_Site_MeanLength<-function(x, min_size=1){  
	# function assumes that x is a data frame with at least the columns/elements listed in base_cols, plus the field_of_interest, in this case CommonFamily
	# function returns a data frame with Site_VisitID, Method, and mean size of fish (total length in cm)
		
	#Base unit will be the entire survey
	base_cols=c("SITEVISITID", "METHOD") 
	pool_cols<-c(base_cols, "SIZE_TL_CM")                          
	
	#set count to zero for all sizes smaller than min size
	x[x$SIZE_TL_CM < (min_size),]$COUNT<-0
	
	#sum total number offishes per SIZE_
	y<-aggregate(x$COUNT,by=x[,pool_cols], sum)
	names(y)<-c(pool_cols, "COUNT")
	y$CS<-y$COUNT*y$SIZE_TL_CM
	
	#now format this more or less as a crosstab, with field of interest as column variable
	y<-aggregate(y[,c("COUNT", "CS")],by=y[,base_cols], sum)
	y$MEAN_SIZE<-y$CS/y$COUNT
		
	return(y[,c(base_cols, "MEAN_SIZE")])
	
} # End Calc_Site_MeanLength

### -------------------------------------------------------------
### CALCULATE FISH SPECIES RICHNESS
# To use: Calc_Site_Species_Rich(datafile)
# Datafile contains cols "SITEVISITID", "METHOD", "REP", "SPECIES"
# Species here means the number of unique species
# Returns dataframe with SITEVISITID mean estimate
### -------------------------------------------------------------

Calc_Site_Species_Rich<-function(x){  
  # Modification fos tandard Calc_Site_Species_Richness to not count species with zero counts (as they can be left in data file to ensure that the site has data records at all) 
  y<-aggregate(x$COUNT,by=x[,c("SITEVISITID", "METHOD", "REP", "SPECIES")], sum)	#convert to count per species per rep
  y[y$x>1,]$x<-1																	#convert any non-zero count to 1, so we can sum those to get total number of species with count>0 
  z<-aggregate(y$x,by=y[,c("SITEVISITID", "METHOD", "REP")], sum)  		            # count number of species with non-zero counts this REP	
  xx<-aggregate(z$x,by=z[,c("SITEVISITID", "METHOD")], mean)				  		# count number of entries per rep	
  dimnames(xx)[[2]]<-c("SITEVISITID", "METHOD", "SPECIESRICHNESS")
  
  return(xx)
  
}
# end Calc_Site_Species_Rich
