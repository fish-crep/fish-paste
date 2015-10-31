# reshape library inclues the cast() function used below
library(reshape)
library(ggplot2) ## to create the diver vs diver graphs

Calc_Site_MeanLength<-function(x, min_size=1){  
	# function assumes that x is a data frame with at least the columns/elements listed in base_cols, plus the field_of_interest, in this case CommonFamily
	# function returns a data frame with Site_VisitID, Method, and mean site biomass(gm2) per each value of the field_of_interest (hard wired as CommonFamily for now!)
		
	#Base unit will be the entire survey
	base_cols=c("SITEVISITID", "METHOD") 
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
		
	return(y[,c(base_cols, "MEAN_SIZE")])
	
} # end Calc_Site_MeanLength

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


Aggregate_InputTable<-function(x, field_list){  
	# function assumes that x is a data frame looking like our standard input
	# field_list is the list of fields to include (could be verything relating to each survey, or everything relating to a fish species)
	# function returns a data frame 
	
	y<-aggregate(x$COUNT,by=x[,field_list], sum)  # aggregate sums total count of all fishes per record, using field_list 
	y<-y[,field_list]                             # drop the count - was just using that to generate a summary table
	
	return(y)
	
} # end Aggregate_InputTables

#calc values which are means of sub-replicates per site (generally will be depth, coral cover, complexity etc...)
Calc_Site_Means<-function(x, survey_id_fields, rep_fields, count_fields, survey_data_fields){  
	# function assumes that x is a data frame with at least the fields mentioned in parameters survey_fields to survey_data_fields
	# survey_id_fields are fields that identify unique site-survey (now .. SiteVisitId and Method)
	# rep_fields are fields that identify the replicate BLT transect or nSPC pair (generally survey_id_fields plus "A","B","C")
	# count_fields are fields needed to sum/mean by the specific count (e.g. nSPC cont as part of a SPC-pair, or one side of a single transect)
	# survey_data_fields are the fields that we want to get mean oer survey eventually (after pooling up in this function)
	# function returns a data frame with survey_id_fields, and means of all the data columns

	#first average survey_data_fields for all replicates
	y<-aggregate(x[,survey_data_fields],by=x[,count_fields], mean)
	names(y)<-c(count_fields, survey_data_fields)
	
	#pool by Rep ("A","B","C" generally), then by survey (i.e. by SiteVisitID and Method)
	idx_first_data_field<-length(count_fields)+1
	y<-aggregate(y[,idx_first_data_field:dim(y)[2]],by=y[,rep_fields], mean)
	idx_first_data_field<-length(rep_fields)+1
	y<-aggregate(y[,idx_first_data_field:dim(y)[2]],by=y[,survey_id_fields], mean)
	
	return(y)
	
} # end Calc_SiteMeans


##calculate the biomass per m2 per record
Calc_Biomassgm2<-function(x){
	# IDW return y .. do not modify x inside the function .. just pass out biomassgm2
	# do this elsewhere - keep this function doing one thing - calculating biomassgm2 y$Srvy.Yr<-as.factor(y$Srvy.Yr)
	Biomassperfish<-x$LW_A*((x$SIZE_*x$LENGTH_CONVERSION_FACTOR)^x$LW_B)
	Biomassperrecord<-Biomassperfish*x$COUNT
	Area<-ifelse(x$METHOD %in% c("nSPC", "nSPC-CCR"), pi*(7.5^2), ifelse(x$SIZE_ < 20, 50, 100))
	return(Biomassperrecord/Area)
	
} #end Calc_Biomassgm2


##calculate the abundance per m2 per record
Calc_Abundm2<-function(x){
	# IDW return y .. do not modify x inside the function .. just pass out biomassgm2
	# do this elsewhere - keep this function doing one thing - calculating biomassgm2 y$Srvy.Yr<-as.factor(y$Srvy.Yr)
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
  # function assumes that x is a data frame with at least the columns/elements listed in base_cols, plus the field_of_interest, in this case Trophic_MonRep
   
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


##### IDW - this function superceeded by Aggregate_InputTable function - there is nothing in it that is specific to survey info (its a general function will summarize results for whatever set of fields is used as input)
Get_SurveyInfo<-function(x, field_list){  
	# function assumes that x is a data frame looking like our standard input
	# field_list is the list of fields to include (generally will be stratum, depth_zone, data, etc...)
	# function returns a data frame with base information per survey 
	
	y<-aggregate(x$Count,by=x[,field_list], sum)  # aggregate sums total count of all fishes per record, using field_list 
	y<-y[,field_list]                             # drop the count - was just using that to generate a summary table
	
	return(y)
	
} # end function

##### IDW - this function superceeded by Calc_SiteMeans function - a general function that does takes a list of fields as input, and generates site means for all of them
Calc_Site_MeanDepth<-function(x){  
	# function assumes that x is a data frame with at least the columns/elements listed in base_cols, plus the "Depth" field
	# function returns a data frame with Site_VisitID, Method, and mean depth 
	
	#Replicate ID is the base unit .. so pool up biomass at ReplicateID level, for the field of interest
	pool_cols=c("SiteVisitID","Method","Rep", "ReplicateID") # minimum set of fields to build up from
	first_data_col=length(pool_cols)+1
	
	#first extract depth for all replicates
	y<-aggregate(x$Depth,by=x[,pool_cols], mean)
	names(y)<-c(pool_cols, "Depth")
	#pool by Rep ("A","B","C" generally), then by site-survey (i.e. by SiteVisitID and Method)
	pool_cols<-c("SiteVisitID","Method","Rep")
	y<-aggregate(y[,first_data_col],by=y[,pool_cols], mean)
	num_row_cols=length(pool_cols) #working data now has fewer columns
	pool_cols<-c("SiteVisitID", "Method")
	y<-aggregate(y[,(num_row_cols+1):dim(y)[2]],by=y[,pool_cols], mean)
	
	return(y)
	
} # end function

#********************
Mode<- function(x) {
	ux<- unique(x)
	ux[which.max(tabulate(match(x, ux)))]
}

#********************

#********************
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

#********************


#calc values which are means of sub-replicates per site (generally will be depth, coral cover, complexity etc...)
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
	
} # end Calc_Site_nSurveysArea
#################

#*****************************************************
#*****************************************************
######## IDW - weights SE's equally ## this is short term temp func ##
tmpPooledSE<- function(x) {
	x<-na.omit(x)
	sum.var<-sum(x^2)
	return (sqrt(sum.var)/length(x))
}
#*****************************************************

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
	
	SD_SH<-sqrt(rowSums((sh_diff^2)*(sh/100)))

	MEAN_SH[!xx$SH_DATA_EXISTS]<-NaN
	MEAN_SH_DIFF[!xx$SH_DATA_EXISTS]<-NaN
	SD_SH[!xx$SH_DATA_EXISTS]<-NaN

	return(list(MEAN_SH, MEAN_SH_DIFF, SD_SH))

} # CalcMeanSHMeanSHDiff
#######################################################################################


######################################################
# function SiteNumLeadingZeros, adds leasing zeros to SITE code numeric parts to make eg OAH-1 become OAH-001
# this function therefore makes it easier to sort site names meaningfully and also removes the problem of eg MAR-22 site being treated in csv output as if it means March 22nd
# some site names have letter in the second 3 portion eg GAR-R3 .. those sites are not changed, because there re very few of those and those are generally well sorted anyway
# (and, it seems harder to work out which situations those are and how to deal with all possible variants .. therefore code just runs for situations where there are only digits in the part of the site name after the hyphen) 
#####################################################
SiteNumLeadingZeros <- function(site_names)
{	
	tmp<-levels(site_names)
	for (i in 1:length(tmp)) {
		s<-tmp[i]
		if (nchar(s)<9) {   # only change values where name length is too short ()
			ss<-strsplit(as.character(tmp[i]),"-")
			s1<-ss[[1]][1]
			s2<-ss[[1]][2]
			if (length(x=grep("[A-Z]",unlist(strsplit(toupper(s2),""))))==0) 
			{
				tmp[i]<-paste(s1, formatC(as.numeric(s2), width=5, flag="0"), sep="-")
			}
		}
	}
	levels(site_names)<-tmp

	return(site_names)
} #SiteNumLeadingZeros




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



# 
# Calculate site cover %, by averaging all photos within a REP, then all REPS within a SITE and YEAR .. do that at either tier 1 or tier 2 level, depending on the value of the tier parameter
#
# NB PHOTO_FIELDS<-c("SITE", "OBS_YEAR", "REP", "PHOTOID", "TIER_1", "TIER_2", "POINTS") .. those originally come from the Oracle StrRdm BIA view .. and the input value to this function (ie bia_data) should be a data frame with those fields..
#
#
Site_BIA<-function (bia_data, tier="TIER_1")
{
	bia_data$TIER<-bia_data[,tier]
	t_lev<-levels(bia_data$TIER)
	#format as a crosstab, with tier as column variable to create one record per photo, to allow for averaging and so on ..
	xx<-cast(bia_data, OBS_YEAR + SITE + REP + PHOTOID ~ TIER, value="POINTS", fill=0, fun.aggregate=sum)
	# now convert to percentages
	xx$TotPoints<-rowSums(xx[,t_lev])
	xx[,t_lev]<-xx[,t_lev]/xx$TotPoints
	#now average within YEAR, SITE, REP
	xx<-aggregate(xx[, t_lev],by=xx[,c("OBS_YEAR","SITE", "REP")], mean)
	#now average within YEAR, SITE
	xx<-aggregate(xx[, t_lev],by=xx[,c("OBS_YEAR","SITE")], mean)
	
	return(xx)
	
} #Site_BIA


#calc values which are means of sub-replicates per site (generally will be depth, coral cover, complexity etc...)
Calc_REP_nSurveysArea<-function(x, survey_id_fields, rep_fields, count_fields, survey_data_fields){  
  
  RETURN_FIELDS<-c("nCounts", "nReps")
  
  #first average survey_data_fields for all replicate-counts
  y<-aggregate(x[,survey_data_fields],by=x[,count_fields], mean)
  
  #	#idw if a rep has NAs in the data fields, then try to set them to the average of other counts in that rep
  #	y<-fill.NA.count(y, rep_fields, survey_data_fields)
  
  y$nCounts<-1
  y$nReps<-1
  names(y)<-c(count_fields, survey_data_fields, RETURN_FIELDS)
  
  #pool by Rep ("A","B","C" generally), then by survey (i.e. by SiteVisitID and Method)
  #	idx_first_data_field<-length(count_fields)+1#
  #	z<-aggregate(y[,idx_first_data_field:dim(y)[2]],by=y[,rep_fields], sum)
  #	y<-aggregate(y[,idx_first_data_field:dim(y)[2]],by=y[,rep_fields], mean)
  #	y$nCounts<-z$nCounts
  
  #	idx_first_data_field<-length(rep_fields)+1
  #	z<-aggregate(y[,idx_first_data_field:dim(y)[2]],by=y[,survey_id_fields], sum)
  #	y<-aggregate(y[,idx_first_data_field:dim(y)[2]],by=y[,survey_id_fields], mean)
  
  #	y$nReps<-z$nReps
  #	y$nCounts<-z$nCounts
  
  return(y)
  
} # end Calc_REP_nSurveysArea
#################

Calc_REP_Species_Richness<-function(x){  
  # I would prefer this to be a function that can work with any value (species, family, genera.., am not there yet) 
  # function returns a data frame with Site_VisitID, Method, and mean species_richness per Rep at the sute (Rep being an nSPC cylinder or a transect)	
  
  y<-aggregate(x$COUNT,by=x[,c("SITEVISITID", "METHOD", "SITE", "REP", "DIVER", "REPLICATEID", "SPECIES")], sum)	#convert to count per species per rep
  z<-aggregate(y$SPECIES,by=y[,c("SITEVISITID", "METHOD", "SITE", "REP", "DIVER", "REPLICATEID")], length)  		# count number of entries per rep	
  dimnames(z)[[2]]<-c("SITEVISITID", "METHOD", "SITE", "REP","DIVER", "REPLICATEID", "SPECIESRICHNESS")
  
  return(z)
  
}
# end Calc_REP_Species_Richness

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



# divervsdiver3 ------------------------------------------------------------
###################################to compare the biomass estimates of one diver versus their buddy 
# required: from working data in fishbase run Calc_Rep_Cover, Calc_Rep_Bio and Calc_Rep_Richness then merge region and years together with this
# currently this is set up for the status specify data, year and region e.g. divervsdiver(working.data, 2012, "MHI")
# it saves one png containing 3 graphs in the working directory - currently the one anonymous version

# note: best to come in and tweak the margins of graphs manually, same for anon vs diver named versions
###################################
divervsdiver3<-function(data, year, region, x_range){
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
    otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% c(paste(i))), drop=FALSE], na.rm = TRUE)
    
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
  suppressMessages(ggsave(filename = paste("divervsdiver_biomass",region,"_",year, "_anon.png", sep = ""), width=13, height = 12.0, units = c("cm")))
  
  bio <- ggplot(toplot, aes(factor(diver), biomass_ratio))
  bio <- bio + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    scale_y_continuous(limits=c(-x_range,x_range))+
    labs(y = expression(paste("Difference in biomass estimate (g ", m^-2,") relative to buddy")), x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_biomass",region,"_",year, "_.png", sep = ""), width=13, height = 12.0, units = c("cm")))     
  
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
  suppressMessages(ggsave(filename = paste("divervsdiver_species",region,"_",year, "_anon.png", sep = ""), width=13, height = 12.0, units = c("cm")))
  
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
  suppressMessages(ggsave(filename = paste("divervsdiver_coral",region,"_",year, "_anon.png", sep = ""), width=13, height = 12.0, units = c("cm")))
  
  coral <- ggplot(toplot, aes(factor(diver), hard_coral))
  coral <- coral + 
    geom_boxplot() + 
    coord_flip() + 
    geom_jitter(colour = "red", size =1, alpha = 0.4) + 
    theme_bw() +
    labs(y = "Difference in coral cover estimate (%) relative to buddy", x = "Diver") + 
    scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
  suppressMessages(ggsave(filename =paste("divervsdiver_coral",region,"_",year, "_.png", sep = ""), width=13, height = 12.0, units = c("cm")))
  
  png(filename = paste("QC_",region,"_",year,"_.png", sep = ""), width = 5, height = 7, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  multiplot(bio, species, coral)
  dev.off()
  
  png(filename = paste("QC_anon_",region,"_",year,"_.png", sep = ""), width = 5, height = 7, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  multiplot(bio_anon, species_anon, coral_anon)
  dev.off()
  
  
}        ## divervsdiver3   


### ## # Multiple plot function -------------------------------------------------------------

###
#
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




