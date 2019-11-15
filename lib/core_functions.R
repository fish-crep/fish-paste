# reshape library inclues the cast() function used below
library(reshape2)
library(plyr)
#library(ggplot2) ## to create the diver vs diver graphs


#Old Input table function- the aggregate function is an older function that can't handle NAs.
Aggregate_InputTable<-function(x, field_list){  
	# function assumes that x is a data frame looking like our standard input
	# field_list is the list of fields to include (could be verything relating to each survey, or everything relating to a fish species)
	# function returns a data frame 
	
	y<-aggregate(x$SITEVISITID,by=x[,field_list], sum)  # aggregate sums total count of all fishes per record, using field_list 
	y<-y[,field_list]                             # drop the count - was just using that to generate a summary table
	
	return(y)
	
} # end Aggregate_InputTables

#New Input table function- this new function uses ddply to summary data and can handle NAs.
new_Aggregate_InputTable<-function(data, field_list) {
  # function assumes that x is a data frame looking like our standard input
  # field_list is the list of fields to include (could be verything relating to each survey, or everything relating to a fish species)
  # function returns a data frame 
    y <- ddply(data,field_list, summarize,count=sum(SITEVISITID,na.rm = T))
    y<-y[,field_list] # drop the count - was just using that to generate a summary table
    return(y)
} # end Aggregate_InputTables


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
	
	#add dummy field to ensure that we dont aggregate on only one data columns ever - as that leads to problems with column names
	x$xDUMMYx<-0
	survey_data_fields<-c(survey_data_fields, "xDUMMYx")
	

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

	y$xDUMMYx<-NULL		#getting rid of this filler column
	
	return(y)
	
} # end Calc_Site_nSurveysArea
#################


Calc_REP_nSurveysArea<-function(x, survey_id_fields, rep_fields, count_fields, survey_data_fields){  
 
	RETURN_FIELDS<-c("nCounts", "nReps")

	#first average survey_data_fields for all replicate-counts
	y<-aggregate(x[,survey_data_fields],by=x[,count_fields], mean)
	

	y$nCounts<-1
	y$nReps<-1
	names(y)<-c(count_fields, survey_data_fields, RETURN_FIELDS)
	
	return(y)
	
} # end Calc_REP_nSurveysArea


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


# # 
# # Calculate site cover %, by averaging all photos within a REP, then all REPS within a SITE and YEAR .. do that at either tier 1 or tier 2 level, depending on the value of the tier parameter
# #
# # NB PHOTO_FIELDS<-c("SITE", "OBS_YEAR", "REP", "PHOTOID", "TIER_1", "TIER_2", "POINTS") .. those originally come from the Oracle StrRdm BIA view .. and the input value to this function (ie bia_data) should be a data frame with those fields..
# #
# #
# Site_BIA<-function (bia_data, tier="TIER_1")
# {
	# bia_data$TIER<-bia_data[,tier]
	# t_lev<-levels(bia_data$TIER)
	# #format as a crosstab, with tier as column variable to create one record per photo, to allow for averaging and so on ..
	# xx<-cast(bia_data, OBS_YEAR + SITE + REP + PHOTOID ~ TIER, value="POINTS", fill=0, fun.aggregate=sum)
	# # now convert to percentages
	# xx$TotPoints<-rowSums(xx[,t_lev])
	# xx[,t_lev]<-xx[,t_lev]/xx$TotPoints
	# #now average within YEAR, SITE, REP
	# xx<-aggregate(xx[, t_lev],by=xx[,c("OBS_YEAR","SITE", "REP")], mean)
	# #now average within YEAR, SITE
	# xx<-aggregate(xx[, t_lev],by=xx[,c("OBS_YEAR","SITE")], mean)
	
	# return(xx)
	
# } #Site_BIA






