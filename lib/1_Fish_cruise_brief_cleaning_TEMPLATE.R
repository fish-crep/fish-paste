rm(list=ls())
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

#####    CLEANING DATA  ########################
################################################

# LOAD FUNCTIONS FOR DATA CLEANING
setwd("/Volumes/TOSHIBA EXT/CRED/Base_R/Base_R_Functions")
source("fish_team_functions.R")
source("Islandwide Mean&Variance Functions.R")

# get strata and sectors data - NB - the data in the raw file should be checked and updated
#setwd("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/data")
setwd("/Volumes/TOSHIBA EXT/CRED/Base_R/Base_Data_Files")
sectors<-read.csv("Sectors-Strata-Areas2015.csv", stringsAsFactors=FALSE)

# load summary data from cruise
setwd("/Volumes/TOSHIBA EXT/CRED/fish_cruise_routine_report/monitoring_brief/2016_sunnary_brief_cruise_reports/MHI2016")
df<-read.csv("FishBaseforR_MHI2016.csv")
head(df)
x<-df
colnames(x)<-toupper(colnames(x))

# HOUSEKEEPING ------------------------------------------------------------
# clean up the data to only fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION" , "REGION_NAME", 
	"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH",
	"HARD_CORAL", "MA",  "CCA",  "SAND", "OTHERBENTHIC","COMPLEXITY",
	"SPECIES", "COUNT", "SIZE_", "OBS_TYPE", 
	"SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150", "MAX_HEIGHT",
	"SCIENTIFIC_NAME",  "TAXONNAME", "COMMONNAME", "GENUS", "FAMILY" , "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#generate a simple "Strata" field, by concatenating Stratum and Depth fields
x$STRATA<-paste(x$REEF_ZONE, x$DEPTH_BIN, sep='')

# Update SITE to have three numeric digits (eg OAH-01 becomes OAH-001)
x$SITE<-SiteNumLeadingZeros(x$SITE)

#x<-subset(x, x$TRAINING_YN==FALSE)   # we do need to add TRAINING_YN into the Access code, and remove those
x<-subset(x, x$METHOD %in% c("nSPC"))
x<-subset(x, x$OBS_TYPE %in% c("U","I","N"))

############################################################################################
###### where there is substrate_height data, work out average height && ave_height_variability so that we get standardized complexity metrics (mean hieght, mean height variability, max-height) 
sh_out<-CalcMeanSHMeanSHDiff(x)
x$MEAN_SH<-sh_out[[1]]

# remove the component SUBSTRATE_HEIGHT fields
x<-x[, setdiff(names(x),c("SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150"))]
############################################################################################
x<-droplevels(x)

#######################
## CLEAN UP NAs #######
#######################
tmp.lev<-levels(x$HABITAT_CODE); head(tmp.lev)
levels(x$HABITAT_CODE)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$SCIENTIFIC_NAME); head(tmp.lev)
levels(x$SCIENTIFIC_NAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$COMMONNAME); head(tmp.lev)
levels(x$COMMONNAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$GENUS); head(tmp.lev)
levels(x$GENUS)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$FAMILY); head(tmp.lev)
levels(x$FAMILY)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$COMMONFAMILYALL); head(tmp.lev)
levels(x$COMMONFAMILYALL)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$TROPHIC_MONREP); head(tmp.lev)
levels(x$TROPHIC_MONREP)<-c(tmp.lev, "UNKNOWN")

x[is.na(x$HABITAT_CODE),"HABITAT_CODE"]<-"UNKNOWN"
x[is.na(x$SCIENTIFIC_NAME),"SCIENTIFIC_NAME"]<-"UNKNOWN"
x[is.na(x$COMMONNAME),"COMMONNAME"]<-"UNKNOWN"
x[is.na(x$GENUS),"GENUS"]<-"UNKNOWN"
x[is.na(x$FAMILY),"FAMILY"]<-"UNKNOWN"
x[is.na(x$COMMONFAMILYALL),"COMMONFAMILYALL"]<-"UNKNOWN"
x[is.na(x$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"

# WORKING WITH POOLING READY DATA FROM HERE ON  -------------------------------------
########################################################################################################################
### MAIN DATA FILTERING AND CLEANING IS DONE ..  SHOULD BE READY TO START WORKING WITH THE DATA FROM HERE ON ###########

# we want to keep x as cleaned up data.. and only work with a 'working data' version of that. Calling that working data 'wd'
wd<-x

#check for NAs and **** FOR NOW **** set those to zero, so rows do not get dropped by the aggregate function (below)
## the error returned are ok to ignore, just refers to invalid factor levels
wd[is.na(wd)]<-0 

#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

#get base survey info, calculate average depth+complexity+so on
SURVEY_INFO<-c("OBS_YEAR", "REGION", "REGION_NAME", "ISLAND", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "STRATA", "LATITUDE", "LONGITUDE", "SITEVISITID", "METHOD")
wd$ANALYSIS_STRATA<-paste(wd$REEF_ZONE,wd$DEPTH_BIN,sep='')
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)
island_table<-Aggregate_InputTable(wd, c("REGION","ISLAND"))
SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL", "MA", "CCA", "SAND", "OTHERBENTHIC", "MEAN_SH", "MAX_HEIGHT")

#NOTE THAT BENTHOS DOES NOT ALWAYS SUM TO 100% .. I THINK BECAUSE OF ERRORS IN THE ORIGINAL DATA ENTERED INTO THE DATABASE. NEW CODE BELOW IS AN ATTEMPT TO FIX THAT
# Go through all surveys checking for situation where some reps have NAs in a particular BENTHIC_FIELDS, but other records have non-zeros - in that situation, we were recording a field but one other diver left it balnk - those should be zeros not NAs
# this is something that should really be fixed in the database rather than here (as its an error at time of data entry)
#### BELOW code does the job, but should be cleaned up and put in a function
BENTHIC_FIELDS<-c("HARD_CORAL", "MA", "CCA", "SAND", "OTHERBENTHIC")
UNIQUE_ROUND<-c("REGION", "OBS_YEAR", "METHOD")
round_table<-Aggregate_InputTable(wd, UNIQUE_ROUND)

wd$countBD<-apply(wd[,BENTHIC_FIELDS], 1, function(xx) length(which(!is.na(xx))))  #IDW 10-22-2013 checking for situation where there is NO benthic data at all
for(i in 1:dim(round_table)[1])
{
	if(round_table[i,"METHOD"]=="nSPC")
	{
		tmp_data<-wd[wd$OBS_YEAR==round_table[i,"OBS_YEAR"] & wd$METHOD==round_table[i,"METHOD"] & wd$REGION==round_table[i,"REGION"],]

		#go through BENTHIC_FIELDS, checking whether there are some NAs and some data values
		for(j in 1:length(BENTHIC_FIELDS))
		{
			## IF there are both non NAs and NAs
			if(length(tmp_data[!is.na(tmp_data[,BENTHIC_FIELDS[j]]),BENTHIC_FIELDS[j]]) > 0 
			        & length(tmp_data[is.na(tmp_data[,BENTHIC_FIELDS[j]]),BENTHIC_FIELDS[j]]) > 0) 
			{
				#set all NAs of that field to 0
				tmp_data[is.na(tmp_data[,BENTHIC_FIELDS[j]]),BENTHIC_FIELDS[j]]<-0	

				#now rewrite the benthic fields with NAs converted to zeros
				wd[wd$OBS_YEAR==round_table[i,"OBS_YEAR"] & wd$METHOD==round_table[i,"METHOD"] & wd$REGION==round_table[i,"REGION"],BENTHIC_FIELDS[j]]<-tmp_data[,BENTHIC_FIELDS[j]]
			}
		}
	
	}
}
# now reset zeros to NAs for all records where there was NO benthic data at all
wd[wd$countBD==0,BENTHIC_FIELDS]<-NA

#setwd("/Volumes/TOSHIBA EXT/CRED/cruise/HARAMP2016/")
save(wd, file="raw_working_data.rdata")
#********************

survey_est_benthos<-Calc_Site_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)   #Calc_Site_nSurveysArea deals better with situations where one REP has benthic data and other doesnt. 

#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)


# SELECT SUMMARY METRICS --------------------------------------------------
###### USE CODE BELOW TO EXCLUDE CERTAIN SPECIES/GROUPS ###########
##################################################################
# WITHOUT  RAYS 
wd<-wd[!wd$SPECIES %in% c("MABI", "AENA"),]

# calc pooled site biomass by consumer group, species, and common family
r1<-Calc_Site_Bio(wd, "TROPHIC_MONREP"); trophic.cols<-names(r1)[3:dim(r1)[2]]
r2<-Calc_Site_Bio_By_SizeClass(wd, c(0,20,50,Inf)); size.cols<-names(r2)[3:dim(r2)[2]]

#Merge Site Data and Count Data Per Site Per Grouping Variable (e.g. Species, Tropic_MonRep, Family) 
xx<-merge(survey_table, survey_est_benthos, by=UNIQUE_SURVEY)

###################
wsd<-merge(xx,r1,by=UNIQUE_SURVEY)
wsd$TotFish<-rowSums(wsd[,trophic.cols])
wsd<-merge(wsd, r2, by=UNIQUE_SURVEY)
names(wsd)
names(wsd)[match(c("(0,20]", "(20,50]","(50,Inf]" ),names(wsd))] <- c("0_20", "20_50", "50_plus")
data.cols<-c(trophic.cols, "TotFish", SURVEY_SITE_DATA, "0_20", "20_50", "50_plus")

####### OUTPUT working_site_data for graphs ############
save(wsd, file="working_site_data.rdata")
write.csv(wsd,file="working_site_data.csv")

### pooling data  ###############################
wsd$ANALYSIS_SEC<-wsd$ISLAND
# wsd$STRATA<-wsd$ANALYSIS_STRATA

SPATIAL_POOLING_BASE<-c("REGION", "ISLAND", "STRATA", "REEF_ZONE")    
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, "OBS_YEAR")

### LOOK AT REPLICATION WITHIN STRATA - TO EYEBALL WHETHER THERE ARE STRATA WITHOUT REPLICATION # KM drop strata with no replication? 
tmp<-aggregate(wsd[,"METHOD"], by=wsd[,c(POOLING_LEVEL ,"SITE")], length)
tmp<-aggregate(tmp[,"x"], by=tmp[,c(POOLING_LEVEL)], length)
#tmp<-merge(sectors, tmp[,c("OBS_YEAR", "ANALYSIS_SEC", "ANALYSIS_STRATA","x")],by=c("ANALYSIS_SEC", "ANALYSIS_STRATA"),all.y=TRUE)
names(tmp)[names(tmp)=="x"]<-"n_sites"
a<-cast(tmp, OBS_YEAR + REGION + ISLAND ~ STRATA, value="n_sites", sum, fill=NA)
a

#clean up the sectors table so pool all sub sectors within a scheme into a total for this scheme's sectors
sectors$STRATA<-paste(sectors$REEF_ZONE, sectors$DEPTH_BIN, sep="")
sectors<-aggregate(sectors[,"AREA_HA"], by=sectors[,c(SPATIAL_POOLING_BASE)], sum)
names(sectors)[names(sectors)=="x"]<-"AREA_HA"

#################################################################################################################################
############################################# NOW DO THE CALCULATION OF WINHIN-STRATA AND POOLED UP DATA VALUES #################
#################################################################################################################################

# get rid of columns w/ NAN
data.cols<-c( "PISCIVORE","PLANKTIVORE","PRIMARY","SECONDARY","TotFish","DEPTH","HARD_CORAL","MA","CCA","0_20","20_50","50_plus")

ADDITIONAL_POOLING_BY<-c("OBS_YEAR", "METHOD")  # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)
#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
data.per.strata<-Calc_PerStrata(wsd, data.cols, POOLING_LEVEL)

write.csv(data.per.strata,file="tmp strata data.csv")

###### REMOVE STRATA with N=1 (cannot pool those up)
data.per.strata$Mean<-data.per.strata$Mean[data.per.strata$Mean$N>1,]
data.per.strata$SampleVar<-data.per.strata$SampleVar[data.per.strata$SampleVar$N>1,]
data.per.strata$SampleSE<-data.per.strata$SampleSE[data.per.strata$SampleSE$N>1,]

#pool up that strata level data to desired output level .. using the sectors df to determine appropriate weighting of each strata within each output level
AGGREGATION_LEVEL<-c("REGION","ISLAND","REEF_ZONE")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
habitat_areas_table<-sectors
dp<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
write.csv(dp,file="data_pooled_is_yr_RZ.csv")
save(dp, file="data_pooled_is_yr_RZ.rdata")

#pool up that strata level data to desired output level .. using the sectors df to determine appropriate weighting of each strata within each output level
AGGREGATION_LEVEL<-c("REGION","ISLAND")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
dp<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
write.csv(dp,file="data_pooled_is_yr.csv")

####### OUTPUT ISLAND LEVEL DATA USED FOR GRAPHS ##########
save(dp, file="data_pooled_is_yr.rdata")

