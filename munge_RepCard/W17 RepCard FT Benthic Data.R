rm(list=ls())
setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Data Requests/RC Workshops Benthic")
# SET UP ------------------------------------------------------------------
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/fish_team_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/Islandwide Mean&Variance Functions.R")

Modified_Site_Species_Richness<-function(x){  
  # Modification fos tandard Calc_Site_Species_Richness to not count species with zero counts (as they can be left in data file to ensure that the site has data records at all) 
  y<-aggregate(x$COUNT,by=x[,c("SITEVISITID", "METHOD", "REP", "SPECIES")], sum)	#convert to count per species per rep
  y[y$x>1,]$x<-1																	#convert any non-zero count to 1, so we can sum those to get total number of species with count>0 
  z<-aggregate(y$x,by=y[,c("SITEVISITID", "METHOD", "REP")], sum)  		            # count number of species with non-zero counts this REP	
  xx<-aggregate(z$x,by=z[,c("SITEVISITID", "METHOD")], mean)				  		# count number of entries per rep	
  dimnames(xx)[[2]]<-c("SITEVISITID", "METHOD", "SPECIESRICHNESS")
  
  return(xx)  
} # end Modified_Site_Species_Richness


# load site master to merge with sector names
sm<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/data/SITE MASTER2016.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)

sectors<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/data/Sectors-Strata-Areas2016.csv", stringsAsFactors=FALSE)



load("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/data/ALL_REA_FISH_RAW.rdata")
x<-df

# HOUSEKEEPING ------------------------------------------------------------
# clean up the data to only fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION" , "REGION_NAME", "SECTOR", "SPECIAL_AREA", "EXCLUDE_FLAG",
"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH", 
"HARD_CORAL", "MA",  "TA",  "CCA",  "SAND",  "SOFT_CORAL", "CLAM" , "SPONGE", "CORALLIMORPH", "CYANO", "TUNICATE", "ZOANTHID" , "OTHER", "COMPLEXITY",
"SPECIES", "COUNT", "SIZE_", "OBS_TYPE", 
"SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150", "MAX_HEIGHT", "TRAINING_YN", 
"SCIENTIFIC_NAME",  "TAXONNAME", "COMMONNAME", "GENUS", "FAMILY" , "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

# by default, remove sites with EXCLUDE_FLAG set to TRUE
x[is.na(x$TRAINING_YN),]$TRAINING_YN<-FALSE   # Training flag of NA is equivalent to a FALSE .. as none of the odler data was 'training data'
x<-subset(x, x$TRAINING_YN==FALSE)
x<-subset(x, x$EXCLUDE_FLAG==0, drop=TRUE)
x<-subset(x, x$OBS_TYPE %in% c("U","I","N"))
#x<-subset(x, x$REGION %in% c("SAMOA", "PRIAs")
x<-subset(x, x$METHOD %in% c("nSPC"))
x<-subset(x, x$OBS_YEAR > 2009)

x$SITE<-SiteNumLeadingZeros(x$SITE)
x<-droplevels(x)

#add SEC_NAME to x  
# this would be better if SECTOR field in database was up to date properly .. rather than merge with the site_Sectors spreadsheet
x<-merge(x, sm[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_STRATA", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE)

#for ones that are missing SEC_NAME, set it to ISLAND
no_secs<-is.na(x$SEC_NAME)
tmp<-as.character(x$SEC_NAME)
tmp[no_secs]<-as.character(x[no_secs,]$ISLAND)
x$SEC_NAME<-tmp

table(x$SEC_NAME)

############################################################################################
# remove the component SUBSTRATE_HEIGHT fields
sh_out<-CalcMeanSHMeanSHDiff(x)
x$MEAN_SH<-sh_out[[1]]
x$MEAN_SH_DIFF<-sh_out[[2]]
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


x[is.na(x$COUNT),]$COUNT<-0
x[is.na(x$SIZE_),]$SIZE_<-0
x[is.na(x$LATITUDE),]$LATITUDE<-0
x[is.na(x$LONGITUDE),]$LONGITUDE<-0

wd<-droplevels(x)

wd$ANALYSIS_YEAR<-wd$OBS_YEAR
wd$ANALYSIS_STRATA<-paste(wd$REEF_ZONE, wd$DEPTH_BIN, sep="")


#island_table<-Aggregate_InputTable(wd, c("REGION","ISLAND"))
OTHER_BENTHIC<-c("CLAM", "CORALLIMORPH", "ZOANTHID", "TUNICATE", "SPONGE", "TA", "OTHER")
wd$OTHER_BENTHIC<-rowSums(wd[,OTHER_BENTHIC],na.rm=T)
SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL", "SAND", "MA", "CCA")

#NOTE THAT BENTHOS DOES NOT ALWAYS SUM TO 100% .. I THINK BECAUSE OF ERRORS IN THE ORIGINAL DATA ENTERED INTO THE DATABASE. NEW CODE BELOW IS AN ATTEMPT TO FIX THAT
# Go through all surveys checking for situation where some reps have NAs in a particular BENTHIC_FIELDS, but other records have non-zeros - in that situation, we were recording a field but one other diver left it balnk - those should be zeros not NAs
# this is something that should really be fixed in the database rather than here (as its an error at time of data entry)
#### BELOW code does the job, but should be cleaned up and put in a function
### IDW- COULD GREATLY SPEED THIS UP BY DOING IT FOR A REGION AND YEAR .. AND LIMIT TO ONLY nSPC
# i.e make the checking for some data and some NAs at the levels of a full survey round .. and also use indixes into the wd structure, rather than create temp dfs (tmp_data)
BENTHIC_FIELDS<-c("HARD_CORAL", "MA", "TA", "CYANO", "CCA", "SAND", "OTHER_BENTHIC")
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

WD_SAVE<-wd

# WORKING WITH POOLING READY DATA FROM HERE ON  -------------------------------------
#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

##### FOR NOW .. SETTING REPORT CARD UNIT TO ISLAND
wd$REP_CARD_UNIT<-wd$ISLAND

#get base survey info, calculate average depth+complexity+so on
SURVEY_INFO<-c("OBS_YEAR", "REGION", "REGION_NAME", "ISLAND", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_STRATA", "REP_CARD_UNIT", "EXCLUDE_FLAG", "SITEVISITID", "METHOD")
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)
#write.csv(survey_table, file="tmpSamoaSites.csv")
survey_est_benthos<-Calc_Site_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)   #Calc_Site_nSurveysArea deals better with situations where one REP has benthic data and other doesnt. 
surveys<-merge(survey_table, survey_est_benthos, by=UNIQUE_SURVEY)

#calculate BSR
surveys$BSR<-0
surveys$tmpMA<-surveys$MA
surveys[surveys$tmpMA==0,]$tmpMA<-1 #setting MA to 1 where macroalgae is nil, so that BCA is not infinte
surveys$BSR<-(surveys$CCA+surveys$HARD_CORAL)/surveys$tmpMA
summary(surveys$BSR)
head(surveys)

#Pull all species information into a separate df, for later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC", "TROPHIC_MONREP", "LW_A", "LW_B", "LMAX", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)


# GENERATE SUMMARY METRICS --------------------------------------------------

## CALCULATE INSTANTANEOUS BIOMASS MINUS SHARKS AND JACKS
wd[!wd$OBS_TYPE %in% c("I"),]$COUNT<-0

r1<-Calc_Site_Bio(wd, "TROPHIC_MONREP"); tmp.cols<-dimnames(r1)[[2]][3:dim(r1)[2]]
r1$TotInstFish<-rowSums(r1[,tmp.cols])

wsd<-merge(surveys, r1, by=UNIQUE_SURVEY)

data.cols<-c("PRIMARY", "TotInstFish", SURVEY_SITE_DATA, "BSR")

#write.csv(wsd, file="tmp AS working site data2.csv")

####################################################################################################################################################################
#
#     CHECK FOR SITUATIONS IN WHICH WE DONT HAVE ENOUGH DATA WITHIN A SECTOR (i.e. FEWER THAN 2 REPS) AND MANUALLY DEAL WITH THAT BY POOLING SECTORS
#
####################################################################################################################################################################

wsd<-droplevels(wsd)

## check which ISLANDS differ between sectors and working data..
setdiff(unique(sectors$SEC_NAME), unique(wsd$SEC_NAME))
setdiff(unique(wsd$ANALYSIS_SEC), unique(sectors$SEC_NAME))

#FOREREEF ONLY AND GREATER THAN 2012
wsd<-wsd[wsd$REEF_ZONE=="Forereef",] 
head(wsd)       

# DETERMINE THE BASIC STRATIFICATION WITHIN SECTORS - DEFAULT IS REEF_ZONE AND DEPTH_BIN, BUT THIS CODE ALLOWS PSSIBILITY OF CHOOSING ANOTHER
sectors$ANALYSIS_STRATA<-paste(sectors$REEF_ZONE, sectors$DEPTH_BIN, sep='')


#generate table to be able to relate ANALYSIS_SEC to REP_CARD_UNIT (as we have one-off reporting units in this case)
rcu<-aggregate(wsd$METHOD, by=wsd[,c("ISLAND", "REP_CARD_UNIT", "ANALYSIS_SEC")], FUN=length)


WSD_SAVED<-wsd
SECTORS_SAVED<-sectors
####################################################################################################################################################################
#
#     SET THE ANALYIS SCHEME (ie WHICH SECTORS ARE WE USING THIS TIME .. IS IT THE BASIC ONES, OR THE ONES THAT WE USED FOR GUAMM2011 SURVEYS OR WHATEVER)
#
#
#     BE AWARE THAT THIS NEXT STEP REQUIRES SOME MANUAL FIDDLING.. WHEN YOU RUN THE CODE YOU MUST DECIDE ON THE APPROPRAITE STRATIFICATION SCHEME. IN SEVERAL CASES IT WILL
#        BE NECESSARY TO RUN SEVERAL SCHEMES (eg MARIANA 2011, then MARIANA 2014, etc...) AND THEN MANUALLY PUT THE DATA TOGETHER INTO A MASTER OUTPUT (eg run all with RAMP_BASIC, 
#        then run just Guam 2011 with MARIAN2011, then run Guam2014 with MARIAN2014, and then pool the various data files (eg by cutting and pasting from MAR2011 output into the master etc..)
#
####################################################################################################################################################################
SPATIAL_POOLING_BASE<-c("REGION","ISLAND","ANALYSIS_SEC","REP_CARD_UNIT", "ANALYSIS_STRATA", "REEF_ZONE")    
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, "ANALYSIS_YEAR")

wsd<-WSD_SAVED
sectors<-SECTORS_SAVED

####################################################################################################################################################################
#
#     CHECK FOR SITUATIONS IN WHICH WE DONT HAVE ENOUGH DATA WITHIN A SECTOR (i.e. FEWER THAN 2 REPS) AND MANUALLY DEAL WITH THAT BY POOLING SECTORS
#
####################################################################################################################################################################
## Nearly always DOING THIS ONLY WITH nSPC data ####
wsd<-subset(wsd, wsd$METHOD=="nSPC")
wsd<-droplevels(wsd)

## check wwhether we have ISLANDS that arent in the sectors file
setdiff(unique(wsd$ISLAND),unique(sectors$ISLAND))
        
#.... Make Sarigan-Guguan-Alamagan be a single 'ISLAND' 
# there MUST already be appropraite records in the sectors table for the new 'ISLAND' name, in this case will be "AGS"
levels(wsd$ISLAND)<-c(levels(wsd$ISLAND), "AGS")
wsd[wsd$ISLAND %in% c("Sarigan", "Guguan", "Alamagan"),"ISLAND"]<-"AGS"
sectors[sectors$ISLAND %in% c("Sarigan", "Guguan", "Alamagan"),"ISLAND"]<-"AGS"

#set all Backreef in NWHI, Samoa, and Marianas as single DEPTH_ZONE ("All")  #IDW-this has to match the sectors file (Obvioulsy!)
wsd[wsd$REGION %in% c("NWHI", "SAMOA", "S.MARIAN", "N.MARIAN") & wsd$REEF_ZONE=="Backreef",]$ANALYSIS_STRATA<-"BackreefAll"
#sectors[sectors$REGION %in% c("NWHI", "SAMOA", "S.MARIAN", "N.MARIAN") & sectors$REEF_ZONE=="Backreef",]$ANALYSIS_STRATA<-"BackreefAll"

#consider doing this for lagoon too .. probably should happen at Rose
# pooling rose lagoon sites
levels(wsd$ANALYSIS_STRATA)<-c(levels(wsd$ANALYSIS_STRATA), "LagoonAll")
levels(wsd$DEPTH_BIN)<-c(levels(wsd$DEPTH_BIN), "All")

wsd[wsd$ISLAND=="Rose" & wsd$REEF_ZONE=="Lagoon",]$DEPTH_BIN<-"All"
wsd[wsd$ISLAND=="Rose" & wsd$REEF_ZONE=="Lagoon",]$ANALYSIS_STRATA<-"LagoonAll"
sectors[sectors$ISLAND=="Rose" & sectors$REEF_ZONE=="Lagoon",]$DEPTH_BIN<-"All"

wsd<-droplevels(wsd)

WSD_SAVED<-wsd
SECTORS_SAVED<-sectors
####################################################################################################################################################################
#
#     SET THE ANALYIS SCHEME (ie WHICH SECTORS ARE WE USING THIS TIME .. IS IT THE BASIC ONES, OR THE ONES THAT WE USED FOR GUAMM2011 SURVEYS OR WHATEVER)
#
#
#     BE AWARE THAT THIS NEXT STEP REQUIRES SOME MANUAL FIDDLING.. WHEN YOU RUN THE CODE YOU MUST DECIDE ON THE APPROPRAITE STRATIFICATION SCHEME. IN SEVERAL CASES IT WILL
#        BE NECESSARY TO RUN SEVERAL SCHEMES (eg MARIANA 2011, then MARIANA 2014, etc...) AND THEN MANUALLY PUT THE DATA TOGETHER INTO A MASTER OUTPUT (eg run all with RAMP_BASIC, 
#        then run just Guam 2011 with MARIAN2011, then run Guam2014 with MARIAN2014, and then pool the various data files (eg by cutting and pasting from MAR2011 output into the master etc..)
#
###################################################################################################################################################################
# COME BACK HERE TO RE RUN FOR EACH ANALYSIS SCHEME
SPATIAL_POOLING_BASE<-c("REGION","ISLAND","REP_CARD_UNIT", "ANALYSIS_SEC", "ANALYSIS_STRATA", "REEF_ZONE")    
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, "ANALYSIS_YEAR")


CURRENT_SCHEMES<-c("RAMP_BASIC", "MARI2011", "MARI2014","TUT10_12", "AS_SANCTUARY")

for(1 in 1:length(CURRENT_SCHEMES)){
	wsd<-WSD_SAVED
	sectors<-SECTORS_SAVED

	CURRENT_SCHEME<-CURRENT_SCHEMES[i]
	
	sectors$ANALYSIS_SEC<-sectors[,CURRENT_SCHEME]
	# DETERMINE THE BASIC STRATIFICATION WITHIN SECTORS - DEFAULT IS REEF_ZONE AND DEPTH_BIN, BUT THIS CODE ALLOWS PSSIBILITY OF CHOOSING ANOTHER
	sectors$ANALYSIS_STRATA<-paste(sectors$REEF_ZONE, sectors$DEPTH_BIN, sep='')
	
	#now deal with those missing sectors - either rename ANALYSIS_SEC OR remove
	if(CURRENT_SCHEME=="RAMP_BASIC") {
#		wsd[wsd$ANALYSIS_SEC %in% c("PATI_PT_MPA", "ACHANG_MPA", "TUMON_BAY_MPA", "PITI_BOMB_MPA", "GUAM_MP_MINUS_ACHANG"),]$ANALYSIS_SEC<-"GUAM_MP"
#		#in this case removing 2014 ACHANG_MPA sites (The shorebased ones) and changing ANALYSIS_SEC for all other GUAM MPA sectors to the RAMP base one "GUAM_MP", also remove SAMOA 2015 sites, they will run in AS_SANCTUARY 2015 and Tutuila 2010&012
#		wsd<-wsd[!(wsd$ANALYSIS_SEC == "ACHANG_MPA" & wsd$ANALYSIS_YEAR==2014),]
		wsd<-wsd[wsd$ISLAND != "Guam",]
		wsd<-wsd[!(wsd$REGION == "SAMOA" & wsd$ANALYSIS_YEAR %in% c(2015, 2016)),]
		wsd<-wsd[!(wsd$ISLAND == "Tutuila" & wsd$ANALYSIS_YEAR %in% c(2010,2012)),]
	}
	if(CURRENT_SCHEME=="MARI2011") {wsd<-wsd[(wsd$ISLAND %in% "Guam" & wsd$OBS_YEAR==2011),]}	#in this case remove everything that isnt Guam surveyed in 2011
	if(CURRENT_SCHEME=="MARI2014") {wsd<-wsd[(wsd$ISLAND %in% "Guam" & wsd$OBS_YEAR==2014),]}	#in this case remove everything that isnt Guam surveyed in 2014
	if(CURRENT_SCHEME=="AS_SANCTUARY") {wsd<-wsd[(wsd$REGION == "SAMOA" & wsd$OBS_YEAR %in% c(2015, 2016)),]}	#in this case remove everything that isnt SAMOA surveyed in 2015
	if(CURRENT_SCHEME=="TUT10_12") {wsd<-wsd[(wsd$ISLAND == "Tutuila" & wsd$OBS_YEAR %in% c(2010,2012)),]}  #in this case remove everything that isnt Tutuila in 2010 or 2012
	
	##DETERMINE WHICH SITES HAVE ANALYSIS STRATA THAT ARE NOT IN THIS 
	analysis_secs<-unique(wsd$ANALYSIS_SEC)
	missing_secs<-unique(analysis_secs[!analysis_secs %in% unique(sectors$ANALYSIS_SEC)])
	if(length(missing_secs)>0) {
		cat("ANALYSIS SECTORS missing from this scheme:", missing_secs)
	}
	tmp<-aggregate(wsd[,"TotInstFish"],by=wsd[,c("REGION", "ISLAND", "ANALYSIS_YEAR", "ANALYSIS_SEC")], sum, na.rm=FALSE)  
	tmp[tmp$ANALYSIS_SEC %in% missing_secs,]
	
	### CHECK REPLICATION WITHIN STRATA
	tmp<-aggregate(wsd[,"METHOD"], by=wsd[,c(POOLING_LEVEL ,"SITE")], length)
	tmp<-aggregate(tmp[,"x"], by=tmp[,c(POOLING_LEVEL)], length)
	tmp<-merge(sectors, tmp[,c("ANALYSIS_YEAR", "ANALYSIS_SEC", "ANALYSIS_STRATA","x")],by=c("ANALYSIS_SEC", "ANALYSIS_STRATA"),all.y=TRUE)
	names(tmp)[names(tmp)=="x"]<-"n_sites"
	a<-cast(tmp, ANALYSIS_YEAR + REGION + ISLAND + ANALYSIS_SEC ~ ANALYSIS_STRATA, value="n_sites", sum, fill=NA)
	a
	
	# Monitoring Report OUTPUT sites per years (appendix 3) -------------------------------------
	#save(a, file=paste(CURRENT_SCHEME, "MRsites_year_reef_zone_depth_bin.rdata")) ## use this for table in appendix 3 - see appendices R file
	
	#clean up the sectors table so pool all sub sectors within a scheme into a total for this scheme's sectors
	sectors<-aggregate(sectors[,"AREA_HA"], by=sectors[,c(SPATIAL_POOLING_BASE)], sum)
	names(sectors)[names(sectors)=="x"]<-"AREA_HA"
	
	#################################################################################################################################
	############################################# NOW DO THE CALCAULTION OF WINHIN-STRATA AND POOLED UP DATA VALUES #################
	#################################################################################################################################
	
	ADDITIONAL_POOLING_BY<-c("ANALYSIS_YEAR", "METHOD")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)
	#generate within strata means and vars
	POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
	data.per.strata<-Calc_PerStrata(wsd, data.cols, POOLING_LEVEL)
	write.csv(data.per.strata,file=paste(CURRENT_SCHEME, "tmp strata data.csv", sep=""))
	#save(data.per.strata, file=paste(CURRENT_SCHEME, "strata_data.rdata", sep=""))
	
	###### REMOVE STRATA with N=1 (cannot pool those up)
	data.per.strata$Mean<-data.per.strata$Mean[data.per.strata$Mean$N>1,]
	data.per.strata$SampleVar<-data.per.strata$SampleVar[data.per.strata$SampleVar$N>1,]
	data.per.strata$SampleSE<-data.per.strata$SampleSE[data.per.strata$SampleSE$N>1,]
	
	### SAVE THE DATA AT WHATEVER LEVEL YOU WANT ... SOME EXAMPLES BELOW..
	
	# e.g. SAVE BY ISLAND AND REEF ZONE PER YEAR
	AGGREGATION_LEVEL<-c("REGION","ISLAND","REEF_ZONE")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
	dp<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
	write.csv(dp,file=paste(CURRENT_SCHEME, "data_pooled_is_yr_RZ.csv", sep=""))
	save(dp, file=paste(CURRENT_SCHEME, "data_pooled_is_yr_RZ.rdata", sep=""))
	
	# e.g. SAVE BY ISLAND PER YEAR
	AGGREGATION_LEVEL<-c("REGION","ISLAND")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
	dp<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
	write.csv(dp,file=paste(CURRENT_SCHEME, "data_pooled_is_yr.csv", sep=""))
	save(dp, file=paste(CURRENT_SCHEME, "data_pooled_is_yr.rdata", sep=""))
	
	# e.g. SAVE BY ISLAND AND SECTOR PER YEAR
	AGGREGATION_LEVEL<-c("REGION","ISLAND", "ANALYSIS_SEC")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
	dp<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
	write.csv(dp,file=paste(CURRENT_SCHEME, "data_pooled_sector_yr.csv", sep=""))
	save(dp, file=paste(CURRENT_SCHEME, "data_pooled_sector_yr.rdata", sep=""))
	
	ADDITIONAL_POOLING_BY<-c("METHOD")  
	
	# e.g. SAVE BY ISLAND ALL YEARS COMBINED
	AGGREGATION_LEVEL<-c("REGION", "ISLAND")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
	dp<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
	write.csv(dp,file=paste(CURRENT_SCHEME, "data_pooled_island.csv", sep=""))
	save(dp, file=paste(CURRENT_SCHEME, "data_pooled_island.rdata", sep=""))
}



