rm(list=ls())
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

#LOAD LIBRARY FUNCTIONS ... 
source("lib/fish_team_functions.R")
source("lib/Islandwide Mean&Variance Functions.R")

#LOAD THE CLEAN wsd and sectors data
load("TMPsectors.Rdata")
load("TMPwsd.Rdata")
load("TMPdata.cols.Rdata")

#Potentially cap the data here!

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
wsd<-WSD_SAVED
sectors<-SECTORS_SAVED

SPATIAL_POOLING_BASE<-c("REGION","ISLAND","ANALYSIS_SEC","ANALYSIS_STRATA", "REEF_ZONE")    
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, "ANALYSIS_YEAR")


# DETERMINE THE BASIC STRATIFICATION WITHIN SECTORS - DEFAULT IS REEF_ZONE AND DEPTH_BIN, BUT THIS CODE ALLOWS PSSIBILITY OF CHOOSING ANOTHER
sectors$ANALYSIS_STRATA<-paste(sectors$REEF_ZONE, sectors$DEPTH_BIN, sep='')

# THIS IS A CRITICAL STEP - SET THE ANALYSIS SCHEME HERE .. ALL STEPS BELOW WILL WORK OFF THIS SCHEME (THIS IS HOW ISLANDS ARE BROKEN DOWN INTO SECTORS #####
#CURRENT_SCHEME<-"RAMP_BASIC"
#CURRENT_SCHEME<-"MARI2011"
#CURRENT_SCHEME<-"MARI2014"
CURRENT_SCHEME<-"TUT10_12"
#CURRENT_SCHEME<-"AS_SANCTUARY"

sectors$ANALYSIS_SEC<-sectors[,CURRENT_SCHEME]

#now deal with those missing sectors - either rename ANALYSIS_SEC OR remove
if(CURRENT_SCHEME=="RAMP_BASIC") {
	#in this case removing 2014 ACHANG_MPA sites (The shorebased ones) and changing ANALYSIS_SEC for all other GUAM MPA sectors to the RAMP base one "GUAM_MP", also remove SAMOA 2015 sites, they will run in AS_SANCTUARY 2015 and Tutuila 2010&012
	wsd<-wsd[!(wsd$ANALYSIS_SEC == "ACHANG_MPA" & wsd$ANALYSIS_YEAR==2014),]
	wsd<-wsd[!(wsd$REGION == "SAMOA" & wsd$ANALYSIS_YEAR==2015),]
	wsd<-wsd[!(wsd$ISLAND == "Tutuila" & wsd$ANALYSIS_YEAR %in% c(2010,2012)),]
	wsd[wsd$ANALYSIS_SEC %in% c("PATI_PT_MPA", "ACHANG_MPA", "TUMON_BAY_MPA", "PITI_BOMB_MPA", "GUAM_MP_MINUS_ACHANG"),]$ANALYSIS_SEC<-"GUAM_MP"
}
if(CURRENT_SCHEME=="MARI2011") {wsd<-wsd[(wsd$REGION %in% c("N.MARIAN", "S.MARIAN") & wsd$OBS_YEAR==2011),]}	#in this case remove everything that isnt MARIANA surveyed in 2011
if(CURRENT_SCHEME=="MARI2014") {wsd<-wsd[(wsd$REGION %in% c("N.MARIAN", "S.MARIAN") & wsd$OBS_YEAR==2014),]}	#in this case remove everything that isnt MARIANA surveyed in 2014
if(CURRENT_SCHEME=="AS_SANCTUARY") {wsd<-wsd[(wsd$REGION == "SAMOA" & wsd$OBS_YEAR==2015),]}	#in this case remove everything that isnt SAMOA surveyed in 2015
if(CURRENT_SCHEME=="TUT10_12") {wsd<-wsd[(wsd$ISLAND == "Tutuila" & wsd$OBS_YEAR %in% c(2010,2012)),]}  #in this case remove everything that isnt Tutuila in 2010 or 2012

##DETERMINE WHICH SITES HAVE ANALYSIS STRATA THAT ARE NOT IN THIS 
analysis_secs<-unique(wsd$ANALYSIS_SEC)
missing_secs<-unique(analysis_secs[!analysis_secs %in% unique(sectors$ANALYSIS_SEC)])
if(length(missing_secs)>0) {
	cat("ANALYSIS SECTORS missing from this scheme:", missing_secs)
}
tmp<-aggregate(wsd[,"TotFish"],by=wsd[,c("REGION", "ISLAND", "ANALYSIS_YEAR", "ANALYSIS_SEC")], sum, na.rm=FALSE)  
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

