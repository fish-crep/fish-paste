rm(list=ls())
#setwd


## REQUIRED LIBRARIES
## --------------------------------------------------
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

#LOAD LIBRARY FUNCTIONS ... 
source("lib/fish_team_functions_share.R")
source("lib/Islandwide Mean&Variance Functions.R")
sectors<-read.csv("data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
site_master<-read.csv("data/SITE MASTER.csv")
site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)

## LOAD THE DATA
## --------------------------------------------------
load(file="REA WORKUPS/FISH_REA_SITE_DATA EXAMPLE.RData")  	#wsd
load(file="REA WORKUPS/tmp data.cols.RData")    			#data.cols

# Merge site data with SITE MASTER, which contains information on the survey design
wsd$SITE<-SiteNumLeadingZeros(wsd$SITE)
wsd<-merge(wsd, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE)

####################################################################################################################################################################
#
#     CHECK FOR SITUATIONS IN WHICH WE DONT HAVE ENOUGH DATA WITHIN A SECTOR (i.e. FEWER THAN 2 REPS) AND MANUALLY DEAL WITH THAT BY POOLING SECTORS
#
####################################################################################################################################################################
## check wwhether we have ISLANDS that arent in the sectors file
setdiff(unique(wsd$ISLAND),unique(sectors$ISLAND))

#set all Backreef to a single DEPTH_ZONE ("All") 
levels(wsd$DEPTH_BIN)<-c(levels(wsd$DEPTH_BIN), "All")
wsd[wsd$REEF_ZONE=="Backreef",]$DEPTH_BIN<-"All"
sectors[sectors$REEF_ZONE %in% c("Backreef"),]$DEPTH_BIN<-"All"

##MOVE THE ONE PRotectedSlope Shallow zurvey into ProtetredSlope Mid
wsd[wsd$REEF_ZONE=="Protected Slope" & wsd$DEPTH_BIN=="Shallow",]$DEPTH_BIN<-"Mid"

wsd$STRATA<-paste(substring(wsd$REEF_ZONE,1,1), substring(wsd$DEPTH_BIN,1,1), sep="")
sectors$STRATA<-paste(substring(sectors$REEF_ZONE,1,1), substring(sectors$DEPTH_BIN,1,1), sep="")

## TREAT GUGUAN, ALAMAGAN, SARIGAN AS ONE ISLAND  (REALLY ONE BASE REPORTING UNIT .. BUT SIMPLER TO STICK TO 'ISLAND')
SGA<-c("Guguan", "Alamagan", "Sarigan")
levels(wsd$ISLAND)<-c(levels(wsd$ISLAND), "AGS")
wsd[wsd$ISLAND %in% SGA,]$ISLAND<-"AGS"
sectors[sectors$ISLAND %in% SGA,]$ISLAND<-"AGS"

###### NOW WOK OUT THE SIZE OF ALL THE DIFFERENT ANALYSIS_SECTORS
SCHEMES<-c("RAMP_BASIC", "MARI2011", "MARI2014", "TUT10_12", "AS_SANCTUARY")
for(i in 1:length(SCHEMES)){
	tmp<-aggregate(sectors$AREA_HA, sectors[,c(SCHEMES[i], "STRATA")], sum)
	tmp$SCHEME<-SCHEMES[i]
	names(tmp)<-c("ANALYSIS_SEC", "STRATA", "AREA_HA", "ANALYSIS_SCHEME")
	if(i==1){
		st<-tmp
	} else {
		st<-rbind(st, tmp)
	}	
}
cast(st, ANALYSIS_SEC ~ ANALYSIS_SCHEME, value="AREA_HA", sum)

#add AREA_HA to wsd
wsd<-merge(wsd, st, by=c("ANALYSIS_SCHEME", "ANALYSIS_SEC", "STRATA"), all.x=T)
summary(wsd)
#check if some are missing an AREA_HA .. which means that they didnt get into the stratification scheme properly
unique(wsd[is.na(wsd$AREA_HA), c("ISLAND", "ANALYSIS_SEC", "SEC_NAME", "OBS_YEAR", "ANALYSIS_YEAR", "ANALYSIS_SCHEME", "STRATA")])

#NOW CHECK HOW MANY REPS WE HAVE PER STRATA
cast(wsd, REGION + ISLAND + ANALYSIS_SEC + ANALYSIS_YEAR ~ STRATA, value="AREA_HA", length)

#Treat Hawaii data as being part of 3 year rounds incorporating NCRMP, PMNM, and RFS surveys
levels(wsd$ANALYSIS_YEAR)<-c(levels(wsd$ANALYSIS_YEAR), "2016x")
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR==2016,]$ANALYSIS_YEAR<-"2016x"
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2010,2012),]$ANALYSIS_YEAR<-"2010-12"
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2013,2015),]$ANALYSIS_YEAR<-"2013-15"

####################################################################################################################################################################
#
#     POOL WSD (WORKING SITE DATA TO STRATA THEN TO HIGHER LEVELS
##
###################################################################################################################################################################

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION","ISLAND","ANALYSIS_SEC", "STRATA")    
ADDITIONAL_POOLING_BY<-c("METHOD", "ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)

#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA"))
save(dps,file="REA WORKUPS/tmp REA per strata.RData")
head(dps$Mean)

###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]

# e.g. SAVE BY ISLAND PER YEAR
OUTPUT_LEVEL<-c("REGION","ISLAND", "ANALYSIS_YEAR") 
dp<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
save(dp, file="REA WORKUPS/Fish REA Biomass By ISLAND-YEAR.rdata")

# e.g. SAVE BY REGION PER YEAR
OUTPUT_LEVEL<-c("REGION", "ANALYSIS_YEAR") 
dp<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
save(dp, file="REA WORKUPS/Fish REA Biomass By REGION-YEAR.rdata")

# e.g. SAVE BY ISLAND PER YEAR- OUTER-REEF ONLY - and drop MHI sectots that are not consistently surveys
OUTPUT_LEVEL<-c("REGION","ISLAND", "ANALYSIS_YEAR") 
unique(dps$Mean$STRATA)
OUTER_REEF<-c("FD", "FS", "FM", "PD", "PM")
M<-dps$Mean; V<-dps$SampleVar
M<-M[M$STRATA %in% OUTER_REEF,]
V<-V[V$STRATA %in% OUTER_REEF,]

dp<-Calc_Pooled_Simple(M, V, data.cols, OUTPUT_LEVEL, "AREA_HA")
save(dp, file="REA WORKUPS/Fish REA Biomass By ISLAND-YEAR OUTER-REEF.rdata")

#e.g. Calc island sclae data for a subset of sectors - in this case the sectors that were surveyed in each round
cast(M[M$REGION=="MHI",], ISLAND + ANALYSIS_SEC ~ ANALYSIS_YEAR, value="N", sum)
MHI_DROP<-c("HAW_HAMAKUA", "HAW_SE", "KAH_NORTH", "KAH_SOUTH", "MAI_HANA", "MAI_KAHULUI", "MAI_NW", "MOL_NW", "OAH_KAENA", "NII_EAST", "NII_WEST", "NII_LEHUA")
M<-M[!M$ANALYSIS_SEC %in% MHI_DROP,]
V<-V[!V$ANALYSIS_SEC %in% MHI_DROP,]
cast(M[M$REGION=="MHI",], ISLAND + ANALYSIS_SEC ~ ANALYSIS_YEAR, value="N", sum)

dp<-Calc_Pooled_Simple(M, V, data.cols, OUTPUT_LEVEL, "AREA_HA")









