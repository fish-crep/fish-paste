rm(list=ls())
setwd("/Users/ivor.williams/Documents/CRED/Fish Team/TMP")
# SET UP ------------------------------------------------------------------
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/fish_team_functions.R")
#source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/Islandwide Mean&Variance Functions.R")


load("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/ALL_REA_FISH_RAW.rdata")
x<-df

# HOUSEKEEPING ------------------------------------------------------------
# clean up the data to only fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION" , "REGION_NAME", "SECTOR", "SPECIAL_AREA", "EXCLUDE_FLAG",
"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH", 
"SPECIES", "COUNT", "SIZE_", "OBS_TYPE", "TRAINING_YN", 
"SCIENTIFIC_NAME",  "TAXONNAME", "COMMONNAME", "GENUS", "FAMILY" , "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

# by default, remove sites with EXCLUDE_FLAG set to TRUE
x[is.na(x$TRAINING_YN),]$TRAINING_YN<-FALSE   # Training flag of NA is equivalent to a FALSE .. as none of the odler data was 'training data'
x<-subset(x, x$TRAINING_YN==FALSE)
x<-subset(x, x$EXCLUDE_FLAG==0, drop=TRUE)
#x<-subset(x, x$OBS_TYPE %in% c("U","I","N"))
x<-subset(x, x$METHOD %in% c("nSPC"))
x<-subset(x, x$OBS_YEAR > 2010)
x<-subset(x, x$ISLAND %in% c("Wake", "Jarvis"))

#x$SITE<-SiteNumLeadingZeros(x$SITE)
#x<-droplevels(x)

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

#x[is.na(x$COUNT),]$COUNT<-0
#x[is.na(x$SIZE_),]$SIZE_<-0

wd<-droplevels(x)



# WORKING WITH POOLING READY DATA FROM HERE ON  -------------------------------------
#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

#get base survey info, calculate average depth+complexity+so on
SURVEY_INFO<-c("OBS_YEAR", "REGION", "ISLAND", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "SITEVISITID", "METHOD")
surveys<-Aggregate_InputTable(wd, SURVEY_INFO)
#write.csv(survey_table, file="tmpSites.csv")

#Pull all species information into a separate df, for later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)

r1<-Calc_Site_Abund(wd, "SPECIES"); data.cols<-dimnames(r1)[[2]][3:dim(r1)[2]]
r1[,data.cols]<-r1[,data.cols]*100   # Convert to Abundance Per100m2
#r1$SITEVISITID<-as.factor(r1$SITEVISITID)
r1$METHOD<-NULL

tmp<-stack(r1, select = -SITEVISITID); head(tmp)
tmp2<-data.frame(SITEVISITID=r1$SITEVISITID, METHOD="nSPC", abund=tmp$values, SPECIES=tmp$ind); tmp2

wsd<-merge(surveys, tmp2, by=UNIQUE_SURVEY) 
wsd$PA<-ifelse(wsd$abund>0,1,0)
wsd<-merge(wsd, species_table, by="SPECIES") 
head(wsd)

d<-cast(wsd, FAMILY + SPECIES + TAXONNAME ~ ISLAND, value="abund", mean)
names(d)[4:dim(d)[2]]<-paste(names(d)[4:dim(d)[2]], "Abund100m2", sep="")
pa<-cast(wsd, SPECIES  ~ ISLAND, value="PA", mean)
names(pa)[2:dim(pa)[2]]<-paste(names(pa)[2:dim(pa)[2]], "EncRate", sep="")
head(d)
head(pa)

fish<-merge(d, pa, by="SPECIES")
write.csv(fish, file="tmp Mean Abundance & Encounter Rates by Species.csv")























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
write.csv(wsd, file="tmp working site data.csv")

####################################################################################################################################################################
#
#     POOL UP
#
####################################################################################################################################################################
## Nearly always DOING THIS ONLY WITH nSPC data ####
wsd<-subset(wsd, wsd$METHOD=="nSPC")
wsd<-droplevels(wsd)

## check which ISLANDS differ between sectors and working data..
setdiff(unique(sectors$SEC_NAME), unique(wsd$SEC_NAME))
setdiff(unique(wsd$ANALYSIS_SEC), unique(sectors$SEC_NAME))
setdiff(unique(wsd$SEC_NAME), unique(sectors$SEC_NAME))

#FOREREEF ONLY AND GREATER THAN 2012
wsd<-wsd[wsd$REEF_ZONE %in% c("Forereef", "Protected Slope"),] 
wsd<-wsd[wsd$OBS_YEAR>2012,] 
head(wsd)       

# DETERMINE THE BASIC STRATIFICATION WITHIN SECTORS - DEFAULT IS REEF_ZONE AND DEPTH_BIN, BUT THIS CODE ALLOWS PSSIBILITY OF CHOOSING ANOTHER
sectors$ANALYSIS_STRATA<-paste(sectors$REEF_ZONE, sectors$DEPTH_BIN, sep='')

#generate table to be able to relate ANALYSIS_SEC to REP_CARD_UNIT (as we have one-off reporting units in this case)
rcu<-aggregate(wsd$METHOD, by=wsd[,c("ISLAND", "REP_CARD_UNIT", "ANALYSIS_SEC")], FUN=length)

#.... Make Sarigan-Guguan-Alamagan be a single 'ISLAND' 
# there MUST already be appropraite records in the sectors table for the new 'ISLAND' name, in this case will be "AGS"
levels(wsd$ISLAND)<-c(levels(wsd$ISLAND), "AGS")
wsd[wsd$ISLAND %in% c("Sarigan", "Guguan", "Alamagan"),"ISLAND"]<-"AGS"
sectors[sectors$ISLAND %in% c("Sarigan", "Guguan", "Alamagan"),"ISLAND"]<-"AGS"

wsd<-droplevels(wsd)

WSD_SAVED<-wsd
SECTORS_SAVED<-sectors

SPATIAL_POOLING_BASE<-c("REGION","ISLAND", "ANALYSIS_SEC", "ANALYSIS_STRATA", "REEF_ZONE")    
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE)


CURRENT_SCHEMES<-c("RAMP_BASIC", "MARI2014", "AS_SANCTUARY")   #IGNORING YEAR = SO GOING TO THE FINEST SCALE IN EACH REGION

for(i in 1:length(CURRENT_SCHEMES)){
	wsd<-WSD_SAVED
	sectors<-SECTORS_SAVED
	wsd$ANALYSIS_SEC<-as.character(wsd$SEC_NAME)

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
		wsd<-wsd[!wsd$REGION == "SAMOA",]
	}
	if(CURRENT_SCHEME=="MARI2014") {
		wsd[wsd$ANALYSIS_SEC %in% c("PATI_PT_MPA", "TUMON_BAY_MPA", "PITI_BOMB_MPA"),]$ANALYSIS_SEC<-"GUAM_MP_MINUS_ACHANG"
		wsd<-wsd[wsd$ISLAND %in% "Guam",]
	}
	if(CURRENT_SCHEME=="AS_SANCTUARY") {
		wsd<-wsd[wsd$REGION == "SAMOA",]
		wsd[wsd$ISLAND=="Tau",]$ANALYSIS_SEC<-"Tau"    # Pooling Tau together HERE - I Think that there is no difference in management (its not actually closed)   IF Tau IS IMPORTANT     CHECK CHECK CHECK 
	}	#in this case remove everything that isnt SAMOA surveyed in 2015
	
	##DETERMINE WHICH SITES HAVE ANALYSIS STRATA THAT ARE NOT IN THIS 
	analysis_secs<-unique(wsd$ANALYSIS_SEC)
	missing_secs<-unique(analysis_secs[!analysis_secs %in% unique(sectors$ANALYSIS_SEC)])
	if(length(missing_secs)>0) {
		cat("ANALYSIS SECTORS missing from this scheme:", missing_secs)
	}
	tmp<-aggregate(wsd[,"TotInstFish"],by=wsd[,c("REGION", "ISLAND", "ANALYSIS_SEC")], sum, na.rm=FALSE)  
	tmp[tmp$ANALYSIS_SEC %in% missing_secs,]
	
	### CHECK REPLICATION WITHIN STRATA
	tmp<-aggregate(wsd[,"METHOD"], by=wsd[,c(POOLING_LEVEL ,"SITE")], length)
	tmp<-aggregate(tmp[,"x"], by=tmp[,c(POOLING_LEVEL)], length)
	tmp<-merge(sectors, tmp[,c("ANALYSIS_SEC", "ANALYSIS_STRATA","x")],by=c("ANALYSIS_SEC", "ANALYSIS_STRATA"),all.y=TRUE)
	names(tmp)[names(tmp)=="x"]<-"n_sites"
	a<-cast(tmp, REGION + ISLAND + ANALYSIS_SEC ~ ANALYSIS_STRATA, value="n_sites", sum, fill=NA)
	a
	
	#clean up the sectors table so pool all sub sectors within a scheme into a total for this scheme's sectors
	sectors<-aggregate(sectors[,"AREA_HA"], by=sectors[,c(SPATIAL_POOLING_BASE)], sum)
	names(sectors)[names(sectors)=="x"]<-"AREA_HA"
	
	#################################################################################################################################
	############################################# NOW DO THE CALCAULTION OF WINHIN-STRATA AND POOLED UP DATA VALUES #################
	#################################################################################################################################
	
	ADDITIONAL_POOLING_BY<-c("METHOD")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)
	#generate within strata means and vars
	POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
	data.per.strata<-Calc_PerStrata(wsd, data.cols, POOLING_LEVEL)
	write.csv(data.per.strata,file=paste(CURRENT_SCHEME, "tmp strata data.csv", sep=""))
	#save(data.per.strata, file=paste(CURRENT_SCHEME, "strata_data.rdata", sep=""))
	
	###### REMOVE STRATA with N=1 (cannot pool those up)
	data.per.strata$Mean<-data.per.strata$Mean[data.per.strata$Mean$N>1,]
	data.per.strata$SampleVar<-data.per.strata$SampleVar[data.per.strata$SampleVar$N>1,]
	data.per.strata$SampleSE<-data.per.strata$SampleSE[data.per.strata$SampleSE$N>1,]
	
	# e.g. SAVE BY ISLAND AND REEF ZONE PER YEAR
	AGGREGATION_LEVEL<-c("REGION","ISLAND","ANALYSIS_SEC")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
	dp<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
	write.csv(dp,file=paste(CURRENT_SCHEME, "data_pooled_SEC.csv", sep=""))
	save(dp, file=paste(CURRENT_SCHEME, "data_pooled_SEC.rdata", sep=""))
	
	# e.g. SAVE BY ISLAND PER YEAR
	AGGREGATION_LEVEL<-c("REGION","ISLAND")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
	dp<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
	write.csv(dp,file=paste(CURRENT_SCHEME, "data_pooled_is.csv", sep=""))
	save(dp, file=paste(CURRENT_SCHEME, "data_pooled_is.rdata", sep=""))
	
}

#LOAD THE data per SCHEME
load("RAMP_BASICdata_pooled_is.rdata")
x<-dp
load("MARI2014data_pooled_is.rdata")
g<-dp
load("AS_SANCTUARYdata_pooled_is.rdata")
as<-dp


X<-x$Mean
G<-g$Mean
AS<-as$Mean
Mean<-rbind(X, G, AS)

X<-x$PooledSE
G<-g$PooledSE
AS<-as$PooledSE
PooledSE<-rbind(X, G, AS)

dp<-list(Mean, PooledSE)
names(dp)<-list("Mean", "PooledSE")

write.csv(dp, file="CREP2013_16 RC Benthic Data.csv")
save(dp, file="CREP2013_16 RC Benthic Data.RData")