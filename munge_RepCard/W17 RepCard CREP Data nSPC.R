rm(list=ls())
setwd("/Users/ivor.williams/Documents/CRED/CRCP/NCRMP/Report Card Workings/RepCard 2018 Data")
# SET UP ------------------------------------------------------------------
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/core_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/fish_team_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/Islandwide Mean&Variance Functions.R")

##### BASELINE FILE!
base<-read.csv("island baselines.csv")


sm<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/SITE MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)
sectors<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)


# FISH REA WORKINGS ----------------------------------------------------------------
load("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/ALL_REA_FISH_RAW.rdata")
#load("/Users/ivor.williams/Documents/CRED/Fish Team/MARAMP17/ALL_REA_FISH_RAW_inclM17.rdata")
x<-df

# HOUSEKEEPING ------------------------------------------------------------
# clean up the data to only fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION" , "REGION_NAME", "SECTOR", "SPECIAL_AREA", "EXCLUDE_FLAG",
"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH", 
"HARD_CORAL", "MA",  "TA",  "CCA",  "SAND",  "SOFT_CORAL", "CLAM" , "SPONGE", "CORALLIMORPH", "CYANO", "TUNICATE", "ZOANTHID" , "OTHER", "COMPLEXITY", "TRAINING_YN", "VISIBILITY",
"SPECIES", "COUNT", "SIZE_", "OBS_TYPE", 
"SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150", "MAX_HEIGHT",
#"RANK", "SCIENTIFIC_NAME",  "TAXONNAME", "COMMONNAME", "GENUS", "FAMILY", "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
"TAXONNAME", "COMMONNAME", "GENUS", "FAMILY", "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

# by default, remove sites with EXCLUDE_FLAG set to TRUE
x[is.na(x$TRAINING_YN),]$TRAINING_YN<-FALSE   # Training flag of NA is equivalent to a FALSE .. as none of the odler data was 'training data'
x<-subset(x, x$TRAINING_YN==FALSE)
x<-subset(x, x$EXCLUDE_FLAG==0, drop=TRUE)
x<-subset(x, x$OBS_TYPE %in% c("U","I","N"))
x<-subset(x, !x$REGION %in% c("CT"))
x<-subset(x, x$METHOD %in% c("nSPC"))
x<-subset(x, !x$ISLAND %in% c("South Bank"))
x<-subset(x, x$OBS_YEAR >2009)
x<-subset(x, x$REEF_ZONE %in% c("Forereef", "Protected Slope"))
x<-subset(x, x$OBS_YEAR <2018)

x$SITE<-SiteNumLeadingZeros(x$SITE)
x<-droplevels(x)

#add SEC_NAME to x  
# this would be better if SECTOR field in database was up to date properly .. rather than merge with the site_Sectors spreadsheet
x<-merge(x, sm[,c("SITE", "SEC_NAME", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE)

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
x$MEAN_SH_SD<-sh_out[[2]]
x<-x[, setdiff(names(x),c("SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150"))]
############################################################################################
x<-droplevels(x)

#######################
## CLEAN UP NAs #######
#######################
tmp.lev<-levels(x$HABITAT_CODE); head(tmp.lev)
levels(x$HABITAT_CODE)<-c(tmp.lev, "UNKNOWN")
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
x[is.na(x$COMMONNAME),"COMMONNAME"]<-"UNKNOWN"
x[is.na(x$GENUS),"GENUS"]<-"UNKNOWN"
x[is.na(x$FAMILY),"FAMILY"]<-"UNKNOWN"
x[is.na(x$COMMONFAMILYALL),"COMMONFAMILYALL"]<-"UNKNOWN"
x[is.na(x$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"

#fixing unknown lat/long from a sites survyeted by Val Brown in Guam in 2015. These values are probably close
# .. putting this in here so that we do not have NAs in the LAT and LONG .. but we do nto want to save these to the actual master data file
x[x$SITE=="GUA-01310",]$LATITUDE<-13.24173
x[x$SITE=="GUA-01310",]$LONGITUDE<-144.70428

x[is.na(x$COUNT),]$COUNT<-0
x[is.na(x$SIZE_),]$SIZE_<-0
x[is.na(x$LENGTH_CONVERSION_FACTOR),]$LENGTH_CONVERSION_FACTOR<-1

wd<-droplevels(x)

#island_table<-Aggregate_InputTable(wd, c("REGION","ISLAND"))
OTHER_BENTHIC<-c("CLAM", "CORALLIMORPH", "ZOANTHID", "TUNICATE", "SPONGE", "OTHER", "CYANO", "TA")
wd$OTHER_BENTHIC<-rowSums(wd[,OTHER_BENTHIC],na.rm=T)

#NOTE THAT BENTHOS DOES NOT ALWAYS SUM TO 100% .. I THINK BECAUSE OF ERRORS IN THE ORIGINAL DATA ENTERED INTO THE DATABASE. NEW CODE BELOW IS AN ATTEMPT TO FIX THAT
BENTHIC_FIELDS<-c("HARD_CORAL", "MA", "TA", "CYANO", "CCA", "SAND", "OTHER")
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

# WORKING WITH POOLING READY DATA FROM HERE ON  -------------------------------------
#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

#get base survey info, calculate average depth+complexity+so on
SURVEY_INFO<-c("OBS_YEAR", "REGION", "ISLAND", "SEC_NAME", "ANALYSIS_SCHEME", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE", "SITEVISITID", "METHOD")
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)
#write.csv(survey_table, file="tmpSamoaSites.csv")

SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL", "SAND", "MA", "CCA", "MEAN_SH")
survey_est_benthos<-Calc_Site_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)   #Calc_Site_nSurveysArea deals better with situations where one REP has benthic data and other doesnt. 
surveys<-merge(survey_table, survey_est_benthos, by=UNIQUE_SURVEY)
#write.csv(surveys, file="tmALLSurveys2010_17.csv")

#Pull all species information into a separate df, for later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC", "TROPHIC_MONREP", "LW_A", "LW_B", "LMAX", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)
#write.csv(species_table, file="tmpSpeciesTable.csv")

# GENERATE SUMMARY METRICS --------------------------------------------------
WD_SAVE<-wd
#wd<-WD_SAVE
wd<-droplevels(wd)

##INSTANANEOUS BIOMASS####################################################################################################
wd<-WD_SAVE

## CALCULATE INSTANTANEOUS BIOMASS MINUS SHARKS AND JACKS
wd[!wd$OBS_TYPE %in% c("I"),]$COUNT<-0
SHARKS_JACKS<-c("Carangidae", "Carcharhinidae", "Ginglymostomatidae", "Sphyrnidae")
wd[wd$FAMILY %in% SHARKS_JACKS,]$COUNT<-0

r1<-Calc_Site_Bio(wd, "TROPHIC_MONREP"); tmp.cols<-dimnames(r1)[[2]][3:dim(r1)[2]]
r1$TotINSTFishNoSJ<-rowSums(r1[,tmp.cols])

wd_rich<-WD_SAVE
wd_rich[!wd_rich$OBS_TYPE %in% c("U", "I", "N"), "COUNT"]<-0

wd_rich[wd_rich$FAMILY %in% c("Blenniidae", "Gobiidae"),]$COUNT<-0
#unique(wd_rich$RANK)
#wd_rich[!wd_rich$RANK %in% c("Species", "Subspecies"),]$COUNT<-0  # not doing this for us, as we have lots of species that are clearly unique, just not identified to species level, so sp. data are probably not species taht are otherwise present in the cylinder

r2<-Modified_Site_Species_Richness(wd_rich)

wsd<-merge(surveys, r1, by=UNIQUE_SURVEY)
wsd<-merge(wsd, r2, by=UNIQUE_SURVEY)
#wsd<-merge(wsd, r3[,c(UNIQUE_SURVEY, "TotPrimeSpawners")], by=UNIQUE_SURVEY)
data.cols<-c(tmp.cols, "TotINSTFishNoSJ", "SPECIESRICHNESS", SURVEY_SITE_DATA)

#write.csv(wsd, file="tmp NCRMP working site data.csv")

####################################################################################################################################################################
#
#     CHECK FOR SITUATIONS IN WHICH WE DONT HAVE ENOUGH DATA WITHIN A SECTOR (i.e. FEWER THAN 2 REPS) AND MANUALLY DEAL WITH THAT BY POOLING SECTORS
#
####################################################################################################################################################################
## Nearly always DOING THIS ONLY WITH nSPC data ####
wsd<-subset(wsd, wsd$METHOD=="nSPC")
wsd<-droplevels(wsd)
dim(wsd)

## check wwhether we have ISLANDS that arent in the sectors file
setdiff(unique(wsd$ISLAND),unique(sectors$ISLAND))

##MOVE THE ONE PRotectedSlope Shallow zurvey into ProtetredSlope Mid
wsd[wsd$REEF_ZONE=="Protected Slope" & wsd$DEPTH_BIN=="Shallow",]$DEPTH_BIN<-"Mid"

wsd$STRATA<-paste(substring(wsd$REEF_ZONE,1,1), substring(wsd$DEPTH_BIN,1,1), sep="")
sectors$STRATA<-paste(substring(sectors$REEF_ZONE,1,1), substring(sectors$DEPTH_BIN,1,1), sep="")

###### NOW WOK OUT THE SIZE OF ALL THE DIFFERENT ANALYSIS_SECTORS
#tmp<-aggregate(sectors$AREA_HA, by=sectors[,c("RAMP_BASIC", "SEC_NAME")], sum)
#tmp[with(tmp, order(RAMP_BASIC)), ]

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

##Make sure that the ANALYSIS_SCHEME and ANALYSIS_YEAR are set up properly
wsd$ANALYSIS_YEAR<-"ALL"
wsd$ANALYSIS_SCHEME<-"RAMP_BASIC"
wsd[wsd$REGION=="SAMOA",]$ANALYSIS_SCHEME<-"AS_SANCTUARY"
wsd[wsd$ISLAND=="Guam",]$ANALYSIS_SCHEME<-"MARI2011"


SCHEMES<-c("RAMP_BASIC", "MARI2011", "AS_SANCTUARY")
for(i in 1:length(SCHEMES)){
	tmp<-aggregate(sectors$AREA_HA, sectors[,c("SEC_NAME", SCHEMES[i])], sum)
	tmp$SCHEME<-SCHEMES[i]
	names(tmp)<-c("SEC_NAME", "ANALYSIS_SEC", "AREA_HA", "ANALYSIS_SCHEME")
	if(i==1){
		xx<-tmp
	} else {
		xx<-rbind(xx, tmp)
	}	
}
cast(xx, SEC_NAME + ANALYSIS_SEC~ ANALYSIS_SCHEME, value="AREA_HA", sum)

#add AREA_HA to wsd
wsd$OLD_AS<-wsd$ANALYSIS_SEC
wsd$ANALYSIS_SEC<-NULL
wsd<-merge(wsd, xx[,c("ANALYSIS_SCHEME", "SEC_NAME", "ANALYSIS_SEC")], by=c("ANALYSIS_SCHEME", "SEC_NAME"), all.x=T)
head(wsd)

#add AREA_HA to wsd
wsd<-merge(wsd, st, by=c("ANALYSIS_SCHEME", "ANALYSIS_SEC", "STRATA"), all.x=T)
summary(wsd)
#check if some are missing an AREA_HA .. which means that they didnt get into the stratification scheme properly
unique(wsd[is.na(wsd$AREA_HA), c("ISLAND", "ANALYSIS_SEC", "SEC_NAME", "OBS_YEAR", "ANALYSIS_YEAR", "ANALYSIS_SCHEME", "STRATA")])

#NOW CHECK HOW MANY REPS WE HAVE PER STRATA
cast(wsd, REGION + ISLAND + ANALYSIS_SEC + ANALYSIS_YEAR ~ STRATA, value="AREA_HA", length)

sec_rc<-cast(sectors, REP_CARD + SEC_NAME ~ STRATA, value="AREA_HA")
wsd<-merge(wsd, sec_rc[,c("REP_CARD", "SEC_NAME")], by="SEC_NAME", all.x=T)
head(wsd)
dim(wsd)

#### NOW ADDING THE ISLAND-SCALE TARGETS TO THE site-data (so those can also get pooled up into report card units!)
head(base)
wsd<-subset(wsd)

wsd<-merge(wsd, base[,c("ISLAND", "INST_BIO", "PRIMARY_PRED", "PREDATOR")], by="ISLAND", all.x=T)
dim(wsd); head(wsd)
unique(wsd[is.na(wsd$INST_BIO),c("ISLAND")])
data.cols<-c(data.cols, c("INST_BIO", "PRIMARY_PRED", "PREDATOR"))
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
wsd<-WSD_SAVED
#sectors<-SECTORS_SAVED

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION", "REP_CARD", "ANALYSIS_SEC", "STRATA")    
ADDITIONAL_POOLING_BY<-c("METHOD", "ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)
#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA"))
#write.csv(dps,file="tmp strata data.csv")
#save(dps,file="tmp strata data.RData")
head(dps$Mean)

###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]

# e.g. SAVE BY REP_CARD UNIT PER YEAR- OUTER-REEF_ONLY
OUTPUT_LEVEL<-c("REGION", "REP_CARD","ANALYSIS_YEAR") 
dp<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
write.csv(dp,file="data_pooled RCU.csv")
#save(dp, file="data_pooled RCU.rdata")




### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION", "REP_CARD", "ISLAND", "ANALYSIS_SEC", "STRATA")    
ADDITIONAL_POOLING_BY<-c("METHOD", "ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA"))
head(dps$Mean)

###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]


# e.g. SAVE BY ISLAND PER YEAR- OUTER-REEF_ONLY
OUTPUT_LEVEL<-c("REGION", "ISLAND","ANALYSIS_YEAR") 
dp<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
write.csv(dp,file="data_pooled ISLAND.csv")
save(dp,file="data_pooled ISLAND.RData")




















