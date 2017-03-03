rm(list=ls())
setwd("E:/CRED/Base_R/Base_R_Functions")
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

source("fish_team_functions.R")
source("Islandwide Mean&Variance Functions.R")
source("Indicator_functions.R")

# get strata and sectors data data - NB - the data in the raw file should be checked and updated
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/fish-paste/data")
sectors<-read.csv("Sectors-Strata-Areas2016.csv", stringsAsFactors=FALSE)
# load site master to merge with sector names
site_master<-read.csv("SITE MASTER2016.csv")
site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)

sectors[sectors$ISLAND %in% c("Guam", "Rota", "Aguijan", "Tinian", "Saipan"),]$REGION<-"S.MARIAN"
sectors[sectors$ISLAND %in% c("Alamagan","Guguan","Sarigan","Pagan", "Agrihan", "Asuncion", "Maug", "Farallon de Pajaros", "Sarigan-Guguan-Alamagan"),]$REGION<-"N.MARIAN"

sectors<-droplevels(sectors)

load("ALL_REA_FISH_RAW.rdata")
x<-df
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report")

# HOUSEKEEPING ------------------------------------------------------------
# clean up the data to only fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION" , "REGION_NAME", "SECTOR", "SPECIAL_AREA", "EXCLUDE_FLAG",
"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH", 
"HARD_CORAL", "MA",  "TA",  "CCA",  "SAND",  "SOFT_CORAL", "CLAM" , "SPONGE", "CORALLIMORPH", "CYANO", "TUNICATE", "ZOANTHID" , "OTHER", "COMPLEXITY",
"SPECIES", "COUNT", "SIZE_", "OBS_TYPE",  "TRAINING_YN", "VISIBILITY",
"SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150", "MAX_HEIGHT",
"SCIENTIFIC_NAME",  "TAXONNAME", "COMMONNAME", "GENUS", "FAMILY" , "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

#generate a simple "Strata" field, by concatenating Stratum and Depth fields
x$STRATA<-paste(x$REEF_ZONE, x$DEPTH_BIN, sep='')

## Update SITE to have three numeric digits (eg OAH-01 becomes OAH-001)
x$SITE<-SiteNumLeadingZeros(x$SITE)

x[is.na(x$TRAINING_YN),]$TRAINING_YN<-FALSE   # Training falg of NA is equivalent to a FALSE .. as none of the odler data was 'training data'
x<-subset(x, x$TRAINING_YN==FALSE)
# by default, remove sites with EXCLUDE_FLAG set to TRUE
x<-subset(x, x$EXCLUDE_FLAG==0, drop=TRUE)
x<-subset(x, x$METHOD %in% c("nSPC"), drop=TRUE)
x<-subset(x, x$OBS_YEAR >2008, drop=TRUE)
x<-subset(x, x$OBS_TYPE %in% c("U","I","N"))
#x<-subset(x, x$REGION %in% c("NWHI", "MARIAN") | x$ISLAND == "Wake")

#add SEC_NAME to x  
# this would be better if SECTOR field in database was up to date properly .. rather than merge with the site_Sectors spreadsheet
x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_STRATA")], by="SITE", all.x=TRUE)

#CHECK THAT all ANALYSIS_SEC are present in the site_master file)
idw<-x[is.na(x$ANALYSIS_SEC)  & x$METHOD=="nSPC", c("REGION", "SITE","OBS_YEAR", "METHOD"),]
if(dim(idw)[1]>0) {cat("nSPC sites with MISSING ANALYSIS_SEC")}   # should be 0

#for ones that are missing, set it to ISLAND
no_secs<-is.na(x$ANALYSIS_SEC)
tmp<-as.character(x$ANALYSIS_SEC)
tmp[no_secs]<-as.character(x[no_secs,]$ISLAND)
x$ANALYSIS_SEC<-tmp

############################################################################################
###### new section .. where there is substrate_height data, work out average height && ave_height_variability so that we get standardized complexity metrics (mean hieght, mean height variability, max-height) 
sh_out<-CalcMeanSHMeanSHDiff(x)
x$MEAN_SH<-sh_out[[1]]
x$MEAN_SH_DIFF<-sh_out[[2]]

# remove the component SUBSTRATE_HEIGHT fields
x<-x[, setdiff(names(x),c("SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150"))]
############################################################################################
x<-droplevels(x)
nine<-x[x$OBS_YEAR=="2009",]
nine<-nine[nine$REGION=="MHI",]

#convert COMPLEXITY to a numeric field ### 
x$COMPLEXITY<-as.vector(toupper(x$COMPLEXITY))
x[is.na(x$COMPLEXITY),"COMPLEXITY"]<-"UNKNOWN"
COMPLEXITY_VALUES<-toupper(c("Low", "Med-Low", "Med", "Med-Hi", "Hi", "Very-Hi"))
x$ComplexityValue<-NaN
for (i in 1:length(COMPLEXITY_VALUES)){
	if(COMPLEXITY_VALUES[i] %in% x$COMPLEXITY){
		x[x$COMPLEXITY==COMPLEXITY_VALUES[i],]$ComplexityValue<-i
	}
}


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
###x[is.na(x$LMAX),]$LMAX<-999

## separate out the north and south marianas 
levels(x$REGION)<-c(levels(x$REGION), "S.MARIAN", "N.MARIAN")
x[x$ISLAND %in% c("Guam", "Rota", "Aguijan", "Tinian", "Saipan"),]$REGION<-"S.MARIAN"
x[x$ISLAND %in% c("Alamagan","Guguan","Sarigan","Pagan", "Agrihan", "Asuncion", "Maug", "Farallon de Pajaros"),]$REGION<-"N.MARIAN"

x<-droplevels(x)

# WORKING WITH POOLING READY DATA FROM HERE ON  -------------------------------------
########################################################################################################################
### MAIN DATA FILTERING AND CLEANING IS DONE ..  SHOULD BE READY TO START WORKING WITH THE DATA FROM HERE ON ###########

# we want to keep x as cleaned up data.. and only work with a 'working data' version of that. Calling that working data 'wd'
wd<-x

#check for NAs and **** FOR NOW **** set those to zero, so rows do not get dropped by the aggregate function (below)
## the error returned are ok to ignore, just refers to invalid factor levels
##wd[is.na(wd)]<-0 

#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

#NOTE THAT BENTHOS DOES NOT ALWAYS SUM TO 100% .. I THINK BECAUSE OF ERRORS IN THE ORIGINAL DATA ENTERED INTO THE DATABASE. NEW CODE BELOW IS AN ATTEMPT TO FIX THAT
# Go through all surveys checking for situation where some reps have NAs in a particular BENTHIC_FIELDS, but other records have non-zeros - in that situation, we were recording a field but one other diver left it balnk - those should be zeros not NAs
# this is something that should really be fixed in the database rather than here (as its an error at time of data entry)
#### BELOW code does the job, but should be cleaned up and put in a function
### IDW- COULD GREATLY SPEED THIS UP BY DOING IT FOR A REGION AND YEAR .. AND LIMIT TO ONLY nSPC
# i.e make the checking for some data and some NAs at the levels of a full survey round .. and also use indixes into the wd structure, rather than create temp dfs (tmp_data)
BENTHIC_FIELDS<-c("HARD_CORAL", "SOFT_CORAL", "MA", "CCA", "TA", "SAND", "CYANO", "CLAM", "CORALLIMORPH", "ZOANTHID", "TUNICATE", "SPONGE", "OTHER")
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

#get base survey info, calculate average depth+complexity+so on
SURVEY_INFO<-c("OBS_YEAR", "REGION", "REGION_NAME", "ISLAND", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_STRATA", "SEC_NAME", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE", "SITEVISITID", "METHOD")
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)
island_table<-Aggregate_InputTable(wd, c("REGION","ISLAND"))
OTHER_BENTHIC<-c("CLAM", "CORALLIMORPH", "ZOANTHID", "TUNICATE", "SPONGE", "TA", "OTHER")
wd$OTHER_BENTHIC<-rowSums(wd[,OTHER_BENTHIC],na.rm=T)
SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL", "SOFT_CORAL", "MA", "CCA", "SAND", "CYANO", "OTHER_BENTHIC", "ComplexityValue", "MEAN_SH", "MEAN_SH_DIFF", "MAX_HEIGHT")



# OUTPUT raw working data (used to create appendix species list) -------------------------
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs")
save(wd, file="raw_working_data.rdata")
#********************

survey_est_benthos<-Calc_Site_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)   #Calc_Site_nSurveysArea deals better with situations where one REP has benthic data and other doesnt. 

#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)


# SELECT SUMMARY METRICS --------------------------------------------------
# if coming in later: 
load("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs/raw_working_data.rdata")
# create data frame with parrotfish only to get biomass by parrotfish size
wdp<-wd
wdp[wdp$FAMILY != "Scaridae",]$COUNT<-0					###### ie keep all records, just set size (and therefore biomass) to 0 for non parrots

# calc pooled site biomass by consumer group, species, and common family
r1<-Calc_Site_Bio(wd, "TROPHIC_MONREP"); trophic.cols<-levels(species_table$TROPHIC_MONREP)
#r2a<-Calc_Site_Abund(wd, "SPECIES"); species.cols<-levels(species_table$SPECIES)
#r2b<-Calc_Site_Bio(wd, "SPECIES"); species.cols<-levels(species_table$SPECIES)
#r3<-Calc_Site_Bio(wd, "COMMONFAMILYALL"); family.cols<-levels(species_table$COMMONFAMILYALL)
r4b<-Calc_Site_Bio_By_SizeClass(wd, c(0,20,50,Inf)); size.cols<-names(r4b)[3:dim(r4b)[2]]
#r5<-Calc_Site_Species_Richness(wd)
# to calculate mean fish length per site - only for sizes of fish that are greater than 40% of Lmax -
r6<-Calc_Site_MeanLength(wd,min_size=0.3)
# if sites have NA values, set them to the minimum size, 10 cm. 
r6[is.na(r6)]<-10
# to calculate mean parrotfish biomass (size class 10-30, 30+)
r7<-Calc_Site_Bio_By_SizeClass(wdp, c(10,30,Inf))

#Merge Site Data and Count Data Per Site Per Grouping Variable (e.g. Species, Tropic_MonRep, Family) 
xx<-merge(survey_table, survey_est_benthos, by=UNIQUE_SURVEY)

wsd<-merge(xx,r1,by=UNIQUE_SURVEY)
wsd$TotFish<-rowSums(wsd[,trophic.cols])
data.cols<-c(trophic.cols, "TotFish", SURVEY_SITE_DATA)
wsd<-merge(wsd, r4b, by=UNIQUE_SURVEY)
data.cols<-c(data.cols, "0_20", "20_50", "50_plus")
## FIXED PROPERLY!
names(wsd)
names(wsd)[match(c("(0,20]", "(20,50]","(50,Inf]" ),names(wsd))] <- c("0_20", "20_50", "50_plus")
names(wsd)
#### 2016: NEW BSR CALCULATION: We don't estimate turf cover anymore, so instead, subtract hard + sand cover for ratio calculation
wsd$BSR<-(wsd$HARD_CORAL+wsd$CCA)/(100-(wsd$HARD_CORAL + wsd$CCA+wsd$SAND))
##
data.cols<-c(data.cols, "BSR")
wsd<-merge(wsd,r6,by=UNIQUE_SURVEY)
data.cols<-c(data.cols, "MEAN_SIZE")
wsd<-merge(wsd,r7,by=UNIQUE_SURVEY, all.x=T)
names(wsd)
names(wsd)[match(c("(10,30]", "(30,Inf]" ),names(wsd))] <- c("10_30", "30_plus")
names(wsd)
data.cols<-c(data.cols, "10_30", "30_plus")

###### NA values in parrot sizes ####
is.na(wsd$`30_plus`)
km<-wsd[is.na(wsd$`30_plus`),]
ksm<-wd[wd$SITEVISITID==7339,]
# all sites have no parrots, set to zero
wsd[is.na(wsd$`30_plus`),]$`30_plus`<-0
wsd[is.na(wsd$`10_30`),]$`10_30`<-0

# OUTPUT working_site_data (appendix 1) -----------------------------------
save(wsd, file="working_site_data.rdata")
load("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs/working_site_data.rdata")
#head(wsd)
#summary(wsd)
#write.csv(wsd,file="E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs/working_site_data.csv")

# save wsd UNCAPPED
wsd.uncap<-wsd
wsd<-wsd.uncap
for(i in 1:(length(data.cols)))
{
	cat(data.cols[i])
	cat(" ")
	cat(round(quantile(wsd[,data.cols[i]], c(0.9,0.95,0.975, 0.99), na.rm = T),1))
	cat("      ")
}

#CAP FISH DATA VALUES TO SOMETHING CLOSE To 97.5% quantile
wsd[wsd$TotFish>450,]$TotFish<-450
wsd[wsd$PISCIVORE>300,]$PISCIVORE<-300
wsd[wsd$PLANKTIVORE>100,]$PLANKTIVORE<-100 
wsd[wsd$PRIMARY>80,]$PRIMARY<-80 
wsd[wsd$SECONDARY>45,]$SECONDARY<-45 
wsd[wsd$"0_20">65,]$"0_20"<-65
wsd[wsd$"20_50">130,]$"20_50"<-130
wsd[wsd$"50_plus">350,]$"50_plus"<-350
wsd[wsd$"MEAN_SIZE">30,]$"MEAN_SIZE"<-30
wsd[wsd$"10_30">20,]$"10_30"<-20
wsd[wsd$"30_plus">25,]$"30_plus"<-25

#cap BSR too ... can be infinte in situations where divers record TA and MA as 0 (rare, but does happen)
#wsd[wsd$BSR>25 & !is.na(wsd$BSR),]$BSR<-25

# OUTPUT (not in Adel's script that I can see, but I think we should include) -----------------------------------
save(wsd, file="working site data CAPPED.rdata")
load("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs/working site data CAPPED.rdata")
write.csv(wsd,file="E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs/working site data CAPPED.csv")

####################################################################################################################################################################
#
#     CHECK FOR SITUATIONS IN WHICH WE DONT HAVE ENOUGH DATA WITHIN A SECTOR (i.e. FEWER THAN 2 REPS) AND MANUALLY DEAL WITH THAT BY POOLING SECTORS
#
####################################################################################################################################################################

# STANDARDIZING DATA ------------------------------------------------------

## DOING THIS ONLY WITH nSPC data ####
wsd<-subset(wsd, wsd$METHOD=="nSPC")
wsd<-droplevels(wsd)

## check which ISLANDS differ between sectors and working data..
setdiff(unique(sectors$ISLAND), unique(wsd$ISLAND))
setdiff(unique(wsd$ISLAND),unique(sectors$ISLAND))
        
# IDW-SOME CLEAN UP
#.... Make Sarigan-Guguan-Alamagan be a single 'ISLAND' 
# there MUST already be appropraite records in the sectors table for the new 'ISLAND' name, in this case will be "AGS"
levels(wsd$ISLAND)<-c(levels(wsd$ISLAND), "AGS")
wsd[wsd$ISLAND %in% c("Sarigan", "Guguan", "Alamagan"),"ISLAND"]<-"AGS"
sectors[sectors$ISLAND %in% c("Sarigan", "Guguan", "Alamagan"),"ISLAND"]<-"AGS"

#set all Backreef in NWHI, Samoa, and Maraians as single DEPTH_ZONE ("All")
wsd[wsd$REGION %in% c("NWHI", "SAMOA", "S.MARIAN", "N.MARIAN") & wsd$REEF_ZONE=="Backreef",]$ANALYSIS_STRATA<-"BackreefAll"
wsd<-droplevels(wsd)

# DO AGAIN WITH UNCAPPED DATA##############################
## DOING THIS ONLY WITH nSPC data ####
wsd.uncap<-subset(wsd.uncap, wsd.uncap$METHOD=="nSPC")
wsd.uncap<-droplevels(wsd.uncap)

## check which ISLANDS differ between sectors and working data..
setdiff(unique(sectors$ISLAND), unique(wsd.uncap$ISLAND))
setdiff(unique(wsd.uncap$ISLAND),unique(sectors$ISLAND)) # should be just Sarigan, Alamagan ad Guguan, fixed below

# IDW-SOME CLEAN UP
#.... Make Sarigan-Guguan-Alamagan be a single 'ISLAND' 
# there MUST already be appropraite records in the sectors table for the new 'ISLAND' name, in this case will be "AGS"
levels(wsd.uncap$ISLAND)<-c(levels(wsd.uncap$ISLAND), "AGS")
wsd.uncap[wsd.uncap$ISLAND %in% c("Sarigan", "Guguan", "Alamagan"),"ISLAND"]<-"AGS"
sectors[sectors$ISLAND %in% c("Sarigan", "Guguan", "Alamagan"),"ISLAND"]<-"AGS"

#set all Backreef in NWHI, Samoa, and Maraians as single DEPTH_ZONE ("All")
wsd.uncap[wsd.uncap$REGION %in% c("NWHI", "SAMOA", "S.MARIAN", "N.MARIAN") & wsd.uncap$REEF_ZONE=="Backreef",]$ANALYSIS_STRATA<-"BackreefAll"
wsd.uncap<-droplevels(wsd.uncap)
# OUTPUT cleaned up working site data NOT CAPPED (appendix 8 and maps) --------------------------
# clean site level data, will do a bit more cleaning up for maps..
save(wsd.uncap, file="clean_working_site_data_used_in_higher_pooling_for_report.Rdata")

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
#################################################################################################################################################################### COME BACK HERE TO RE RUN FOR EACH ANALYSIS SCHEME #####################
wsd<-WSD_SAVED
sectors<-SECTORS_SAVED

# pooling rose lagoon sites

levels(wsd$ANALYSIS_STRATA)<-c(levels(wsd$ANALYSIS_STRATA), "LagoonAll")
wsd$DEPTH_BIN<-as.character(wsd$DEPTH_BIN)# won't change value to "All" if it is a factor
wsd[wsd$ISLAND=="Rose" & wsd$REEF_ZONE=="Lagoon",]$DEPTH_BIN<-"All"
wsd[wsd$ISLAND=="Rose" & wsd$REEF_ZONE=="Lagoon",]$ANALYSIS_STRATA<-"LagoonAll"
sectors[sectors$ISLAND=="Rose" & sectors$REEF_ZONE=="Lagoon",]$DEPTH_BIN<-"All"
wsd$DEPTH_BIN<-as.factor(wsd$DEPTH_BIN)# change back to factor

wsd[wsd$REEF_ZONE=="Backreef",]$ANALYSIS_STRATA<-"BackreefAll"
sectors[sectors$REEF_ZONE=="Backreef",]$DEPTH_BIN<-"All"

# DETERMINE THE BASIC STRATIFICATION WITHIN SECTORS - DEFAULT IS REEF_ZONE AND DEPTH_BIN, BUT THIS CODE ALLOWS POSSIBILITY OF CHOOSING ANOTHER
sectors$ANALYSIS_STRATA<-paste(sectors$REEF_ZONE, sectors$DEPTH_BIN, sep='')

# THIS IS A CRITICAL STEP - SET THE ANALYSIS SCHEME HERE .. ALL STEPS BELOW WILL WORK OFF THIS SCHEME (THIS IS HOW ISLANDS ARE BROKEN DOWN INTO SECTORS #####
# Analysis Schemes are : RAMP_BASIC", "MARI2011", "MARI2014", "TUT10_12", "AS_SANCTUARY"
CURRENT_SCHEME<-"RAMP_BASIC"
#CURRENT_SCHEME<-"MARI2011"
#CURRENT_SCHEME<-"MARI2014"
#CURRENT_SCHEME<-"TUT10_12"
#CURRENT_SCHEME<-"AS_SANCTUARY"

sectors$ANALYSIS_SEC<-sectors[,CURRENT_SCHEME]


SPATIAL_POOLING_BASE<-c("REGION","ISLAND","ANALYSIS_SEC","ANALYSIS_STRATA", "REEF_ZONE")    
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, "ANALYSIS_YEAR")

##DETERMINE WHICH SITES HAVE ANALYSIS STRATA THAT ARE NOT IN THIS 
analysis_secs<-unique(wsd$ANALYSIS_SEC)
missing_secs<-unique(analysis_secs[!analysis_secs %in% unique(sectors$ANALYSIS_SEC)])
if(length(missing_secs)>0) {
	cat("ANALYSIS SECTORS missing from this scheme:", missing_secs)
}
tmp<-Aggregate_InputTable(wd, c("REGION", "ISLAND", "ANALYSIS_YEAR", "ANALYSIS_SEC"))
tmp[tmp$ANALYSIS_SEC %in% missing_secs,]
#now deal with those missing sectors - either rename ANALYSIS_SEC OR remove
if(CURRENT_SCHEME=="RAMP_BASIC") {
	#in this case removing 2014 ACHANG_MPA sites (The shorebased ones) and changing ANALYSIS_SEC for all other GUAM MPA sectors to the RAMP base one "GUAM_MP", also remove SAMOA 2015 sites, they will run in AS_SANCTUARY 2015 and Tutuila 2010&012
	wsd<-wsd[!(wsd$ANALYSIS_SEC == "ACHANG_MPA" & wsd$ANALYSIS_YEAR==2014),]
	wsd<-wsd[!(wsd$REGION == "SAMOA" & wsd$ANALYSIS_YEAR %in% c(2015, 2016)),]
	wsd<-wsd[!(wsd$ISLAND == "Tutuila" & wsd$ANALYSIS_YEAR %in% c(2010,2012)),]
	wsd[wsd$ANALYSIS_SEC %in% c("PATI_PT_MPA", "ACHANG_MPA", "TUMON_BAY_MPA", "PITI_BOMB_MPA", "GUAM_MP_MINUS_ACHANG"),]$ANALYSIS_SEC<-"GUAM_MP"
}
if(CURRENT_SCHEME=="MARI2011") {wsd<-wsd[(wsd$REGION %in% c("N.MARIAN", "S.MARIAN") & wsd$OBS_YEAR==2011),]}	#in this case remove everything that isnt MARIANA surveyed in 2011
if(CURRENT_SCHEME=="MARI2014") {wsd<-wsd[(wsd$REGION %in% c("N.MARIAN", "S.MARIAN") & wsd$OBS_YEAR==2014),]}	#in this case remove everything that isnt MARIANA surveyed in 2014
if(CURRENT_SCHEME=="AS_SANCTUARY") {wsd<-wsd[(wsd$REGION == "SAMOA" & wsd$OBS_YEAR %in% c(2015,2016)),]}	#in this case remove everything that isnt SAMOA surveyed in 2015
if(CURRENT_SCHEME=="TUT10_12") {wsd<-wsd[(wsd$ISLAND == "Tutuila" & wsd$OBS_YEAR %in% c(2010,2012)),]}  #in this case remove everything that isnt Tutuila in 2010 or 2012

### LOOK AT REPLICATION WITHIN STRATA - TO EYEBALL WHETHER THERE ARE STRATA WITHOUT REPLICATION # KM drop strata with no replication? 
tmp<-aggregate(wsd[,"METHOD"], by=wsd[,c(POOLING_LEVEL ,"SITE")], length)
tmp<-aggregate(tmp[,"x"], by=tmp[,c(POOLING_LEVEL)], length)
tmp<-merge(sectors, tmp[,c("ANALYSIS_YEAR", "ANALYSIS_SEC", "ANALYSIS_STRATA","x")],by=c("ANALYSIS_SEC", "ANALYSIS_STRATA"),all.y=TRUE)
names(tmp)[names(tmp)=="x"]<-"n_sites"
a<-cast(tmp, ANALYSIS_YEAR + REGION + ISLAND + ANALYSIS_SEC ~ ANALYSIS_STRATA, value="n_sites", sum, fill=NA)
#b<-cast(tmp, REGION + ISLAND + SEC_NAME ~ REEF_ZONE + DEPTH_BIN, value="AREA_HA", sum, fill=NA)
a # check for NAs in: analysis year, region, island, and analysis sec

# OUTPUT sites per years (appendix 3) -------------------------------------
save(a, file=paste(CURRENT_SCHEME, "sites_year_reef_zone_depth_bin.rdata")) ## use this for table in appendix 3 - see appendices R file
#save(b, file="area_region_reef_zone_depth_bin.rdata") 



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

#pool up that strata level data to desired output level .. using the sectors df to determine appropriate weighting of each strata within each output level
AGGREGATION_LEVEL<-c("REGION","ISLAND","REEF_ZONE")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
data_pooled_is_yr<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)# if this gives errors: check that each sector is included in site master file
write.csv(data_pooled_is_yr,file=paste(CURRENT_SCHEME, "data_pooled_is_yr_RZ.csv", sep=""))
save(data_pooled_is_yr, file=paste(CURRENT_SCHEME, "data_pooled_is_yr_RZ.rdata", sep=""))

#pool up that strata level data to desired output level .. using the sectors df to determine appropriate weighting of each strata within each output level
AGGREGATION_LEVEL<-c("REGION","ISLAND")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
data_pooled_is_yr<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
write.csv(data_pooled_is_yr,file=paste(CURRENT_SCHEME, "data_pooled_is_yr.csv", sep=""))
save(data_pooled_is_yr, file=paste(CURRENT_SCHEME, "data_pooled_is_yr.rdata", sep=""))

#pool up that strata level data to desired output level .. using the sectors df to determine appropriate weighting of each strata within each output level
## note: adding a new level of region to separate out the north and south marianas
ADDITIONAL_POOLING_BY<-c("METHOD")  
AGGREGATION_LEVEL<-c("REGION","ISLAND", "REEF_ZONE")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
data_pooled_is<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
write.csv(data_pooled_is,file=paste(CURRENT_SCHEME, "data_pooled_is_RZ.csv", sep=""))
save(data_pooled_is, file=paste(CURRENT_SCHEME, "data_pooled_is_RZ.rdata", sep=""))

#pool up that strata level data to desired output level .. using the sectors df to determine appropriate weighting of each strata within each output level
ADDITIONAL_POOLING_BY<-c("METHOD")  
AGGREGATION_LEVEL<-c("REGION", "REEF_ZONE")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
data_pooled_rg<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
write.csv(data_pooled_rg,file=paste(CURRENT_SCHEME, "data_pooled_rg_RZ.csv", sep=""))
save(data_pooled_rg, file=paste(CURRENT_SCHEME, "data_pooled_rg_RZ.rdata", sep=""))
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#       STOP!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# GO BACK UP TO " SET THE ANALYIS SCHEME" 
######################
# CODE ABOVE GENEREATES DATAFILES FOR wd, wsd, and ISLAND-SCALE values for all habitats and for reef-zones .. for each Monitoring Scheme.
# Code below integrates the data from the different schemes into single files (MONREP*.rdata), then generates a weighted regional average
#**************

#### ALL DATA ########
#work this out to merge the RAMP_BASIC with the MAR2011 and MAR2014 and AS_SANCTUARY data
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs")
load("RAMP_BASICdata_pooled_is_yr.rdata")
x<-data_pooled_is_yr
load("MARI2011data_pooled_is_yr.rdata")
m11<-data_pooled_is_yr
load("MARI2014data_pooled_is_yr.rdata")
m14<-data_pooled_is_yr
load("AS_SANCTUARYdata_pooled_is_yr.rdata")
as<-data_pooled_is_yr
load("TUT10_12data_pooled_is_yr.rdata")
tut<-data_pooled_is_yr

#make 2011 and 2014 mariana structures ONLY have Guam data and drop Guam 2011 and 2014 data from the main df
M11<-m11$Mean[m11$Mean$ISLAND=="Guam",]
M14<-m14$Mean[m14$Mean$ISLAND=="Guam",]
X<-x$Mean[!(x$Mean$ISLAND=="Guam" & x$Mean$ANALYSIS_YEAR %in% c(2011, 2014)),]
# add AS sanc
AS<-as$Mean[as$Mean$REGION=="SAMOA",]
# add TUT10_12
TUT<-tut$Mean[tut$Mean$ISLAND=="Tutuila",]
Mean<-rbind(X, M11, M14,AS,TUT)

#make 2011 and 2014 mariana structures ONLY have Guam data and drop Guam 2011 and 2014 data from the main df
M11<-m11$PooledSE[m11$PooledSE$ISLAND=="Guam",]
M14<-m14$PooledSE[m14$PooledSE$ISLAND=="Guam",]
X<-x$PooledSE[!(x$PooledSE$ISLAND=="Guam" & x$PooledSE$ANALYSIS_YEAR %in% c(2011, 2014)),]
# Add AS sanc
AS<-as$PooledSE[as$PooledSE$REGION=="SAMOA",]
# add tut10-12
TUT<-tut$PooledSE[tut$PooledSE$ISLAND=="Tutuila",]
PooledSE<-rbind(X, M11, M14,AS,TUT)

data_pooled_is_yr<-list(Mean, PooledSE)
names(data_pooled_is_yr)<-list("Mean", "PooledSE")

save(data_pooled_is_yr, file="MONREPdata_pooled_is_yr.rdata")

#
# GET ISLAND AND REGIONAL AVERAGES
#means of island scale values
DATA_COLS<-c("PRIMARY", "SECONDARY", "PLANKTIVORE", "PISCIVORE", "TotFish", "HARD_CORAL", "SOFT_CORAL", "MA", "CCA", "TA", "SAND", "CYANO", "OTHER_BENTHIC", "0_20", "20_50", "50_plus", "BSR", "MEAN_SIZE","10_30","30_plus")

data_pooled_is<-aggregate(Mean[,c(DATA_COLS, "TotalArea")], by=Mean[,c("REGION", "ISLAND")], FUN=mean, na.rm = T)#; data_pooled_is
save(data_pooled_is, file="MONREPdata_pooled_is.rdata")


#create structure - then update row by row  #DOING ONLY FOR Forereef
reg_mean<-aggregate(data_pooled_is[,DATA_COLS], by=list(data_pooled_is[,c("REGION")]), FUN=mean)
reg_mean
#reg_mean<-subset(reg_mean, reg_mean$REEF_ZONE=="Forereef")
#reg_mean$REEF_ZONE<-NULL
#data_pooled_is<-subset(data_pooled_is, data_pooled_is$REEF_ZONE=="Forereef")
reg_mean<-droplevels(reg_mean); reg_mean
for(i in 3:19)
{
	data_pooled_is$DATA_VAL<-data_pooled_is[,i]
	reg_mean[,i-1]<-by(data_pooled_is, data_pooled_is$REGION, function(x) weighted.mean(x$DATA_VAL, x$TotalArea))	
}
data_pooled_reg<-reg_mean; data_pooled_reg
by(data_pooled_is, data_pooled_is$REGION, function(x) weighted.mean(x$BSR, x$TotalArea))	
save(data_pooled_reg, file="MONREPdata_pooled_reg.rdata")


#### FOREEF DATA  ONLY########

rm(list=ls())
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs")
#work this out to merge the RAMP_BASIC with the MAR2011 and MAR2014 data
load("RAMP_BASICdata_pooled_is_yr_RZ.rdata")
x<-data_pooled_is_yr
load("MARI2011data_pooled_is_yr_RZ.rdata")
m11<-data_pooled_is_yr
load("MARI2014data_pooled_is_yr_RZ.rdata")
m14<-data_pooled_is_yr
load("AS_SANCTUARYdata_pooled_is_yr_RZ.rdata")
as<-data_pooled_is_yr
load("TUT10_12data_pooled_is_yr_RZ.rdata")
tut<-data_pooled_is_yr


#make 2011 and 2014 mariana structures ONLY have Guam data and drop Guam 2011 and 2014 data from the main df
M11<-m11$Mean[m11$Mean$ISLAND=="Guam",]
M14<-m14$Mean[m14$Mean$ISLAND=="Guam",]
X<-x$Mean[!(x$Mean$ISLAND=="Guam" & x$Mean$ANALYSIS_YEAR %in% c(2011, 2014)),]
# add AS sanc
AS<-as$Mean[as$Mean$REGION=="SAMOA",]
# add TUT10_12
TUT<-tut$Mean[tut$Mean$ISLAND=="Tutuila",]
Mean<-rbind(X, M11, M14,AS,TUT)

#make 2011 and 2014 mariana structures ONLY have Guam data and drop Guam 2011 and 2014 data from the main df
M11<-m11$PooledSE[m11$PooledSE$ISLAND=="Guam",]
M14<-m14$PooledSE[m14$PooledSE$ISLAND=="Guam",]
X<-x$PooledSE[!(x$PooledSE$ISLAND=="Guam" & x$PooledSE$ANALYSIS_YEAR %in% c(2011, 2014)),]
# ADD AS_SANC
AS<-as$PooledSE[as$PooledSE$REGION=="SAMOA",]
# add tut10-12
TUT<-tut$PooledSE[tut$PooledSE$ISLAND=="Tutuila",]
PooledSE<-rbind(X, M11, M14,AS,TUT)

#rm(data_pooled_is_yr)
data_pooled_is_yr<-list(Mean, PooledSE)
names(data_pooled_is_yr)<-list("Mean", "PooledSE")

save(data_pooled_is_yr, file="MONREPdata_pooled_is_yr_RZ.rdata")

## function to calculate pooled SE
pool_se<-function(se_vals, weights){
  #se_vals<-c(NA,NA,NA,NA)
  #weights<-c(3,1,1,9)
  
  df<-data.frame(se=se_vals, wt=weights)
  df<-df[!is.na(df$se),]
  
  if(dim(df)[1]==0) return(NaN)	
  
  weights<-df$wt/sum(df$wt)  #convert weights to portions
  tmp<-(df$se^2)*(weights^2)
  pooled.se<- sqrt(sum(tmp))
  return(pooled.se)
} #end pool_se

#
# GET ISLAND AND REGIONAL AVERAGES
#means of island scale values
DATA_COLS<-c("PRIMARY", "SECONDARY", "PLANKTIVORE", "PISCIVORE", "TotFish", "HARD_CORAL", "SOFT_CORAL", "MA", "CCA", "TA", "SAND", "CYANO", "OTHER_BENTHIC", "0_20", "20_50", "50_plus", "BSR","MEAN_SIZE","10_30","30_plus")

MeanIs<-aggregate(Mean[,c(DATA_COLS, "TotalArea")], by=Mean[,c("REGION", "ISLAND", "REEF_ZONE")], FUN=mean, na.rm = T); MeanIs
MeanIs$N<-0
SEIs<-MeanIs #create SE structure
for(i in 1:dim(SEIs)[1])
{
  base_d<-PooledSE[PooledSE$ISLAND==SEIs[i,]$ISLAND & PooledSE$REEF_ZONE==SEIs[i,]$REEF_ZONE,]
  SEIs[i,]$N<-MeanIs[i,]$N<-sum(base_d$N)
  SEIs[i, DATA_COLS]<-apply(base_d[,DATA_COLS],2, function(x) pool_se(x,rep(1,length(x))))
}

data_pooled_is<-list(MeanIs, SEIs)
names(data_pooled_is)<-list("Mean", "PooledSE")
save(data_pooled_is, file="MONREPdata_pooled_is_RZ.rdata")

#data_pooled_is<-aggregate(Mean[,c(DATA_COLS, "TotalArea")], by=Mean[,c("REGION", "ISLAND", "REEF_ZONE")], FUN=mean, na.rm = T); data_pooled_is # comment this out to run FRF code at the bottom.


# save(data_pooled_is, file="MONREPdata_pooled_is_RZ.rdata")


# #create structure - then update row by row  #DOING ONLY FOR Forereef
# reg_mean<-aggregate(data_pooled_is[,DATA_COLS], by=data_pooled_is[,c("REGION", "REEF_ZONE")], FUN=mean); reg_mean
# reg_mean<-subset(reg_mean, reg_mean$REEF_ZONE=="Forereef")
# reg_mean$REEF_ZONE<-NULL
# data_pooled_is<-subset(data_pooled_is, data_pooled_is$REEF_ZONE=="Forereef")
# reg_mean<-droplevels(reg_mean); reg_mean
# for(i in 4:20)
# {
# 	data_pooled_is$DATA_VAL<-data_pooled_is[,i]
# 	reg_mean[,i-2]<-by(data_pooled_is, data_pooled_is$REGION, function(x) weighted.mean(x$DATA_VAL, x$TotalArea))	
# }
# data_pooled_reg<-reg_mean; data_pooled_reg
# by(data_pooled_is, data_pooled_is$REGION, function(x) weighted.mean(x$BSR, x$TotalArea))	
# save(data_pooled_reg, file="MONREPdata_pooled_reg_FRF.rdata")


#create structure - then update row by row  #DOING ONLY FOR Forereef
frf_mean<-subset(data_pooled_is$Mean, data_pooled_is$Mean$REEF_ZONE=="Forereef" & data_pooled_is$Mean$ISLAND !="South Bank")
frf_se<-subset(data_pooled_is$PooledSE, data_pooled_is$PooledSE$REEF_ZONE=="Forereef" & data_pooled_is$PooledSE$ISLAND !="South Bank")

#remove South Bank from here! We do not want to include in regional averages

reg_mean<-aggregate(frf_mean[,DATA_COLS], by=frf_mean[,c("REGION", "REEF_ZONE")], FUN=mean); reg_mean  #building the data structure
reg_mean$N<-0
reg_se<-reg_mean #make SE structure
for(i in 1:dim(reg_se)[1])
{
  base_mean<-frf_mean[frf_mean$REGION==reg_mean[i,]$REGION,]
  base_se<-frf_se[frf_se$REGION==reg_se[i,]$REGION,]
  reg_se[i,]$N<-reg_mean[i,]$N<-sum(base_mean$N)
  
  #weight by island forereef area
  reg_se[i, DATA_COLS]<-apply(base_se[,DATA_COLS],2, function(x) pool_se(x,base_se$TotalArea)) 
  reg_mean[i,DATA_COLS]<-apply(base_mean[,DATA_COLS],2, function(x) weighted.mean(x, base_mean$TotalArea)) 
  #weight each island equally
  #reg_se[i, DATA_COLS]<-apply(base_se[,DATA_COLS],2, function(x) pool_se(x, rep(1,dim(base_se)[1])))
  #reg_mean[i,DATA_COLS]<-apply(base_mean[,DATA_COLS],2, function(x) mean(x)) 
}

data_pooled_reg<-list(reg_mean, reg_se)
names(data_pooled_reg)<-list("Mean", "PooledSE")
save(data_pooled_reg, file="MONREPdata_pooled_reg_FRF.rdata")






