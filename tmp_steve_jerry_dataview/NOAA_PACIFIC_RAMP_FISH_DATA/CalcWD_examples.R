## Script for data provided NOAA Ecosystem Sciences Division
## Stationary point count survey data collected for the NOAA Pacific Reef Assessment and Monitoring Program
## 2010-2016 only      
## anonymous diver and cleaned data to be shared publically


## To cite data: 
## insert citation once we have it


## POINT TO FOLDER LOCATION
#setwd("..../NOAA_PACIFIC_RAMP_FISH_DATA")


## REQUIRED LIBRARIES
## --------------------------------------------------
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

#LOAD LIBRARY FUNCTIONS ... 
source("lib/fish_team_functions_share.R")

## LOAD THE DATA
## --------------------------------------------------
load('data/NOAA_PACIFIC_RAMP_FISH_SPC_2010_2016_SCI_DATA_.Rdata')
head(wd)

## FILTER BY LOCATION, YEARS, METHOD, AND OBS_TYPE HERE! 
##--------------------------------------------------
wd[!wd$OBS_TYPE %in% c("U", "I", "N"), ]$COUNT<-0 ### Instanteous and Non-instaneous counts only   
## NOTE OBS_TYPE "P" SHOULD NEVER BE USED FOR BIOMASS OR ABUNDANCE ESTIMATES
## NOTE IF OBS_TYPE "F" and "T" ARE USED SUBSET FROM 2012 ONWARDS

#wd<-subset(wd, wd$REGION %in% c("SAMOA"))
#wd<-droplevels(wd)

## GENERATE A FISH SPECIES LIST
##--------------------------------------------------
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMON_FAMILY", "CONSUMER_GROUP", "LW_A", "LW_B", "LMAX", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)
head(species_table)


## GENERATE BASE SURVEY INFORMATION
##--------------------------------------------------
UNIQUE_SURVEY<-c("SITEVISITID", "METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

wd$METHOD<-"nSPC" ## allows functions which require method type to run
SURVEY_INFO<-c("OBS_YEAR", "REGION","ISLAND","SITE", "DATE", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE", "SITEVISITID", "METHOD")
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)


## COMPLEXITY ESTIMATES
sh_out<-CalcMeanSHMeanSHDiff(wd)
wd$MEAN_SH<-sh_out$MEAN_SH ## mean height
wd$SD_SH_DIFF<-sh_out$SD_SH_DIFF ## mean height variability

## SPECIFY NON-FISH SITE FIELDS TO BE CALCULATED
SURVEY_SITE_DATA<-c("DEPTH_M", "HARD_CORAL", "MA", "CCA", "SAND", "OTHER", "COMPLEXITY", "MEAN_SH", "SD_SH_DIFF", "MAX_HEIGHT")
survey_est_benthos<-Calc_Site_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)   #Calc_Site_nSurveysArea deals better with situations where one REP has benthic data and other doesnt. 
surveys<-merge(survey_table, survey_est_benthos, by=UNIQUE_SURVEY)

## GENERATE SUMMARY FISH METRICS - EXAMPLES
##-------------------------------------------------------------
# ALWAYS DROP OBS TYPE "P" BEFORE BIOMASS OR ABUNDANCE ESTIMATES
wd<-subset(wd, wd$OBS_TYPE != "P") # Do not calculate biomass, abundance, or richness with OBS_TYPE "P"

## FISH BIOMASS, ABUNDANCE, RICHNESS AND Biomass by Size Class

x1<-Calc_Site_Bio(wd, "CONSUMER_GROUP"); names.cols<-names(x1[3:dim(x1)[2]]) # BIOMASS BY CONS.GRP
head(x1)
#x2<-Calc_Site_Abund(wd, "COMMON_FAMILY") # ABUNDANCE BY COMMON FAMILY
#x3<-Calc_Site_Bio(wd, "SPECIES") # BIOMASS BY SPECIES
#x4<-Calc_Site_Bio_By_SizeClass(wd, c(0,20,50,Inf)) # BIOMASS BY SIZE CLASS
#x5<-Calc_Site_Abund_By_SizeClass(wd, c(0,20,50,Inf)) # ABUNDANCE BY SIZE CLASS
#x6<-Calc_Site_MeanLength(wd,min_size=1)  # MEAN FISH SIZE 
#x7<-Calc_Site_Species_Rich(wd) #SPECIES RICHNESS


## MERGE BASE SITE SURVEY INFO WITH FISH AND BENTHIC METRICS
## -------------------------------------------------------------
wsd<-merge(surveys, x1, by=UNIQUE_SURVEY)
wsd$TotFish<-rowSums(wsd[,names.cols])
#wsd<-merge(wsd, x7, by=UNIQUE_SURVEY) 

# SAVE the output
save(wsd, file="data/NOAA_PACIFIC_RAMP_FISH_SITE_LEVEL_DATA.RData")

## NOTES
## -------------------------------------------------------------
## wsd = analysis ready site level fish and benthic metrics
## Pooling these data to generate island-level estimates requires knowledge of the statistical sampling scheme for each year and whether there were any additional projects that deviate from the standard Pacific RAMP design, such as an intensive survey effort within a particular bay. 

## For this reason, we encourage data users to contact us (email: nmfs.pic.credinfo@noaa.gov with subject line: For the Attention of the Fish Team Lead) to discuss how best to handle these instances. 


