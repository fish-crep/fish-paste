rm(list=ls())
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

#LOAD LIBRARY FUNCTIONS ... 
source("lib/fish_team_functions.R")
#source("lib/Islandwide Mean&Variance Functions.R")

#LOAD THE CLEAN wd 
load("TMPwd.Rdata")

## FILTER BY LOCATION, YEARS, METHOD, AND OBS_TYPE HERE!
wd[!wd$OBS_TYPE %in% c("U", "I", "N"), ]$COUNT<-0
wd<-subset(wd, wd$METHOD %in% c("nSPC"))
wd<-droplevels(wd)

#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

#get base survey info, calculate average depth+complexity+so on
SURVEY_INFO<-c("OBS_YEAR", "REGION", "REGION_NAME", "ISLAND", "ANALYSIS_SCHEME", "ANALYSIS_YEAR", "SEC_NAME", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE", "SITEVISITID", "METHOD")
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)

OTHER_BENTHIC<-c("CLAM", "CORALLIMORPH", "ZOANTHID", "TUNICATE", "SPONGE", "TA", "CYANO", "OTHER", "SOFT_CORAL")
wd$OTHER_BENTHIC<-rowSums(wd[,OTHER_BENTHIC],na.rm=T)
SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL", "MA", "CCA", "SAND", "OTHER_BENTHIC", "ComplexityValue", "MEAN_SH", "SD_SH_DIFF", "MAX_HEIGHT")

# Generate a data frame with all benthic and site level information for each survey
survey_est_benthos<-Calc_Site_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)   #Calc_Site_nSurveysArea deals better with situations where one REP has benthic data and other doesnt. 
surveys<-merge(survey_table, survey_est_benthos, by=UNIQUE_SURVEY)
save(surveys, file="TMPsurveys.Rdata")

#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)
save(species_table, file="TMPspecies.Rdata")

# GENERATE SUMMARY METRICS --------------------------------------------------
r1<-Calc_Site_Bio(wd, "TROPHIC_MONREP"); trophic.cols<-names(r1[3:dim(r1)[2]])
#r2a<-Calc_Site_Abund(wd, "SPECIES"); species.cols<-levels(species_table$SPECIES)
#r2b<-Calc_Site_Bio(wd, "SPECIES"); species.cols<-levels(species_table$SPECIES)
#r3<-Calc_Site_Bio_By_SizeClass(wd, c(0,20,50,Inf)); size.cols<-names(r4b)[3:dim(r4b)[2]]
#r4<-Modified_Site_Species_Richness(wd)
# to calculate mean fish length per site
#r5<-Calc_Site_MeanLength(wd,min_size=0.4)

#Merge Site Data and Count Data Per Site Per Grouping Variable (e.g. Species, Tropic_MonRep, Family) 
wsd<-merge(surveys,r1,by=UNIQUE_SURVEY)
wsd$TotFish<-rowSums(wsd[,trophic.cols])
data.cols<-c(trophic.cols, "TotFish", SURVEY_SITE_DATA)
#wsd<-merge(wsd, r4b, by=UNIQUE_SURVEY)
#data.cols<-c(data.cols, "0_20", "20_50", "50_plus")
#names(wsd)[match(c("(0,20]", "(20,50]","(50,Inf]" ),names(wsd))] <- c("0_20", "20_50", "50_plus")
#wsd$BSR<-(wsd$HARD_CORAL+wsd$CCA)/(100-wsd$HARD_CORAL+wsd$CCA)  #we dont have TA anymore!
#data.cols<-c(data.cols, "BSR")


# OUTPUT working_site_data  -----------------------------------
save(wsd, file="TMPwsd.Rdata")
save(data.cols, file="TMPdata.cols.Rdata")


# CAPPING OF DATA (e.g. for Monitoring report) - but I dont think this sholud necessearily be a routine step
# ACTUALLY, I THINK THIS SHUOLD BE DONE INSIDE THE MONITORING REPORT SCRIPT .. ie that script should load the uncapped wsd Rdata file, then cap it at the start
# ACTUALLY, I THINK THIS SHUOLD BE DONE INSIDE THE MONITORING REPORT SCRIPT .. ie that script should load the uncapped wsd Rdata file, then cap it at the start

#display quantile ranges of the data.cols
for(i in 1:(length(data.cols)))
{
	cat(data.cols[i]); 	cat(" ")
	cat(round(quantile(wsd[,data.cols[i]], c(0.9,0.95,0.975, 0.99), na.rm = T),1)); 	cat("      ")
}

#CAP FISH DATA VALUES TO SOMETHING CLOSE To 97.5% quantile
wsd[wsd$TotFish>450,]$TotFish<-450
wsd[wsd$PISCIVORE>300,]$PISCIVORE<-300
wsd[wsd$PLANKTIVORE>100,]$PLANKTIVORE<-100 # KM changed from 50
wsd[wsd$PRIMARY>80,]$PRIMARY<-80 # km changed from 85
wsd[wsd$SECONDARY>45,]$SECONDARY<-45 # KM changed from 40

#cap BSR too ... can be infinte in situations where divers record TA and MA as 0 (rare, but does happen)
#wsd[wsd$BSR>25 & !is.na(wsd$BSR),]$BSR<-25   #IDW - 25 is super high!

# OUTPUT (not in Adel's script that I can see, but I think we should include) -----------------------------------
save(wsd, file="TMPwsdCAPPED.rdata")




