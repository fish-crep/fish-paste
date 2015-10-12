rm(list=ls())
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

source("lib/fish_team_functions.R")
load("data/ALL_REA_FISH_RAW_merged.rdata")
x<-df

#change to factor to merge... change back to date format later
#x$DATE_<-as.factor(x$DATE_)
#x$SECTOR<-as.factor(x$SECTOR)

# HOUSEKEEPING ------------------------------------------------------------
# clean up the data to only fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION" , "REGION_NAME", "SECTOR", "SPECIAL_AREA", "EXCLUDE_FLAG",
"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH", 
"HARD_CORAL", "MA",  "TA",  "CCA",  "SAND",  "SOFT_CORAL", "CLAM" , "SPONGE", "CORALLIMORPH", "CYANO", "TUNICATE", "ZOANTHID" , "COMPLEXITY",
"SPECIES", "COUNT", "SIZE_", "OBS_TYPE", 
"SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150", "MAX_HEIGHT",
"SCIENTIFIC_NAME",  "TAXONNAME", "COMMONNAME", "GENUS", "FAMILY" , "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]


# Read the access data file
newdata<-read.csv("REA FISH BASE_FOR_R.csv")    #NB... 'REA FISH BASE FOR R' IS THE ACCESS QUERY THAT GENERATES OUTPUT TO MATCH THE DATA_COLS ABOVE
newdata$EXCLUDE_FLAG<-0
newdata$HABITAT_CODE<-newdata$HABITAT_TYPE

#have to have same date types!
# seems to be the case that Dates are Often weird, so this needs checking
x$DATE_<-as.character(x$DATE_)
newdata$DATE_<-as.POSIXct(strptime(newdata$DATE_, format="%m/%d/%y"))
newdata$DATE_<-as.character(newdata$DATE_)

head(newdata[,DATA_COLS])

newdata<-newdata[,DATA_COLS]
newdata$SITEVISITID<-newdata$SITEVISITID+max(x$SITEVISITID)   # this line necessary to ensure that the mhi2013 data from the Access database do not ahve same SITEVISITID as the data from Oracle
#doing same for REPLICATEID, but now treating them as
max_xR<-as.double(max(x$REPLICATEID))
newdata$REPLICATEID<-newdata$REPLICATEID+max_xR

x<-rbind(x, newdata)
x$SITE<-SiteNumLeadingZeros(x$SITE)
x<-droplevels(x)

df<-x
save(df, file="data/ALL_REA_FISH_RAW_merged.Rdata")
