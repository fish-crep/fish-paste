rm(list=ls())
setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Data Requests/GUFF")

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/fish_team_functions.R")
load("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/data/ALL_REA_FISH_RAW.rdata")
x<-df

#change to factor to merge... change back to date format later
#x$DATE_<-as.factor(x$DATE_)
#x$SECTOR<-as.factor(x$SECTOR)
x$DATE_<-as.character(x$DATE_)

# HOUSEKEEPING ------------------------------------------------------------
# clean up the data to only fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION" , "REGION_NAME", "SECTOR", "SPECIAL_AREA", "EXCLUDE_FLAG",
"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH", 
"HARD_CORAL", "MA",  "TA",  "CCA",  "SAND",  "SOFT_CORAL", "CLAM" , "SPONGE", "CORALLIMORPH", "CYANO", "TUNICATE", "ZOANTHID" , "OTHER", "COMPLEXITY",
"SPECIES", "COUNT", "SIZE_", "OBS_TYPE", 
"SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150", "MAX_HEIGHT",
"SCIENTIFIC_NAME",  "TAXONNAME", "COMMONNAME", "GENUS", "FAMILY" , "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP", "VISIBILITY", "TRAINING_YN")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

# TO REMOVE THE TRAINING RECORDS .. NEXT TWO LINES WILL DO THAT
# x[is.na(x$TRAINING_YN),]$TRAINING_YN<-FALSE   # Training flag of NA is equivalent to a FALSE .. as none of the odler data was 'training data'
# x<-subset(x, x$TRAINING_YN==FALSE)

# Read the access data file
newdata<-read.csv("REA FISH BASE_FOR_R.csv")    #NB... 'REA FISH BASE FOR R' IS THE ACCESS QUERY THAT GENERATES OUTPUT TO MATCH THE DATA_COLS ABOVE
newdata<-newdata[,DATA_COLS]

#have to have same date types!  Somehow the formatting seems to change in every iteration. This might be due to how individuals have Access and/or Excel set up ... so probably need to be ready to fiddle this
# seems to be the case that Dates are Often weird, so this needs checking
# NB POSIXct formatting codes are defined at http://astrostatistics.psu.edu/su07/R/html/base/html/strptime.html
#newdata$DATE_<-as.POSIXct(strptime(newdata$DATE_, format="%d-%b-%y")) ... this is for abbrevaited month eg "mar"
#newdata$DATE_<-as.POSIXct(strptime(newdata$DATE_, format="%b-%d-%y")) ... this was month first
#newdata$DATE_<-as.POSIXct(strptime(newdata$DATE_, format="%m/%d/%Y"))  #... this is how it is for Adel's csv file
newdata$DATE_<-as.POSIXct(strptime(newdata$DATE_, format="%m/%d/%y"))   #  this is for year without century .. (Ivor's csv file)
newdata$DATE_<-as.character(newdata$DATE_)

# Check dates look OK
# Dates should look something like "2017-03-16" "2017-03-16" 
head(newdata$DATE_)   

# NOW UPDATE THE SITEVISITID adn REPLICATEID .. so that they can not overlap with existing values
newdata$SITEVISITID<-newdata$SITEVISITID+max(x$SITEVISITID)   # this line necessary to ensure that the mhi2013 data from the Access database do not ahve same SITEVISITID as the data from Oracle
max_xR<-as.double(max(x$REPLICATEID))
newdata$REPLICATEID<-newdata$REPLICATEID+max_xR

#WORK OUT SITUATIONS WHERE WE ARE MISSING FISH DATA COMPLETELY - IE BENTHIC VIS ESTIAMTE SITES ONLY
newdata[newdata$SPECIES=="",]
newdata[newdata$SPECIES=="",]$LW_A<-0
newdata[newdata$SPECIES=="",]$LW_B<-1
newdata[newdata$SPECIES=="",]$LENGTH_CONVERSION_FACTOR<-1
levels(newdata$OBS_TYPE)<-c(levels(newdata$OBS_TYPE), "B")
levels(newdata$SPECIES)<-c(levels(newdata$SPECIES), "XXBE")
newdata[newdata$SPECIES=="",]$OBS_TYPE<-"B"
newdata[newdata$SPECIES=="",]$SPECIES<-"XXBE"

# Now look for records where the LW or other critical data are lacking .. FLAG them and (For now at least, DROP THEM)
miss_T<-newdata[is.na(newdata$TROPHIC_MONREP),]$SPECIES
missLW<-newdata[is.na(newdata$LW_A) | is.na(newdata$LW_B),]$SPECIES
miss_LCV<-newdata[is.na(newdata$LENGTH_CONVERSION_FACTOR),]$SPECIES
miss_sp<-unique(levels(newdata$SPECIES)[c(miss_T, missLW, miss_LCV)]); miss_sp

#ACTUALLY .. not going to cut these out .. as these records will include sites that have benthic replicates, but no fish data
if(length (miss_sp)>0) {
#	newdata<-newdata[!newdata$SPECIES %in% miss_sp, ]
# INSTEAD Flag them up
	cat("SPECIES WITH NO LW DATA .. NOT THAT XXBE AS SOECIES CODE MEANS BENTHIC ONLY SITE, OTHER SPECIES CODES ARE FOR SPECIES THAT HAVE NOT YET GOT COMPLETE INFORMATION IN SPECIES TABLE")
	miss_sp
}

### NOW MERGE THE DATA
x<-rbind(x, newdata)
x$SITE<-SiteNumLeadingZeros(x$SITE)
df<-droplevels(x)
summary(df)

save(df, file="ALL_REA_FISH_RAW_M17.Rdata")  #USE THIS FILE FOR ANALYSIS .. eg FOR Data Brief and so on...
