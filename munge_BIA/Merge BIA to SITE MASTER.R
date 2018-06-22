rm(list=ls())

#### 2017 Howland, Baker, Wake, Jarvis, 2016 Jarvis)
#### Strata scale BIA  ... coral 

setwd("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data")

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(RODBC)            # to connect to oracle

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")


# #BIA data - this is from CPCE
load("ALL_BIA_STR_RAW_NEW.rdata")   #bia
bia$SITE<-SiteNumLeadingZeros(bia$SITE)

#CNET data - from CoralNet
load("ALL_BIA_STR_CNET.rdata") #load data
cnet$SITE<-SiteNumLeadingZeros(cnet$SITE)


##Generate Table of all the bia categories
head(bia)
bia_tab<-aggregate(bia$POINTS, by=bia[,c("TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME")], FUN=length)
#write.csv(bia_tab, file="BIA categories.csv")
table(bia$TIER_1)
table(bia$TIER_2)

##Generate Table of all the bia categories
head(cnet)
cnet_tab<-aggregate(cnet$ROUNDID, by=cnet[,c("CATEGORY_CODE", "CATEGORY_NAME", "SUBCATEGORY_CODE", "SUBCATEGORY_NAME", "GENERA_CODE", "GENERA_NAME", "SHORT_CODE", "FUNCTIONAL_GROUP")], FUN=length)


##### MERGE THEM TOGETHER - LOOKING LIKE BIA) #############
bia$METHOD<-"CPCE"
bia$SHORT_CODE<-"BIA"

cnet$POINTS<-1
cnet$METHOD<-"CNET"
cnet$REP<-cnet$REPLICATE
cnet$IMAGE_NAME<-cnet$ORIGINAL_FILE_NAME
cnet$PHOTOID<-cnet$IMAGE_NUMBER
cnet$TIER_1<-cnet$CATEGORY_CODE
cnet$TIER_2<-cnet$SUBCATEGORY_CODE
cnet$TIER_3<-cnet$GENERA_CODE

#Combined bia and cnet data
FIELDS_TO_RETAIN<-c("MISSIONID","METHOD", "REGION", "REGION_NAME","OBS_YEAR","ISLAND", "SITEVISITID","SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "CLIMATE_STATION_YN", "MIN_DEPTH", "MAX_DEPTH", "HABITAT_CODE", "DATE_", "REP", "IMAGE_NAME", "PHOTOID", "ANALYST", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "SHORT_CODE", "POINTS")
x<-bia[,FIELDS_TO_RETAIN]; head(x)
y<-cnet[,FIELDS_TO_RETAIN]; head(y)

ab<-rbind(x, y)
#ab$DATE_ <- as.Date(ab$DATE_, "%m/%d/%Y")
ab$DATE_ <- as.factor(ab$DATE_)


SURVEY_INFO<-c("OBS_YEAR", "REGION",  "ISLAND")
survey_island<-Aggregate_InputTable(cnet, SURVEY_INFO)

SURVEY_INFO<-c("MISSIONID","REGION","OBS_YEAR", "ISLAND", "SITEVISITID","SITE","LATITUDE","LONGITUDE","REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "CLIMATE_STATION_YN", "MIN_DEPTH", "MAX_DEPTH", "HABITAT_CODE","DATE_")
survey_site<-Aggregate_InputTable(ab, SURVEY_INFO)

#Change column names to match with SM
colnames(survey_site)[colnames(survey_site)=="MIN_DEPTH"]<-"SITE_MIN_DEPTH_FT" #Change column name
colnames(survey_site)[colnames(survey_site)=="MAX_DEPTH"]<-"SITE_MAX_DEPTH_FT" #Change column name


#Save list of sites
write.csv(survey_site,"All Photoquad Sites.csv")

#Remove Lat,Long and date before merging with SITE MASTER. The Lat and Long are 1 digit longer in the SM than the BIA data and will not merge properly with SM
survey_site.<- subset(survey_site, select=-c(LATITUDE,LONGITUDE,DATE_))

#Read in original SITE MASTER
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SITE MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)
sm$DATE_ <- as.factor(sm$DATE_)


#Merge Site master with list of BIA sites
allsite<-merge(survey_site.,sm,by=c("REGION","ISLAND", "OBS_YEAR","SITEVISITID","SITE","REEF_ZONE", "DEPTH_BIN","SITE_MAX_DEPTH_FT", "SITE_MIN_DEPTH_FT", "HABITAT_CODE"),all=TRUE)
head(allsite)

#Indentify sites that did not merge properly
test<-ddply(allsite,.(SITE),summarize,sites=length(SITE));subset(test,sites>1) #this should return 0 rows

#Generate a list of sites found in the BIA data, but not SM and remove all columns except OBS_YEAR and SITEVISITID
miss.sites<-allsite[is.na(allsite$METHOD), ]
miss.sites.<- subset(miss.sites, select=c(OBS_YEAR,SITEVISITID))
nrow(miss.sites.)


#Extract all of the metadata for the sites that need to be incorporated into the SITE MASTER file. 
#The missing are a combination of permanent sites, climate stations, special projects and BAK and HOW 2017 benthic sites that were only surveyed for juveniles and photoquads
sitestomerge<-merge(survey_site,miss.sites.,by=c("OBS_YEAR","SITEVISITID"));nrow(sitestomerge)
sitestomerge<- subset(sitestomerge, select=-c(MISSIONID))
sitestomerge$METHOD<-NA
sitestomerge$EXCLUDE_FLAG<-NA
sitestomerge$SEC_NAME<-NA
sitestomerge$HUMANS20<-NA
sitestomerge$HUMANS200<-NA
sitestomerge$TYPE<-NA
sitestomerge$ANALYSIS_SCHEME<-NA
sitestomerge$ANALYSIS_YEAR<-NA
sitestomerge$TUT2012<-NA
sitestomerge$OTHER_AREA_GROUPING<-NA
sitestomerge$BENTHIC_SEC_CODE<-NA

#add columns to sm to rbind correctly with sitestomerge
sm$CLIMATE_STATION_YN<-NA
sm$PERM_SITE<-NA

#Rbind the original SITE MASTER and missing sites. I tried to use merge(), but the lat, long and dates aren't matching. this is the cleanest way to combine dfs
sm.new<-rbind(sm,sitestomerge)
write.csv(sm.new,"tmpSITE MASTER.csv")  #Courtney manually modified this version to generate SITE MASTER_V2.csv (tweaks to Exclude flag, sec_name, analysis_scheme, etc)
