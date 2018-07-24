rm(list=ls())

#### 2017 Howland, Baker, Wake, Jarvis, 2016 Jarvis)
#### Strata scale BIA  ... coral 

setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Base R/Base Data Files")

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(RODBC)            # to connect to oracle
source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/core_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/fish_team_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/Islandwide Mean&Variance Functions.R")


#BIA data - this is from CPCE
load("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/ALL_BIA_STR_RAW_NEW.rdata")   #bia
bia$SITE<-SiteNumLeadingZeros(bia$SITE)

#CNET data - from CoralNet
load("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/ALL_BIA_STR_CNET.rdata")	#cnet
cnet$SITE<-SiteNumLeadingZeros(cnet$SITE)

#CLI Site data - this is from CPCE
load("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/ALL_BIA_CLIMATE_PERM.rdata")   #cli
cli$SITE<-SiteNumLeadingZeros(cli$SITE)

##Just adding CLIMATE SITES to end of bia
bia<-rbind(bia, cli)


##Generate Table of all the bia categories
head(bia)
bia_tab<-aggregate(bia$POINTS, by=bia[,c("TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME")], FUN=length)
#write.csv(bia_tab, file="BIA categories.csv")
table(bia$TIER_1)
table(bia$TIER_2)

##Generate Table of all the bia categories
head(cnet)
cnet_tab<-aggregate(cnet$ROUNDID, by=cnet[,c("CATEGORY_CODE", "CATEGORY_NAME", "SUBCATEGORY_CODE", "SUBCATEGORY_NAME", "GENERA_CODE", "GENERA_NAME", "SHORT_CODE", "FUNCTIONAL_GROUP")], FUN=length)
#write.csv(cnet_tab, file="CNET categories.csv")


##### MERGE THEM TOGETHER - LOOKING LIKE BIA) #############
bia$METHOD<-"CPCE"
bia$SHORT_CODE<-"BIA"
#bia$FUNCTIONAL_GROUP<-"BIA"    # IDW - ACTUALLY FUNCTIONAL_GROUP SEEMS A BIT MIXED UP ... FROM QUICK LOOK AT CNET FILE, IT CAN TAKE DIFFERENT VALUES FOR SAME CODES (eg ALGAE or Hare Substrate) - SO GOING TO IGNORE IT!

cnet$POINTS<-1
cnet$METHOD<-"CNET"
cnet$REP<-cnet$REPLICATE
cnet$IMAGE_NAME<-cnet$ORIGINAL_FILE_NAME
cnet$PHOTOID<-cnet$IMAGE_NUMBER
cnet$TIER_1<-cnet$CATEGORY_CODE
cnet$TIER_2<-cnet$SUBCATEGORY_CODE
cnet$TIER_3<-cnet$GENERA_CODE


FIELDS_TO_RETAIN<-c("METHOD", "REGION", "OBS_YEAR", "ISLAND", "SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN", "PERM_SITE", "CLIMATE_STATION_YN", "MIN_DEPTH", "MAX_DEPTH", "HABITAT_CODE", "DATE_", "REP", "IMAGE_NAME", "PHOTOID", "ANALYST", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "SHORT_CODE", "POINTS")
x<-bia[,FIELDS_TO_RETAIN]; head(x)
y<-cnet[,FIELDS_TO_RETAIN]; head(y)

ab<-rbind(x, y)
#write.csv(ab, file="tmp All BIA BOTH METHODS.csv")


CATEGORY_FIELDS<-c("METHOD", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "SHORT_CODE")
summary(ab[,CATEGORY_FIELDS])
levels(ab$TIER_3)<-c(levels(ab$TIER_3), levels(ab$TIER_2))
levels(ab$GENERA_NAME)<-c(levels(ab$GENERA_NAME), levels(ab$SUBCATEGORY_NAME))
ab[is.na(ab$TIER_3), ]$TIER_3<-ab[is.na(ab$TIER_3), ]$TIER_2
ab[is.na(ab$GENERA_NAME), ]$GENERA_NAME<-ab[is.na(ab$GENERA_NAME), ]$SUBCATEGORY_NAME
ab<-droplevels(ab)

#all.tab<-aggregate(ab$POINTS, by=ab[,c("METHOD", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "SHORT_CODE")], FUN=length)
#write.csv(all.tab, file="tmp All CATEGORIES BOTH METHODS.csv")

### SOME CLEAN UP

#CREATING CLASS EMA "Encrusting Macroalgae
levels(ab$TIER_1)<-c(levels(ab$TIER_1), "EMA")
levels(ab$CATEGORY_NAME)<-c(levels(ab$CATEGORY_NAME), "Encrusting macroalga")
ab[ab$GENERA_NAME %in% c("Peyssonnelia sp", "Encrusting macroalga"),]$TIER_1<-"EMA"
ab[ab$GENERA_NAME %in% c("Peyssonnelia sp", "Encrusting macroalga"),]$TIER_2<-"EMA"
ab[ab$GENERA_NAME %in% c("Peyssonnelia sp", "Encrusting macroalga"),]$SUBCATEGORY_NAME<-"Encrusting macroalga"
ab[ab$GENERA_NAME %in% c("Peyssonnelia sp", "Encrusting macroalga"),]$CATEGORY_NAME<-"Encrusting macroalga"


#Some other Clean UP
unique(ab[ab$GENERA_NAME %in% c("Blue-green macroalga"), c("TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME")])
ab[ab$GENERA_NAME %in% c("Blue-green macroalga"),]$SUBCATEGORY_NAME <-"Blue-green macroalga"
ab[ab$GENERA_NAME %in% c("Blue-green macroalga"),]$TIER_2 <-"BGMA"

unique(ab[ab$GENERA_NAME %in% c("Halimeda sp"),c("TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME")])
ab[ab$GENERA_NAME %in% c("Halimeda sp"),]$SUBCATEGORY_NAME<-"Upright macroalga"
ab[ab$GENERA_NAME %in% c("Halimeda sp"),]$TIER_2<-"HAL"
ab[ab$GENERA_NAME %in% c("Halimeda sp"),]$TIER_3<-"HALI"

unique(ab[ab$GENERA_NAME %in% c("Lobophora sp"), c("TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME")]) # This is msot disturbing one as it can be either!
ab[ab$GENERA_NAME %in% c("Lobophora sp"),]$SUBCATEGORY_NAME<-"Upright macroalga"
ab[ab$GENERA_NAME %in% c("Lobophora sp"),]$TIER_2<-"UPMA"



all.tab<-aggregate(ab$POINTS, by=ab[,c("METHOD", "TIER_1", "CATEGORY_NAME", "TIER_2", "SUBCATEGORY_NAME", "TIER_3", "GENERA_NAME", "SHORT_CODE")], FUN=length)
write.csv(all.tab, file="All CATEGORIES BOTH METHODS CLEANED.csv")

save(ab, file="All BIA BOTH METHODS cleanUPD.RData")


#Generate a SITE table
SITE_FIELDS<-c("METHOD", "REGION", "OBS_YEAR", "DATE_", "ISLAND", "PERM_SITE", "CLIMATE_STATION_YN", "SITE", "LATITUDE", "LONGITUDE", "REEF_ZONE", "DEPTH_BIN")
summary(ab[,SITE_FIELDS])
levels(ab$REEF_ZONE)<-c(levels(ab$REEF_ZONE), "UNKNOWN")
levels(ab$DEPTH_BIN)<-c(levels(ab$DEPTH_BIN), "UNKNOWN")

ab[is.na(ab$REEF_ZONE),]$REEF_ZONE<-"UNKNOWN"
ab[is.na(ab$DEPTH_BIN),]$DEPTH_BIN<-"UNKNOWN"

sites<-aggregate(ab[,"POINTS"],by=ab[,SITE_FIELDS], sum)
sites$x<-NULL
write.csv(sites, file="All BIA Sites.csv")
dim(sites)

