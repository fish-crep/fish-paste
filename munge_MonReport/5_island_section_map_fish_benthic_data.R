rm(list=ls())
setwd("D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/data/Data Outputs")

##  selecting the map data
load("clean_working_site_data_used_in_higher_pooling_for_report.rdata")
maps<-wsd.uncap


## fishbiomass and benthic data is going to be treated differently - as benthic data wasn't collected in 2009, 
## which creates NAs and screws up Arc, so drop them.....

## site fish data

summary(maps)

##  select metadata and data cols

fishmaps<-maps[,c("OBS_YEAR", "ANALYSIS_YEAR","REGION", "ISLAND","SEC_NAME", "REEF_ZONE","SITE", "DEPTH_BIN", "LATITUDE", "LONGITUDE",
                  "ANALYSIS_SCHEME", "PRIMARY", "SECONDARY", "PISCIVORE", "PLANKTIVORE", "TotFish", "0_20", "20_50", "50_plus")]

## cap values at 500 for map (including all the outliers make bubble sizes mental)
fishmaps$fish_500<-fishmaps$TotFish
fishmaps$fish_500[which(fishmaps$fish_500 >500)]<-501
  
## cap values at 1000 for map (including all the outliers make bubble sizes mental)
fishmaps$fish_1000<-fishmaps$TotFish
fishmaps$fish_1000[which(fishmaps$fish_1000 >1000)]<-1001  

## cap values at 100 for map (for populated islands)
fishmaps$fish_100<-fishmaps$TotFish
fishmaps$fish_100[which(fishmaps$fish_100 >100)]<-101  

## cap values at 200 for map (for populated islands)
fishmaps$fish_200<-fishmaps$TotFish
fishmaps$fish_200[which(fishmaps$fish_200 >200)]<-201 

write.csv(fishmaps, "fishmaps_nspc_clean_wsd_2009_19.csv")


## site benthic data
benthicmaps<-maps[,c("OBS_YEAR", "REGION", "ISLAND","SEC_NAME","REEF_ZONE", "SITE", "DEPTH_BIN", "LATITUDE", "LONGITUDE",
                 "ANALYSIS_SCHEME","HARD_CORAL", "MA", "CCA", "SAND","OTHER_BENTHIC", "BSR")]

## drop the 2009 sites without benthic data (n = 380, all PRIAs and NWHI from 2009)
benthicmaps<-benthicmaps[-which(is.na(benthicmaps$HARD_CORAL)),]


## a couple of problem sites for the benthic substrate ratio
which(benthicmaps$BSR == Inf)
## change these these three deeps sites from 2010 which had Inf values for the BSR (PAL-154,KIN-129, KIN-128) 
## just put BSR as 25 (upper level of BSR as it stands, so won't affect symbol size) (i.e. drop the NA otherwise Arc will cock up) and exclude these sites from the BSR map

benthicmaps[which(benthicmaps$BSR == Inf),"BSR"]<-as.numeric(25)

# Cap BSR at 15 so bubbles aren't outrageous
benthicmaps$BSR_15<-benthicmaps$BSR
benthicmaps$BSR_15[which(benthicmaps$BSR_15>15)]<-16

write.csv(benthicmaps, "benthicmaps_nspc_clean_wsd_2009_19.csv")


#### NOW GIVE THESE FILES TO PAULA ####
