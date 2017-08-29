rm(list=ls())
#setwd


library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
source("lib/fish_team_functions_share.R")
source("lib/Islandwide Mean&Variance Functions.R")

#################################################################### TOW WORKUP ###########################################################################

load("data/NOAA_PACIFIC_RAMP_FISH_TOW_RAW.RData")
head(wd)
 
## CONSIDER REMOVING SEGMENTS WITH APPARENTLY BOGUS LENGTHS
# wd[wd$SEGMENT_LENGTH_M<30,]
 
#summarize tow information (length, depth, lat-long, date)
#    first do that by segment
TOW_DATA<-c("REGION", "ISLAND", "CENTROIDLAT", "CENTROIDLON", "DATE", "DEPTH_M", "START_TIME", "REEF_ZONE", "SEGMENT_LENGTH_M", "DIVEID")   

SEGMENT_ID2<-c( "DIVEID", "SEGMENT")
SEGMENT_INFO<-c("REGION", "ISLAND", "DATE", "OBS_YEAR")
SEGMENT_INFO_TO_SUM<-c("SEGMENT_LENGTH_M")
SEGMENT_INFO_TO_AVE<-c("CENTROIDLAT", "CENTROIDLON", "DEPTH_M", "TEMPERATURE")
SEGMENT_INFO_TO_MIN<-c("START_TIME")
SEGMENT_INFO_TO_MODE<-c("REEF_ZONE")

SEGMENT_FIELDS<-c(SEGMENT_INFO, SEGMENT_INFO_TO_SUM, SEGMENT_INFO_TO_AVE, SEGMENT_INFO_TO_MODE, SEGMENT_ID2)
DIVE_INFO<-c("DIVEID", SEGMENT_INFO)

length(unique(wd$DIVEID))

wd$biomass_g<-wd$LW_A*wd$COUNT*((wd$SIZE*wd$LENGTH_CONVERSION_FACTOR)^wd$LW_B)
#wd[is.na(wd$biomass_g),]$biomass_g<-0

segment.info<-aggregate(wd$COUNT, by=wd[,SEGMENT_FIELDS], sum, na.rm=F)## aggregate sums total count of all fishes per record, using field_list 
segment.info<-segment.info[,SEGMENT_FIELDS] # drop the count - was just using that to generate a summary table

length(unique(segment.info$DIVEID))
setdiff(wd$DIVEID,segment.info$DIVEID)
	
#sum up to total length etc.. for the dive ID
#set depth, and centroid lat-long field to NaN if zero ... 
segment.info[segment.info$DEPTH==0,"DEPTH"]<-NaN
segment.info[segment.info$CENTROIDLAT==0,"CENTROIDLAT"]<-NaN
segment.info[segment.info$CENTROIDLON==0,"CENTROIDLON"]<-NaN
segment.info[segment.info$TEMPERATURE==0,"TEMPERATURE"]<-NaN

sum.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_SUM],by=segment.info[,DIVE_INFO], sum, na.rm=TRUE);
dimnames(sum.segments)[[2]]<-c(DIVE_INFO, SEGMENT_INFO_TO_SUM)
ave.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_AVE],by=segment.info[,DIVE_INFO], mean, na.rm=TRUE)  
med.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_AVE],by=segment.info[,DIVE_INFO], median, na.rm=TRUE)  
mode.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_MODE],by=segment.info[,DIVE_INFO], Mode)
dimnames(mode.segments)[[2]]<-c(DIVE_INFO, SEGMENT_INFO_TO_MODE)

tt<-merge(ave.segments, mode.segments[,c("DIVEID",SEGMENT_INFO_TO_MODE)], by="DIVEID")
dive.info<-merge(tt, sum.segments[,c("DIVEID",SEGMENT_INFO_TO_SUM)], by="DIVEID")
dim(dive.info)

di<-dive.info
#write.csv(table(di$OBS__YEAR, di$ISLAND), file="tmpTOWS.csv")

#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","FAMILY", "TAXONNAME", "CONSUMER_GROUP")
t.species.table<-aggregate(wd$COUNT,by=wd[,FISH_SPECIES_FIELDS], sum, na.rm=FALSE)
write.csv(t.species.table, file="TOW WORKUPS/Fish Species.csv")

sum.abund.bio<-aggregate(wd[,c("COUNT", "biomass_g")],by=wd[,c("DIVEID", "FAMILY", "SPECIES")], sum, na.rm=TRUE)
dim(sum.abund.bio)
length(unique(sum.abund.bio$DIVEID))

tfd<-merge(sum.abund.bio, dive.info[,c("DIVEID","SEGMENT_LENGTH_M")], by="DIVEID")
tfd$BIOGM2<-tfd$biomass_g / (10*tfd$SEGMENT_LENGTH_M)
tfd$ABUNM2<-tfd$COUNT / (10*tfd$SEGMENT_LENGTH_M)
dim(tfd)
## add consumer group to tow data, filter to forereef ONLY, add depth to give option to later filter by depth range .. then pool up by island & year and save SE

# add data about the tow (island, zone, year, depth)
tfd<-merge(tfd, dive.info[, c("DIVEID", "REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR", "CENTROIDLAT", "CENTROIDLON", "DEPTH_M", "TEMPERATURE")], by="DIVEID")
dim(tfd)
head(tfd)
summary(tfd)
length(unique(tfd$DIVEID))

summary(tfd$DEPTH)
length(unique(tfd$DIVEID))

#set Depth of NA to -999
#tfd[is.na(tfd$DEPTH_M),]$DEPTH_M<- -999
tfd[is.na(tfd$CENTROIDLAT),]$CENTROIDLAT<- 0
tfd[is.na(tfd$CENTROIDLON),]$CENTROIDLON<- 0

tfd<-droplevels(tfd)
length(unique(tfd$DIVEID))

# SET DATA_VAL to EITHER ABUN OR BIO, DEPENDING ON VARIABLE OF INTEREST


#### EXAMPLE TO POOL DATA AT TOW LEVEL - IN THIS CASE FOR BIOMASS PER FAMILY
tfd$DATA_VAL<-tfd$BIOGM2
xx<-aggregate(tfd$DATA_VAL, by=tfd[,c("DIVEID", "REGION", "ISLAND", "OBS_YEAR", "REEF_ZONE", "CENTROIDLAT", "CENTROIDLON", "SEGMENT_LENGTH_M", "DEPTH_M", "FAMILY")], sum, na.rm=TRUE)
wtd<-cast(xx, DIVEID + REGION + ISLAND + OBS_YEAR + REEF_ZONE + CENTROIDLAT + CENTROIDLON + SEGMENT_LENGTH_M + DEPTH_M ~ FAMILY, value="x", fun.aggregate=sum, fill=0); head(wtd)
wtd$TotFish<-rowSums(wtd[,levels(xx$FAMILY)])
# wtd$TotFishMinusManta<-wtd$TotFish-wtd$Myliobatidae
length(unique(wtd$DIVEID))
#write.csv(wtd, file="ESD NCRMP TowData.csv")
save(wtd, file="TOW WORKUPS/Fish Tow example FAMILY Biomass.RData")



#### EXAMPLE TO POOL DATA AT TOW LEVEL - IN THIS CASE FOR ABUNDANCE PER SPECIES
tfd$DATA_VAL<-tfd$ABUNM2
xx<-aggregate(tfd$DATA_VAL, by=tfd[,c("DIVEID", "REGION", "ISLAND", "OBS_YEAR", "REEF_ZONE", "CENTROIDLAT", "CENTROIDLON", "SEGMENT_LENGTH_M", "DEPTH_M", "SPECIES")], sum, na.rm=TRUE)
wtd<-cast(xx, DIVEID + REGION + ISLAND + OBS_YEAR + REEF_ZONE + CENTROIDLAT + CENTROIDLON + SEGMENT_LENGTH_M + DEPTH_M ~ SPECIES, value="x", fun.aggregate=sum, fill=0); head(wtd)
wtd$TotFish<-rowSums(wtd[,levels(xx$FAMILY)])
# wtd$TotFishMinusManta<-wtd$TotFish-wtd$Myliobatidae
length(unique(wtd$DIVEID))
#write.csv(wtd, file="ESD NCRMP TowData.csv")
save(wtd, file="TOW WORKUPS/Fish Tow example SPECIES Abundance.RData")
