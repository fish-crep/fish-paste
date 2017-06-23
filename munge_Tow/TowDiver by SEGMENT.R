rm(list=ls())
#setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Data Requests/Adel TowedDiver")
#### THIS TOW GENRATES SECTOR LEVEL INFORMATION ON BIOMASS BY TOW SPECIES, USING SEGMENT AS THE BASE UNIT
#### CODE HERE CAN BE USED AS DEFAULT TO GENERATE BIOMASS OR ABUNDANCE AT SEGMENT OR TOW LEVEL

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/fish_team_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/Islandwide Mean&Variance Functions.R")

#read info that gives sector for each segment
towss<-read.csv("data/tow_segment_sectors.csv")
towss$SEC_NAME<-as.character(towss$ANALYSIS_SEC)
towss$ANALYSIS_SEC<-NULL
table(towss$SEC_NAME)
#write.csv(unique(towss$ANALYSIS_SEC), file="List of SECS.csv")

#read in information in depths of tows with known missing depths
tmd<-read.csv("Tow Missing Depths.csv")
tmd<-tmd[,c("DIVEID", "Average")]



load(file="data/ALL_TOW_FISH_RAW.rdata")
wd<-df

nosc<-which(wd$SPECIES == "NOSC")
wd<-wd[-nosc,]
miss<-which(wd$SPECIES == "MISS")
wd<-wd[-miss,]

############################# FILTER TO FOREREEF FROM 2010 ONWARDS ###########################
# NB this means that we lose tows and segments where REEF_ZONE is Unspecified .. so setting those to Forereef where we know what they are

wd<-subset(wd, wd$OBS_YEAR>2009)

unique(wd[wd$REEF_ZONE=="Unspecified",]$ISLAND)
wd[wd$ISLAND=="Tutuila",]$REEF_ZONE<-"Forereef"     # Tut tows are always forereef
unique(wd[wd$REEF_ZONE=="Unspecified", c("ISLAND", "OBS_YEAR", "DIVEID", "DEPTH")])
# Kingman & Palmyra tows are deep enough that I think we can assume they are FOREREEF
wd[wd$REEF_ZONE=="Unspecified" & wd$ISLAND %in% c("Kingman", "Palmyra"),]$REEF_ZONE<-"Forereef"     # Tut tows are always forereef
unique(wd[wd$REEF_ZONE=="Unspecified", c("ISLAND", "OBS_YEAR", "DIVEID")])    ##### THERE LIEKLY IS METADATA THAT WOULD ALLOW US TO FIX THES, BUT FOR NOW I THINK THEY SHOULD BE DROPPED - THEY WILL BE BY CODE BELOW

wd<-subset(wd, wd$REEF_ZONE %in% c("Forereef"))
wd<-droplevels(wd)
dim(wd)

#merge in the SEC_NAME
head(towss)
wd<-merge(wd, towss[,c("DIVEID", "SEGMENT", "SEC_NAME")], by=c("DIVEID", "SEGMENT"), all.x=T)
summary(wd)

wd$ISLAND<-as.character(wd$ISLAND)
wd[wd$REGION %in% c("PRIAs", "NWHI", "MARIAN") & wd$ISLAND != "Guam",]$SEC_NAME<-wd[wd$REGION %in% c("PRIAs", "NWHI", "MARIAN") & wd$ISLAND != "Guam",]$ISLAND
wd[wd$ISLAND=="South Bank",]$SEC_NAME<-"South Bank"
table(wd[is.na(wd$SEC_NAME),]$ISLAND)
tmp<-wd[is.na(wd$SEC_NAME),]
tmp[,c("ISLAND", "DIVEID", "SEGMENT", "LATITUDE", "LONGITUDE", "OBS_YEAR", "DEPTH", "REEF_ZONE")]
#seem to all be <10m or greater than 20m


#CONSIDER REMOVING SEGMENTS WITH APPARENTLY BOGUS LENGTHS
wd[wd$PROJECTEDLENGTH<30, c("REGION", "DIVEID", "SEGMENT", "PROJECTEDLENGTH", "OBS_YEAR", "SPECIES", "COUNT")]
#wd[wd$DIVEID %in% tmp, c("DIVEID", "SEGMENT", "PROJECTEDLENGTH", "OBS_YEAR", "SPECIES", "COUNT"),]
#head(wd)

length(unique(wd$DIVEID))

wd[is.na(wd$COUNT),]$COUNT<-0
wd[is.na(wd$PROJECTEDLENGTH),]$PROJECTEDLENGTH
wd[is.na(wd$DEPTH),]$DEPTH<-0	
wd[is.na(wd$SIZE_),]$SIZE_<-0	
wd[is.na(wd$CENTROIDLAT),]$CENTROIDLAT<-0	
wd[is.na(wd$CENTROIDLON),]$CENTROIDLON<-0	
wd[is.na(wd$BIOMASS),]$BIOMASS<-0
wd[is.na(wd$BIOMASS_G_M2),]$BIOMASS_G_M2<-0
wd[is.na(wd$TEMPERATURE),]$TEMPERATURE<-0	

tmp.lev<-levels(wd$REEF_ZONE); head(tmp.lev)
levels(wd$REEF_ZONE)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wd$FAMILY); head(tmp.lev)
levels(wd$FAMILY)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wd$TAXONNAME); head(tmp.lev)
levels(wd$TAXONNAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wd$TROPHIC_MONREP); head(tmp.lev)
levels(wd$TROPHIC_MONREP)<-c(tmp.lev, "UNKNOWN")

wd[is.na(wd$REEF_ZONE),"REEF_ZONE"]<-"UNKNOWN"
wd[is.na(wd$TAXONNAME),"TAXONNAME"]<-"UNKNOWN"
wd[is.na(wd$FAMILY),"FAMILY"]<-"UNKNOWN"
wd[is.na(wd$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"
wd[is.na(wd$SEC_NAME),"SEC_NAME"]<-"UNKNOWN"

sort(unique(wd$COMMONFAMILYALL))
WIERD_FAMILIES<-c("Tuna", "Conger or Garden Eel", "Unspecified Fish", "Worm or Snake Eel",  "Moray")
wd[wd$COMMONFAMILYALL %in% WIERD_FAMILIES,]$COUNT<-0
wd[wd$COMMONFAMILYALL %in% WIERD_FAMILIES,]$SPECIES<-"NONE"


wd<-droplevels(wd)

#summarize tow information (length, depth, lat-long, date)
#    first do that by segment
TOW_DATA<-c("REGION", "ISLAND", "SEC_NAME", "CENTROIDLAT", "CENTROIDLON", "DATE_", "DEPTH", "STARTLOCALTIME", "STRATA", "PROJECTEDLENGTH", "DIVEID")   

SEGMENT_ID2<-c( "DIVEID", "SEGMENT")
SEGMENT_INFO<-c("REGION", "ISLAND", "DATE_", "OBS_YEAR")
SEGMENT_INFO_TO_SUM<-c("PROJECTEDLENGTH")
SEGMENT_INFO_TO_AVE<-c("CENTROIDLAT", "CENTROIDLON", "DEPTH", "TEMPERATURE")
SEGMENT_INFO_TO_MIN<-c("STARTLOCALTIME")
SEGMENT_INFO_TO_MODE<-c("REEF_ZONE")

SEGMENT_FIELDS<-c(SEGMENT_INFO, SEGMENT_INFO_TO_SUM, SEGMENT_INFO_TO_AVE, SEGMENT_INFO_TO_MODE, SEGMENT_ID2)
DIVE_INFO<-c("DIVEID", SEGMENT_INFO)

length(unique(wd$DIVEID))

wd$biomass_g<-wd$LW_A*wd$COUNT*((wd$SIZE*wd$LENGTH_CONVERSION_FACTOR)^wd$LW_B)
#wd[is.na(wd$biomass_g),]$biomass_g<-0

length(unique(wd$DIVEID))

#Fix errors in the database
wd[wd$SIZE_<50 & wd$SIZE_ !=0, c("OBS_YEAR", "REGION", "DIVEID", "SEGMENT", "DIVER1", "SPECIES", "COUNT", "SIZE_")]
wd[wd$SPECIES=="CHUD" & wd$SIZE_<50,]$COUNT<-0    # SHould be a PRESENCE recrd
wd<-droplevels(wd)

SEGMENT_ID<-c( "DIVEID", "SEGMENT", "SEGMENTID")
SEGMENT_INFO<-c("REGION", "ISLAND", "SEC_NAME", "DATE_", "OBS_YEAR", "PROJECTEDLENGTH", "CENTROIDLAT", "CENTROIDLON", "DEPTH", "TEMPERATURE", "REEF_ZONE")
SEGMENT_FIELDS<-c(SEGMENT_ID, SEGMENT_INFO)

si<-aggregate(wd$COUNT, by=wd[,SEGMENT_FIELDS], sum, na.rm=F)## aggregate sums total count of all fishes per record, using field_list 
si<-si[,SEGMENT_FIELDS] # drop the count - was just using that to generate a summary table

length(unique(si$DIVEID))
setdiff(wd$DIVEID,si$DIVEID)
	
#sum up to total length etc.. for the dive ID
#set depth, and centroid lat-long field to NaN if zero ... 
si[si$DEPTH==0,"DEPTH"]<-NaN
si[si$CENTROIDLAT==0,"CENTROIDLAT"]<-NaN
si[si$CENTROIDLON==0,"CENTROIDLON"]<-NaN
si[si$TEMPERATURE==0,"TEMPERATURE"]<-NaN

#Fixing Others from a csv file with missing depth info
dim(si)
si<-merge(si, tmd, by="DIVEID", all.x=T)
summary(si)
si[!is.na(si$Average),]$DEPTH
si[!is.na(si$Average),]$DEPTH<-si[!is.na(si$Average),]$Average
si$Average<-NULL

length(unique(si$DIVEID))

si$WDEPTH<-si$DEPTH
si[is.na(si$DEPTH),]$WDEPTH<-15.010101010101010101
si<-subset(si, si$WDEPTH<20.000000000000001)
si<-subset(si, si$WDEPTH>9.9999999999999999)
si$WDEPTH<-NULL
summary(si)

length(unique(si$DIVEID))
unique(si$ANALYSIS_SEC)
write.csv(si, file="tmp Segments.csv")

#Now limmit wd to segments that are in si
wd<-subset(wd, wd$SEGMENTID %in% unique(si$SEGMENTID))
length(unique(si$DIVEID))
length(unique(wd$DIVEID))


############################################################
### Now sum abundance and biomass data per species per segment,
### and convert to gm2 and abund m2
############################################################
#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","FAMILY", "TAXONNAME", "TROPHIC_MONREP")
t.species.table<-aggregate(wd$COUNT,by=wd[,FISH_SPECIES_FIELDS], sum, na.rm=FALSE)

sum.abund.bio<-aggregate(wd[,c("COUNT", "biomass_g")],by=wd[,c("DIVEID", "SEGMENT", "FAMILY", "SPECIES")], sum, na.rm=TRUE)
dim(sum.abund.bio)
length(unique(sum.abund.bio$DIVEID))

tfd<-merge(sum.abund.bio, si[,c("DIVEID", "SEGMENT", "PROJECTEDLENGTH")], by=c("DIVEID", "SEGMENT"))
tfd$BIOGM2<-tfd$biomass_g / (10*tfd$PROJECTEDLENGTH)
tfd$ABUNM2<-tfd$COUNT / (10*tfd$PROJECTEDLENGTH)
dim(tfd)
## add consumer group to tow data, filter to forereef ONLY, add depth to give option to later filter by depth range .. then pool up by island & year and save SE

# add data about the segment (island, zone, year, depth)
tfd<-merge(tfd, si[, c("DIVEID", "SEGMENT", "SEGMENTID", "REGION", "ISLAND", "SEC_NAME", "REEF_ZONE", "OBS_YEAR", "CENTROIDLAT", "CENTROIDLON", "DEPTH", "TEMPERATURE")], by=c("DIVEID", "SEGMENT"))
dim(tfd)
head(tfd)
summary(tfd)
length(unique(tfd$DIVEID))
write.csv(tfd, file="TMPsegmentData.csv")

summary(tfd$DEPTH)
length(unique(tfd$DIVEID))

#set Depth of NA to -999
head(tfd)
tfd[is.na(tfd$DEPTH),]$DEPTH<- -999
tfd[is.na(tfd$CENTROIDLAT),]$CENTROIDLAT<- 0
tfd[is.na(tfd$CENTROIDLON),]$CENTROIDLON<- 0
tfd[is.na(tfd$TEMPERATURE),]$TEMPERATURE<- -999

tfd<-droplevels(tfd)
length(unique(tfd$DIVEID))

# SET DATA_VAL to the NUMBER WE WANT TO USE ... SO WE CAN RUN ALL CODE BELOW FOR EITHER ABUN OR BIO
tfd$DATA_VAL<-tfd$BIOGM2

xx<-aggregate(tfd$DATA_VAL, by=tfd[,c("DIVEID", "SEGMENT", "SEGMENTID", "REGION", "ISLAND", "SEC_NAME", "OBS_YEAR", "REEF_ZONE", "CENTROIDLAT", "CENTROIDLON", "PROJECTEDLENGTH", "DEPTH", "FAMILY", "SPECIES")], sum, na.rm=TRUE)
dimnames(xx)[[2]]<-c("DIVEID", "SEGMENT", "SEGMENTID", "REGION", "ISLAND", "SEC_NAME", "YEAR", "STRATA", "CENTROIDLAT", "CENTROIDLON", "PROJECTEDLENGTH", "DEPTH_M", "FAMILY", "SPECIES", "DATA_VAL")
#now format this more or less as a crosstab, with field of interest as column variable
wsd<-cast(xx, DIVEID + SEGMENT + SEGMENTID + REGION + ISLAND + SEC_NAME + YEAR + STRATA + CENTROIDLAT + CENTROIDLON + PROJECTEDLENGTH + DEPTH_M ~ SPECIES, value="DATA_VAL", fun.aggregate=sum, fill=0); head(wsd)
length(unique(wsd$DIVEID))
wsd$TotFish<-rowSums(wsd[,levels(xx$SPECIES)])
dim(wsd)
length(unique(wsd$DIVEID))
write.csv(wsd, file="Adel SegmentData.csv")


######## Now aggregate at the sector level

data.cols<-c(unique(as.character(xx$SPECIES)), "TotFish")
wsd$SITEVISITID<-wsd$SEGMENTID    #Treat each segment as the base unit      (or each partial tow?)

data.cols<-c(unique(as.character(xx$SPECIES)), "TotFish")
tsd<-Calc_PerStrata(wsd, data.cols, c("REGION", "ISLAND", "SEC_NAME"))
save(tsd, file="Adel SEC Data By segment.RData")
  # CAN instead pool to partial tow first, then average those - actually is probably better

wtd<-aggregate(wsd[,data.cols], by=wsd[,c("REGION", "ISLAND", "SEC_NAME", "DIVEID")], FUN=mean)
dim(wtd)
wtd$SITEVISITID<-wtd$DIVEID
ttd<-Calc_PerStrata(wtd, data.cols, c("REGION", "ISLAND", "SEC_NAME"))
save(ttd, file="Adel SEC Data By TOW.RData")
  # CAN instead pool to partial tow first, then average those - actually is probably better
