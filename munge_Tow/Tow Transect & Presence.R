rm(list=ls())
setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Data Requests/BOMU")
# SET UP ------------------------------------------------------------------
#setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Base R/Base Data Files")
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/fish_team_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/Islandwide Mean&Variance Functions.R")

#################################################################### TOW WORKUP ###########################################################################


#MERGE THE TOW OBSERVATIONS WITH TOW PRESENCE ABSENCE

load(file="/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/data/ALL_TOW_FISH_RAW.rdata")
wd<-df
wd$OBS_TYPE<-"LINE"
head(wd)
## drop any rows which have NOSC and MISS in the species field, these are tows which were aborted part way through
## remove these so that the tow length is corrected....
nosc<-which(wd$SPECIES == "NOSC")
wd<-wd[-nosc,]
miss<-which(wd$SPECIES == "MISS")
wd<-wd[-miss,]

FISH_SPECIES_FIELDS<-c("SPECIES", "FAMILY", "COMMONFAMILYALL", "COMMONNAME", "TAXONNAME", "TROPHIC", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species.table<-aggregate(wd$COUNT,by=wd[,FISH_SPECIES_FIELDS], sum, na.rm=FALSE)
write.csv(species.table, file="tmo TOWspecies.csv")

load(file="/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/data/ALL_TOW_FISH_SIGHTINGS.rdata")
od<-df
od$OBS_TYPE<-"PRES"

#some small fixes
od[is.na(od$SIZE_),]$SIZE_<-0
od[od$SIZE_<15 & od$SIZE_!=0,]
od[od$SPECIES=="CAAB" & od$SIZE_==9,]$COUNT<-9; od[od$SPECIES=="CAAB" & od$SIZE_==9,]$SIZE_<-90
od[od$SPECIES=="CHUD" & od$SIZE_==1 & od$DIVEID==201003172,]$COUNT<-1; od[od$SPECIES=="CHUD" & od$SIZE_==1 & od$DIVEID==201003172,]$SIZE_<-110
od[od$SPECIES=="LUBO" & od$SIZE_==1 & od$DIVEID==201003105,]$COUNT<-1; od[od$SPECIES=="LUBO" & od$SIZE_==1 & od$DIVEID==201003105,]$SIZE_<-55
od[od$SPECIES=="TROB" & od$SIZE_==1 & od$DIVEID==201003105,]$COUNT<-1; od[od$SPECIES=="TROB" & od$SIZE_==1 & od$DIVEID==201003105,]$SIZE_<-120
od[od$SPECIES=="CHUD" & od$SIZE_==1 & od$DIVEID==201003105,]$COUNT<-1; od[od$SPECIES=="CHUD" & od$SIZE_==1 & od$DIVEID==201003105,]$SIZE_<-70
od[od$SPECIES=="DOLP" & od$SIZE_==2 & od$DIVEID==201004162,]$COUNT<-2; od[od$SPECIES=="DOLP" & od$SIZE_==2 & od$DIVEID==201004162,]$SIZE_<-250
od[od$SPECIES=="CHUD" & od$COUNT==80 & od$DIVEID==201103241,]$SIZE_<-80;od[od$SPECIES=="CHUD" & od$COUNT==80 & od$DIVEID==201103241,]$COUNT<-1
od[od$SPECIES=="CHUD" & od$COUNT==90 & od$DIVEID==201103241,]$SIZE_<-90;od[od$SPECIES=="CHUD" & od$COUNT==90 & od$DIVEID==201103241,]$COUNT<-1
od[od$SPECIES=="CHUD" & od$SIZE_==1 & od$DIVEID==201404081,]$COUNT<-1; od[od$SPECIES=="CHUD" & od$SIZE_==1 & od$DIVEID==201404081,]$SIZE_<-55
od[od$SIZE_<30 & od$SIZE_!=0,]

head(od)

#filling in fields that are missing from one or other query (but which are still useful to include in merged data)
wd$COMMENT_<-"fill"
DATA_FIELDS<-c("ROUNDID", "MISSIONID", "REGION", "ISLAND", "OBS_YEAR", "DATE_", "REEF_ZONE", "DIVER1", "DIVER2", "LATITUDE", "LONGITUDE", "DIVEID", "SEGMENT", "SEGMENTID", "PROJECTEDLENGTH", "TOW_TYPE", "TOWKEY", "DEPTH", "TEMPERATURE", "STARTLOCALTIME", "ENDLOCALTIME", "CENTROIDLAT", "CENTROIDLON", "SPECIES", "COUNT", "SIZE_", "OBS_TYPE", "COMMENT_")

head(wd[,DATA_FIELDS])
head(od[,DATA_FIELDS])

wd<-rbind(wd[, DATA_FIELDS], od[, DATA_FIELDS])

#wd<-subset(wd, wd$REGION %in% c("PRIAs"))
wd<-merge(wd, species.table, by="SPECIES", all.x=T)     # add the species data back in ...
#DISTINGUISH between COUNT (for biomass and abundance calcaultions adn number observed .. for other calculations)
wd$N_OBSERVED<-wd$COUNT
wd[wd$OBS_TYPE!="LINE",]$COUNT<-0
wd<-droplevels(wd)
dim(wd)


#CONSIDER REMOVING SEGMENTS WITH APPARENTLY BOGUS LENGTHS
#wd[wd$PROJECTEDLENGTH<30,]
#wd[wd$DIVEID %in% tmp, c("DIVEID", "SEGMENT", "PROJECTEDLENGTH", "OBS_YEAR", "SPECIES", "COUNT"),]
#head(wd)

length(unique(wd$DIVEID))

wd[is.na(wd$COUNT),]$COUNT<-0
#wd[is.na(wd$PROJECTEDLENGTH),]$PROJECTEDLENGTH<-0
wd[is.na(wd$DEPTH),]$DEPTH<-0	
wd[is.na(wd$SIZE_),]$SIZE_<-0	
wd[is.na(wd$CENTROIDLAT),]$CENTROIDLAT<-0	
wd[is.na(wd$CENTROIDLON),]$CENTROIDLON<-0	
#wd[is.na(wd$BIOMASS),]$BIOMASS<-0
#wd[is.na(wd$BIOMASS_G_M2),]$BIOMASS_G_M2<-0
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

wd<-droplevels(wd)

#summarize tow information (length, depth, lat-long, date)
#    first do that by segment
TOW_DATA<-c("REGION", "ISLAND", "REP_CARD_UNIT", "CENTROIDLAT", "CENTROIDLON", "DATE_", "DEPTH", "STARTLOCALTIME", "STRATA", "PROJECTEDLENGTH", "DIVEID")   

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
wd[wd$SIZE_<50 & wd$SIZE_ !=0 & wd$OBS_TYPE=="LINE", c("OBS_YEAR", "REGION", "DIVEID", "SEGMENT", "DIVER1", "SPECIES", "COUNT", "SIZE_", "OBS_TYPE")]

wd[wd$SPECIES=="NALI" & wd$SIZE_ < 50,]$COUNT<-0    # THis is really a presence record ... NOT a QUAN cont record
wd[wd$SPECIES=="NALI" & wd$SIZE_ < 50,]$OBS_TYPE<-"PRES"
wd[wd$SPECIES=="CASE" & wd$SIZE_ == 40,]$COUNT<-50; wd[wd$SPECIES=="CASE" & wd$SIZE_ == 40,]$SIZE_<-50    #IDW CHECK THIS ONE .. NOT SURE IF IT REALLY IS 50 at 40 (ALTHOUGH THAT SEEMS UNLIKELY)

wd[wd$SIZE_<50 & wd$SIZE_ !=0  & wd$OBS_TYPE=="LINE", c("OBS_YEAR", "REGION", "DIVEID", "SEGMENT", "DIVER1", "SPECIES", "COUNT", "SIZE_")]

wd<-droplevels(wd)


save(wd, file="CleanPRIATowWD.RData")
write.csv(wd[wd$SPECIES=="MABI",], file="CleanPRIATow BOMU-CHUD WD.csv")


########### DATA IS CLEAN NOW !!!! (AT LEAST THAT IS THE GOAL) ##################

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

dive.info[is.na(dive.info$DEPTH),]

#Fixing Others from a csv file with missing depth info
tmd<-read.csv("tmpTMD.csv")
tmd<-tmd[,c("DIVEID", "Average")]
dim(dive.info)
dive.info<-merge(dive.info, tmd, by="DIVEID", all.x=T)
dim(dive.info)
dive.info[!is.na(dive.info$Average),]$DEPTH<-dive.info[!is.na(dive.info$Average),]$Average
dive.info$Average<-NA

tmp<- dive.info[is.na(dive.info$DEPTH), c("REGION", "ISLAND", "OBS_YEAR", "DIVEID")]; tmp
write.csv(tmp, file="TowsWithMissingDepths.csv")

length(unique(dive.info$DIVEID))

write.csv(dive.info, file="tmp Tows.csv")
############################################################
### Now sum abundance and biomass data per species per dive,
### and convert to gm2 and abund m2
############################################################
di<-dive.info


#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","FAMILY", "TAXONNAME", "TROPHIC_MONREP")
t.species.table<-aggregate(wd$COUNT,by=wd[,FISH_SPECIES_FIELDS], sum, na.rm=FALSE)

# JUST DOING THIS FOR MABI
wd[!wd$SPECIES %in% c("BOMU", "CHUD"),]$COUNT<-0
wd[!wd$SPECIES %in% c("BOMU", "CHUD"),]$N_OBSERVED<-0
wd[!wd$SPECIES %in% c("BOMU", "CHUD"),]$SPECIES<-"NONE"

sum.abund.bio<-aggregate(wd[,c("N_OBSERVED", "COUNT", "biomass_g")],by=wd[,c("DIVEID", "SPECIES")], sum, na.rm=TRUE)
dim(sum.abund.bio)
length(unique(sum.abund.bio$DIVEID))

tfd<-merge(sum.abund.bio, dive.info[,c("DIVEID","PROJECTEDLENGTH")], by="DIVEID")

x1<-cast(tfd, DIVEID + PROJECTEDLENGTH ~ SPECIES, value="COUNT", sum); names(x1)<-c("DIVEID", "PROJECTEDLENGTH", "BOMU_COUNT", "CHUD_COUNT", "OTHERS_C")
x2<-cast(tfd, DIVEID  ~ SPECIES, value="N_OBSERVED", sum); names(x2)<-c("DIVEID", "BOMU_OBS", "CHUD_OBS", "OTHERS_OBS")

tmp<-merge(x1, x2, by="DIVEID")
head(tmp)

# add data about the tow (island, zone, year, depth)
tfd<-merge(tmp, dive.info[, c("DIVEID", "REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR", "CENTROIDLAT", "CENTROIDLON", "DEPTH", "TEMPERATURE")], by="DIVEID")
dim(tfd)
head(tfd)
summary(tfd)
length(unique(tfd$DIVEID))
write.csv(tfd, file="TMPtowDataBOMUCHUD.csv")
















summary(tfd$DEPTH)

length(unique(tfd$DIVEID))

#set Depth of NA to -999
head(tfd)
tfd[is.na(tfd$DEPTH),]$DEPTH<- -999
tfd[is.na(tfd$CENTROIDLAT),]$CENTROIDLAT<- 0
tfd[is.na(tfd$CENTROIDLON),]$CENTROIDLON<- 0

tfd<-droplevels(tfd)
length(unique(tfd$DIVEID))

# SET DATA_VAL to the NUMBER WE WANT TO USE ... SO WE CAN RUN ALL CODE BELOW FOR EITHER ABUN OR BIO
tfd$DATA_VAL<-tfd$BIOGM2

xx<-aggregate(tfd$DATA_VAL, by=tfd[,c("DIVEID", "REGION", "ISLAND", "OBS_YEAR", "REEF_ZONE", "CENTROIDLAT", "CENTROIDLON", "PROJECTEDLENGTH", "DEPTH", "FAMILY")], sum, na.rm=TRUE)
dimnames(xx)[[2]]<-c("DIVEID", "REGION", "ISLAND", "YEAR", "STRATA", "CENTROIDLAT", "CENTROIDLON", "PROJECTEDLENGTH", "DEPTH_M", "FAMILY", "DATA_VAL")
#now format this more or less as a crosstab, with field of interest as column variable
wtd<-cast(xx, DIVEID + REGION + ISLAND + YEAR + STRATA + CENTROIDLAT + CENTROIDLON + PROJECTEDLENGTH + DEPTH_M ~ FAMILY, value="DATA_VAL", fun.aggregate=sum, fill=0); head(wtd)
wtd$TotFish<-rowSums(wtd[,levels(xx$FAMILY)])
# wtd$TotFishMinusManta<-wtd$TotFish-wtd$Myliobatidae

# REEF_SHARKS<-c("Carcharhinidae", "Ginglymostomatidae", "Sphyrnidae")
# wtd$ReefSharks<-rowSums(wtd[,REEF_SHARKS])

dim(wtd)
length(unique(wtd$DIVEID))
write.csv(wtd, file="CREP PRIA TowData.csv")


## NOW DO BOMU and CHUD Abundance and Bio
tfdSAVE<-tfd
tfd[tfd$SPECIES != c("CHUD", "BOMU"),]$SPECIES<-"NONE"
xx<-aggregate(tfd$BIOGM2, by=tfd[,c("DIVEID", "REGION", "ISLAND", "OBS_YEAR", "REEF_ZONE", "CENTROIDLAT", "CENTROIDLON", "PROJECTEDLENGTH", "DEPTH", "SPECIES")], sum, na.rm=TRUE)
dimnames(xx)[[2]]<-c("DIVEID", "REGION", "ISLAND", "YEAR", "STRATA", "CENTROIDLAT", "CENTROIDLON", "PROJECTEDLENGTH", "DEPTH_M", "SPECIES", "BIOGM2")
wtd2<-cast(xx, DIVEID + REGION + ISLAND + YEAR + STRATA + CENTROIDLAT + CENTROIDLON + PROJECTEDLENGTH + DEPTH_M ~ SPECIES, value="BIOGM2", fun.aggregate=sum, fill=0); head(wtd2)
names(wtd2)[match(c("CHUD", "BOMU"),names(wtd2))] <- c("CHUD_BIOGM2", "BOMU_BIOGM2")

xy<-aggregate(tfd$ABUNM2, by=tfd[,c("DIVEID", "REGION", "ISLAND", "OBS_YEAR", "REEF_ZONE", "CENTROIDLAT", "CENTROIDLON", "PROJECTEDLENGTH", "DEPTH", "SPECIES")], sum, na.rm=TRUE)
dimnames(xy)[[2]]<-c("DIVEID", "REGION", "ISLAND", "YEAR", "STRATA", "CENTROIDLAT", "CENTROIDLON", "PROJECTEDLENGTH", "DEPTH_M", "SPECIES", "ABUNM2")
wtd3<-cast(xy, DIVEID + REGION + ISLAND + YEAR + STRATA + CENTROIDLAT + CENTROIDLON + PROJECTEDLENGTH + DEPTH_M ~ SPECIES, value="ABUNM2", fun.aggregate=sum, fill=0); head(wtd3)
names(wtd3)[match(c("CHUD", "BOMU"),names(wtd3))] <- c("CHUD_ABUNM2", "BOMU_ABUNM2")

length(unique(wtd$DIVEID))
length(unique(wtd2$DIVEID))
length(unique(wtd3$DIVEID))

wtd<-merge(wtd, wtd2[,c("DIVEID", "CHUD_BIOGM2", "BOMU_BIOGM2")], by="DIVEID", all.x=T)
wtd<-merge(wtd, wtd3[,c("DIVEID", "CHUD_ABUNM2", "BOMU_ABUNM2")], by="DIVEID", all.x=T)
length(unique(wtd$DIVEID))

write.csv(wtd, file="CREP PRIA TowData incl BOMU_CHUD.csv")




# REPORT_COLS<-c("Acanthuridae", "Scaridae", "Serranidae", "Carangidae", "Labridae", "Lutjanidae", "Lethrinidae", "Sphyraenidae", "ReefSharks", "Chanidae", "Kyphosidae", "Myliobatidae", "Dasyatidae", "TotFish", "TotFishMinusManta")

############### CAPPING ALL DATA VALUES TO the 95th PERCENTILE ################
####  IDW - CHECK THAT THIS IS SAME APPROACH FOR MAIN BODY OF REPORT (OR WHETHER WE WANT TO DO THAT)
### NOTICING THAT eg ACANTHURIDAE 95% CAP ACROSS ALL TOWS is ~5g/m2 BUT THAT VALUE IS COMMONLY EXCEEDED IN PRIA.PHOENIX .. THUS MAY NEED REGIONAL CAPS TO DO THIS PRPERLY
## THS ... FOR EACH REGION AND DATA FIELD.. CALCULATE AND APPLY REGIONAL CAPS SEEMS BEST... MAY WELL BE SOME SORT OF LAPPLY./TAPPLY THING NEEDED....

#show how many sites we have for any particular region and year
table(wtd$REGION, wtd$YEAR)
# #there are so few tow from 2001 in PRIA and NWHI. so I think its better to pool those two together
# wtd[wtd$YEAR==2001 & wtd$REGION %in% c("PRIA.LINE", "PRIA.PHOENIX", "NWHI"),]$YEAR<-2002


# QUANTILE_CAP<-95
# regions<-unique(wtd$REGION)
# for(i in 1:length(regions))
# {
	# for(j in 1: length(REPORT_COLS))
	# {
			# lim<-quantile(wtd[wtd$REGION==regions[i], REPORT_COLS[j]], probs=c(QUANTILE_CAP)/100)
			# wtd[wtd$REGION==regions[i] & wtd[, REPORT_COLS[j]]>lim, REPORT_COLS[j]]<-lim
		
	# }
# }


# write.csv(wtd, file="TMPtowData2-CAPPED.csv")













#aggregate - average per island/strata/year
tfi.mean<-aggregate(wtd[,REPORT_COLS],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], mean, na.rm=TRUE)
tfi.n<-aggregate(wtd[,REPORT_COLS],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], length)
tfi.var<-aggregate(wtd[,REPORT_COLS],by=wtd[,c("REGION", "ISLAND", "STRATA", "YEAR")], var, na.rm=TRUE)
tfi.se<-tfi.mean
tfi.se[,REPORT_COLS]<-sqrt(tfi.var[,REPORT_COLS])/sqrt(tfi.n$TotFish)

# add the N to the mean and se dfs before writing them
tfi.mean$n<-tfi.se$n<-tfi.n$TotFish



write.csv(tfi.mean, file="tow_fish.csv")
write.csv(tfi.se, file="tow_fish_se.csv")


### IDW POOL AT REGION SCALE  -WEIGHT BY REEF_AREA   AND DROP ISLANDS THAT ARE NOT ROUTINELY SURVEYED (PARTICULARLY NWHI ISLANDS)
### IDW POOL AT REGION SCALE  -WEIGHT BY REEF_AREA   AND DROP ISLANDS THAT ARE NOT ROUTINELY SURVEYED (PARTICULARLY NWHI ISLANDS)

sectors<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/Base R/Base Data Files/Sectors-Strata-Areas2015.csv", stringsAsFactors=FALSE)
head(sectors)
sectors<-sectors[sectors$REEF_ZONE=="Forereef",]
isl.size<-aggregate(sectors$AREA_HA, by=sectors[,c("REGION", "ISLAND")], FUN=sum)
names(isl.size)<-c("REGION", "ISLAND", "AREA_HA")

#### GET RID OF DATA FROM ISLANDS THAT WERE ONLY INTERMITTENTLY SAMPLED
m<-subset(tfi.mean, !tfi.mean$ISLAND %in% c("South Bank", "Kaula", "Raita", "Anatahan", "Gardner") & !tfi.mean$REGION %in% c("MARIAN.OTH"))
se<-subset(tfi.se, !tfi.se$ISLAND %in% c("South Bank", "Kaula", "Raita", "Anatahan", "Gardner") & !tfi.se$REGION %in% c("MARIAN.OTH"))
m<-merge(m, isl.size[,c("ISLAND", "AREA_HA")], by="ISLAND")
se<-merge(se, isl.size[,c("ISLAND", "AREA_HA")], by="ISLAND")

sum(m$n) #number of sites

regions<-unique(m$REGION); regions

islands<-sort(unique(m$ISLAND)); islands
cast(data=m, REGION + ISLAND ~ YEAR, fun.aggregate=sum, value="n")
#table(rm$ISLAND, rm$YEAR)

for(r in 1:length(regions))
{

	rm<-m[m$REGION==regions[r],]
	rse<-se[se$REGION==regions[r],]
	rm<-droplevels(rm)
	
	years<-sort(unique(rm$YEAR)); years
	
	#build data output structures
	reg_m<-array(data=NA, dim=c(length(years), length(REPORT_COLS)+3), dimnames=list(years, c("REGION", "OBS_YEAR", "n", REPORT_COLS)));reg_m
	reg_m<-as.data.frame(reg_m)
	reg_m$REGION<-regions[r]
	reg_se<-reg_m
	
	#loop through, building the region-wide data
	for(i in 1:length(years))
	{
		tm<-rm[rm$YEAR == years[i],]
		tse<-rse[rse$YEAR == years[i],]
	
		tot_ha<-sum(tm$AREA_HA)
		tse$PCT_AREA<-tm$PCT_AREA<-tm$AREA_HA/tot_ha
		tm
		
		reg_m[i, "OBS_YEAR"]<-reg_se[i, "OBS_YEAR"]<-years[i]
		reg_m[i, "n"]<-reg_se[i, "n"]<-sum(tm$n)
	
		reg_m[i, REPORT_COLS]<-colSums(tm[,REPORT_COLS]*tm$PCT_AREA)
		reg_se[i, REPORT_COLS]<-sqrt(colSums((tse[, REPORT_COLS]^2)*(tse$PCT_AREA^2)))
	}
	if(r==1) 
	{
		out.m<-reg_m
		out.se<-reg_se
	}
	else
	{
		out.m<-rbind(out.m, reg_m)
		out.se<-rbind(out.se, reg_se)
	}
}

all<-list(Means=out.m, SE=out.se)
write.csv(all, file="RegionPerYear.csv")
save(all, file="RegionPerYear.Rdata")


### NOW TO DO FIGURES
library(ggplot2)
library(ggthemes)
library(plyr)
library(scales)
library(reshape)           # reshape library includes the cast() function used below
require(RColorBrewer)


setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Monitoring Report/Towed Diver Workings")


load("RegionPerYear.Rdata")
m<-all$Means
se<-all$SE

m$OBS_YEAR<-as.factor(m$OBS_YEAR)
m$n<-as.factor(m$n)
se$OBS_YEAR<-as.factor(se$OBS_YEAR)
se$n<-as.factor(se$n)

m2<-melt(m)
se2<-melt(se)
#x`se2[is.na(se2$value),]$value<-0
tfish<-cbind(m2, se2$value)
names(tfish)<-c("REGION", "YEAR", "N", "FishGroup", "fMean", "fSE")

tfish$UPL<-ceiling((tfish$fMean+tfish$fSE)/5)*5
tfish$YEAR<-as.numeric(as.character(tfish$YEAR))
levels(tfish$FishGroup)<-c(levels(tfish$FishGroup), "All Fishes > 50cm TL")
tfish[tfish$FishGroup=="TotFish",]$FishGroup<-"All Fishes > 50cm TL"
levels(tfish$FishGroup)<-c(levels(tfish$FishGroup), "Reef Sharks")
tfish[tfish$FishGroup=="ReefSharks",]$FishGroup<-"Reef Sharks"

GROUPS<-c("All Fishes > 50cm TL", "Reef Sharks", "Carangidae", "Scaridae", "Lutjanidae", "Acanthuridae", "Sphyraenidae")
PALETTE<-"Oranges"

#Two plots per region - one big one for all fish; one set of smaller ones - for the various families groupings 
regions<-unique(m$REGION)
for(i in 1:length(regions))
{
	tf<-tfish[tfish$REGION==regions[i] & tfish$FishGroup %in% GROUPS,]
	n_groups<-length(unique(tf$FishGroup))
	tf<-droplevels(tf)
	ymax<-max(tf$UPL)

	ggplot(tf[tf$FishGroup=="All Fishes > 50cm TL",], aes(fill=FishGroup, x=YEAR, y=fMean,fSE=fSE)) + 
		scale_fill_brewer(palette = PALETTE) +
		geom_errorbar(aes(ymin=fMean-fSE, ymax=fMean+fSE), width=0.25) +    
		geom_bar(position="dodge", stat="identity", color="black", fill=brewer.pal(n_groups, PALETTE)[n_groups]) +
	    facet_wrap(~FishGroup, scales ="fixed") + 
		scale_y_continuous(breaks=pretty_breaks(n=5)) +
		scale_x_continuous(breaks=unique(tf$YEAR)) +
	    theme_bw() + theme(legend.position="none") +
   	    theme(axis.title.x = element_blank()) +
	    coord_cartesian(ylim = c(0, ymax)) +
	    labs(title = "LARGE FISH (>50 cm TL) DENSITY", y = expression(paste("Abundance (number ", Ha^-2,")"))) +
	    ggsave(paste(regions[i], "AllBigFishPlot.png", sep="_"), width=4, height=4)
	

	tf<-tf[tf$FishGroup!="All Fishes > 50cm TL",]
	tf<-droplevels(tf)
	tf$UPL<-ceiling((tf$fMean+tf$fSE)/2)*2
	ymax<-max(tf$UPL)
	
	ggplot(tf, aes(fill=FishGroup, x=YEAR, y=fMean,fSE=fSE)) + 
		scale_fill_manual(values=brewer.pal(n_groups, PALETTE)[1:n_groups-1]) +
		geom_errorbar(aes(ymin=fMean-fSE, ymax=fMean+fSE), width=0.25) +    
		geom_bar(position="dodge", stat="identity", color="black") +
	    facet_wrap(~FishGroup, scales ="fixed", nrow=3) + 
		scale_y_continuous(breaks=pretty_breaks(n=5)) +
		scale_x_continuous(breaks=unique(tf$YEAR)) +
	    theme_bw() + theme(legend.position="none") +
   	    theme(axis.title.x = element_blank(), axis.text.x=element_text(size=8, angle=90, vjust=1, hjust=0.5)) +
	    coord_cartesian(ylim = c(0, ymax)) +
	    labs(title = "MAJOR LARGE FISH (>50 cm TL) GROUPINGS", y = expression(paste("Abundance (number ", Ha^-2,")"))) +
   	    theme(plot.title = element_text(size=12))
	    ggsave(paste(regions[i], "BigFishPlot.png", sep="_"), width=4, height=6)

}





