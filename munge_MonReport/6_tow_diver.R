rm(list=ls())
# SET UP ------------------------------------------------------------------
#setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Base R/Base Data Files")
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
source("E:/CRED/Base_R/Base_R_Functions/fish_team_functions.r")
source("E:/CRED/Base_R/Base_R_Functions/Islandwide Mean&Variance Functions.R")

#################################################################### TOW WORKUP ###########################################################################

##save(df, file="ALL_TOW_FISH_RAW.rdata")
#write.table(df, file = "ALL_TOW_FISH_RAW.csv", sep = ",", col.names = NA, qmethod = "double")
load(file="E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/fish-paste/data/ALL_TOW_FISH_RAW.rdata")
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs/tow_diver")

 
# FISH TOW WORKINGS -------------------------------------------------------
wd<-df
wd<-droplevels(wd)

## drop any rows which have NOSC and MISS in the species field, these are tows which were aborted part way through
## remove these so that the tow length is corrected....
nosc<-which(wd$SPECIES == "NOSC")
wd<-wd[-nosc,]

miss<-which(wd$SPECIES == "MISS")
wd<-wd[-miss,]

#LIMIT TO FOREEF (retaining tows that are currently listed as 'Unspecified' - from 2012)
wd<-subset(wd, !wd$REEF_ZONE %in% c("Backreef", "Lagoon"))
wd[wd$REEF_ZONE=="Unspecified" & wd$ISLAND=="Tutuila",]$REEF_ZONE<-"Forereef"
#CONSIDER REMOVING SEGMENTS WITH APPARENTLY BOGUS LENGTHS
tmp<-wd[wd$PROJECTEDLENGTH<10,]$DIVEID
wd[wd$DIVEID %in% tmp, c("DIVEID", "SEGMENT", "PROJECTEDLENGTH", "OBS_YEAR", "SPECIES", "COUNT"),]
head(wd)

length(unique(wd$DIVEID))

wd[is.na(wd$COUNT),]$COUNT<-0
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

#DIVIDE MARIAN INTO SUB-REGIONS
table(wd$OBS_YEAR, wd$REGION)
levels(wd$REGION)<-c(levels(wd$REGION), "MARIAN.STH", "MARIAN.NTH", "MARIAN.OTH", "PRIA.LINE", "PRIA.PHOENIX", "PRIA.JOH", "PRIA.WAKE")
wd[wd$REGION=="MARIAN",]$REGION<-"MARIAN.NTH"
wd[wd$ISLAND %in% c("Guam", "Rota", "Saipan", "Aguijan", "Tinian"),]$REGION<-"MARIAN.STH"
wd[wd$ISLAND %in% c("Supply", "Stingray", "Supply", "Santa Rosa", "Tatsumi", "Arakane", "Pathfinder"),]$REGION<-"MARIAN.OTH"
wd[wd$REGION=="PRIAs",]$REGION<-"PRIA.JOH"
wd[wd$ISLAND %in% c("Palmyra", "Kingman", "Jarvis"),]$REGION<-"PRIA.LINE"
wd[wd$ISLAND %in% c("Howland", "Baker"),]$REGION<-"PRIA.PHOENIX"
wd[wd$ISLAND %in% c("Wake"),]$REGION<-"PRIA.WAKE"

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
wd[wd$SIZE_<50 & wd$SIZE_ !=0, c("OBS_YEAR", "REGION", "DIVEID", "SEGMENT", "DIVER1", "SPECIES", "COUNT", "SIZE_")]

wd[wd$SPECIES=="CHUD" & wd$SIZE_ < 50,]$COUNT<-0    # THis is really a presence record ... NOT a QUAN cont record
wd[wd$SPECIES=="SPHE" & wd$SIZE_ == 48,]$COUNT<-48; wd[wd$SPECIES=="SPHE" & wd$SIZE_ == 48,]$SIZE_<-50
wd[wd$SPECIES=="CHCH" & wd$SIZE_ == 27,]$COUNT<-27; wd[wd$SPECIES=="CHCH" & wd$SIZE_ == 27,]$SIZE_<-55
wd[wd$SPECIES=="NALI" & wd$SIZE_ < 50,]$COUNT<-0 
wd[wd$SIZE_<50 & wd$SIZE_ !=0, c("OBS_YEAR", "REGION","ISLAND", "DIVEID", "SEGMENT", "DIVER1", "SPECIES", "COUNT", "SIZE_")]

wd<-droplevels(wd)
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
###FILING IN SOME MISSING DEPTH VALUES FOR TOWS FROM THE CRUISE REPORT METADATAS
dive.info[dive.info$DIVEID == 200202261,]$DEPTH<-23
dive.info[dive.info$DIVEID == 200202262,]$DEPTH<-23.5
dive.info[dive.info$DIVEID == 200202263,]$DEPTH<-17.5
dive.info[dive.info$DIVEID == 200602192,]$DEPTH<-17  
dive.info[dive.info$DIVEID == 200602193,]$DEPTH<-17  
dive.info[dive.info$DIVEID == 200602194,]$DEPTH<-21.5  
dive.info[dive.info$DIVEID == 200602195,]$DEPTH<-16.5  
dive.info[dive.info$DIVEID == 200602196,]$DEPTH<-16.5  

dive.info[dive.info$DIVEID == 200602235,]$DEPTH<-15.83333333
dive.info[dive.info$DIVEID == 200803111,]$DEPTH<-15.60606061
dive.info[dive.info$DIVEID == 200803112,]$DEPTH<-14.39393939
dive.info[dive.info$DIVEID == 200803113,]$DEPTH<-18.18181818
dive.info[dive.info$DIVEID == 200803114,]$DEPTH<-17.72727273
dive.info[dive.info$DIVEID == 200803115,]$DEPTH<-17.12121212
dive.info[dive.info$DIVEID == 200803116,]$DEPTH<-15.15151515

dive.info[dive.info$DIVEID == 200202264,]$DEPTH<-0  #NO DEPTH INFO IN METADATFILE
dive.info[dive.info$DIVEID == 200202184,]$DEPTH<-0  #NO DEPTH INFO IN METADATFILE
dive.info[dive.info$DIVEID == 200402115,]$DEPTH<-0  #CAN NOT FIND THIS SURVEY IN THE CRUISE REPORT OR THE ORIGINAL CRUISE DATA
dive.info[dive.info$DIVEID == 200402116,]$DEPTH<-0  #CAN NOT FIND THIS SURVEY IN THE CRUISE REPORT OR THE ORIGINAL CRUISE DATA
dive.info[dive.info$DIVEID == 201503131,]$DEPTH<-18.9  #Average manually lifted from the SeaBird file
dive.info[dive.info$DIVEID == 201503132,]$DEPTH<-15.3  #Average manually lifted from the SeaBird file
dive.info[dive.info$DIVEID == 201503133,]$DEPTH<-17.4  #Average manually lifted from the SeaBird file
dive.info[dive.info$DIVEID == 201503134,]$DEPTH<-16.7  #Average manually lifted from the SeaBird file
dive.info[dive.info$DIVEID == 201503135,]$DEPTH<-16.9  #Average manually lifted from the SeaBird file
dive.info[dive.info$DIVEID == 200010031 ,]$DEPTH<-13.04  #PartialBenthic SeaBird

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

#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","FAMILY", "TAXONNAME", "TROPHIC_MONREP")
t.species.table<-aggregate(wd$COUNT,by=wd[,FISH_SPECIES_FIELDS], sum, na.rm=FALSE)

sum.abund.bio<-aggregate(wd[,c("COUNT", "biomass_g")],by=wd[,c("DIVEID", "FAMILY")], sum, na.rm=TRUE)
dim(sum.abund.bio)

length(unique(sum.abund.bio$DIVEID))

tfd<-merge(sum.abund.bio, dive.info[,c("DIVEID","PROJECTEDLENGTH")], by="DIVEID")
tfd$BIOGM2<-tfd$biomass_g / (10*tfd$PROJECTEDLENGTH)
tfd$ABUNM2<-tfd$COUNT / (10*tfd$PROJECTEDLENGTH)
dim(tfd)
## add consumer group to tow data, filter to forereef ONLY, add depth to give option to later filter by depth range .. then pool up by island & year and save SE

# add data about the tow (island, zone, year, depth)
tfd<-merge(tfd, dive.info[, c("DIVEID", "REGION", "ISLAND", "REEF_ZONE", "OBS_YEAR", "DEPTH", "TEMPERATURE")], by="DIVEID")
dim(tfd)

length(unique(tfd$DIVEID))

write.csv(tfd, file="TMPtowData.csv")

summary(tfd$DEPTH)

tfd<-subset(tfd, tfd$REEF_ZONE %in% c("Forereef", "Unspecified"))
#First, lets look at the habitat of different tows
table(tfd$REEF_ZONE, tfd$ISLAND)
tfd[tfd$REEF_ZONE=="Unspecified",]
#Assume 'Unspecified" REEF_ZONE are Forereef
tfd[tfd$REEF_ZONE=="Unspecified",]$REEF_ZONE<-"Forereef"
tfd<-droplevels(tfd)

length(unique(tfd$DIVEID))

#Filter out tows less than 8m OR greater than 20m
#HOWEVER, INCLUDE TOWS THAT HAVE NA for DEPTH!
unique(tfd[is.na(tfd$DEPTH),]$ISLAND)
DUMMY_DEPTH<-14.99999999999999999999999999999999999999
tfd[is.na(tfd$DEPTH),]$DEPTH<-DUMMY_DEPTH #setting to recognizable mid-depth value, so they gets include
tfd<-subset(tfd, tfd$DEPTH>8 & tfd$DEPTH<20)

tfd<-droplevels(tfd)
length(unique(tfd$DIVEID))

# SET DATA_VAL to the NUMBER WE WANT TO USE ... SO WE CAN RUN ALL CODE BELOW FOR EITHER ABUN OR BIO
tfd$DATA_VAL<-tfd$ABUNM2*10000

xx<-aggregate(tfd$DATA_VAL, by=tfd[,c("DIVEID", "REGION", "ISLAND", "OBS_YEAR", "REEF_ZONE", "FAMILY")], sum, na.rm=TRUE)
dimnames(xx)[[2]]<-c("DIVEID", "REGION", "ISLAND", "YEAR", "STRATA", "FAMILY", "DATA_VAL")
#now format this more or less as a crosstab, with field of interest as column variable
wtd<-cast(xx, DIVEID + REGION + ISLAND + YEAR + STRATA ~ FAMILY, value="DATA_VAL", fun.aggregate=sum, fill=0); head(wtd)
wtd$TotFish<-rowSums(wtd[,levels(xx$FAMILY)])
wtd$TotFishMinusManta<-wtd$TotFish-wtd$Myliobatidae

REEF_SHARKS<-c("Carcharhinidae", "Ginglymostomatidae", "Sphyrnidae")
wtd$ReefSharks<-rowSums(wtd[,REEF_SHARKS])

dim(wtd)

length(unique(wtd$DIVEID))

REPORT_COLS<-c("Acanthuridae", "Scaridae", "Serranidae", "Carangidae", "Labridae", "Lutjanidae", "Lethrinidae", "Sphyraenidae", "ReefSharks", "Chanidae", "Kyphosidae", "Myliobatidae", "Dasyatidae", "TotFish", "TotFishMinusManta")

############### CAPPING ALL DATA VALUES TO the 95th PERCENTILE ################
####  IDW - CHECK THAT THIS IS SAME APPROACH FOR MAIN BODY OF REPORT (OR WHETHER WE WANT TO DO THAT)
### NOTICING THAT eg ACANTHURIDAE 95% CAP ACROSS ALL TOWS is ~5g/m2 BUT THAT VALUE IS COMMONLY EXCEEDED IN PRIA.PHOENIX .. THUS MAY NEED REGIONAL CAPS TO DO THIS PRPERLY
## THS ... FOR EACH REGION AND DATA FIELD.. CALCULATE AND APPLY REGIONAL CAPS SEEMS BEST... MAY WELL BE SOME SORT OF LAPPLY./TAPPLY THING NEEDED....
write.csv(wtd, file="TMPtowData2.csv")

#show how many sites we have for any particular region and year
table(wtd$REGION, wtd$YEAR)
table(wtd$ISLAND,wtd$YEAR) #for appendix 2, tows per island per year
#there are so few tow from 2001 in PRIA and NWHI. so I think its better to pool those two together
wtd[wtd$YEAR==2001 & wtd$REGION %in% c("PRIA.LINE", "PRIA.PHOENIX", "NWHI"),]$YEAR<-2002


QUANTILE_CAP<-95
regions<-unique(wtd$REGION)
for(i in 1:length(regions))
{
	for(j in 1: length(REPORT_COLS))
	{
			lim<-quantile(wtd[wtd$REGION==regions[i], REPORT_COLS[j]], probs=c(QUANTILE_CAP)/100)
			wtd[wtd$REGION==regions[i] & wtd[, REPORT_COLS[j]]>lim, REPORT_COLS[j]]<-lim
		
	}
}


write.csv(wtd, file="TMPtowData2-CAPPED.csv")

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
sectors<-read.csv("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/fish-paste/data/Sectors-Strata-Areas2016.csv", stringsAsFactors=FALSE)

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


### TOW FIGURES #######################################
library(ggplot2)
library(ggthemes)
library(plyr)
library(scales)
library(reshape)           # reshape library includes the cast() function used below
require(RColorBrewer)


setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs/tow_diver")


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
# for KM
#write.csv(tfish,file="E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs/tow_diver/tow_fig_data.csv")

GROUPS<-c("All Fishes > 50cm TL", "Reef Sharks", "Carangidae", "Scaridae", "Lutjanidae", "Acanthuridae", "Serranidae")
PALETTE<-"Oranges"

# save figures to figure folder
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures/tow")
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
	    facet_wrap(~FishGroup, scales ="free_y") + 
		#scale_y_continuous(breaks=pretty_breaks(n=5)) +
		scale_x_continuous(breaks=unique(tf$YEAR)) +
	    theme_bw() + theme(legend.position="none") +
   	    theme(axis.title.x = element_blank(),axis.text.x=element_text(size=8, angle=90,hjust=-1)) +
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
	    facet_wrap(~FishGroup, scales ="free_y", nrow=3) + 
		#scale_y_continuous(breaks=pretty_breaks(n=5)) +
		scale_x_continuous(breaks=unique(tf$YEAR)) +
	    theme_bw() + theme(legend.position="none") +
   	    theme(axis.title.x = element_blank(), axis.text.x=element_text(size=8, angle=90, vjust=1, hjust=0.5)) +
	    #coord_cartesian(ylim = c(0, ymax)) +
	    labs(title = "MAJOR LARGE FISH (>50 cm TL) GROUPINGS", y = expression(paste("Abundance (number ", Ha^-2,")"))) +
   	    theme(plot.title = element_text(size=10.5))
	    ggsave(paste(regions[i], "BigFishPlot.png", sep="_"), width=4.5, height=6)

}





