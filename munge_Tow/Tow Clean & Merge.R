rm(list=ls())
### This merges the tow presence and transect data .. cleans up NAs, and depths, and other data errors .. generates clean data file suitable for analysis elsewhere

setwd("/Users/ivor.williams/Documents/CRED/Fish Team/PRIA Report")
# SET UP ------------------------------------------------------------------
#setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Base R/Base Data Files")
library(gdata)             # needed for drop_levels()
library(reshape2)           # reshape library inclues the cast() function used below
source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/fish_team_functions.R")
#source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/Islandwide Mean&Variance Functions.R")

#################################################################### TOW WORKUP ###########################################################################
#MERGE THE TOW OBSERVATIONS WITH TOW PRESENCE ABSENCE

load(file="/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/ALL_TOW_FISH_RAW.rdata")
wd<-df
wd$OBS_TYPE<-"LINE"
head(wd)
## drop any rows which have NOSC and MISS in the species field, these are tows which were aborted part way through
## remove these so that the tow length is corrected....
nosc<-which(wd$SPECIES == "NOSC")
wd<-wd[-nosc,]
miss<-which(wd$SPECIES == "MISS")
wd<-wd[-miss,]

wd[is.na(wd$COUNT),]$COUNT<-0
wd[is.na(wd$SIZE_),]$SIZE_<-0	

#Fix errors in the database
wd[wd$SIZE_<50 & wd$SIZE_ !=0, c("OBS_YEAR", "REGION", "DIVEID", "SEGMENT", "DIVER1", "SPECIES", "COUNT", "SIZE_")]
wd[wd$DIVEID==201205123 & wd$SPECIES=="CASE" & wd$SIZE_==40,]$COUNT<-40		# COUNT and SIZE swapped
wd[wd$DIVEID==201205123 & wd$SPECIES=="CASE" & wd$SIZE_==40,]$SIZE_<-50    

#all remainder should become PRES data
wd[wd$SIZE_<50 & wd$SIZE_ !=0, c("OBS_YEAR", "REGION", "DIVEID", "SEGMENT", "DIVER1", "SPECIES", "COUNT", "SIZE_")]
wd[wd$SIZE_<50 & wd$SPECIES !="NONE",]$OBS_TYPE<-"PRES" 
wd[wd$SPECIES =="CHUD"& wd$COUNT==0 & wd$OBS_TYPE=="PRES",]$COUNT<-1  #CHUD was observed ... does need a size

#wd[is.na(wd$LENGTH_CONVERSION_FACTOR),]$SPECIES
LW_FIELDS<-c("SPECIES","LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR", "TROPHIC_MONREP")
lw<-aggregate(wd$COUNT,by=wd[,LW_FIELDS], sum, na.rm=FALSE)
lw$x<-NULL

load(file="/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/ALL_TOW_FISH_SIGHTINGS.rdata")
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

head(od[od$SPECIES=="CAAB",])
od[od$SPECIES=="CAAM" ,]$FAMILY<-"Carcharhinidae"
od[od$SPECIES=="CAAM" ,]$TAXONNAME<-"Carcharhinus amblyrhynchos"
od[od$SPECIES=="CAAM" ,]$COMMONFAMILYALL<-"Requiem"
od[od$SPECIES=="CAAM" ,]$COMMONNAME<-"Grey reef shark"
od[od$SPECIES=="CAAM" ,]$SPECIES<-"CAAB"

head(od[od$SPECIES=="CHUD",])
od[od$SPECIES=="CHUL" ,]$FAMILY<-"Labridae"
od[od$SPECIES=="CHUL" ,]$TAXONNAME<-"Cheilinus undulatus"
od[od$SPECIES=="CHUL" ,]$COMMONFAMILYALL<-"Wrasse"
od[od$SPECIES=="CHUL" ,]$COMMONNAME<-"Humphead wrasse"
od[od$SPECIES=="CHUL" ,]$SPECIES<-"CHUD"

head(od[od$SPECIES=="SPQE",])
od[od$SPECIES=="SPJE" ,]$FAMILY<-"Sphyraenidae"
od[od$SPECIES=="SPJE" ,]$TAXONNAME<-"Sphyraena qenie"
od[od$SPECIES=="SPJE" ,]$COMMONFAMILYALL<-"Barracuda"
od[od$SPECIES=="SPJE" ,]$COMMONNAME<-"Blackfin barracuda"
od[od$SPECIES=="SPJE" ,]$SPECIES<-"SPQE"
head(od)

head(od[od$SPECIES=="SCSP",])
od[od$SPECIES=="SCTR" ,]$FAMILY<-"Scaridae"
od[od$SPECIES=="SCTR" ,]$TAXONNAME<-"Scarus sp"
od[od$SPECIES=="SCTR" ,]$COMMONFAMILYALL<-"Parrotfish"
od[od$SPECIES=="SCTR" ,]$COMMONNAME<-"Scarus genus species"
od[od$SPECIES=="SCTR" ,]$SPECIES<-"SCSP"

#filling in fields that are missing from one or other query (but which are still useful to include in merged data)
wd$COMMENT_<-"fill"
DATA_FIELDS<-c("ROUNDID", "MISSIONID", "REGION", "ISLAND", "OBS_YEAR", "DATE_", "REEF_ZONE", "DIVER1", "DIVER2", "LATITUDE", "LONGITUDE", "DIVEID", "SEGMENT", "SEGMENTID", "PROJECTEDLENGTH", "TOW_TYPE", "TOWKEY", "DEPTH", "TEMPERATURE", "STARTLOCALTIME", "ENDLOCALTIME", "CENTROIDLAT", "CENTROIDLON", "SPECIES",  "FAMILY", "COMMONFAMILYALL", "TAXONNAME", "COMMONNAME", "COUNT", "SIZE_", "OBS_TYPE", "COMMENT_")

head(wd[,DATA_FIELDS])
head(od[,DATA_FIELDS])

#Make wd be all data put together! #####
wd<-rbind(wd[, DATA_FIELDS], od[, DATA_FIELDS])

#Add Length-weight info back in
wd<-merge(wd, lw, by="SPECIES", all.x=T)
unique(wd[is.na(wd$LW_A),]$TAXONNAME)
wd[is.na(wd$LW_A),]$LW_A<-0
wd[is.na(wd$LW_B),]$LW_B<-0
wd[is.na(wd$LENGTH_CONVERSION_FACTOR),]$LENGTH_CONVERSION_FACTOR<-1
levels(wd$TROPHIC_MONREP)<-c(levels(wd$TROPHIC_MONREP), "UNKNOWN")
NOT_FISH<-c("HKTU", "STLO", "TUTR", "MENO", "DOLP", "GRTU", "MONK", "TURT")
wd[wd$SPECIES %in% c("NONE", NOT_FISH),]$TROPHIC_MONREP<-"UNKNOWN"
unique(wd[is.na(wd$TROPHIC_MONREP), c("SPECIES", "TAXONNAME", "COMMONNAME")])

wd[wd$SPECIES=="MUVA",]$TROPHIC_MONREP<-"SECONDARY"
wd[wd$SPECIES %in% c("DEMA", "GARD"),]$TROPHIC_MONREP<-"PLANKTIVORE"
wd[wd$SPECIES=="VAAL",]$TROPHIC_MONREP<-"PISCIVORE"

wd[is.na(wd$DEPTH),]$DEPTH<-0	
wd[is.na(wd$CENTROIDLAT),]$CENTROIDLAT<-0	
wd[is.na(wd$CENTROIDLON),]$CENTROIDLON<-0	
wd[is.na(wd$TEMPERATURE),]$TEMPERATURE<-0	

levels(wd$REEF_ZONE)<-c(levels(wd$REEF_ZONE), "UNKNOWN")
levels(wd$FAMILY)<-c(levels(wd$FAMILY), "UNKNOWN")
levels(wd$TAXONNAME)<-c(levels(wd$TAXONNAME), "UNKNOWN")
levels(wd$COMMONNAME)<-c(levels(wd$COMMONNAME), "UNKNOWN")
levels(wd$COMMONFAMILYALL)<-c(levels(wd$COMMONFAMILYALL), "UNKNOWN", "Whale")
wd[wd$SPECIES=="MENO",]$COMMONFAMILYALL<-"Whale"

wd[is.na(wd$TAXONNAME),"TAXONNAME"]<-"UNKNOWN"
wd[is.na(wd$FAMILY),"FAMILY"]<-"UNKNOWN"
wd[is.na(wd$REEF_ZONE),"REEF_ZONE"]<-"UNKNOWN"
wd[is.na(wd$COMMONFAMILYALL),"COMMONFAMILYALL"]<-"UNKNOWN"
unique(wd[is.na(wd$COMMONNAME), c("SPECIES", "TAXONNAME", "OBS_TYPE")])
wd[is.na(wd$COMMONNAME),"COMMONNAME"]<-"UNKNOWN"

#DISTINGUISH between COUNT (for biomass and abundance calculations and number observed .. for other calculations)
wd$N_OBSERVED<-wd$COUNT
wd[wd$OBS_TYPE!="LINE",]$COUNT<-0
wd<-droplevels(wd)
dim(wd)

##### NOW FIXING DEPTHS ###############################

#set depth, and centroid lat-long field to NaN if zero ... 
#read in information in depths of tows with known missing depths
tmd<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/munge_Tow/Tow Missing Depths.csv")
tmd<-tmd[,c("DIVEID", "Average")]

wd[wd$DEPTH==0,"DEPTH"]<-NaN

#Using twd to assign depths io cases where that information is missing
dim(wd)
wd<-merge(wd, tmd, by="DIVEID", all.x=T)
wd[is.na(wd$DEPTH),]$DEPTH<-wd[is.na(wd$DEPTH),]$Average
wd$Average<-NULL

summary(wd)

unique(wd[wd$REEF_ZONE=="Unspecified", c("ISLAND")])

load("PRIA Tow RZ.RData")  #rz
rz$RZ<-rz$REEF_ZONE  
rz[is.na(rz$RZ),]				#Only one egment in Johnston is missing .. and it seems from examination of the data to be Protected slope

wd<-merge(wd, rz[,c("DIVEID", "SEGMENT", "RZ")], by=c("DIVEID", "SEGMENT"), all.x=T)
unique(rz$ISLAND)
wd[wd$ISLAND =="Kingman", c("ISLAND", "OBS_YEAR", "DIVEID", "SEGMENT", "DEPTH", "REEF_ZONE", "RZ")]

levels(wd$REEF_ZONE)<-c(levels(wd$REEF_ZONE), "Protected Slope")
wd[wd$ISLAND %in% unique(rz$ISLAND), ]$REEF_ZONE<-wd[wd$ISLAND %in% unique(rz$ISLAND), ]$RZ
wd$RZ<-NULL

wd[wd$REEF_ZONE=="Unspecified" & wd$ISLAND %in% c("Tutuila"),]$REEF_ZONE<-"Forereef"
wd[wd$REEF_ZONE=="Unspecified" & wd$ISLAND %in% c("Jarvis"),]$REEF_ZONE<-"Forereef"
wd[wd$REEF_ZONE=="Unspecified" ,]

wd[wd$SEGMENT>10,]
wd<-subset(wd, wd$SEGMENT %in% seq(1,10))
wtd<-droplevels(wd)

save(wtd, file="ALL_TOW_FISH_CLEANED.RData")