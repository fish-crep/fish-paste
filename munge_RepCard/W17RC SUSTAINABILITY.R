rm(list=ls())
setwd("/Users/ivor.williams/Documents/CRED/CRCP/NCRMP/Report Card Workshop/Fish Indicators 2017")
# SET UP ------------------------------------------------------------------
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

#bs<-read.csv("island baselines.csv")

source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/fish_team_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/Islandwide Mean&Variance Functions.R")

sm<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/SITE MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)
sectors<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)


# FISH REA WORKINGS ----------------------------------------------------------------
load("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/ALL_REA_FISH_RAW.rdata")
#load("/Users/ivor.williams/Documents/CRED/Fish Team/MARAMP17/ALL_REA_FISH_RAW.rdata")
# #FIXING GLITCH FOR SITE TIN-474
# unique(df[df$SITE=="TIN-00474", c("REP", "SITEVISITID", "DIVER")])
# df[df$SITE=="TIN-00474" & df$DIVER=="VAB",]$REP<-"B"
# df[df$SITE=="TIN-00474",]$SITEVISITID<-11015
# save(df, file="ALL_REA_FISH_RAW_inclM17.Rdata")

x<-df

# HOUSEKEEPING ------------------------------------------------------------
# clean up the data to only fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION" , "REGION_NAME", "SECTOR", "SPECIAL_AREA", "EXCLUDE_FLAG",
"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH", 
"HARD_CORAL", "MA",  "TA",  "CCA",  "SAND",  "SOFT_CORAL", "CLAM" , "SPONGE", "CORALLIMORPH", "CYANO", "TUNICATE", "ZOANTHID" , "OTHER", "COMPLEXITY", "TRAINING_YN", "VISIBILITY",
"SPECIES", "COUNT", "SIZE_", "OBS_TYPE", 
"TAXONNAME", "COMMONNAME", "GENUS", "FAMILY", "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

# by default, remove sites with EXCLUDE_FLAG set to TRUE
x[is.na(x$TRAINING_YN),]$TRAINING_YN<-FALSE   # Training flag of NA is equivalent to a FALSE .. as none of the odler data was 'training data'
x<-subset(x, x$TRAINING_YN==FALSE)
x<-subset(x, x$EXCLUDE_FLAG==0, drop=TRUE)
#x<-subset(x, x$OBS_TYPE %in% c("U","I","N"))
x<-subset(x, !x$REGION %in% c("CT"))
x<-subset(x, x$METHOD %in% c("nSPC"))
x<-subset(x, x$OBS_YEAR >2009)
x<-subset(x, x$REEF_ZONE %in% c("Forereef", "Protected Slope"))
x<-subset(x, !x$ISLAND %in% c("South Bank"))

x$SITE<-SiteNumLeadingZeros(x$SITE)

#######################
## CLEAN UP NAs #######
#######################
tmp.lev<-levels(x$HABITAT_CODE); head(tmp.lev)
levels(x$HABITAT_CODE)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$COMMONNAME); head(tmp.lev)
levels(x$COMMONNAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$GENUS); head(tmp.lev)
levels(x$GENUS)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$FAMILY); head(tmp.lev)
levels(x$FAMILY)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$COMMONFAMILYALL); head(tmp.lev)
levels(x$COMMONFAMILYALL)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$TROPHIC_MONREP); head(tmp.lev)
levels(x$TROPHIC_MONREP)<-c(tmp.lev, "UNKNOWN")

x[is.na(x$HABITAT_CODE),"HABITAT_CODE"]<-"UNKNOWN"
x[is.na(x$COMMONNAME),"COMMONNAME"]<-"UNKNOWN"
x[is.na(x$GENUS),"GENUS"]<-"UNKNOWN"
x[is.na(x$FAMILY),"FAMILY"]<-"UNKNOWN"
x[is.na(x$COMMONFAMILYALL),"COMMONFAMILYALL"]<-"UNKNOWN"
x[is.na(x$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"

x[is.na(x$SIZE_),]$SIZE_<-0
x[is.na(x$COUNT),]$COUNT<-0

x<-droplevels(x)

x[x$SPECIES=="SCFE",]$LMAX<-45
x[x$SPECIES=="LUFU",]$LMAX<-40
dim(x)

#add SEC_NAME to x  
# this would be better if SECTOR field in database was up to date properly .. rather than merge with the site_Sectors spreadsheet
x<-merge(x, sm[,c("SITE", "SEC_NAME")], by="SITE", all.x=TRUE)
#Determine Report Card Unit for this SEC

rcu<-aggregate(sectors$AREA_HA, by=sectors[,c("SEC_NAME", "REP_CARD")], FUN=mean)
tmp<-unique(x$SEC_NAME);tmp
tmp2<-unique(rcu$SEC_NAME)
tmp[tmp %in% tmp2]

dim(x)
x<-merge(x, rcu[,c("SEC_NAME", "REP_CARD")], by='SEC_NAME', all.x=T)
unique(x$REP_CARD)
unique(x$REGION)
dim(x)

#Put Islands into BIO_REGIONS, so we are comparing like with like
x$BIOREGION<-as.character(x$REGION)
x[x$REGION %in% c("MHI", "NWHI") | x$ISLAND == "Johnston",]$BIOREGION<-"HAWAII"
wd<-droplevels(x)


### DETERMINE WHICH SPECIES TO INCLUDE
# (1) ONLY OF TARGET FAMILIES .. JACKS, SNAPPER, GROUPER, GOATFISH, EMPEROR, SOLDIERFISH, SURGEONFISH
# (2) ONLY WITH LMAX > 40 OR HIGHER
# (3) AT LEAST 50 OBSERVATIONS IN THE BIOREGION  (POSSIBLY HIGHER BAR WOULD BE IDEAL)
### LATER GOING TO add MIN CUT OFF SIZE - TO REMOVE YOY; and others!

FAMILIES_OF_INTEREST<-c("Scaridae", "Lutjanidae", "Carangidae", "Lethrinidae", "Serranidae", "Acanthuridae", "Holocentridae", "Scombridae", "Mullidae")
MAX_SIZE_CUT_OFF<-40
MIN_OBSERVATIONS<-20  #REGION
PROP_SIZE_CUT<-0.4
NUM_OBS_CUT<-5		 #ISLAND


sp<-unique(wd[wd$FAMILY %in% FAMILIES_OF_INTEREST & wd$LMAX>MAX_SIZE_CUT_OFF, c("FAMILY", "SPECIES", "TAXONNAME")])
sp<-sp[!sp$SPECIES %in% c("CASP", "CLSP", "CESS", "SURG", "GOAT", "SAPP", "LETH", "NASP", "SCSP", "PARR", "EMPE", "JACK", "GROU", "EPSP", "ACSP"),]
sp

write.csv(cast(wd[wd$SPECIES %in% sp$SPECIES,], BIOREGION + FAMILY + TAXONNAME + SPECIES ~ OBS_TYPE, value="COUNT", sum), file="tmp species counts.csv")
#write.csv(aggregate(wd$COUNT, by=wd[,c("FAMILY", "SPECIES", "TAXONNAME", "LMAX")], FUN=sum), file="tmp species list.csv")



#sp<-unique(wd[wd$FAMILY %in% FAMILIES_OF_INTEREST & wd$LMAX>MAX_SIZE_CUT_OFF & wd$RANK=="Species",]$SPECIES)
wd[!wd$OBS_TYPE %in% c("U", "I", "N"),]$COUNT<-0
wd[!wd$SPECIES %in% sp$SPECIES,]$COUNT<-0
wd[!wd$SPECIES %in% sp$SPECIES,]$SPECIES<-"NONE"

### Calculate bioregion MAX size (rMAX) for each species. 

L1<-aggregate(wd$SIZE_, by=wd[,c("BIOREGION", "SPECIES", "LMAX")], FUN=max)
names(L1)[4]<-"rMAX"
C1<-aggregate(wd$COUNT, by=wd[,c("BIOREGION", "SPECIES")], FUN=sum)
names(C1)[3]<-"NUM"

F1<-merge(L1, C1, by=c("BIOREGION", "SPECIES"))
F1<-subset(F1, F1$NUM>MIN_OBSERVATIONS)

cast(F1, SPECIES + LMAX ~ BIOREGION, value="rMAX", sum)
#F1[F1$rMAX<0.6*F1$LMAX,]

PECULIAR_SPECIES<-c("CAGA", "TRBL", "ELBI", "SEDU")				#These are mostly always way way smaller than LMAX in all regions
F1<-subset(F1, !F1$SPECIES %in% PECULIAR_SPECIES )

#merge back in critical information from F1 - and 
head(F1)
wd<-merge(wd, F1[,c("BIOREGION", "SPECIES", "rMAX")], by=c("BIOREGION", "SPECIES"), all.x=T)
wd[is.na(wd$rMAX),]$rMAX<-0
wd[wd$rMAX==0,]$COUNT<-0
wd<-droplevels(wd)
head(wd)
## ENSURE rMAX is at least 60% of LMAX
wd[wd$rMAX<(0.6*wd$LMAX) & wd$rMAX>0, ]$rMAX<-wd[wd$rMAX<(0.6*wd$LMAX) & wd$rMAX>0, ]$LMAX*0.6

#generate Lm values
#Fish Species Table
# RAYFINNED FISHES: log10(Lm) = -0.1189 + 0.9157 * log10(Lmax) (Binholand and Froese J Appl Ichthyology 2009)
# ELASMOBRANCHES: log10(Lm)  = -0.1246 + 0.9924 * log10(Lmax)
# [Carcharhinidae, Dasyatidae, Ginglymostomatidae, Myliobatidae]
#######
wd$Lm<-10^(-0.1189+(0.9157*log10(wd$LMAX)))
wd$rLm<-10^(-0.1189+(0.9157*log10(wd$rMAX)))
ELASMO<-c("Carcharhinidae", "Dasyatidae", "Ginglymostomatidae", "Myliobatidae")
wd[wd$FAMILY %in% ELASMO,]$Lm<-10^(-0.1246+(0.9924*log10(wd[wd$FAMILY %in% ELASMO,]$LMAX)))
wd[wd$FAMILY %in% ELASMO,]$rLm<-10^(-0.1246+(0.9924*log10(wd[wd$FAMILY %in% ELASMO,]$rMAX)))

#######Cap sizes - in case overly large size estimate blows out the mean size
######wd[wd$COUNT>0 & wd$SIZE_>wd$LMAX*1.1,]$SIZE_<-wd[wd$COUNT>0 & wd$SIZE_>wd$LMAX*1.1,]$LMAX*1.1

wd<-droplevels(wd)
WD_SAVE<-wd

## MEAN SIZE OF TARGET SPECIES  ##########################################
table(wd$ISLAND)
wd<-WD_SAVE

#head(bio)
#HI_TARGET_SPECIES<-c("NAUN", "NALI", "ACBL", "ACDU", "NAHE", "NABR", "SCRU", "CHSO", "CHPE", "PAPO", "PACY", "MUFL", "MUVA", "CAME", "APVI", "MOGR", "MYBE") # These are all atrgeted fishes in Marc's analysis (that we are at all likely to have data for)
#removed PAIN ..and PIAN is rare in NWHI .. which is our reference areas
#HI_TARGET_SPECIES<-c("NAUN", "ACBL", "ACDU", "NAHE", "SCRU", "CHPE", "CHSO", "PAPO", "PACY", "CAME", "CAIG", "APVI", "MOGR", "MYBE")

#Set COUNT to zero for all fishes less than 40% OF rMAX SIZE
# and Only include Species where there at least 5 observations in the size class of interest
wd[wd$SIZE_<wd$rMAX*PROP_SIZE_CUT,]$COUNT<-0
wd<-droplevels(wd)

### NOW WORK OUT MEAN SIZE PER SPECIES PER REPORT CARD UNIT
wd$SC<-wd$SIZE_*wd$COUNT
rcd<-aggregate(wd[,c("SC", "COUNT")], by=wd[,c("REGION", "REP_CARD", "SPECIES", "LMAX", "Lm", "rLm")], sum)
rcd$MEAN_SIZE<-rcd$SC/rcd$COUNT

cast(rcd, REGION + REP_CARD ~ SPECIES, value="COUNT", sum)
rcd<-subset(rcd, rcd$COUNT>NUM_OBS_CUT)
head(rcd)
rcd$SUST<-rcd$MEAN_SIZE/rcd$rLm
SUST_OUT<-cast(rcd, REGION + REP_CARD ~ SPECIES, value="SUST", mean)
write.csv(SUST_OUT, file="tmp SUST RC17.csv")



