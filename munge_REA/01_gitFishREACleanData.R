rm(list=ls())
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

#LOAD LIBRARY FUNCTIONS ... 
source("lib/fish_team_functions.R")
#source("lib/Islandwide Mean&Variance Functions.R")

# get strata and sectors data data - NB - the data in the raw file should be checked and updated
sectors<-read.csv("data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
# load site master to merge with sector names
site_master<-read.csv("data/SITE MASTER.csv")
site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)

## LOAD AND CLEAN fish data
load("data/ALL_REA_FISH_RAW.rdata")
x<-df

# HOUSEKEEPING ------------------------------------------------------------
# clean up the data to only fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION" , "REGION_NAME", "SECTOR", "SPECIAL_AREA", "EXCLUDE_FLAG", "TRAINING_YN",
"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH", 
"HARD_CORAL", "MA",  "TA",  "CCA",  "SAND",  "SOFT_CORAL", "CLAM" , "SPONGE", "CORALLIMORPH", "CYANO", "TUNICATE", "ZOANTHID" , "OTHER", "OTHER_TYPE", 
"SPECIES", "COUNT", "SIZE_", "OBS_TYPE", 
"COMPLEXITY", "SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150", "MAX_HEIGHT", "VISIBILITY",
"SCIENTIFIC_NAME",  "TAXONNAME", "COMMONNAME", "GENUS", "FAMILY" , "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

## Update SITE to have three numeric digits (eg OAH-01 becomes OAH-001)
x$SITE<-SiteNumLeadingZeros(x$SITE)

x[is.na(x$TRAINING_YN),]$TRAINING_YN<-FALSE   # Training flag of NA is equivalent to a FALSE .. as none of the older data was 'training data'
x<-subset(x, x$TRAINING_YN==FALSE)
x<-subset(x, x$EXCLUDE_FLAG==0, drop=TRUE)
x<-subset(x, x$METHOD %in% c("nSPC", "nSPC-CCR"), drop=TRUE)
#x<-subset(x, x$OBS_YEAR >2008, drop=TRUE)
x<-subset(x, x$OBS_TYPE %in% c("U","I","N", "F", "T", "P"))  # note this includes all the data .. wlll need to add filtering to the scripts that analyse the data

#add SITE MASTER information to x  #IDW - note that if we join on SITE then SITE MASTER would also join to all surveys at a site .. for nSPC there are no duplicates, but some of those sites were oldeer BLT sites that were also survyed in earlier years.
#x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE)
x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE)  #..  should actually pick up ANALYSIS_SEC from the sectors file.

#CHECK THAT all ANALYSIS_SCHEMES are present in the site_master file)
idw<-x[is.na(x$ANALYSIS_SCHEME)  & x$METHOD=="nSPC", c("REGION", "SITE","OBS_YEAR", "METHOD"),]
if(dim(idw)[1]>0) {cat("nSPC sites with MISSING ANALYSIS_SCHEME")}   # should be 0

############################################################################################
###### new section .. where there is substrate_height data, work out average height && ave_height_variability so that we get standardized complexity metrics (mean hieght, mean height variability, max-height) 
sh_out<-CalcMeanSHMeanSHDiff(x)
x$MEAN_SH<-sh_out[[1]]
x$SD_SH_DIFF<-sh_out[[3]]

# remove the component SUBSTRATE_HEIGHT fields
x<-x[, setdiff(names(x),c("SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150"))]
############################################################################################
x<-droplevels(x)

#convert COMPLEXITY to a numeric field ### 
x$COMPLEXITY<-as.vector(toupper(x$COMPLEXITY))
x[is.na(x$COMPLEXITY),"COMPLEXITY"]<-"UNKNOWN"
COMPLEXITY_VALUES<-toupper(c("Low", "Med-Low", "Med", "Med-Hi", "Hi", "Very-Hi"))
x$ComplexityValue<-NaN
for (i in 1:length(COMPLEXITY_VALUES)){
	if(COMPLEXITY_VALUES[i] %in% x$COMPLEXITY){
		x[x$COMPLEXITY==COMPLEXITY_VALUES[i],]$ComplexityValue<-i
	}
}


#######################
## CLEAN UP NAs #######
#######################
tmp.lev<-levels(x$HABITAT_CODE); head(tmp.lev)
levels(x$HABITAT_CODE)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$SCIENTIFIC_NAME); head(tmp.lev)
levels(x$SCIENTIFIC_NAME)<-c(tmp.lev, "UNKNOWN")
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
x[is.na(x$SCIENTIFIC_NAME),"SCIENTIFIC_NAME"]<-"UNKNOWN"
x[is.na(x$COMMONNAME),"COMMONNAME"]<-"UNKNOWN"
x[is.na(x$GENUS),"GENUS"]<-"UNKNOWN"
x[is.na(x$FAMILY),"FAMILY"]<-"UNKNOWN"
x[is.na(x$COMMONFAMILYALL),"COMMONFAMILYALL"]<-"UNKNOWN"
x[is.na(x$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"

x[is.na(x$COUNT),]$COUNT<-0
x[is.na(x$SIZE_),]$SIZE_<-0
###x[is.na(x$LMAX),]$LMAX<-999

## separate out the north and south marianas 
levels(x$REGION)<-c(levels(x$REGION), "S.MARIAN", "N.MARIAN")
x[x$ISLAND %in% c("Guam", "Rota", "Aguijan", "Tinian", "Saipan"),]$REGION<-"S.MARIAN"
x[x$ISLAND %in% c("Alamagan","Guguan","Sarigan","Pagan", "Agrihan", "Asuncion", "Maug", "Farallon de Pajaros"),]$REGION<-"N.MARIAN"

sectors[sectors$ISLAND %in% c("Guam", "Rota", "Aguijan", "Tinian", "Saipan"),]$REGION<-"S.MARIAN"
sectors[sectors$ISLAND %in% c("Alamagan","Guguan","Sarigan","Pagan", "Agrihan", "Asuncion", "Maug", "Farallon de Pajaros"),]$REGION<-"N.MARIAN"
sectors<-droplevels(sectors)
save(sectors, file="TMPsectors.Rdata")  # save cleaned up sectors file

###NEED TO SET VISIBILITY to -9999 when its NA
### NOTE THAT NWHI 2012 has VISIBLITY OF NA
unique(x[is.na(x$VISIBILITY),c("REGION", "OBS_YEAR")])
x[is.na(x$VISIBILITY),]$VISIBILITY<- -999
#x[x$VISIBILITY>30,]$VISIBILITY<- 30

#fixing unknown lat/long from a sites survyeted by Val Brown in Guam in 2015. These values are probably close
# .. putting this in here so that we do not have NAs in the LAT and LONG .. but we do nto want to save these to the actual master data file
x[x$SITE=="GUA-01310",]$LATITUDE<-13.24173
x[x$SITE=="GUA-01310",]$LONGITUDE<-144.70428

x<-droplevels(x)

#BENTHOS DOES NOT ALWAYS SUM TO 100% .. THIS IS A (LONG-LIVED!) TEMP FIX .. PROBABLY BETTER TO FIX THIS INSIDE ORACLE
BENTHIC_FIELDS<-c("HARD_CORAL", "SOFT_CORAL", "MA", "CCA", "TA", "SAND", "CYANO", "CLAM", "CORALLIMORPH", "ZOANTHID", "TUNICATE", "SPONGE", "OTHER")
UNIQUE_ROUND<-c("REGION", "OBS_YEAR", "METHOD")
round_table<-Aggregate_InputTable(x, UNIQUE_ROUND)

x$countBD<-apply(x[,BENTHIC_FIELDS], 1, function(xx) length(which(!is.na(xx))))  #IDW 10-22-2013 checking for situation where there is NO benthic data at all
for(i in 1:dim(round_table)[1])
{
	if(round_table[i,"METHOD"] %in% c("nSPC", "nSPC-CCR"))
	{
		tmp_data<-x[x$OBS_YEAR==round_table[i,"OBS_YEAR"] & x$METHOD==round_table[i,"METHOD"] & x$REGION==round_table[i,"REGION"],]

		#go through BENTHIC_FIELDS, checking whether there are some NAs and some data values
		for(j in 1:length(BENTHIC_FIELDS))
		{
			## IF there are both non NAs and NAs
			if(length(tmp_data[!is.na(tmp_data[,BENTHIC_FIELDS[j]]),BENTHIC_FIELDS[j]]) > 0 
			        & length(tmp_data[is.na(tmp_data[,BENTHIC_FIELDS[j]]),BENTHIC_FIELDS[j]]) > 0) 
			{
				#set all NAs of that field to 0
				tmp_data[is.na(tmp_data[,BENTHIC_FIELDS[j]]),BENTHIC_FIELDS[j]]<-0	

				#now rewrite the benthic fields with NAs converted to zeros
				x[x$OBS_YEAR==round_table[i,"OBS_YEAR"] & x$METHOD==round_table[i,"METHOD"] & x$REGION==round_table[i,"REGION"],BENTHIC_FIELDS[j]]<-tmp_data[,BENTHIC_FIELDS[j]]
			}
		}
	}
}
# now reset zeros to NAs for all records where there was NO benthic data at all
x[x$countBD==0,BENTHIC_FIELDS]<-NA

wd<-droplevels(x)
save(wd, file="TMPwd.Rdata")  #Save clean working data


