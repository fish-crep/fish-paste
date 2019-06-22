rm(list=ls())
setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Base R/Base Data Files")

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(RODBC)            # to connect to oracle
source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/core_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/fish_team_functions.R")
#source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/Islandwide Mean&Variance Functions.R")


## CREATE A SURVEY METADATA file
load("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/ALL_SITE_SITEVISIT.RData")
head(df)
table(df$TYPE)
df$OLD_SITE<-df$SITE
df$XX<-1

#sm target REGION + ISLAND + SITE + REEF_ZONE + DEPTH_BIN + ROUNDID + MISSIONID + SITEVISITID + METHOD + OBS_YEAR + DATE_ + HABITAT_CODE + LATITUDE + LONGITUDE + EXCLUDE_FLAG
df$OBS_YEAR<-as.numeric(substring(df$DATE_, 1, 4))
df$SITE<-SiteNumLeadingZeros(df$SITE)

table(df$TYPE)
df[df$TYPE %in% c("ARMS Only", "ARMS"),]$TYPE<-"ARMS"
df[df$TYPE %in% c("Benthic Only", "Benthic", "Benthic & CAU"),]$TYPE<-"Benthic"
df[df$TYPE %in% c("CAU Only", "CAU"),]$TYPE<-"CAU"
df[df$TYPE %in% c("Oceanography Only", "Oceanography", "Oteam Only"),]$TYPE<-"Oceanography"
df[df$TYPE %in% c("Fish Only", "Fish"),]$TYPE<-"Fish"
df[df$TYPE %in% c("Benthic & Fish", "Both"),]$TYPE<-"Both"
df<-droplevels(df)
table(df$TYPE)

head(df)
names(df)

sum_field<-function(d, field_list){
	for(i in 1:length(field_list)){
		d[is.na(d[,field_list[i]]),field_list[i]]<-0
		d[d[,field_list[i]]!=0,field_list[i]]<-1
	}	
	return(rowSums(d[,field_list]))
} #set field

df$benth<-ifelse(sum_field(df, c("LPI_YN", "CORAL_BELT_ADULT_YN", "CORAL_BELT_JUVENILE_YN")) > 0,1,0)
df$fish<-ifelse(sum_field(df, c("FISH_REA_YN", "FISH_REA_YN")) > 0,1,0)
df$arms_d<-ifelse(sum_field(df, c("ARMS_DEPLOY_YN", "ARMS_DEPLOY_YN")) > 0,1,0)
df$arms_r<-ifelse(sum_field(df, c("ARMS_RECOVERED", "ARMS_RETRIEVE_YN")) > 0, 1, 0)
df$cau_d<-ifelse(sum_field(df, c("CAUS_DEPLOYED", "CAUS_DEPLOY_YN")) > 0, 1, 0)
df$cau_r<-ifelse(sum_field(df, c("CAUS_RECOVERED", "CAUS_RETRIEVE_YN")) > 0, 1, 0)
df$microb<-ifelse(sum_field(df, c("MICROBIAL_SAMPLE", "MICROBIAL_SAMPLE")) > 0, 1, 0)

#sv<-cast(df, REGION + ISLAND + SITE + OLD_SITE + REEF_ZONE + DEPTH_BIN + ROUNDID + MISSIONID + SITEVISITID + FISH_SITEVISITID + BENTHIC_SITEVISITID + OBS_YEAR + DATE_ + HABITAT_CODE + LATITUDE_SV + LONGITUDE_SV + LATITUDE_LOV + LONGITUDE_LOV + EXCLUDE_FLAG + CLIMATE_STATION_YN + PERM_SITE + PERM_MARKER + TRANSECT_PHOTOS + PHOTOMOSAIC_YN + MIN_Z_M+ MAX_Z_M + MIN_DEPTH_M + MAX_DEPTH_M ~ TYPE, value="XX", sum)
#write.csv(sv, file="tmpALLsurvey.csv")
sv<-cast(df, REGION + ISLAND + SITE + OLD_SITE + REEF_ZONE + DEPTH_BIN + ROUNDID + MISSIONID + SITEVISITID + OBS_YEAR + DATE_ + HABITAT_CODE + LATITUDE_SV + LONGITUDE_SV + LATITUDE_LOV + LONGITUDE_LOV + EXCLUDE_FLAG + CLIMATE_STATION_YN + PERM_SITE + TRANSECT_PHOTOS + PHOTOMOSAIC_YN + MIN_Z_M+ MAX_Z_M + MIN_DEPTH_M + MAX_DEPTH_M + benth + fish + arms_d + arms_r + cau_d + cau_r + microb~ TYPE, value="XX", sum)
#write.csv(sv, file="tmpALLsurvey.csv")
head(sv)

#Actually, need to deal with this better - can be microbial and fish for example
sv[sv$Both>0,]$Fish<-1
sv[sv$Both>0,]$Benthic<-1
sv$Both<-NULL
#save(sv, file="SURVEY METDATA.RData")

# load("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/ALL_REA_FISH_RAW.rdata")
# x<-df
# x$SITE<-SiteNumLeadingZeros(x$SITE)
# length(unique(x$SITEVISITID))
# CCRs<-unique(x[x$METHOD=="nSPC-CCR",]$SITE)
# SPCs<-unique(x[x$SITE %in% CCRs & x$METHOD=="nSPC",]$SITE) #SITES WHERE THERE IS BOTH AN SPC AND A SPC-CCR
# SPCs
# CCRsv<-unique(x[x$METHOD=="nSPC-CCR",]$SITEVISITID)
# dCCRsv<-unique(x[x$SITE %in% SPCs & x$METHOD=="nSPC-CCR",]$SITEVISITID)

#x<-subset(x, x$METHOD %in% c("nSPC", "BLT"))
# ## Update SITE to have three numeric digits (eg OAH-01 becomes OAH-001)
#x$DUMMY<-"nFISH_COUNTED"
#f<-cast(x, REGION + ISLAND + SITE + REEF_ZONE + DEPTH_BIN + ROUNDID + MISSIONID + SITEVISITID + METHOD + OBS_YEAR + DATE_ + HABITAT_CODE + LATITUDE + LONGITUDE + EXCLUDE_FLAG ~ DUMMY, value="COUNT", sum)
#save(f, file="tmp ALl Fish Surveys.RData")
# CCR<-unique(f[f$METHOD=="nSPC-CCR",]$SITEVISITID)
# SPC<-f[f$SITEVISITID %in% CCR & f$METHOD=="nSPC",]$SITEVISITID
# #write.csv(f[f$SITEVISITID %in% SPC,], file="tmpCCR NW Sites.csv")
# #bCCR<-CCR[!CCR %in% SPC]

#Now generate SITE DESIGN! (using data from existing SITE MASTER)
sm<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/SURVEY MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)
head(sm)
dim(sv)
#FIELDS TO PICK UP FROM SITE MASTER
SM_FIELDS<-c("SEC_NAME", "HUMANS20", "HUMANS200", "TYPE", "ANALYSIS_SCHEME", "bANALYSIS_SCHEME", "ANALYSIS_YEAR", "TUT2012", "OTHER_AREA_GROUPING", "SPECIAL_PROJ_YN", "SPECIAL_PROJ_DESCRIPTION", "CHANGE.NOTES")
z<-merge(sv, sm[,c("SITEVISITID", SM_FIELDS)], by="SITEVISITID", all.x=T)
dim(z)

#fiddling the SCP-CCR sites .. those are sites where there is both an SPC and a CCR
#ccr.sv<-z[z$SITE %in% SPCs,]

# # write.csv(zg, "MOD SURVEY MASTER.csv")
z[z$SITE=="TUT-02353",]

write.csv(z, "SURVEY MASTER.csv", row.names = FALSE)

#Now generate SURVEY OCEANOGRAPHY!
o<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/SITE OCEANOGR.csv")
head(o)
dim(o)
length(unique(o$SITE))
names(o)
#sv2<-subset(sv, !sv$SITEVISITID %in% dCCRsv)
# z<-merge(sv2[,c("SITE", "OBS_YEAR", "REGION", "ISLAND", "LATITUDE_SV", "LONGITUDE_SV")], o[,c("SITE", "OBS_YEAR", "MEAN_HII_40", "MEAN_HII_40_ADJUSTED_BY_AREA",                  
# "TUR_MEAN", "TUR_SD", "TUR_CLIM_M", "TUR_ANOM_M", "TUR_ANOM_F", "SST_MEAN", "SST_SD", "SST_CLIM_M", "SST_ANOM_M", "SST_ANOM_F", "PAR_MEAN", "PAR_SD", "PAR_CLIM_M", "PAR_ANOM_M", "PAR_ANOM_F", "CHL_MEAN", "CHL_SD", "CHL_CLIM_M", "CHL_ANOM_M", "CHL_ANOM_F", "WAVES_MEAN", "WAVES_SD", "WAVES_CLIM", "WAVES_ANOM" ,"WAVES_AN_1")])
# save(z, file="SURVEY OCEANOGR.RData")

