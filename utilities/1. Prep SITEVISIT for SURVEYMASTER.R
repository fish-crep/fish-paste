#Read in SITEVISIT table from Oracle & prepare file to match the columns in SURVEY MASTER file.
#Send this revised SITEVISIT table to Kisei to merge sector name

sv<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/V0_SITE_VISIT_DATA.csv")
#sv<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/V0_SITE_VISIT_ALL_2022_DATA.csv")


#Subet just occ sites with photoquads (also include SfM sites)- May want to eventually include all OCC sites into survey master- this will be a bigger lift
occ<-subset(sv,TRANSECT_PHOTOS==-1)
fb<-subset(sv,TYPE %in% c("Fish","Benthic"))

new.sv<-rbind(occ,fb)
#Tweaks
colnames(sv)[colnames(sv)=="MAX_DEPTH_M"]<-"new_MAX_DEPTH_M"
colnames(sv)[colnames(sv)=="MIN_DEPTH_M"]<-"new_MIN_DEPTH_M"
colnames(sv)[colnames(sv)=="LONGITUDE_LOS"]<-"LONGITUDE_LOV"
colnames(sv)[colnames(sv)=="LATITUDE_LOS"]<-"LATITUDE_LOV"
colnames(sv)[colnames(sv)=="MICROBIAL_YN"]<-"microb"
colnames(sv)[colnames(sv)=="CAUS_DEPLOY_YN"]<-"cau_d"
colnames(sv)[colnames(sv)=="CAUS_RETRIEVE_YN"]<-"cau_r"


sv$Benthic<-ifelse(sv$TYPE=="Benthic",-1,0)
sv$Fish<-ifelse(sv$TYPE=="Fish",-1,0)
sv$Oceanography<-ifelse(sv$TYPE=="Oceanography",-1,0)
sv$CAU<-ifelse(sv$cau_r==-1,-1,0)

sv$PERM_SITE<-NA
sv$OLD_SITE<-NA
sv$benth<-NA
sv$fish<-NA
sv$ARMS<-NA
sv$microb<-NA
sv$arms_r<-NA
sv$arms_d<-NA
sv$SEC_NAME<-NA
sv$HUMANS20<-NA 
sv$HUMANS200<-NA 
sv$TYPE<- NA
sv$ANALYSIS_SCHEME<-NA
sv$bANALYSIS_SCHEME<-NA
sv$ANALYSIS_YEAR<-NA
sv$TUT2012<-NA
sv$OTHER_AREA_GROUPING<-NA
sv$SPECIAL_PROJ_YN<-NA
sv$SPECIAL_PROJ_DESCRIPTION<-NA
sv$OBS_YEAR<-2022


cols.keep<-c("SITEVISITID",	"REGION",	"ISLAND",	"SITE",	"OLD_SITE",	"OCC_SITEID",	"REEF_ZONE",	"DEPTH_BIN",
             "ROUNDID",	"MISSIONID",	"OBS_YEAR",	"DATE_",	"HABITAT_CODE",	"LATITUDE_SV",	"LONGITUDE_SV",	"LATITUDE_LOV",	"LONGITUDE_LOV",
             "EXCLUDE_FLAG",	"CLIMATE_STATION_YN",	"PERM_SITE",	"TRANSECT_PHOTOS",	"PHOTOMOSAIC_YN",	"new_MIN_DEPTH_M",	"new_MAX_DEPTH_M",	"benth",
             "fish",	"arms_d",	"arms_r",	"cau_d",	"cau_r",	"microb",	"ARMS",	"Benthic",	"CAU",	"Fish",	"Oceanography",	"SEC_NAME",	"HUMANS20",	
             "HUMANS200",	"TYPE",	"ANALYSIS_SCHEME",	"bANALYSIS_SCHEME",	"ANALYSIS_YEAR",
             "TUT2012",	"OTHER_AREA_GROUPING",	"SPECIAL_PROJ_YN",	"SPECIAL_PROJ_DESCRIPTION")

sv<-sv[,cols.keep]

write.csv(sv,file="T:/Benthic/Data/REA Coral Demography & Cover/Cleaned_sitevisit_112322.csv")
