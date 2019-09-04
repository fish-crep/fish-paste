#IDW-POSSIBLY NEEDS SOME WORK!
setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Base R/Base Data Files")
rm(list=ls())
library(plyr)

library(sp)
library(rgdal) 
library(rgeos)
library(maptools)

sm<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/SURVEY MASTER.csv")
SM_FIELDS<-c("SITEVISITID", "REGION", "ISLAND", "SITE", "REEF_ZONE", "OBS_YEAR", "LATITUDE_SV", "LONGITUDE_SV", "LATITUDE_LOV", "LONGITUDE_LOV", "SEC_NAME")
head(sm[,SM_FIELDS])
sm<-sm[,SM_FIELDS]
sm$LONGITUDE<-sm$LONGITUDE_SV
sm$LATITUDE<-sm$LATITUDE_SV
sm[is.na(sm$LATITUDE),]$LATITUDE<-sm[is.na(sm$LATITUDE),]$LATITUDE_LOV
sm[is.na(sm$LONGITUDE),]$LONGITUDE<-sm[is.na(sm$LONGITUDE),]$LONGITUDE_LOV
head(sm)

get_RZ<-function(x, shp){ 
 
  # function assumes that x is a data frame with at least some identifier column (eg SITEVISITID), LATITUDE, LONGITUDE
  # shp is a Shapefile
  
  proj<-proj4string(shp)
  wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" ## projection for field collected data
  
  datr<-x
  coordinates(datr)<- ~ LONGITUDE + LATITUDE
  #class(datr); str(datr); coordinates(datr); range(datr@coords[,2]); range(datr@coords[,1])
  
  ## assign dataframe as unprojected data points as the coordinate reference system
  proj4string(datr)
  proj4string(datr) <- CRS(wgs.84)
  range(datr@coords[,2]); range(datr@coords[,1])
  datr<- spTransform(datr, proj) ### transform projection to match the shape file proj
  
  tmp<-over(datr, shp)
  x$OLD_REEF_ZONE<-x$REEF_ZONE
  x$SHP_REEF_ZONE<-tmp$REEF_ZONE
  
  return(x)		
} # end get_RZ

##getting reef zone

kOGR<-readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/sector_mpa_shape_files/kin_reefzones.shp")
fOGR<-readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/sector_mpa_shape_files/ffs_reefzones.shp")
jOGR<-readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/sector_mpa_shape_files/joh_reefzones.shp")

k<-get_RZ(sm[sm$ISLAND=="Kingman",], kOGR)   #I have subdirectory "ReefZones" with all the shapefile information
head(k)
f<-get_RZ(sm[sm$ISLAND=="French Frigate",], fOGR)   #I have subdirectory "ReefZones" with all the shapefile information
head(f)
j<-get_RZ(sm[sm$ISLAND=="Johnston",], jOGR)   #I have subdirectory "ReefZones" with all the shapefile information
head(j)

rz<-rbind(k,f,j)
sm<-merge(sm, rz[,c("SITEVISITID", "OLD_REEF_ZONE", "SHP_REEF_ZONE")], by="SITEVISITID", all.x=T)

get_SECNAME<-function(x, shp, LOCATION="MHI"){  

# x<-sm[!is.na(sm$LATITUDE) & sm$ISLAND=="Guam", ]
# shp<-gOGR
# LOCATION<-"Guam"

	# function assumes that x is a data frame with at least some identifier column (eg SITEVISITID), LATITUDE, LONGITUDE
	# shp is a Shapefile, LOCATION tells function how to deal with differences among regions (eg that Guam needs two calls)

	proj<-proj4string(shp)
	wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" ## projection for field collected data

	datr<-x
	coordinates(datr)<- ~ LONGITUDE + LATITUDE
	#class(datr); str(datr); coordinates(datr); range(datr@coords[,2]); range(datr@coords[,1])
	
	## assign dataframe as unprojected data points as the coordinate reference system
	proj4string(datr)
	proj4string(datr) <- CRS(wgs.84)
	range(datr@coords[,2]); range(datr@coords[,1])
	datr<- spTransform(datr, proj) ### transform projection to match the shape file proj
	
	tmp<-over(datr, shp)
	GIS_SEC<-tmp$SEC_NAME
	
	if(LOCATION == "Guam"){
		### GUAM has MPAs that overlap the sectors so do it again for mpas and create separate col
		##create a shape file of non open sectors
		mpa.data<-shp[shp$SEC_NAME %in% c("PATI_PT_MPA", "PITI_BOMB_MPA", "SASA_BAY_MPA", "TUMON_BAY_MPA", "ACHANG_MPA"),]
		tmp<-over(datr, mpa.data)
		MP<-tmp$SEC_NAME
		
		GIS_SEC[!is.na(MP)]<-MP[!is.na(MP)]		
	}
	x$GIS_SEC<-GIS_SEC
	return(x)		
} # end get_SECNAME

gOGR<-readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/sector_mpa_shape_files/guam_sectors_2017.shp")
hOGR<-readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/sector_mpa_shape_files/MHI_sectors_2019.shp")
#sOGR<-readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/sector_mpa_shape_files/NMSAS_PY.shp")
sOGR<-readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/sector_mpa_shape_files/amsm_sanctuary_fish_for_adel.shp")
tOGR<-readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/sector_mpa_shape_files/tut_sector.shp")

sm$OLD_SEC_NAME<-as.character(sm$SEC_NAME)
sm[is.na(sm$OLD_SEC_NAME),]$OLD_SEC_NAME<-as.character(sm[is.na(sm$OLD_SEC_NAME),]$ISLAND)

g<-get_SECNAME(sm[!is.na(sm$LATITUDE) & sm$ISLAND=="Guam", ], gOGR, LOCATION="Guam"); head(g)
table(g$GIS_SEC)
g$GIS_SEC<-revalue(g$GIS_SEC, c("ACHANG_MPA"="GUA_ACHANG", "GUAM_EAST_OPEN"="GUA_EAST_OPEN", "GUAM_HARBOR"="GUA_HARBOR",  "GUAM_WEST_OPEN"="GUA_WEST_OPEN", "PATI_PT_MPA"="GUA_PATI_POINT", "PITI_BOMB_MPA"="GUA_PITI_BOMB", "SASA_BAY_MPA"="GUA_SASA_BAY", "TUMON_BAY_MPA"="GUA_TUMON"))

h<-get_SECNAME(sm[!is.na(sm$LATITUDE) & sm$REGION=="MHI", ], hOGR, LOCATION="MHI"); head(h)
h$GIS_SEC<-revalue(h$GIS_SEC, c("HAW_HAMAK"="HAW_HAMAKUA"))

### SCREWY LAT-LONGS of SOME TUTUILA SITES
x<-sm[!is.na(sm$LATITUDE) & sm$ISLAND=="Tutuila",]; head(x)
summary(x)
#sm<-SM_SAVE
sm[!is.na(sm$LATITUDE) & sm$LATITUDE < - 50,]
screwy_sv<-unique(sm[!is.na(sm$LATITUDE) & sm$LATITUDE < - 50,]$SITEVISITID)
lon<-sm[sm$SITEVISITID %in% screwy_sv,]$LATITUDE; lon
sm[sm$SITEVISITID %in% screwy_sv,]$LATITUDE<-sm[sm$SITEVISITID %in% screwy_sv,]$LONGITUDE
sm[sm$SITEVISITID %in% screwy_sv,]$LONGITUDE<-lon
sm[!is.na(sm$LATITUDE) & sm$LATITUDE < - 50,]
### SCREWY LAT-LONGS now FIXED
t<-get_SECNAME(sm[!is.na(sm$LATITUDE) & sm$ISLAND=="Tutuila",], tOGR, LOCATION="Tutuila"); head(t)
unique(t$GIS_SEC); t[t$GIS_SEC=="TUT_LAND",]

s<-get_SECNAME(sm[!is.na(sm$LATITUDE) & sm$REGION=="SAMOA", ], sOGR, LOCATION="SAMOA"); head(s)
s<-subset(s, s$ISLAND %in% c("Rose", "Tau", "Swains"))
s$GIS_SEC<-as.character(s$GIS_SEC)
s[is.na(s$GIS_SEC),]$GIS_SEC<-s[is.na(s$GIS_SEC),]$OLD_SEC_NAME
str(s)
table(s$GIS_SEC)
s$GIS_SEC<-revalue(s$GIS_SEC, c("ROSE_SANCTUARY"="ROS_SANCTUARY", "SWAINS_SANCTUARY"="SWA_SANCTUARY", "Rose"="ROS_INNER", "Swains"="SWA_OPEN", "Tau"="TAU_OPEN"))

sec<-rbind(g, h, t, s)
sm<-merge(sm, sec[,c("SITEVISITID", "GIS_SEC")], by="SITEVISITID", all.x=T)

sm$GIS_SEC<-as.character(sm$GIS_SEC)
sm[sm$ISLAND %in% c("French Frigate", "Kingman", "Johnston"),]$GIS_SEC<-sm[sm$ISLAND %in% c("French Frigate", "Kingman", "Johnston"),]$OLD_SEC_NAME


sm_fix<-sm[sm$REGION %in% c("MHI") | sm$ISLAND %in% c("Guam", "French Frigate", "Johnston", "Kingman", "Tau", "Rose", "Swains", "Tutuila"),]
write.csv(sm_fix, file="SURVEY MASTER sector & rz.csv", row.names=F)
save(sm, file="SURVEY MASTER sector & rz.RData")
write.csv(sm, file="SURVEY MASTER sector & rz.csv")



#NOW UPDATE THE ACTUAL SITE MASTER FILE
head(sm)
upd<-sm[,c("SITEVISITID", "OLD_REEF_ZONE", "SHP_REEF_ZONE", "OLD_SEC_NAME", "GIS_SEC")]
sm<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/SURVEY MASTER.csv")
sm<-merge(sm, upd, by="SITEVISITID", all.x=T)
write.csv(sm, file="SURVEY MASTER RZ_SEC.csv", row.names=F)


