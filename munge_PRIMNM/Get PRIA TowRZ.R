setwd("/Users/ivor.williams/Documents/CRED/Fish Team/PRIA Report")
rm(list=ls())
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

library(sp)
library(rgdal) 
library(rgeos)
library(maptools)

# load PRIA segment info
stp<-read.csv("D:/CRED/PRIMNM_REPORT/data/tmpPRIAtow.csv")    		#I created this already .. its a list of all the  PRIA segments  

# load FFS segment info
ffs<-read.csv("D:/CRED/Towboard/data/tmpFFStow.csv")
# merge to one file
st<-rbind(ffs,stp)

st$LONGITUDE<-st$CENTROIDLON			#creating LATITUDE and LONGITUDE fields .. as that is what the code below expects
st$LATITUDE<-st$CENTROIDLAT
head(st)
st$X<-NULL

source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/fish_team_functions.R")

#This fuction pulls REEF_ZONE from the shapefile for the sites ... 
# this code as written assumes that the shapefile is in wgs.84 and that it has  value REEF_ZONE that can be assocaited with a lat/long
# REEF_ZONE will be attached to the input data.frame x and returned.
get_RZ<-function(x, shp){ 
#x is a data.frame with each record being a location of interest (eg sruvey site or tow segment) and including fields LATITUDE & LONGITUDE
#shp is a shapefile structure created using a function call like ... readOGR("./ReefZones/kin_reefzones.shp")
#requires these libraries
# library(sp)
# library(rgdal) 
# library(rgeos)
# library(maptools)


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
	x$REEF_ZONE<-tmp$REEF_ZONE
	
	return(x)		
} # end get_RZ


k<-get_RZ(st[st$ISLAND=="Kingman",], readOGR("D:/CRED/fish_team_R/fish-paste/data/sector_mpa_shape_files/kin_reefzones.shp"))   #I have subdirectory "ReefZones" with all the shapefile information
head(k)    #same as st .. but with REEF_ZONE added at the end
j<-get_RZ(st[st$ISLAND=="Johnston",], readOGR("D:/CRED/fish_team_R/fish-paste/data/sector_mpa_shape_files/joh_reefzones.shp"))   #I have subdirectory "ReefZones" with all the shapefile information
head(j)    #same as st .. but with REEF_ZONE added at the end
p<-get_RZ(st[st$ISLAND=="Palmyra",], readOGR("D:/CRED/fish_team_R/fish-paste/data/sector_mpa_shape_files/pal_reefzones.shp"))   #I have subdirectory "ReefZones" with all the shapefile information
head(p)    #same as st .. but with REEF_ZONE added at the end
f<-get_RZ(st[st$ISLAND=="French Frigate",], readOGR("D:/CRED/fish_team_R/fish-paste/data/sector_mpa_shape_files/ffs_reefzones.shp"))   #I have subdirectory "ReefZones" with all the shapefile information
head(f)    #same as st .. but with REEF_ZONE added at the end

rz<-rbind(k, j, p,f)
summary(rz)

rz[is.na(rz$REEF_ZONE) & rz$ISLAND=="Johnston",]$REEF_ZONE<-"Protected Slope"   #have got this from some checking of the specific point - against other known segments
rz[is.na(rz$REEF_ZONE) & rz$SEGMENTID==20100910101,]$REEF_ZONE<-"Forereef"   #have got this from some checking of the specific point - against other known segments
# several segments with NA - all tows with no associated data, so remove them
rz<-rz[complete.cases(rz$REEF_ZONE),]

rz[rz$REEF_ZONE=="Unknown",]  # Bunch of deeper outer Palmyra - that should be Forereef I assume
rz[rz$REEF_ZONE=="Unknown",]$REEF_ZONE<-"Forereef"
getwd()
save(rz, file="PRIA Tow RZ.RData")  #USe this file to get the correct ReefZone information

#save(rz, file="D:/CRED/fish_cruise_routine_report/monitoring_report/2017_status_report/data/Data Outputs/tow_diver/PRIA Tow RZ.RData")  #USe this file to get the correct ReefZone information

