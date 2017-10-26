setwd("/Users/ivor.williams/Documents/CRED/Fish Team/PRIA Report")
rm(list=ls())
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

library(sp)
library(rgdal) 
library(rgeos)
library(maptools)

source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/fish_team_functions.R")

get_GEOREGION<-function(x, shp){ 
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
	x$GEOREGION<-tmp$GEOREGION
	
	return(x)		
} # end get_GEOREGION


## LOAD AND CLEAN fish data
load("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/ALL_REA_FISH_RAW.rdata")
x<-df

# HOUSEKEEPING ------------------------------------------------------------
# clean up the data to only fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "REGION", "ISLAND", "OBS_YEAR", "SITE", "LATITUDE", "LONGITUDE")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

## Update SITE to have five numeric digits (eg OAH-01 becomes OAH-00001)
x$SITE<-SiteNumLeadingZeros(x$SITE)
x<-subset(x, x$REGION == "PRIAs") 
wd<-droplevels(x)

#get base survey info, calculate average depth+complexity+so on
wd$COUNT<-0  #just need for next function
st<-Aggregate_InputTable(wd, DATA_COLS)

ISLANDS<-unique(wd$ISLAND)

w<-get_GEOREGION(st[st$ISLAND=="Wake",], readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/PRIA Report/Georegions/WAK_Georegion.shp"))
summary(w)
j<-get_GEOREGION(st[st$ISLAND=="Jarvis",], readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/PRIA Report/Georegions/JAR_Georegion.shp"))
summary(j)
k<-get_GEOREGION(st[st$ISLAND=="Kingman",], readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/PRIA Report/Georegions/KIN_Georegion.shp"))
summary(k)
b<-get_GEOREGION(st[st$ISLAND=="Baker",], readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/PRIA Report/Georegions/BAK_Georegion.shp"))
summary(b)
h<-get_GEOREGION(st[st$ISLAND=="Howland",], readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/PRIA Report/Georegions/HOW_Georegion.shp"))
summary(h)
p<-get_GEOREGION(st[st$ISLAND=="Palmyra",], readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/PRIA Report/Georegions/PAL_Georegion.shp"))
summary(p)
jo<-get_GEOREGION(st[st$ISLAND=="Johnston",], readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/PRIA Report/Georegions/JOH_Georegion.shp"))
summary(jo)

all.geo<-rbind(w, j, k, b, h, p, jo)
summary(all.geo)

save(all.geo, file="PRIA Site Georegions.RData")


