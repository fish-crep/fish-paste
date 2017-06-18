### merging site lat longs with sector and MPA shapefiles
rm(list=ls())
## spatial data libraries
library(sp)
library(rgdal) ## make sure GDAL and PROJ.4 libraries load successfully in message - if not try update R version
library(rgeos)
library(maptools)
## 

#### read in right shape files
nameREG<-"MARIAN" ## MHI, NWHI, PRIAs, SAMOA
if(nameREG == "MARIAN"){
	shp<-readOGR("data/sector_mpa_shape_files/guam_sectors_2017.shp")
	proj<-proj4string(shp)
}

if(nameREG == "MHI"){
	shp<-readOGR("data/sector_mpa_shape_files/guam_sectors_2017.shp")
	proj<-proj4string(shp)
}

if(nameREG == "SAMOA"){
	shp<-readOGR("data/sector_mpa_shape_files/guam_sectors_2017.shp")
	proj<-proj4string(shp)
}



gua_shp<-readOGR("data/sector_mpa_shape_files/guam_sectors_2017.shp")
GUA_PROJ<-proj4string(gua_shp)
#plot(gua_shp)

## read in site / segment lat longs
## data has to be in format region island identifier 1 identifier 2 latitude 1 latitude 2 
## have an if else statement if tow take these cols
## if spc take these cols

load('~/Documents/GitHub/fish-paste/data/ALL_TOW_FISH_RAW.rdata')

tow<-df
tow<-tow[,c("REGION", "ISLAND", "SEGMENTID", "CENTROIDLAT", "CENTROIDLON")]
names(tow)<-c("REGION", "ISLAND", "SEGMENTID", "LATITUDE", "LONGITUDE")

list_regs<-split(tow, tow$REGION)

region<-"MARIAN"
tmp<-data.frame(list_regs[region])
names(tmp)<-names(tow)
tmp$ANALYSIS_SEC<-tmp$ISLAND

tmp2<-tmp[tmp$ISLAND == "Guam",]
tmp<-tmp[!tmp$ISLAND == "Guam",]

tmp2<-tmp2[complete.cases(tmp2),] ## for now stripping out missing values - awaiting proper tow corrected file with no lat longs missing
tmp2<-droplevels(tmp2)
coordinates(tmp2)<- ~ LONGITUDE + LATITUDE
#proj4string()








