## read in data file that has region, unique id, site / segment lat longs
## data has to be in format region island identifier 1 identifier 2 latitude 1 latitude 2 
## have an if else statement if tow take these cols
## if spc take these cols

### merging site lat longs with sector and MPA shapefiles
rm(list=ls())
## spatial data libraries
library(sp)
library(rgdal) ## make sure GDAL and PROJ.4 libraries load successfully in message - if not try update R version
library(rgeos)
library(maptools)
## 
dat<-read.csv("data/tow segments to classify.csv")
### temporary manual fixes till the tow dataset gets cleaned up
dat[is.na(dat$CENTROIDLAT),]
dat[c(1433:1438),]

dat[1435,]$CENTROIDLAT<- 22.22924
dat[1436,]$CENTROIDLAT<-22.19440

dat[1435,]$CENTROIDLON<- -159.5854
dat[1436,]$CENTROIDLON<- -159.3190

dat[is.na(dat$CENTROIDLAT),]
dat[c(2329:2336),]

dat[2331,]$CENTROIDLAT<- -14.24595
dat[2332 ,]$CENTROIDLAT<--14.28574

dat[2331 ,]$CENTROIDLON<- -170.5720
dat[2332 ,]$CENTROIDLON<- -170.5641

####

datl<-split(dat, dat$REGION)


#### specify region, tow data type and run for each separately
nameREG<-"SAMOA_SANC" ## MHI, NWHI, PRIAs, SAMOA, SAMOA_SANC
datatype<-"TOW"
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" ## projection for field collected data
#### 

if(nameREG == "MARIAN"){
	shp<-readOGR("data/sector_mpa_shape_files/guam_sectors.shp")
	proj<-proj4string(shp)
}

if(nameREG == "MHI"){
	shp<-readOGR("data/sector_mpa_shape_files/MHI_Sectors_2017.shp")
	proj<-proj4string(shp)
}

if(nameREG == "SAMOA"){
	shp<-readOGR("data/sector_mpa_shape_files/tutuila_sectors.shp")
	proj<-proj4string(shp)
}

if(nameREG == "SAMOA_SANC"){
	shp<-readOGR("data/sector_mpa_shape_files/amsm_sanctuary_fish_for_adel.shp")
	proj<-proj4string(shp)
}


if(datatype=="TOW"){
	DATA_COLS_IN<-c("REGION", "ISLAND","DIVEID", "SEGMENT", "CENTROIDLAT", "CENTROIDLON")
	DATA_COLS_OUT<-c("REGION", "ISLAND","DIVEID", "SEGMENT", "ANALYSIS_SEC")
	nameREG<-"SAMOA"
}

if(datatype=="SPC"){
	DATA_COLS_IN<-c("REGION", "ISLAND","SITEVISITID", "SITE", "LATITUDE", "LONGITUDE")
	DATA_COLS_OUT<-c("REGION", "ISLAND","SITEVISITID", "SITE", "ANALYSIS_SEC")
}

###
datr<-data.frame(datl[nameREG])
names(datr)<-names(dat)
datr<-datr[,DATA_COLS_IN] ### get data into separate regions with these fields only file of region island identifier 1 identifier 2 latitude 1 longitude 2
datr$ANALYSIS_SEC<-datr$ISLAND
names(datr)<-c("REGION", "ISLAND","ID_1", "ID_2", "LATITUDE", "LONGITUDE", "ANALYSIS_SEC")
coordinates(datr)<- ~ LONGITUDE + LATITUDE
class(datr);str(datr); coordinates(datr); range(datr@coords[,2]); range(datr@coords[,1])

## assign dataframe as unprojected data points as the coordinate reference system
proj4string(datr)
proj4string(datr) <- CRS(wgs.84)
range(datr@coords[,2]); range(datr@coords[,1])
datr<- spTransform(datr, proj) ### transform projection to match the shape file proj

## ADEL TO COME BACK TO HERE AND MAKE IT GENERIC ONCE THE SHAPEFILES ARE STANDARDIZED IN NAMES
## FOR NOW USING THE OLD GUAM FILE UNTIL THE AREA ESTIMATES ARE UPDATED
#######################################################################
#### FOR GUAM - remove unncessary cols
drops <- c("SEC_FISHIN","SEC_HABITA","Site_ID","Area_KM","Site_Name","Gov_Level","NS_Full","Prot_Lvl","Mgmt_Plan","Mgmt_Agen","Fish_Rstr","Pri_Con_Fo","Prot_Focus", "Permanence","Constancy","Estab_Yr","URL","Site_Label","Vessel","Anchor") # list of col names
shp <- shp[,!(names(shp) %in% drops)] #remove columns

datr_SAVED<-datr
tmp<-over(datr, shp)
datr$ANALYSIS_SEC<-tmp[,c("Sector")]
datr<-datr@data
names(GUA)[[11]]<-"ANALYSIS_SEC"

### GUAM has MPAs that overlap the sectors so do it again for mpas and create separate col

##create a shape file of non open sectors
mpa.data<-shp[shp$Sector %in% c("Achang Reef Flat","Apra Harbor","Pati Point", "Piti Bomb Holes","Tumon Bay"),]
class(mpa.data)

tmp<-over(datr_SAVED, mpa.data)
GUA_MPA<-cbind(datr_SAVED@data, tmp[,c("Sector")])
names(GUA_MPA)[[6]]<-"ANALYSIS_SEC_MPA"

test<-cbind(GUA_MPA$ANALYSIS_SEC_MPA, datr)
names(test)[1]<-"GUA_MPA"
levels(test$GUA_MPA)<-c("ACHANG_MPA", "GUAM_EAST_OPEN", "GUAM_WEST_OPEN", "PATI_PT_MPA", "PITI_BOMB_MPA", "TUMON_BAY_MPA")
test$ANALYSIS_SECS<-test$GUA_MPA

levels(test$ANALYSIS_SECS)<-c(levels(test$ANALYSIS_SECS), "Open East", "Open West")

test[is.na(test$ANALYSIS_SECS),]$ANALYSIS_SECS<-test[is.na(test$ANALYSIS_SECS),]$ANALYSIS_SEC
test[test$ANALYSIS_SECS %in% c("Open East"),]$ANALYSIS_SECS<-"GUAM_EAST_OPEN"
test[test$ANALYSIS_SECS %in% c("Open West"),]$ANALYSIS_SECS<-"GUAM_WEST_OPEN"

test<-droplevels(test)

test$ANALYSIS_SEC<-test$ANALYSIS_SECS
test<-test[,c(names(datr))]
names(test)<-DATA_COLS_OUT
guam<-test

#######################################################################
### MHI
drops <- c("SEC_FISHIN","SEC_HABITA","Shape_Leng","Shape_Area") # list of col names
shp <- shp[,!(names(shp) %in% drops)] #remove columns

datr_SAVED<-datr
tmp<-over(datr, shp)
summary(tmp)

tmp[is.na(tmp$SEC_NAME),] ## this last segments seems not to fall within any sector?
## row 1336
tmp[c(1334:1338),]
tmp[is.na(tmp$SEC_NAME),]$SEC_NAME<-c("MOL_PALI")
tmp[is.na(tmp$OBJECTID),]$OBJECTID<-16
summary(tmp)
mhi<-tmp
### looks good
test<-cbind(tmp$SEC_NAME, datr@data)
names(test)[1]<-"SEC_NAME"
test$ANALYSIS_SEC<-test$SEC_NAME
test<-test[,c(names(datr))]
names(test)<-DATA_COLS_OUT
mhi<-test

#######################################################################
### SAMOA
levels(shp$LABEL)<-c("AUNUU_SANCTUARY_B", "FAGATELE_SANCTUARY", "TUT_NE", "TUT_NW", "TUT_SE", "TUT_SW")
summary(datr)
datr<-datr[datr$ISLAND == "Tutuila",]
datr<-droplevels(datr)
tmp<-over(datr, shp)
summary(tmp)

datr$ANALYSIS_SEC<-tmp$LABEL
datr<-datr[,c(names(datr))]
names(datr)<-DATA_COLS_OUT
tut<-datr@data

#######################################################################
### SAMOA SANCT
levels(shp$LABEL)<-c("AUNUU_SANCTUARY_B", "FAGATELE_SANCTUARY", "TUT_NE", "TUT_NW", "TUT_SE", "TUT_SW")
summary(datr)
datr<-datr[!datr$ISLAND == "Tutuila",]
datr<-droplevels(datr)
tmp<-over(datr, shp)
summary(tmp)

datr$ANALYSIS_SEC<-tmp$SEC_NAME
#datr<-datr[,c(names(datr))]
names(datr)<-DATA_COLS_OUT
sam<-datr@data

## merge together tut and sam and give NAs island values
tmp<-rbind(tut, sam)
tmp<-droplevels(tmp)
levels(tmp$ANALYSIS_SEC)<-c(levels(tmp$ANALYSIS_SEC), levels(tmp$ISLAND))
tmp[is.na(tmp$ANALYSIS_SEC),]$ANALYSIS_SEC<-tmp[is.na(tmp$ANALYSIS_SEC),]$ISLAND
tmp<-droplevels(tmp)

sam<-tmp
### save out

write.csv(guam, file="data/data_with_sectors_guam.csv")
write.csv(mhi, file="data/data_with_sectors_mhi.csv")
write.csv(sam, file="data/data_with_sectors_sam.csv")

### 

guam<-read.csv(file="data/data_with_sectors_guam.csv")
mhi<-read.csv(file="data/data_with_sectors_mhi.csv")
sam<-read.csv(file="data/data_with_sectors_sam.csv")

### 

all_tow_sec<-rbind(guam, mhi, sam)
write.csv(all_tow_sec, file="data/tow_data_with_sectors_GUA_SAM_MHI.csv")



