

#This fuction pulls REEF_ZONE from the associated island shapefile for the sites/tow segments. This requires the input data structure to have fields LATITUDE and LONGITUDE, and as written this code expects the shapefile to have a field REEF_ZONE. This fucntion requires the rgdal package

library(rgdal)
get_RZ<-function(x, shp){ 
  
  #
  
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
} 
# end get_RZ

# EXAMPLE: 
# k<-get_RZ(st[st$ISLAND=="Kingman",], readOGR("D:/CRED/GIS/Common shapefiles/REEF_ZONE/kin_reefzones.shp"))   #I have subdirectory "ReefZones" with all the shapefile information


get_SECNAME<-function(x, shp, LOCATION="MHI"){  
	
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
	
	if(LOCATION %in% "Tutuila"){
		GIS_SEC<-tmp$LABEL
	} else {
		GIS_SEC<-tmp$SEC_NAME
	}


	if(LOCATION == "Guam"){

		### GUAM has MPAs that overlap the sectors so do it again for mpas and create separate col
		##create a shape file of non open sectors
		mpa.data<-shp[shp$SEC_NAME %in% c("TUT_PATI_PT", "TUT_PITI_BOMB", "TUT_SASA_BAY", "TUT_TUMON_BAY", "TUT_ACHANG"),]
		tmp<-over(datr, mpa.data)
		MP<-tmp$SEC_NAME
		
		GIS_SEC[!is.na(MP)]<-MP[!is.na(MP)]		
	}
	x$GIS_SEC<-GIS_SEC
	return(x)		
} # end get_SECNAME

##EXANPLE CALL
# g<-wd[wd$ISLAND=="Guam", c("SITE", "SITEVISITID", "LATITUDE", "LONGITUDE", "SEC_NAME")]
# g<-droplevels(g)
# shp<-readOGR("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/data/sector_mpa_shape_files/guam_sectors_2017.shp")
# tmp<-get_SECNAME(g, shp, LOCATION="Guam")

