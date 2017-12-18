

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