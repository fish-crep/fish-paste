
#### Add fish fields to common map girds  #####
rm(list=ls(all=TRUE))
# bring in common csv file, change file name for each island
### not all islands have hard/soft information or reef zones

wd<-read.csv("T:/Common/Maps/ProductFolder/ASRAMP_2018/ros_grids_50m_table.csv")
head(wd)

# create a field for depth bin
wd$DEPTH_BIN<-"MESO"
levels(wd$DEPTH_BIN)<-c("SHALLOW","MID","DEEP", "LAND",levels(wd$DEPTH_BIN))

# change bathy dept bin to number
wd$Bty_MidPt<-as.numeric(wd$Bty_MidPt)
# label depth ranges on mean bathy value 
wd[which(wd$Bty_Mean > -1),]$DEPTH_BIN<-"NS" # not samplable
wd[which(wd$Bty_Mean >-6 & wd$Bty_Mean < -1),]$DEPTH_BIN<-"SHALLOW"
wd[which(wd$Bty_Mean >-18 & wd$Bty_Mean < -6),]$DEPTH_BIN<-"MID"
wd[which(wd$Bty_Mean >-30 & wd$Bty_Mean < -18),]$DEPTH_BIN<-"DEEP"
# not all grids have this, but some have values of 1
wd[which(wd$Bty_Mean >= 0),]$DEPTH_BIN<-"LAND"

# create a field for hard soft
wd$HS_50<-"SOFT"
levels(wd$HS_50)<-c("HARD","UNKNOWN",levels(wd$HS_50))
wd[which(wd$Hard_Per > 50),]$HS_50<-"HARD"
wd[which(wd$Unk_Per == 100),]$HS_50<-"UNKNOWN"

# save grid as samplable areas: filter out soft bottom and meso depths, and land
wd<-wd[wd$DEPTH_BIN != "LAND",]
wd<-wd[wd$DEPTH_BIN != "MESO",]
#wd<-wd[wd$HS_50 != "SOFT",]
wd<-droplevels(wd)

# add reef zone field
wd$REEF_ZONE<-"FR"
levels(wd$REEF_ZONE)<-c("BR","LG","PS",levels(wd$REEF_ZONE))
wd[which(wd$RZ_FRF_Per >50),]$REEF_ZONE<-"FR"
wd[which(wd$RZ_BRF_Per >50),]$REEF_ZONE<-"BR" # NOT ALL ISLANDS HAVE THIS REEF ZONE
wd[which(wd$RZ_LAG_Per >50),]$REEF_ZONE<-"LG" # NOT ALL ISLANDS HAVE THIS REEF ZONE
wd[which(wd$RZ_PRS_Per >50),]$REEF_ZONE<-"PS" # NOT ALL ISLANDS HAVE THIS REEF ZONE
wd<-droplevels(wd)
# save file 
write.csv(wd,file="T:/Fish/Cruise Prep/ISLAND_GRIDS/ros_50m_grid_FISH.csv")


######################################################################
###################################################################3

#### Add fish fields to common map girds 50 percent hard example #####
rm(list=ls(all=TRUE))
# bring in common csv file, change file name for each island
### not all islands have hard/soft information or reef zones

wd<-read.csv("T:/Common/Maps/ROS_PAL_example/pal_grids_50m_table_example.csv")
head(wd)

# create a field for depth bin
wd$DEPTH_BIN_midpt<-"MESO"
levels(wd$DEPTH_BIN_midpt)<-c("SHALLOW","MID","DEEP", "LAND",levels(wd$DEPTH_BIN_midpt))

# change bathy dept bin to number
wd$Bty_MidPt<-as.numeric(wd$Bty_MidPt)

# label depth ranges on midpt bathy value 

wd[which(wd$Bty_MidPt > -1),]$DEPTH_BIN_midpt<-"NS" # not samplable
wd[which(wd$Bty_MidPt >-6 & wd$Bty_MidPt < -1),]$DEPTH_BIN_midpt<-"SHALLOW"
wd[which(wd$Bty_MidPt >-18 & wd$Bty_MidPt < -6),]$DEPTH_BIN_midpt<-"MID"
wd[which(wd$Bty_MidPt >-30 & wd$Bty_MidPt < -18),]$DEPTH_BIN_midpt<-"DEEP"
# not all grids have this, but some have values of 1
wd[which(wd$Bty_MidPt >= 0),]$DEPTH_BIN_midpt<-"LAND"


# create a field for hard soft
wd$HS_50<-"SOFT"
levels(wd$HS_50)<-c("HARD","UNKNOWN",levels(wd$HS_50))
wd[which(wd$Hard_Per > 50),]$HS_50<-"HARD"
wd[which(wd$Unk_Per == 100),]$HS_50<-"UNKNOWN"

# save grid as samplable areas: filter out soft bottom and meso depths, and land
wd<-wd[wd$DEPTH_BIN != "LAND",]
wd<-wd[wd$DEPTH_BIN != "MESO",]
#wd<-wd[wd$HS_50 != "SOFT",]
wd<-droplevels(wd)

# add reef zone field
wd$REEF_ZONE<-"FR"
levels(wd$REEF_ZONE)<-c("BR","LG","PS",levels(wd$REEF_ZONE))
wd[which(wd$RZ_FRF_Per >50),]$REEF_ZONE<-"FR"
wd[which(wd$RZ_BRF_Per >50),]$REEF_ZONE<-"BR" # NOT ALL ISLANDS HAVE THIS REEF ZONE
wd[which(wd$RZ_LAG_Per >50),]$REEF_ZONE<-"LG" # NOT ALL ISLANDS HAVE THIS REEF ZONE
wd[which(wd$RZ_PRS_Per >50),]$REEF_ZONE<-"PS" # NOT ALL ISLANDS HAVE THIS REEF ZONE
wd<-droplevels(wd)
# save file 
write.csv(wd,file="T:/Fish/Cruise Prep/ISLAND_GRIDS/pal_grids_50m_table_FISH_midpt.csv")

