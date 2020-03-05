#### Add fish fields to common map grrds  #####

####!!!!!!! This code is a compilation of all islands in the MHI. As each island becomes available, that island is added to the GRIDFILES list. When that island is added, to run this code JUST FOR THAT ISLAND, indicate the item number in the list as INDEX. For example, if you added niihau and are running that island, INDEX<-5

library(tidyverse)
rm(list=ls(all=TRUE))
setwd("X:/GIS/Projects/CommonMaps/02_PostProcess/01_gdbexport/MHI_2018")

# bring in common csv file, change file name for each island
### not all islands have hard/soft information or reef zones - BUT AT THIS POINT, ALL *SHOULD* HAVE REEF ZONE
a<-read.csv("lan_50m_grid_join_table.csv")

GRIDFILES<-c("oah_50m_grid_join_table1.csv", "oah_50m_grid_join_table2.csv", "mol_50m_grid_join_table.csv", "kau_50m_grid_join_table.csv", "nii_50m_grid_join_table.csv", "lan_50m_grid_join_table.csv")

# set this for island you are running, MOST LIKELY last item in the GRIDFILES list
INDEX<-6
gridf<-GRIDFILES[INDEX]

wd<-orig.wd<-read.csv(gridf)
head(wd)
summary(wd)

# create a field for depth bin
wd$DEPTH_BIN<-"MISSING"
wd[is.na(wd$Bty_Mean),]$DEPTH_BIN<-"UNKN"
# label depth ranges on mean bathy value 
wd[which(wd$Bty_Mean >= -1),]$DEPTH_BIN<-"ONEM" # not samplable
wd[which(wd$Bty_Mean >= -6 & wd$Bty_Mean < -1),]$DEPTH_BIN<-"SHAL"
wd[which(wd$Bty_Mean >= -18 & wd$Bty_Mean < -6),]$DEPTH_BIN<-"MIDD"
wd[which(wd$Bty_Mean >= -30 & wd$Bty_Mean < -18),]$DEPTH_BIN<-"DEEP"
wd[which(wd$Bty_Mean < -30),]$DEPTH_BIN<-"MESO"
# not all grids have this, but some have values of 1
#wd[which(wd$Bty_Mean >= 0),]$DEPTH_BIN<-"LAND"    #Possibly set to SHAL! (as default) .. as these are right next to land!

# check for missing depth bin values
head(wd %>% filter(DEPTH_BIN == "MISSING")) # SHOULD RETURN ZERO ROWS

# check data to see if we have a range of hard bottom percentage values
table(wd$DEPTH_BIN, wd$Hard_Per)

if("Hard_Per" %in% colnames(wd)){
	
	#Change hard and soft and unknown per to be proportion of not land
	if("Hard_Per" %in% colnames(wd)){
		nl<-which(wd$Land_Per<100)
		wd$NOT_LAND<-(100-wd$Land_Per)/100
		wd[nl, c("Hard_Per", "Unk_Per", "Soft_Per")]<-wd[nl, c("Hard_Per", "Unk_Per", "Soft_Per")]/wd[nl, ]$NOT_LAND
	}	
	
	# create fields for hard soft
	wd$HARD_50<-wd$HARD_10<-"MISSING"
	## HARD_1O set to "H" if hard >= 10%, otherwise set to whatever is higher of soft or unknown
	table(wd$Hard_Per)
	h<-which(wd$Hard_Per>=10); 	if (length(h)>0)	 	wd[h,]$HARD_10<-"H"
	s<-which(wd$HARD_10=="MISSING" & wd$Soft_Per>=wd$Unk_Per); 	if (length(s)>0)		 	wd[s,]$HARD_10<-"S"
	u<-which(wd$HARD_10=="MISSING" & wd$Unk_Per>wd$Soft_Per); 	if (length(u)>0)	 		wd[u,]$HARD_10<-"U"
	table(wd$HARD_10)
	
	## HARD_50 sets to Unknown if > 50% Unkown .. otherwise set to majority of hard/soft
	u<-which(wd$Unk_Per>50); 	if (length(u)>0)	 	wd[u,]$HARD_50<-"U"
	s<-which(wd$HARD_50=="MISSING" & wd$Soft_Per>wd$Hard_Per); 	if (length(s)>0)	 	wd[s,]$HARD_50<-"S"
	h<-which(wd$HARD_50=="MISSING" & wd$Hard_Per>=wd$Soft_Per); 	if (length(h)>0)	 	wd[h,]$HARD_50<-"H"
	table(wd$HARD_50)
} else {  
	# No hard/soft information
	wd$HARD_10<-wd$HARD_50<-"U"
}


wd$ZONE_CODE<-"FRF"   #Default
# add reef zone field - whatever is largest
# set INDEX to islands in GRIDFILES list that DO NOT HAVE REEF ZONE FIELDS ! but all SHOULD have! 
if(!INDEX %in% c(999)){
#Modified to not set to LND here ... can only do that below (if LND is > 50) .. so just picking the dominant reef type
	RZ_NAMES<-c("RZ_FRF_Per", "RZ_BRF_Per", "RZ_LAG_Per", "RZ_RCF_Per", "RZ_PRS_Per", "RZ_OTH_Per")
	wd$MAX_POS<-apply(wd[,RZ_NAMES],1,which.max)
	RZONES<-c("FRF", "BRF", "LAG", "RCF", "PRS", "OTH")
	wd$ZONE_CODE<-as.factor(wd$MAX_POS)
	levels(wd$ZONE_CODE)<-RZONES[sort(unique(wd$MAX_POS))]
#	table(wd$ZONE_CODE, wd$MAX_POS)
	wd$MAX_POS<-NULL
}

#set all grid cells with >50% LND to LAND 
#first create a column for Land% - from the 2 fields that might be in the datafile
wd$LND<-0
if("Land_Per" %in% colnames(wd)){
	wd$LND<-wd$Land_Per
} else {
	wd$LND<-wd$RZ_LND_Per
}
if(max(wd$LND)>50){
	##Set anything to Land if Land_Per is > 50%
	wd[which(wd$LND>50),]$DEPTH_BIN<-"LAND"
	wd[which(wd$LND>50),]$HARD_10<-"L"
	wd[which(wd$LND>50),]$HARD_50<-"L"
	levels(wd$ZONE_CODE)<-c(levels(wd$ZONE_CODE), "LND")
	wd[which(wd$LND>50),]$ZONE_CODE<-"LND"
	wd[which(wd$LND>50),]$DEPTH_BIN<-"LAND"

}
wd$LND<-NULL

wd.out<-merge(orig.wd, wd[,c("OBJECTID", "DEPTH_BIN", "HARD_10", "HARD_50", "ZONE_CODE")], by="OBJECTID", all.x=T)  #was previously joining on OBJECTID

# save file 
setwd("X:/GIS/Projects/CommonMaps/02_PostProcess/02_rscriptoutput/20190809")
write.csv(wd.out,file=paste("FISH_FIELDS",gridf, sep="_"), row.names = FALSE)
#write.csv(wd.out,file=paste("IDWx", gridf, sep="_"))

table(wd$ZONE_CODE)
# in next processing step, check OTHER delineations (channel, etc.)