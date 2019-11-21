#### Add fish fields to common map grrds  #####
rm(list=ls(all=TRUE))
setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Grids")

# bring in common csv file, change file name for each island
### not all islands have hard/soft information or reef zones

#GRIDFILES<-c("ros_grids_50m_table.csv", "swa_grids_50m_table.csv", "ofu_grids_50m_table.csv", "tut_grids_50m_table.csv", "tau_grids_50m_table.csv", "how_grids_50m_table.csv", "bak_50m_grids_table.csv", "kin_50m_grids_table.csv", "joh_50m_grids_table.csv", "grids_50m_grids_table.csv", "jar_50m_grids_table.csv")
GRIDFILES<-c("ros_50m_grids_table.csv", "swa_50m_grids_table.csv", "ofu_50m_grids_table.csv", "tut_50m_grids_table.csv", "tau_50m_grids_table.csv", "how_50m_grids_table.csv", "bak_50m_grids_table.csv", "kin_50m_grids_table.csv", "joh_50m_grids_table.csv", "pal_50m_grids_table.csv", "jar_50m_grids_table.csv", "gua_grids_50m_table.csv")
INDEX<-4
gridf<-GRIDFILES[INDEX]

wd<-orig.wd<-read.csv(gridf)
head(wd)
summary(wd)

# create a field for depth bin
wd$DEPTH_BIN<-"MISSING"
wd[is.na(wd$Bty_Mean),]$DEPTH_BIN<-"UNKN"
# label depth ranges on mean bathy value 
wd[which(wd$Bty_Mean > -1),]$DEPTH_BIN<-"ONEM" # not samplable
wd[which(wd$Bty_Mean >= -6 & wd$Bty_Mean < -1),]$DEPTH_BIN<-"SHAL"
wd[which(wd$Bty_Mean >= -18 & wd$Bty_Mean < -6),]$DEPTH_BIN<-"MIDD"
wd[which(wd$Bty_Mean >= -30 & wd$Bty_Mean < -18),]$DEPTH_BIN<-"DEEP"
wd[which(wd$Bty_Mean < -30),]$DEPTH_BIN<-"MESO"
# not all grids have this, but some have values of 1
#wd[which(wd$Bty_Mean >= 0),]$DEPTH_BIN<-"LAND"    #Possibly set to SHAL! (as default) .. as these are right next to land!
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
if(!INDEX %in% c(101, 108, 109, 110)){
	# RZ_NAMES<-c("RZ_FRF_Per", "RZ_BRF_Per", "RZ_LAG_Per", "RZ_RCF_Per", "RZ_PRS_Per", "RZ_LND_Per", "RZ_UNK_Per")
	# wd$MAX_POS<-apply(wd[,RZ_NAMES],1,which.max)
	# RZONES<-c("FRF", "BRF", "LAG", "RCF", "PRS", "LND", "UNK")
#Modified to not set to LND here ... can only do that below (if LND is > 50) .. so just picking the dominant reef type
	RZ_NAMES<-c("RZ_FRF_Per", "RZ_BRF_Per", "RZ_LAG_Per", "RZ_RCF_Per", "RZ_PRS_Per", "RZ_UNK_Per")
	wd$MAX_POS<-apply(wd[,RZ_NAMES],1,which.max)
	RZONES<-c("FRF", "BRF", "LAG", "RCF", "PRS", "UNK")
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

wd.out<-merge(orig.wd, wd[,c("FID", "DEPTH_BIN", "HARD_10", "HARD_50", "ZONE_CODE")], by="FID", all.x=T)  #was previously joining on OBJECTID

# save file 
write.csv(wd.out,file=paste("IDW", gridf, sep="_"))
table(wd$ZONE_CODE)
