rm(list=ls())
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

#LOAD LIBRARY FUNCTIONS ... 
source("lib/fish_team_functions.R")
source("lib/Islandwide Mean&Variance Functions.R")

#LOAD THE data per SCHEME
load("RAMP_BASICdata_pooled_is_yr.rdata")
x<-data_pooled_is_yr
load("MARI2011data_pooled_is_yr.rdata")
m11<-data_pooled_is_yr
load("MARI2014data_pooled_is_yr.rdata")
m14<-data_pooled_is_yr
load("AS_SANCTUARYdata_pooled_is_yr.rdata")
as<-data_pooled_is_yr
load("TUT10_12data_pooled_is_yr.rdata")
tut<-data_pooled_is_yr

#make 2011 and 2014 mariana structures ONLY have Guam data and drop Guam 2011 and 2014 data from the main df
M11<-m11$Mean[m11$Mean$ISLAND=="Guam",]
M14<-m14$Mean[m14$Mean$ISLAND=="Guam",]
X<-x$Mean[!(x$Mean$ISLAND=="Guam" & x$Mean$ANALYSIS_YEAR %in% c(2011, 2014)),]
AS<-as$Mean[as$Mean$REGION=="SAMOA",]
TUT<-tut$Mean[tut$Mean$ISLAND=="Tutuila",]
Mean<-rbind(X, M11, M14,AS,TUT)

#make 2011 and 2014 mariana structures ONLY have Guam data and drop Guam 2011 and 2014 data from the main df
M11<-m11$PooledSE[m11$PooledSE$ISLAND=="Guam",]
M14<-m14$PooledSE[m14$PooledSE$ISLAND=="Guam",]
X<-x$PooledSE[!(x$PooledSE$ISLAND=="Guam" & x$PooledSE$ANALYSIS_YEAR %in% c(2011, 2014)),]
AS<-as$PooledSE[as$PooledSE$REGION=="SAMOA",]
TUT<-tut$PooledSE[tut$PooledSE$ISLAND=="Tutuila",]
PooledSE<-rbind(X, M11, M14,AS,TUT)

data_pooled_is_yr<-list(Mean, PooledSE)
names(data_pooled_is_yr)<-list("Mean", "PooledSE")

save(data_pooled_is_yr, file="MONREPdata_pooled_is_yr.rdata")

#
# GET ISLAND AND REGIONAL AVERAGES
#means of island scale values
DATA_COLS<-c("PRIMARY", "SECONDARY", "PLANKTIVORE", "PISCIVORE", "TotFish", "HARD_CORAL", "SOFT_CORAL", "MA", "CCA", "TA", "SAND", "CYANO", "OTHER_BENTHIC", "0_20", "20_50", "50_plus", "BSR", "MEAN_SIZE","10_35","35_plus")

data_pooled_is<-aggregate(Mean[,c(DATA_COLS, "TotalArea")], by=Mean[,c("REGION", "ISLAND")], FUN=mean, na.rm = T)#; data_pooled_is
save(data_pooled_is, file="MONREPdata_pooled_is.rdata")


#create structure - then update row by row  #DOING ONLY FOR Forereef
reg_mean<-aggregate(data_pooled_is[,DATA_COLS], by=list(data_pooled_is[,c("REGION")]), FUN=mean); reg_mean
#reg_mean<-subset(reg_mean, reg_mean$REEF_ZONE=="Forereef")
#reg_mean$REEF_ZONE<-NULL
#data_pooled_is<-subset(data_pooled_is, data_pooled_is$REEF_ZONE=="Forereef")
reg_mean<-droplevels(reg_mean); reg_mean
for(i in 3:19)
{
	data_pooled_is$DATA_VAL<-data_pooled_is[,i]
	reg_mean[,i-1]<-by(data_pooled_is, data_pooled_is$REGION, function(x) weighted.mean(x$DATA_VAL, x$TotalArea))	
}
data_pooled_reg<-reg_mean; data_pooled_reg
by(data_pooled_is, data_pooled_is$REGION, function(x) weighted.mean(x$BSR, x$TotalArea))	
save(data_pooled_reg, file="MONREPdata_pooled_reg.rdata")


#### FOREEF DATA  ONLY########
# IDW - This is repetitive of the above, but with some different inputs files ..and with the REEF_ZONE file being sued at some point
# IDW - I am almost sure it would be better to slightly modify the above code to ahve soem flexilivty .. and/or to jsut ahve a different inital section to load and filter the data

rm(list=ls())
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2015_status_report/data/Data Outputs")
#work this out to merge the RAMP_BASIC with the MAR2011 and MAR2014 data
load("RAMP_BASICdata_pooled_is_yr_RZ.rdata")
x<-data_pooled_is_yr
load("MARI2011data_pooled_is_yr_RZ.rdata")
m11<-data_pooled_is_yr
load("MARI2014data_pooled_is_yr_RZ.rdata")
m14<-data_pooled_is_yr
load("AS_SANCTUARYdata_pooled_is_yr_RZ.rdata")
as<-data_pooled_is_yr
load("TUT10_12data_pooled_is_yr_RZ.rdata")
tut<-data_pooled_is_yr


#make 2011 and 2014 mariana structures ONLY have Guam data and drop Guam 2011 and 2014 data from the main df
M11<-m11$Mean[m11$Mean$ISLAND=="Guam",]
M14<-m14$Mean[m14$Mean$ISLAND=="Guam",]
X<-x$Mean[!(x$Mean$ISLAND=="Guam" & x$Mean$ANALYSIS_YEAR %in% c(2011, 2014)),]
AS<-as$Mean[as$Mean$REGION=="SAMOA",]
TUT<-tut$Mean[tut$Mean$ISLAND=="Tutuila",]
Mean<-rbind(X, M11, M14,AS,TUT)

#make 2011 and 2014 mariana structures ONLY have Guam data and drop Guam 2011 and 2014 data from the main df
M11<-m11$PooledSE[m11$PooledSE$ISLAND=="Guam",]
M14<-m14$PooledSE[m14$PooledSE$ISLAND=="Guam",]
X<-x$PooledSE[!(x$PooledSE$ISLAND=="Guam" & x$PooledSE$ANALYSIS_YEAR %in% c(2011, 2014)),]
# ADD AS_SANC
AS<-as$PooledSE[as$PooledSE$REGION=="SAMOA",]
# add tut10-12
TUT<-tut$PooledSE[tut$PooledSE$ISLAND=="Tutuila",]
PooledSE<-rbind(X, M11, M14,AS,TUT)

#rm(data_pooled_is_yr)
data_pooled_is_yr<-list(Mean, PooledSE)
names(data_pooled_is_yr)<-list("Mean", "PooledSE")

save(data_pooled_is_yr, file="MONREPdata_pooled_is_yr_RZ.rdata")

## function to calculate pooled SE
pool_se<-function(se_vals, weights){
  #se_vals<-c(NA,NA,NA,NA)
  #weights<-c(3,1,1,9)
  
  df<-data.frame(se=se_vals, wt=weights)
  df<-df[!is.na(df$se),]
  
  if(dim(df)[1]==0) return(NaN)	
  
  weights<-df$wt/sum(df$wt)  #convert weights to portions
  tmp<-(df$se^2)*(weights^2)
  pooled.se<- sqrt(sum(tmp))
  return(pooled.se)
} #end pool_se

#
# GET ISLAND AND REGIONAL AVERAGES
#means of island scale values
DATA_COLS<-c("PRIMARY", "SECONDARY", "PLANKTIVORE", "PISCIVORE", "TotFish", "HARD_CORAL", "SOFT_CORAL", "MA", "CCA", "TA", "SAND", "CYANO", "OTHER_BENTHIC", "0_20", "20_50", "50_plus", "BSR","MEAN_SIZE","10_35","35_plus")

MeanIs<-aggregate(Mean[,c(DATA_COLS, "TotalArea")], by=Mean[,c("REGION", "ISLAND", "REEF_ZONE")], FUN=mean, na.rm = T); MeanIs
MeanIs$N<-0
SEIs<-MeanIs #create SE structure
for(i in 1:dim(SEIs)[1])
{
  base_d<-PooledSE[PooledSE$ISLAND==SEIs[i,]$ISLAND & PooledSE$REEF_ZONE==SEIs[i,]$REEF_ZONE,]
  SEIs[i,]$N<-MeanIs[i,]$N<-sum(base_d$N)
  SEIs[i, DATA_COLS]<-apply(base_d[,DATA_COLS],2, function(x) pool_se(x,rep(1,length(x))))
}

data_pooled_is<-list(MeanIs, SEIs)
names(data_pooled_is)<-list("Mean", "PooledSE")
save(data_pooled_is, file="MONREPdata_pooled_is_RZ.rdata")

data_pooled_is<-aggregate(Mean[,c(DATA_COLS, "TotalArea")], by=Mean[,c("REGION", "ISLAND", "REEF_ZONE")], FUN=mean, na.rm = T); data_pooled_is # comment this out to run FRF code at the bottom.


#create structure - then update row by row  #DOING ONLY FOR Forereef
frf_mean<-subset(data_pooled_is$Mean, data_pooled_is$Mean$REEF_ZONE=="Forereef" & data_pooled_is$Mean$ISLAND !="South Bank")
frf_se<-subset(data_pooled_is$PooledSE, data_pooled_is$PooledSE$REEF_ZONE=="Forereef" & data_pooled_is$PooledSE$ISLAND !="South Bank")

#remove South Bank from here! We do not want to include in regional averages

reg_mean<-aggregate(frf_mean[,DATA_COLS], by=frf_mean[,c("REGION", "REEF_ZONE")], FUN=mean); reg_mean  #building the data structure
reg_mean$N<-0
reg_se<-reg_mean #make SE structure
for(i in 1:dim(reg_se)[1])
{
  base_mean<-frf_mean[frf_mean$REGION==reg_mean[i,]$REGION,]
  base_se<-frf_se[frf_se$REGION==reg_se[i,]$REGION,]
  reg_se[i,]$N<-reg_mean[i,]$N<-sum(base_mean$N)
  
  #weight by island forereef area
  reg_se[i, DATA_COLS]<-apply(base_se[,DATA_COLS],2, function(x) pool_se(x,base_se$TotalArea)) 
  reg_mean[i,DATA_COLS]<-apply(base_mean[,DATA_COLS],2, function(x) weighted.mean(x, base_mean$TotalArea)) 
  #weight each island equally
  #reg_se[i, DATA_COLS]<-apply(base_se[,DATA_COLS],2, function(x) pool_se(x, rep(1,dim(base_se)[1])))
  #reg_mean[i,DATA_COLS]<-apply(base_mean[,DATA_COLS],2, function(x) mean(x)) 
}

data_pooled_reg<-list(reg_mean, reg_se)
names(data_pooled_reg)<-list("Mean", "PooledSE")
save(data_pooled_reg, file="MONREPdata_pooled_reg_FRF.rdata")






