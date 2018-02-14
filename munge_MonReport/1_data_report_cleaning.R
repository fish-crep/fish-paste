rm(list=ls())
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report")

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/core_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/fish_team_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/Git/fish-paste/lib/Islandwide Mean&Variance Functions.R")
source("Indicator_functions.R")   #NOT SURE WHAT THIS IS ... AND WHY IT IS NOT IN FISH PASTE?

# get strata and sectors data data - NB - the data in the raw file should be checked and updated
sectors<-read.csv("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
# load site master to merge with sector names
site_master<-read.csv("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/fish-paste/data/SITE MASTER.csv")
site_master$SITE<-SiteNumLeadingZeros(site_master$SITE)

## LOAD AND CLEAN fish data
load("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/fish-paste/data/ALL_REA_FISH_RAW.rdata")
x<-df

# HOUSEKEEPING ------------------------------------------------------------
# clean up the data to only fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION" , "REGION_NAME", "EXCLUDE_FLAG", "TRAINING_YN",
"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH", 
"HARD_CORAL", "MA",  "TA",  "CCA",  "SAND",  "SOFT_CORAL", "CLAM" , "SPONGE", "CORALLIMORPH", "CYANO", "TUNICATE", "ZOANTHID" , "OTHER", 
"SPECIES", "COUNT", "SIZE_", "OBS_TYPE", 
"COMPLEXITY", "SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150", "MAX_HEIGHT", 
"SCIENTIFIC_NAME",  "TAXONNAME", "COMMONNAME", "GENUS", "FAMILY" , "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

x$SITE<-SiteNumLeadingZeros(x$SITE)

x[is.na(x$TRAINING_YN),]$TRAINING_YN<-FALSE   # Training falg of NA is equivalent to a FALSE .. as none of the odler data was 'training data'
x<-subset(x, x$TRAINING_YN==FALSE)
# by default, remove sites with EXCLUDE_FLAG set to TRUE
x<-subset(x, x$EXCLUDE_FLAG==0, drop=TRUE)
x<-subset(x, x$METHOD %in% c("nSPC"), drop=TRUE)
x<-subset(x, x$OBS_YEAR >2008, drop=TRUE)
x<-subset(x, x$OBS_TYPE %in% c("U","I","N"))

#add SEC_NAME to x  
# this would be better if SECTOR field in database was up to date properly .. rather than merge with the site_Sectors spreadsheet
x<-merge(x, site_master[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE)

#CHECK THAT all ANALYSIS_SEC are present in the site_master file)
idw<-x[is.na(x$ANALYSIS_SEC)  & x$METHOD=="nSPC", c("REGION", "SITE","OBS_YEAR", "METHOD"),]
if(dim(idw)[1]>0) {cat("nSPC sites with MISSING ANALYSIS_SEC")}   # should be 0

#for ones that are missing, set it to ISLAND
no_secs<-is.na(x$ANALYSIS_SEC)
tmp<-as.character(x$ANALYSIS_SEC)
tmp[no_secs]<-as.character(x[no_secs,]$ISLAND)
x$ANALYSIS_SEC<-tmp

############################################################################################
###### new section .. where there is substrate_height data, work out average height && ave_height_variability so that we get standardized complexity metrics (mean hieght, mean height variability, max-height) 
sh_out<-CalcMeanSHMeanSHDiff(x)
x$MEAN_SH<-sh_out[[1]]
x$MEAN_SH_DIFF<-sh_out[[2]]

# remove the component SUBSTRATE_HEIGHT fields
x<-x[, setdiff(names(x),c("SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150"))]
############################################################################################
x<-droplevels(x)
# nine<-x[x$OBS_YEAR=="2009",]
# nine<-nine[nine$REGION=="MHI",]

#######################
## CLEAN UP NAs #######
#######################
tmp.lev<-levels(x$HABITAT_CODE); head(tmp.lev)
levels(x$HABITAT_CODE)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$SCIENTIFIC_NAME); head(tmp.lev)
levels(x$SCIENTIFIC_NAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$COMMONNAME); head(tmp.lev)
levels(x$COMMONNAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$GENUS); head(tmp.lev)
levels(x$GENUS)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$FAMILY); head(tmp.lev)
levels(x$FAMILY)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$COMMONFAMILYALL); head(tmp.lev)
levels(x$COMMONFAMILYALL)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$TROPHIC_MONREP); head(tmp.lev)
levels(x$TROPHIC_MONREP)<-c(tmp.lev, "UNKNOWN")

x[is.na(x$HABITAT_CODE),"HABITAT_CODE"]<-"UNKNOWN"
x[is.na(x$SCIENTIFIC_NAME),"SCIENTIFIC_NAME"]<-"UNKNOWN"
x[is.na(x$COMMONNAME),"COMMONNAME"]<-"UNKNOWN"
x[is.na(x$GENUS),"GENUS"]<-"UNKNOWN"
x[is.na(x$FAMILY),"FAMILY"]<-"UNKNOWN"
x[is.na(x$COMMONFAMILYALL),"COMMONFAMILYALL"]<-"UNKNOWN"
x[is.na(x$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"

x[is.na(x$COUNT),]$COUNT<-0
x[is.na(x$SIZE_),]$SIZE_<-0
###x[is.na(x$LMAX),]$LMAX<-999

## separate out the north and south marianas 
levels(x$REGION)<-c(levels(x$REGION), "S.MARIAN", "N.MARIAN")
x[x$ISLAND %in% c("Guam", "Rota", "Aguijan", "Tinian", "Saipan"),]$REGION<-"S.MARIAN"
x[x$ISLAND %in% c("Alamagan","Guguan","Sarigan","Pagan", "Agrihan", "Asuncion", "Maug", "Farallon de Pajaros"),]$REGION<-"N.MARIAN"

sectors[sectors$ISLAND %in% c("Guam", "Rota", "Aguijan", "Tinian", "Saipan"),]$REGION<-"S.MARIAN"
sectors[sectors$ISLAND %in% c("Alamagan","Guguan","Sarigan","Pagan", "Agrihan", "Asuncion", "Maug", "Farallon de Pajaros"),]$REGION<-"N.MARIAN"
sectors<-droplevels(sectors)

# WORKING WITH POOLING READY DATA FROM HERE ON  
########################################################################################################################
wd<-droplevels(x)

#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

#NOTE THAT BENTHOS DOES NOT ALWAYS SUM TO 100% .. I THINK BECAUSE OF ERRORS IN THE ORIGINAL DATA ENTERED INTO THE DATABASE. NEW CODE BELOW IS AN ATTEMPT TO FIX THAT
# Go through all surveys checking for situation where some reps have NAs in a particular BENTHIC_FIELDS, but other records have non-zeros - in that situation, we were recording a field but one other diver left it balnk - those should be zeros not NAs
# this is something that should really be fixed in the database rather than here (as its an error at time of data entry)
BENTHIC_FIELDS<-c("HARD_CORAL", "SOFT_CORAL", "MA", "CCA", "TA", "SAND", "CYANO", "CLAM", "CORALLIMORPH", "ZOANTHID", "TUNICATE", "SPONGE", "OTHER")
UNIQUE_ROUND<-c("REGION", "OBS_YEAR", "METHOD")
round_table<-Aggregate_InputTable(wd, UNIQUE_ROUND)
wd$countBD<-apply(wd[,BENTHIC_FIELDS], 1, function(xx) length(which(!is.na(xx))))  
for(i in 1:dim(round_table)[1])
{
	if(round_table[i,"METHOD"]=="nSPC")
	{
		tmp_data<-wd[wd$OBS_YEAR==round_table[i,"OBS_YEAR"] & wd$METHOD==round_table[i,"METHOD"] & wd$REGION==round_table[i,"REGION"],]

		#go through BENTHIC_FIELDS, checking whether there are some NAs and some data values
		for(j in 1:length(BENTHIC_FIELDS))
		{
			## IF there are both non NAs and NAs
			if(length(tmp_data[!is.na(tmp_data[,BENTHIC_FIELDS[j]]),BENTHIC_FIELDS[j]]) > 0 
			        & length(tmp_data[is.na(tmp_data[,BENTHIC_FIELDS[j]]),BENTHIC_FIELDS[j]]) > 0) 
			{
				#set all NAs of that field to 0
				tmp_data[is.na(tmp_data[,BENTHIC_FIELDS[j]]),BENTHIC_FIELDS[j]]<-0	

				#now rewrite the benthic fields with NAs converted to zeros
				wd[wd$OBS_YEAR==round_table[i,"OBS_YEAR"] & wd$METHOD==round_table[i,"METHOD"] & wd$REGION==round_table[i,"REGION"],BENTHIC_FIELDS[j]]<-tmp_data[,BENTHIC_FIELDS[j]]
			}
		}
	
	}
}
# now reset zeros to NAs for all records where there was NO benthic data at all
wd[wd$countBD==0,BENTHIC_FIELDS]<-NA

# OUTPUT raw working data (used to create appendix species list) -------------------------
#save(wd, file="E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs/raw_working_data.rdata")

#get base survey info, calculate average depth+complexity+so on
SURVEY_INFO<-c("OBS_YEAR", "REGION", "REGION_NAME", "ISLAND", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_SCHEME", "SEC_NAME", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE", "SITEVISITID", "METHOD")
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)
island_table<-Aggregate_InputTable(wd, c("REGION","ISLAND"))

OTHER_BENTHIC<-c("CLAM", "CORALLIMORPH", "ZOANTHID", "TUNICATE", "SPONGE", "TA", "CYANO", "SOFT_CORAL", "OTHER")
wd$OTHER_BENTHIC<-rowSums(wd[,OTHER_BENTHIC],na.rm=T)
SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL", "MA", "CCA", "SAND", "OTHER_BENTHIC", "MEAN_SH", "MEAN_SH_DIFF", "MAX_HEIGHT")
survey_est_benthos<-Calc_Site_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)   #Calc_Site_nSurveysArea deals better with situations where one REP has benthic data and other doesnt. 
surveys<-merge(survey_table, survey_est_benthos, by=UNIQUE_SURVEY)

#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)

# SELECT SUMMARY METRICS --------------------------------------------------
##load("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs/raw_working_data.rdata")
# calc pooled site biomass by consumer group, species, and common family
r1<-Calc_Site_Bio(wd, "TROPHIC_MONREP"); trophic.cols<-levels(species_table$TROPHIC_MONREP)
r4<-Calc_Site_Bio_By_SizeClass(wd, c(0,20,50,Inf)); size.cols<-names(r4)[3:dim(r4)[2]]
r6<-Calc_Site_MeanLength(wd, min_size=0.3)   # Calcaulte mean fish length for species > 30% of LMax
r6[is.na(r6)]<-10      						 # default min size ot 10cm, so we do not have zero size at a site
wdp<-wd; wdp[wdp$FAMILY != "Scaridae",]$COUNT<-0	 #Version of wsd with size (therefore biomass) set to 0 for non parrots
r7<-Calc_Site_Bio_By_SizeClass(wdp, c(10,30,Inf))    #This is parrotfish biomass by Size Class

wsd<-merge(surveys,r1,by=UNIQUE_SURVEY)
wsd$TotFish<-rowSums(wsd[,trophic.cols])
wsd<-merge(wsd, r4, by=UNIQUE_SURVEY)
names(wsd)[match(c("[0,20]", "(20,50]","(50,Inf]"),names(wsd))] <- c("0_20", "20_50", "50_plus")

#### 2016: NEW BSR CALCULATION: We don't estimate turf cover anymore, so instead, subtract hard + sand cover for ratio calculation
wsd$BSR<-(wsd$HARD_CORAL+wsd$CCA)/(100-(wsd$HARD_CORAL + wsd$CCA+wsd$SAND))
wsd<-merge(wsd,r6,by=UNIQUE_SURVEY)
wsd<-merge(wsd,r7,by=UNIQUE_SURVEY, all.x=T)
names(wsd)[match(c("[10,30]", "(30,Inf]" ),names(wsd))] <- c("P10_30", "P30_plus")
data.cols<-c(trophic.cols, "TotFish", SURVEY_SITE_DATA, "0_20", "20_50", "50_plus", "BSR", "MEAN_SIZE", "P10_30", "P30_plus")

###### NA values in parrot sizes ####
is.na(wsd$P30_plus)
##km<-wsd[is.na(wsd$P30_plus),]
##ksm<-wd[wd$SITEVISITID==7339,]
# all sites have no parrots, set to zero
wsd[is.na(wsd$P30_plus),]$P30_plus<-0
wsd[is.na(wsd$P10_30),]$P10_30<-0

# OUTPUT working_site_data (appendix 1) -----------------------------------
head(wsd)
save(wsd, file="working_site_data.rdata")

# save wsd UNCAPPED
##wsd.uncap<-wsd
##wsd<-wsd.uncap

### CAP DATA TO SOMEWHERE AROUND 97.5% PERCENTILE
for(i in 1:(length(data.cols)))
{
	cat(data.cols[i])
	cat(" ")
	cat(round(quantile(wsd[,data.cols[i]], c(0.9,0.95,0.975, 0.99), na.rm = T),1))
	cat("      ")
}
#
#   REALLY IMPORTANT THAT THESE CAPPING VALUES ARE SET APPROPRIATELY FOR EACH REPORT .. IE DO NOT JSUT USE THE SAME VALUES EACH ITERATION
#
wsd[wsd$TotFish>450,]$TotFish<-450
wsd[wsd$PISCIVORE>300,]$PISCIVORE<-300
wsd[wsd$PLANKTIVORE>100,]$PLANKTIVORE<-100 
wsd[wsd$PRIMARY>80,]$PRIMARY<-80 
wsd[wsd$SECONDARY>45,]$SECONDARY<-45 
wsd[wsd$"0_20">65,]$"0_20"<-65
wsd[wsd$"20_50">130,]$"20_50"<-130
wsd[wsd$"50_plus">350,]$"50_plus"<-350
wsd[wsd$MEAN_SIZE>30,]$MEAN_SIZE<-30
wsd[wsd$P10_30>20,]$P10_30<-20
wsd[wsd$P30_plus>25,]$P30_plus<-25
#cap BSR too ... can be infinite in situations where divers record TA and MA as 0 (rare, but does happen)
wsd[wsd$BSR>10 & !is.na(wsd$BSR),]$BSR<-10   #Changed this cap to 10 from 25

# OUTPUT (not in Adel's script that I can see, but I think we should include) -----------------------------------
save(wsd, file="working site data CAPPED.rdata")
##write.csv(wsd,file="E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs/working site data CAPPED.csv")

####################################################################################################################################################################
#
#     CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS, EG SET BACKREEF DEPTH_ZONE TO ALL, CREATE THE "SGA" LOCATION
#
####################################################################################################################################################################

## check wwhether we have ISLANDS that arent in the sectors file
setdiff(unique(wsd$ISLAND),unique(sectors$ISLAND))

#set all Backreef to a single DEPTH_ZONE ("All") 
levels(wsd$DEPTH_BIN)<-c(levels(wsd$DEPTH_BIN), "All")
wsd[wsd$REEF_ZONE=="Backreef",]$DEPTH_BIN<-"All"
sectors[sectors$REEF_ZONE=="Backreef",]$DEPTH_BIN<-"All"

wsd$DEPTH_BIN<-as.character(wsd$DEPTH_BIN)# won't change value to "All" if it is a factor
wsd[wsd$ISLAND=="Rose" & wsd$REEF_ZONE=="Lagoon",]$DEPTH_BIN<-"All"
sectors[sectors$ISLAND=="Rose" & sectors$REEF_ZONE=="Lagoon",]$DEPTH_BIN<-"All"
wsd$DEPTH_BIN<-as.factor(wsd$DEPTH_BIN)# change back to factor

wsd$STRATA<-paste(substring(wsd$REEF_ZONE,1,1), substring(wsd$DEPTH_BIN,1,1), sep="")
sectors$STRATA<-paste(substring(sectors$REEF_ZONE,1,1), substring(sectors$DEPTH_BIN,1,1), sep="")

## TREAT GUGUAN, ALAMAGAN, SARIGAN AS ONE ISLAND  (REALLY ONE BASE REPORTING UNIT .. BUT SIMPLER TO STICK TO 'ISLAND')
SGA<-c("Guguan", "Alamagan", "Sarigan")
levels(wsd$ISLAND)<-c(levels(wsd$ISLAND), "AGS")
wsd[wsd$ISLAND %in% SGA,]$ISLAND<-"AGS"
sectors[sectors$ISLAND %in% SGA,]$ISLAND<-"AGS"

levels(wsd$ANALYSIS_YEAR)<-c(levels(wsd$ANALYSIS_YEAR), "2016on")
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR==2016,]$ANALYSIS_YEAR<-"2016on"
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2010,2012),]$ANALYSIS_YEAR<-"2010-12"
wsd[wsd$REGION %in% c("MHI", "NWHI") & wsd$OBS_YEAR %in% seq(2013,2015),]$ANALYSIS_YEAR<-"2013-15"

## generate a complete list of all ANALYSIS STRATA and their size
SCHEMES<-c("RAMP_BASIC", "MARI2011", "MARI2014", "TUT10_12", "AS_SANCTUARY")
for(i in 1:length(SCHEMES)){
	tmp<-aggregate(sectors$AREA_HA, sectors[,c(SCHEMES[i], "STRATA")], sum)
	tmp$SCHEME<-SCHEMES[i]
	names(tmp)<-c("ANALYSIS_SEC", "STRATA", "AREA_HA", "ANALYSIS_SCHEME")
	if(i==1){
		st<-tmp
	} else {
		st<-rbind(st, tmp)
	}	
}
cast(st, ANALYSIS_SEC ~ ANALYSIS_SCHEME, value="AREA_HA", sum)
wsd<-merge(wsd, st, by=c("ANALYSIS_SCHEME", "ANALYSIS_SEC", "STRATA"), all.x=T)
#check if some are missing an AREA_HA .. which means that they didnt get into the stratification scheme properly
unique(wsd[is.na(wsd$AREA_HA), c("ISLAND", "ANALYSIS_SEC", "SEC_NAME", "OBS_YEAR", "ANALYSIS_YEAR", "ANALYSIS_SCHEME", "STRATA")])

#NOW CHECK HOW MANY REPS WE HAVE PER STRATA
a<-cast(wsd, REGION + ANALYSIS_SCHEME + ISLAND + ANALYSIS_SEC + ANALYSIS_YEAR ~ STRATA, value="AREA_HA", length); a

# OUTPUT sites per years (appendix 3) -------------------------------------
save(a, file="sites_year_reef_zone_depth_bin.rdata") ## use this for table in appendix 3 - see appendices R file


####################################################################################################################################################################
#
#     POOL WSD (WORKING SITE DATA TO STRATA THEN TO HIGHER LEVELS
##
###################################################################################################################################################################

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION", "ISLAND", "ANALYSIS_SEC", "REEF_ZONE", "STRATA")    
ADDITIONAL_POOLING_BY<-c("METHOD", "ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)

#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA"))
#save(dps,file="tmp REA per strata.RData")
head(dps$Mean)

###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]

# e.g. SAVE BY ISLAND PER YEAR
OUTPUT_LEVEL<-c("REGION", "ISLAND", "REEF_ZONE", "ANALYSIS_YEAR") 
dp<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
save(dp, file="MONREPdata_pooled_is_yr_RZ.Rdata")

# e.g. SAVE BY REGION PER YEAR
OUTPUT_LEVEL<-c("REGION", "ANALYSIS_YEAR") 
dpR<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA")
save(dpR, file="MONREPdata_pooled_reg.rdata")


# GET ISLAND AND REGIONAL AVERAGES
## function to calculate pooled SE
pool_se<-function(se_vals, weights){
  df<-data.frame(se=se_vals, wt=weights)
  df<-df[!is.na(df$se),]
  if(dim(df)[1]==0) return(NaN)	
  
  weights<-df$wt/sum(df$wt)  #convert weights to portions
  tmp<-(df$se^2)*(weights^2)
  pooled.se<- sqrt(sum(tmp))
  return(pooled.se)
} #end pool_se

m<-dp$Mean; s<-dp$PooledSE
DATA_COLS<-c("PRIMARY", "SECONDARY", "PLANKTIVORE", "PISCIVORE", "TotFish", "HARD_CORAL", "MA", "CCA", "SAND", "OTHER_BENTHIC", "0_20", "20_50", "50_plus", "BSR","MEAN_SIZE","P10_30","P30_plus")
MeanIs<-aggregate(m[,c(DATA_COLS, "TOT_AREA_WT")], by=m[,c("REGION", "ISLAND", "REEF_ZONE")], FUN=mean, na.rm = T); MeanIs
MeanIs$N<-0
SEIs<-MeanIs #create SE structure
for(i in 1:dim(SEIs)[1])
{
  base_d<-s[s$ISLAND==SEIs[i,]$ISLAND & s$REEF_ZONE==SEIs[i,]$REEF_ZONE,]
  SEIs[i,]$N<-MeanIs[i,]$N<-sum(base_d$N)
  SEIs[i, DATA_COLS]<-apply(base_d[,DATA_COLS],2, function(x) pool_se(x,rep(1,length(x))))
}

dpI<-list(MeanIs, SEIs)
names(dpI)<-list("Mean", "PooledSE")
save(dpI, file="MONREPdata_pooled_is_RZ.rdata")

#Now get regional average for Forereef only
f_mean<-subset(dpI$Mean, dpI$Mean$REEF_ZONE =="Forereef" & dpI$Mean$ISLAND !="South Bank")
f_se<-subset(dpI$PooledSE, dpI$PooledSE$REEF_ZONE=="Forereef" & dpI$PooledSE$ISLAND !="South Bank")

reg_mean<-aggregate(f_mean[,DATA_COLS], by=f_mean[,c("REGION", "REEF_ZONE")], FUN=mean); reg_mean  #building the data structure
reg_mean$N<-0
reg_se<-reg_mean #make SE structure
for(i in 1:dim(reg_se)[1])
{
  base_mean<-f_mean[f_mean$REGION==reg_mean[i,]$REGION,]
  base_se<-f_se[f_se$REGION==reg_se[i,]$REGION,]
  reg_se[i,]$N<-reg_mean[i,]$N<-sum(base_mean$N)
  
  #weight by island forereef area
  reg_se[i, DATA_COLS]<-apply(base_se[,DATA_COLS],2, function(x) pool_se(x,base_se$TOT_AREA_WT)) 
  reg_mean[i,DATA_COLS]<-apply(base_mean[,DATA_COLS],2, function(x) weighted.mean(x, base_mean$TOT_AREA_WT)) 
}

dpR<-list(reg_mean, reg_se)
names(dpR)<-list("Mean", "PooledSE")
save(dpR, file="MONREPdata_pooled_reg_FRF.rdata")







