rm(list=ls())
library(dplyr)
source("D:/CRED/fish_team_R/fish-paste/lib/core_functions.R")
source("D:/CRED/fish_team_R/fish-paste/lib/fish_team_functions.R")
source("D:/CRED/fish_team_R/fish-paste/lib/Islandwide Mean&Variance Functions.R")

setwd("D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/data/Data Outputs")
library(reshape)

##### Table 2 in report body ######
load("D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/data/Data Outputs/MONREPdata_pooled_reg_FRF.rdata")
ref<-as.data.frame(dpR) # forereef only reference
# get relevant columns with mean and SE
test<-ref[,c("Mean.REGION","Mean.N","Mean.TotFish","PooledSE.TotFish","Mean.PISCIVORE","PooledSE.PISCIVORE","Mean.SECONDARY","PooledSE.SECONDARY","Mean.PRIMARY","PooledSE.PRIMARY","Mean.PLANKTIVORE","PooledSE.PLANKTIVORE","Mean.0_20","PooledSE.0_20","Mean.20_50","PooledSE.20_50","Mean.50_plus","PooledSE.50_plus")]
# round to 1 decimal place
temp<-round(test[,c(3:18)],digits=1)
test<-cbind(test$Mean.REGION,test$Mean.N,temp)
# rename region
names(test)[1]<-"Region"
names(test)[2]<-"Sites"

# combine mean and SE in one column for each metric
test$All_fishes<-paste(test$Mean.TotFish," (",test$PooledSE.TotFish,")",sep="")
test$Piscivores<-paste(test$Mean.PISCIVORE," (",test$PooledSE.PISCIVORE,")",sep="")
test$Sec_consumers<-paste(test$Mean.SECONDARY," (",test$PooledSE.SECONDARY,")",sep="")
test$Pri_consumers<-paste(test$Mean.PRIMARY," (",test$PooledSE.PRIMARY,")",sep="")
test$Planktivores<-paste(test$Mean.PLANKTIVORE," (",test$PooledSE.PLANKTIVORE,")",sep="")
test$small_cm_TL<-paste(test$Mean.0_20," (",test$PooledSE.0_20,")",sep="")
test$mid_cm_TL<-paste(test$Mean.20_50," (",test$PooledSE.20_50,")",sep="")
test$great_cm_TL<-paste(test$Mean.50_plus," (",test$PooledSE.50_plus,")",sep="")

# just keep relevant columns
colnames(test)
teste<-test[,c(1,2,19:26)]
head(teste)
tests<-rbind(teste[c(2,1,6,5,3,4),])

write.csv(tests,file="D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/tables/table_2.csv")


### appendices
# appendix 2: All surveys per region per year and method used -----------------
library(plyr)
library(dplyr)
library(tidyr)
load("working_site_data.rdata")
# add '1' to each site so that can be summed for sites by region, year, and method
wsd$N<-1
# use ddply to make a pivot table
site<-ddply(wsd, .(REGION,OBS_YEAR,METHOD),summarize,"n_sites"=sum(N))
# cast table to be region by year, just need from 2009 on...
year<-cast(site,REGION~OBS_YEAR)
# get regions in order: N. Mariana, S Mariana, MHI,NWHI,PRIMNM,Am. Samoa
year
year<-rbind(year[c(6,5,1,2,3,4),])
# this gives NA values for some years, replace that with a dash
year[is.na(year)] <-"-"
#save file
write.csv(year,"D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/tables/appendix_surveys_per_method_per_region_per_year.csv")

# appendix 4: strata sector by year ---------------------------------------
# these show sites at the level they were pooled for analysis. Backreef and ROSE lagoon sites are all pooled. Drop sectors that have 1 site, these were dropped for analysis
library(reshape)
getwd()
load("sites_year_reef_zone_depth_bin.Rdata") # loads as 'a'
wd<-a
head(wd)
# get 2019 sites
wd<-wd[wd$ANALYSIS_YEAR=="2019",]
write.csv(wd,file="D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/tables/sites_year_reef_zone_depth_bin2019.csv")
# wd$Forereef<-(wd$FD+wd$FM+wd$FS)
# wd$Lagoon<-(wd$LD+wd$LM+wd$LS) # should equal LA = LAGOON ALL?
# wd$protected_slope<-(wd$PD+wd$PM+wd$PS)
# wd$Backreef<-wd$BA
# wd$all_sites<-(wd$Forereef+wd$Lagoon+wd$LA+wd$protected_slope+wd$Backreef)

# get relevant columns for appendix
colnames(wd)
wda<-wd[,c("REGION","ISLAND","SEC_NAME","BA","FD","FM","FS","LA","LD","LM","LS","PD","PM","PS")]
#rename fields
colnames(wda)[1]<-"Region"
colnames(wda)[2]<-"Island"
colnames(wda)[3]<-"Sector"
colnames(wda)[4]<-"Backreef"
colnames(wda)[5]<-"Forereef-D"
colnames(wda)[6]<-"Forereef-M"
colnames(wda)[7]<-"Forereef-S"
colnames(wda)[8]<-"Lagoon"
colnames(wda)[9]<-"Lagoon-D"
colnames(wda)[10]<-"Lagoon-M"
colnames(wda)[11]<-"Lagoon-S"
colnames(wda)[12]<-"Protected Slope-D"
colnames(wda)[13]<-"Protected Slope-M"
colnames(wda)[14]<-"Protected Slope-S"

# remove strata with zero sites
summary(wda)
wdb<-wda[,c("Region","Island","Sector","Forereef-D","Forereef-M","Forereef-S")]

write.csv(wdb,file="D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/tables/appendix_4.csv")

# appendix 5: diver vs diver comparisons ----------------------------------
getwd()
load("raw_working_data.rdata")

## need to read in the species_table from Fish Base
#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)

## using Calc_REP functions (from fish_team_functions) to get richness and biomass estimates per replicate....
r1<-Calc_REP_Bio(wd, "FAMILY"); family.cols<-names(r1)[7:dim(r1)[2]]
# naming difference for unknown.. quick temp fix...drop unknown because we can't compare those?
family.cols # where is UNKNOWN?
#family.cols<-family.cols[-68] ## drop level UNKNOWN
r1$TotFish<-rowSums(r1[,family.cols])

r2<-Calc_REP_Species_Richness(wd)

UNIQUE_SURVEY<-c("SITE", "SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")
SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL")

r3<-Calc_REP_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)

COMPARE_ON<-c("SITEVISITID", "SITE", "REP", "REPLICATEID")
compdata<-merge(r1[,c(COMPARE_ON, "TotFish")], r2[,c(COMPARE_ON, "SPECIESRICHNESS")], by=COMPARE_ON, all.x=T)
compdata<-merge(compdata[,c(COMPARE_ON, "TotFish", "SPECIESRICHNESS")], r3[,c(COMPARE_ON, "HARD_CORAL")], by=COMPARE_ON, all.x=T)


## get year, region and island data
a<-unique(wd[,c("SITE","SITEVISITID","OBS_YEAR","ISLAND","REGION","REPLICATEID","DIVER","ANALYSIS_YEAR")])

test<-merge(compdata, a, by="REPLICATEID", all.x=T) ## this collates year, region etc. with the indiv diver ests per rep
compdata<-test

## need to rename some of the cols after the merge
names(compdata)<-c("REPLICATEID","SITEVISITID","SITE","REP","TotFish","SPECIESRICHNESS","HARD_CORAL",
                   "SITE.y","SITEVISITID.y","OBS_YEAR","ISLAND","REGION","DIVER","ANALYSIS_YEAR")


# set wd
setwd("D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/figures/appendix")
## divervsdiver4 - creates an anonymous and named version of diver comparisons for totfish, richness and coral estimates 
##- need to look at the range of the data per year region to tweak the x axis range
dataMHI<-compdata[compdata$REGION=="MHI"&compdata$ANALYSIS_YEAR==2019,]
summary(dataMHI)# look at max value for tot fish and adjust x_range below - may need to adjust for extreme outliers

## to create a multigraph with total fish, richness and coral estimates run divervsdiver3
divervsdiver4(data=compdata, year = "2019", region="MHI", x_range= 150)


# appendix 6: random stratified sites surveyed per region / island -------------####
# last year's table is in tables folder from 2017, I just added sites from excel from current year

#OR: 
load("D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/data/Data Outputs/clean_working_site_data_used_in_higher_pooling_for_report.Rdata")
site.data.nspc<-wsd.uncap
# @#$^^$% SOUTH BANK!!
site.data.nspc<-site.data.nspc[site.data.nspc$ISLAND !="South Bank",]
site.data.nspc<-drop.levels(site.data.nspc)
#subset for "REGION","OBS_YEAR","ISLAND","ANALYSIS_YEAR","SITE"
colnames(site.data.nspc)
site.data.nspc<-site.data.nspc[,c("REGION","OBS_YEAR","ISLAND","ANALYSIS_YEAR","SITE")]
# give a count to each site to sum by year and island
site.data.nspc$N<-1
head(site.data.nspc)
levels(site.data.nspc$REGION)<-c("Main HI", "Northwestern HI", "PRIMNM", "Am.Samoa", "S. Mariana","N. Mariana")
site.data.nspc$REGION<-factor(site.data.nspc$REGION, levels(site.data.nspc$REGION)[c(2,1,6,5,3,4)]) # NWHI, MHI, N. Mariana, S. Mariana, PRIA, Samoa
df<-site.data.nspc
levels(site.data.nspc$ISLAND)
#match island to factor number so that island list matches the following: 
#df$ISLAND<-factor(df$ISLAND, levels(df$ISLAND)[c("Kure","Midway","Pearl & Hermes","Lisianski","Laysan","Gardner","Maro", "French Frigate","Necker", "Nihoa","Niihau","Kauai","Oahu","Molokai","Lanai","Maui","Kahoolawe","Hawaii","Farallon de Pajaros","Maug","Asuncion","Agrihan","Pagan","AGS","Saipan","Tinian","Aguijan","Rota","Guam","Wake","Johnston","Kingman","Palmyra","Howland","Baker", "Jarvis", "Swains","Ofu & Olosega", "Tau","Tutuila","Rose")])

# this *should* work:
site.data.nspc$ISLAND<-factor(site.data.nspc$ISLAND, levels(site.data.nspc$ISLAND)[c(17,24,33,20,19,8,21,7,26,27,28,15,29,25,18,23,14,10,6,22,4,1,31,2,36,39,3,35,9,41,13,16,32,11,5,12,37,30,38,40,34)])
# Check to make sure island list matches above commented out list
levels(site.data.nspc$ISLAND)

a<-tapply(site.data.nspc$N, list(site.data.nspc$ISLAND, site.data.nspc$OBS_YEAR), sum)
a<-data.frame(a)
a$Total<-rowSums(a, na.rm = TRUE)
a[is.na(a)]<-" "

a$ISLAND<-as.factor(row.names(a))

a$REGION<-as.vector(c(rep("Northwestern HI", 10), rep("Main HI", 8), rep("N. Mariana", 6),rep("S. Mariana", 5), rep("PRIMNM", 7), rep("Am.Samoa", 5)))

names(a)<-c("2009", "2010", "2011" ,"2012", "2013","2014", "2015","2016","2017","2018","2019","Total", "Island", "Region")
a<-a[,c("Region", "Island", "2009", "2010", "2011", "2012", "2013", "2014", "2015","2016","2017","2018","2019","Total")]

write.csv(a, file="D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/tables/appendix_nspc_surveys_per_island.csv")

# Table for island figures and sites surveyed per year and habitat --------------------------

# load site data
library(plyr)
load("D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/data/Data Outputs/working_site_data.rdata")
head(wsd)
summary(wsd)
unique(wsd$OBS_YEAR)

# add count column to sum sites
wsd$N<-1
# sum by island, reef zone, year
sum<-ddply(wsd, .(ISLAND,METHOD,OBS_YEAR,REEF_ZONE),summarize,"n_sites"=sum(N))
head(sum)
write.csv(sum, file="D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/tables/nspc_surveys_per_island_year_zone.csv")
