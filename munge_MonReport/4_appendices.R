rm(list=ls())

setwd("E:/CRED/fish_team_R/R")
source("fish_team_functions.R")
source("Islandwide Mean_Variance Functions.R")
source("diver_v_diver4.R")
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs")
library(reshape)

##### Table 2 in report body ######
load("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs/MONREPdata_pooled_reg_FRF.rdata")
ref<-as.data.frame(data_pooled_reg) # forereef only reference
# get relevant columns
test<-ref[,c("Mean.REGION","Mean.N","Mean.TotFish","Mean.PISCIVORE","Mean.SECONDARY","Mean.PRIMARY","Mean.PLANKTIVORE","Mean.0_20","Mean.20_50","Mean.50_plus")]
# round to 1 decimal place
temp<-round(test[,c(2:10)],digits=1)
test<-cbind(test$Mean.REGION,temp)
# rename region
names(test)[1]<-"REGION"
# order: NWHI, MHI, N. MARIAN, S.MARIAN, PRIAs, SAMOA
#levels(test$REGION)<-c("NWHI", "MHI", "N. MARIAN", "S.MARIAN", "PRIAs", "SAMOA")
# save file
write.csv(test,file="E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/tables/table_2.csv")

# separate table for st dev
test<-ref[,c("Mean.REGION","Mean.N","PooledSE.TotFish","PooledSE.PISCIVORE","PooledSE.SECONDARY","PooledSE.PRIMARY","PooledSE.PLANKTIVORE","PooledSE.0_20","PooledSE.20_50","PooledSE.50_plus")]
temp<-round(test[,c(3:10)],digits=1)
test<-cbind(test$Mean.REGION,test$Mean.N,temp)
# rename region
names(test)[1]<-"REGION"
# order: NWHI, MHI, N. MARIAN, S.MARIAN, PRIAs, SAMOA
#levels(test$REGION)<-c("NWHI", "MHI", "N. MARIAN", "S.MARIAN", "PRIAs", "SAMOA")
# save file
write.csv(test,file="E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/tables/table_2stdev.csv")

### appendices
# appendix 2: All surveys per region per year and method used -----------------

load("working_site_data.rdata")
site.data<-wsd

## table... regions surveyed per year
levels(site.data$REGION_NAME)<-c("Am.Samoa","Main HI","Mariana Arch.","Northwest HI", "PRIAs")
tmp<-table(site.data$REGION_NAME, site.data$OBS_YEAR, site.data$METHOD)
test<-as.data.frame(tmp)
write.csv(test,"E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/tables/appendix_surveys_per_method_per_region_per_yearKM.csv")
tmp[which(tmp[,] == 0)]<-" "

write.csv(tmp, "E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/tables/appendix_surveys_per_method_per_region_per_yearIVOR.csv")
write.csv(site.data,file= "E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/tables/appendix_surveys_per_method_TEMP.csv")

# appendix 4: strata sector by year ---------------------------------------
# these show sites at the level they were pooled for analysis. Backreef and ROSE lagoon sites are all pooled. Drop sectors that have 1 site, these were dropped for analysis
library(reshape)
getwd()
load("clean_working_site_data_used_in_higher_pooling_for_report.Rdata")
secs<-wsd.uncap
head(secs)
# @#$^^$% SOUTH BANK!!
secs<-secs[secs$ISLAND !="South Bank",]
secs<-droplevels(secs)
head(secs)
names(secs)
secs<-secs[,c("REGION","ISLAND","ANALYSIS_SEC","ANALYSIS_YEAR", "ANALYSIS_STRATA","SITE")]# ANALYSIS_YEAR, REGION, ISLAND, ANALYSIS_SEC, ANALYSIS_STRATA 
secs$N<-1 # to sum number of sites in each sector/depth bin
# select only sites from 2016
test<-secs[secs$ANALYSIS_YEAR=="2016",]
summary(test)
test<-droplevels(test)
# example code below for cast function
#a<-cast(tmp, OBS_YEAR + REGION + ISLAND + SEC_NAME ~ REEF_ZONE + DEPTH_BIN, value="n_sites", sum, fill=NA)
temp<-as.data.frame(cast(test, REGION+ISLAND+ANALYSIS_SEC+ANALYSIS_YEAR~ANALYSIS_STRATA,value="N",sum, fill=NA))
summary(temp)
names(temp)
# use only relevant strata 
df<-temp[,c("ANALYSIS_YEAR","REGION" ,"ISLAND","ANALYSIS_SEC" , "ForereefDeep" ,"ForereefMid" , "ForereefShallow", "LagoonMid" , "LagoonShallow","Protected SlopeDeep","Protected SlopeMid")]

names(df) # change to match relevant strata
names(df) <- c("Year", "Region", "Island", "Sector", "Forereef-D", "Forereef-M", "Forereef-S", "Lagoon-M", "Lagoon-S","Protected Slope-D","Protected Slope-M")
# # sum lagoon and protected slope 
# str(df)
# df[is.na(df)] <- 0
# df$lagoonall<-rowSums(df[,c("Lagoon-M","Lagoon-S")])
# #df$psall<-rowSums(df[,c("Protected Slope-D","Protected Slope-M")])
# # get rid of lagoon and backreef shal,mid,deep, just keep totals
# names(df)
# df2<-df[,c("Year","Region","Island","Sector","Forereef-D","Forereef-M","Forereef-S","lagoonall", "psall")]
# #change names
# names(df2)
# names(df2)<-c("Year", "Region", "Island", "Sector", "Forereef-D", "Forereef-M", "Forereef-S","Lagoon-All","Protected Slope-All")
# # change zeros to blank entries
# df2[df2 == 0]<-NA

# DROP sectors that only have 1 rep, change NAs to blank entries
df[df == 1]<-NA
df[is.na(df)]<-" " 
write.csv(df, file="E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/tables/appendix_sector_year_data_table.csv")

# appendix 5: diver vs diver comparisons ----------------------------------

load("raw_working_data.rdata")
## need to read in the species_table from Fish Base
#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)

## using Calc_REP functions (from fish_team_functions) to get richness and biomass estimates per replicate....
r1<-Calc_REP_Bio(wd, "FAMILY"); family.cols<-levels(species_table$FAMILY)
# naming difference for unknown.. quick temp fix
family.cols<-family.cols[-1] ## drop level UNKNOWN
r1$TotFish<-rowSums(r1[,family.cols])

r2<-Calc_REP_Species_Richness(wd)

UNIQUE_SURVEY<-c("SITE", "SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")
SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL", "SOFT_CORAL", "MA", "CCA", "TA", "SPONGE", "SAND", "CYANO", "MEAN_SH", "MEAN_SH_DIFF", "MAX_HEIGHT")

r3<-Calc_REP_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)

COMPARE_ON<-c("SITEVISITID", "SITE", "REP", "REPLICATEID")
compdata<-merge(r1[,c(COMPARE_ON, "TotFish")], r2[,c(COMPARE_ON, "SPECIESRICHNESS")], by=COMPARE_ON, all.x=T)
compdata<-merge(compdata[,c(COMPARE_ON, "TotFish", "SPECIESRICHNESS")], r3[,c(COMPARE_ON, "HARD_CORAL","SOFT_CORAL","MA","CCA","TA","SAND","CYANO","MEAN_SH", "MEAN_SH_DIFF", "MAX_HEIGHT")], by=COMPARE_ON, all.x=T)


## get year, region and island data
a<-unique(wd[,c("SITE","SITEVISITID","OBS_YEAR","ISLAND","REGION","REPLICATEID","DIVER","ANALYSIS_YEAR")])

test<-merge(compdata, a, by="REPLICATEID", all.x=T) ## this collates year, region etc. with the indiv diver ests per rep
compdata<-test

## need to rename some of the cols after the merge
names(compdata)<-c("REPLICATEID","SITEVISITID","SITE","REP","TotFish","SPECIESRICHNESS","HARD_CORAL","SOFT_CORAL",
                   "MA","CCA","TA","SAND","CYANO","MEAN_SH","MEAN_SH_DIFF","MAX_HEIGHT",
                   "SITE.y","SITEVISITID.y","OBS_YEAR","ISLAND","REGION","DIVER","ANALYSIS_YEAR")


# set wd
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures/appndx")
## divervsdiver3 - creates an anonymous and named version of diver comparisons for totfish, richness and coral estimates 
##- need to look at the range of the data per year region to tweak the x axis range
dataNWHI<-compdata[compdata$REGION=="NWHI"&compdata$ANALYSIS_YEAR==2016,]
summary(dataNWHI)# look at max value for tot fish and adjust x_range below
dataSAMOA<-compdata[compdata$REGION=="SAMOA"&compdata$ANALYSIS_YEAR==2016,]
summary(dataSAMOA)
## to create a multigraph with total fish, richness and coral estimates run divervsdiver3
divervsdiver3(data=compdata, year = "2016", region="NWHI", x_range= 100)
divervsdiver3(data=compdata, year = "2016", region="SAMOA", x_range= 100)
divervsdiver3(data=compdata, year = "2016", region="PRIAs", x_range= 100)
divervsdiver3(data=compdata, year = "2016", region="MHI", x_range= 100)

# trying to make graphs taller to see graphs with lots of divers
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures/appndx/diver_v_diver")
#divervsdiver4(data=compdata, year = "2016", region="NWHI", x_range= 100)
divervsdiver4(data=compdata, year = "2016", region="SAMOA", x_range= 100)
#divervsdiver4(data=compdata, year = "2016", region="PRIAs", x_range= 100)
#divervsdiver4(data=compdata, year = "2016", region="MHI", x_range= 100)

 
#------------- appendix 6: random stratified sites surveyed per region / island -------------
load("clean_working_site_data_used_in_higher_pooling_for_report.Rdata")
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
levels(site.data.nspc$REGION)<-c("Main HI", "Northwest HI", "PRIA", "Am.Samoa", "S. Mariana","N. Mariana")
site.data.nspc$REGION<-factor(site.data.nspc$REGION, levels(site.data.nspc$REGION)[c(2,1,6,5,3,4)]) # NWHI, MHI, N. Mariana, S. Mariana, PRIA, Samoa
df<-site.data.nspc
levels(site.data.nspc$ISLAND)
#match island to factor number(16, 23,32,19,18,7,20,6,25,26,27,14,28,24,17,22,13,9,5,21,3,1,30,42,35,39,2,34,8,41,12,15,31,10,4,11,37,29,38,40,33,36)
#df$ISLAND<-factor(df$ISLAND, levels(df$ISLAND)[c("Kure","Midway","Pearl & Hermes","Lisianski","Laysan","Gardner","Maro", "French Frigate","Necker", "Nihoa","Niihau","Kauai","Oahu","Molokai","Lanai","Maui","Kahoolawe","Hawaii","Farallon de Pajaros","Maug","Asuncion","Agrihan","Pagan","AGS","Saipan","Tinian","Aguijan","Rota","Guam","Wake","Johnston","Kingman","Palmyra","Howland","Baker", "Jarvis", "Swains","Ofu & Olosega", "Tau","Tutuila","Rose", "South Bank")])

site.data.nspc$ISLAND<-factor(site.data.nspc$ISLAND, levels(site.data.nspc$ISLAND)[c(16, 23,32,19,18,7,20,6,25,26,27,14,28,24,17,22,13,9,5,21,3,1,30,42,35,39,2,34,8,41,12,15,31,10,4,11,37,29,38,40,33,36)])



a<-tapply(site.data.nspc$N, list(site.data.nspc$ISLAND, site.data.nspc$OBS_YEAR), sum)
a<-data.frame(a)
a$Total<-rowSums(a, na.rm = TRUE)
a[is.na(a)]<-" "

a$ISLAND<-as.factor(row.names(a))

a$REGION<-as.vector(c(rep("Northwest HI", 10), rep("Main HI", 8), rep("N. Mariana", 6),rep("S. Mariana", 5), rep("PRIA", 7), rep("Am.Samoa", 6)))

names(a)<-c("2009", "2010", "2011" ,"2012", "2013","2014", "2015","2016","Total", "Island", "Region")
a<-a[,c("Region", "Island", "2009", "2010", "2011", "2012", "2013", "2014", "2015","2016","Total")]

write.csv(a, file="E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/tables/appendix_nspc_surveys_per_island.csv")

