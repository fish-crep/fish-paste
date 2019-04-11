#### For NOAA computers: in order to use this script you will need to contact ITS to set up the ODBC drivers on your computer. You will not have adminstrative rights
# You will also need to set up an oracle account if you haven't already
##Once everything it set up you can download the individual datasets. If you experience errors that datasets can't be found double check your oracle account to make sure you have access
#if you don't have access you will need to ask someone in ESD Data Management to have you access to whatever views you are interested in.

#ITS will be providing the steps to set up your ODBC drivers, which will be included here soon.


rm(list=ls())
library(RODBC)            # to connect to oracle
setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Base R/Base Data Files")

Sys.setenv(ODBCINI = "/library/ODBC/odbc.ini")

##*******## jump down to FISH REA WORKINGS if already saved as a .rfile
ch <- odbcConnect("CRED_Oracle", uid = "IWILLIAMS", pwd = "XXXXXXXXXXXX")
##
## #list available tables
tv<-sqlTables(ch, tableType = "VIEW")
a<-as.vector(tv$TABLE_NAME[grep("V0_", as.character(tv$TABLE_NAME))])
b<-as.vector(tv$TABLE_NAME[grep("VS_", as.character(tv$TABLE_NAME))])
c<-as.vector(tv$TABLE_NAME[grep("V_BIA_PERC_COVER_PHOTO_STR", as.character(tv$TABLE_NAME))])
d<-as.vector(tv$TABLE_NAME[grep("V_BIA", as.character(tv$TABLE_NAME))])
##
rawtables<-c(a,b,c)
rawtables

df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V0_SITE_VISIT_ALL")); head(df)
save(df, file="ALL_SITE_SITEVISIT.rdata")

df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V0_FISH_REA")); head(df)
save(df, file="ALL_REA_FISH_RAW.rdata")

#Raw adult coral colony data from REA surveys using method e
df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V0_CORAL_OBS_E")); head(df)
save(df, file="ALL_REA_ADULTCORAL_RAW.rdata")

#Raw juvenile coral colony data from REA surveys using method f
df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V0_CORAL_OBS_F")); head(df)
save(df, file="ALL_REA_JUVCORAL_RAW.rdata")

##
##

#TOW FISH PRESENCE DATA#
df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.VS_FISH_TDS_SIGHTINGS")); head(df)
save(df, file="ALL_TOW_FISH_SIGHTINGS.rdata")


#TOW FISH #
df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.VS_FISH_TDS")); head(df)
save(df, file="ALL_TOW_FISH_RAW.rdata")


#TOW BENTHIC#
df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.VS_BENT_TDS")); head(df)
save(df, file="ALL_TOW_BENT_RAW.rdata")

#BENTHIC REA
#load("ALL_BIA_STR_RAW.rdata")
bia <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V_BIA_PERC_COVER_PHOTO_STR_")); head(bia)
save(bia, file="ALL_BIA_STR_RAW_NEW.rdata")

cli <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V_BIA_PERC_COVER_PHOTO_CLI_")); head(cli)
save(cli, file="ALL_BIA_CLIMATE_PERM.rdata")


# Coral Net Benthic Data
cnet <- sqlQuery(ch, paste("SELECT * FROM GISDAT.MV_BIA_CNET_ANALYSIS_DATA")); head(cnet)
save(cnet, file="ALL_BIA_STR_CNET.rdata")

