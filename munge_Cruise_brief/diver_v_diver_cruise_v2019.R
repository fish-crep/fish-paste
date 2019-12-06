# mid-cruise diver vs diver comparisons ----------------------------------

rm(list=ls()) # clean workspace
library(dplyr)
library(reshpae)
# load core fish functions for data cleaning, etc.
source("D:/CRED/fish_team_R/fish-paste/lib/core_functions.R")
source("D:/CRED/fish_team_R/fish-paste/lib/fish_team_functions.R")
source("D:/CRED/fish_team_R/fish-paste/lib/Islandwide Mean&Variance Functions.R")

# go to the cruise data entry app and download the fish rea base report and save it to your computer. Then load it:

wd<-read.csv("D:/CRED/cruise/1.MHI_2019/data_leg_3/report__fish_rea_base7_29.csv")

#Pull all species information into a separate df
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)

## using Calc_REP functions (from fish_team_functions) to get richness and biomass estimates per replicate....
r1<-Calc_REP_Bio(wd, "FAMILY"); family.cols<-names(r1)[7:dim(r1)[2]]

# if there are any UNKNOWN fish records, run the code below: 
#family.cols # where is UNKNOWN?
#family.cols<-family.cols[-68] ## drop level UNKNOWN

# calculate total biomass per replicate
r1$TotFish<-rowSums(r1[,family.cols])

# calculate species richness per replicate
r2<-Calc_REP_Species_Richness(wd)

UNIQUE_SURVEY<-c("SITE", "SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")
SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL")

r3<-Calc_REP_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)

COMPARE_ON<-c("SITEVISITID", "SITE", "REP", "REPLICATEID")
compdata<-merge(r1[,c(COMPARE_ON, "TotFish")], r2[,c(COMPARE_ON, "SPECIESRICHNESS")], by=COMPARE_ON, all.x=T)
compdata<-merge(compdata[,c(COMPARE_ON, "TotFish", "SPECIESRICHNESS")], r3[,c(COMPARE_ON, "HARD_CORAL")], by=COMPARE_ON, all.x=T)


## get year, region and island data - needed for diver v diver function to run
a<-unique(wd[,c("SITE","SITEVISITID","OBS_YEAR","ISLAND","REGION_NAME","REPLICATEID","DIVER")])

test<-merge(compdata, a, by="REPLICATEID", all.x=T) ## this collates year, region etc. with the individual diver estimates per rep
compdata<-test

## need to rename some of the cols after the merge
names(compdata)<-c("REPLICATEID","SITEVISITID","SITE","REP","TotFish","SPECIESRICHNESS","HARD_CORAL","SITE.y","SITEVISITID.y","OBS_YEAR","ISLAND","REGION","DIVER")


## divervsdiver4 - creates an anonymous and named version of diver comparisons for totfish, richness and coral estimates 
##- need to look at the range of the data per year region to tweak the x axis range
data<-compdata[compdata$REGION=="MHI",]
summary(data)# look at max value for tot fish and adjust x_range below

# SET WD to where you want to save figures
setwd("D:/CRED/cruise/1.MHI_2019/data_leg_3")
## to create a multigraph with total fish, richness and coral estimates, call data, year, region, and x-range:

divervsdiver4(data=data, year = "2019", region="MHI", x_range= 100)
# save data file if you need:
write.csv(data,file="D:/CRED/cruise/1.MHI_2019/data_leg_3/buddy_comp.csv")

####################################################
### SIZE COMPARISONS ###############################
####################################################

table(wd$SPECIES)
# select common species to compare sizes on and colate species of interest list: 
SPECIES_OF_INTEREST<-c("ACNC", "ACLI", "CTCY", "NALI", "APAP", "LETA", "LUWH", "MYBE", "STAU", "PSBA", "HAHO", "GOVA", "BAUN", "CALU", "CEAR", "CELO", "CHMA", "CTMA", "EPFA", "SCRU")
wd<-all.x[all.x$SPECIES %in% SPECIES_OF_INTEREST,]

#remove tiddlers from this! (for all non-brits, tiddlers = tiny fish)
wd[wd$SIZE_<0.2*wd$LMAX,]$COUNT<-0

wd<-wd[,c("DIVER", "SPECIES", "SIZE_", "COUNT")]

divers<-levels(wd$DIVER); divers

#set count to max of 25 ... so this doesnt get absurdly huge and slow
wd[wd$COUNT>25,]$COUNT<-25
wd<-wd[wd$COUNT>0,]

wd2<-wd[1, c("DIVER", "SPECIES", "SIZE_")]
if(wd[1, "COUNT"]>1)
  for (j in 2:wd[1, "COUNT"])
    wd2<-rbind(wd2,wd[1,c("DIVER", "SPECIES", "SIZE_")])

for(i in 2:dim(wd)[1])
{
  for (j in 1:wd[i, "COUNT"])
    wd2<-rbind(wd2,wd[i,c("DIVER", "SPECIES", "SIZE_")])
}

for(i in 1:length(SPECIES_OF_INTEREST))
{
  sp<-SPECIES_OF_INTEREST[i]
  wd_sp<-wd2[wd2$SPECIES==sp,]
  
  tmp_name<-paste(sp, ".png", collapse="")
  png(filename=tmp_name)
  #	boxplot(SIZE_ ~ DIVER, data=wd_sp, main=sp)
  
  ggplot(wd_sp, aes(x = DIVER, y = SIZE_)) + 
    geom_boxplot()+ 
    scale_y_continuous(limits = c(0,max(wd_sp$SIZE_)*1.1), breaks=pretty_breaks(n=5)) +
    geom_jitter(colour = "blue", size =4, alpha = 0.4)+
    labs(x = sp) 
  
  ggsave(filename=tmp_name)
  
  dev.off()
  graphics.off()
}

# viola, you should have some pretty figures in your working directory

