# mid-cruise diver vs diver comparisons ----------------------------------
rm(list=ls())
library(dplyr)
source("D:/CRED/fish_team_R/fish-paste/lib/core_functions.R")
source("D:/CRED/fish_team_R/fish-paste/lib/fish_team_functions.R")
source("D:/CRED/fish_team_R/fish-paste/lib/Islandwide Mean&Variance Functions.R")
library(reshape)

# load data so far
wd<-read.csv("T:/CruiseData/RA-22-01_LEG5/Project Group/Fish/data/Report_ Fish REA Base.csv")

#wd[wd$FAMILY=="",]$FAMILY<-"UNKNOWN"

## need to read in the species_table from Fish Base
#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)

## using Calc_REP functions (from fish_team_functions) to get richness and biomass estimates per replicate....
r1<-Calc_REP_Bio(wd, "FAMILY"); family.cols<-names(r1)[7:dim(r1)[2]]
# naming difference for unknown.. quick temp fix...drop unknown because we can't compare those?
#family.cols # where is UNKNOWN?
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
a<-unique(wd[,c("SITE","SITEVISITID","OBS_YEAR","ISLAND","REGION_NAME","REPLICATEID","DIVER")])

test<-merge(compdata, a, by="REPLICATEID", all.x=T) ## this collates year, region etc. with the indiv diver ests per rep
compdata<-test

## need to rename some of the cols after the merge
names(compdata)<-c("REPLICATEID","SITEVISITID","SITE","REP","TotFish","SPECIESRICHNESS","HARD_CORAL","SITE.y","SITEVISITID.y","OBS_YEAR","ISLAND","REGION","DIVER")


## divervsdiver4 - creates an anonymous and named version of diver comparisons for totfish, richness and coral estimates 
##- need to look at the range of the data per year region to tweak the x axis range
data<-compdata[compdata$REGION=="MARIAN",]
summary(data)# look at max value for tot fish and adjust x_range below

# SET WD
setwd("T:/CruiseData/RA-22-01_LEG5/Project Group/Fish/data/")
## to create a multigraph with total fish, richness and coral estimates run divervsdiver4

divervsdiver4(data=data, year = "2022", region="MARIAN", x_range= 100)

write.csv(data,file="T:/CruiseData/RA-22-01_LEG5/Project Group/Fish/data/buddy_comp.csv")

####################################################
### SIZE COMPARISONS ###############################
####################################################
x<-wd
sort(table(wd$SPECIES))
SPECIES_OF_INTEREST<-c("SCSC","SUCH","CHSL","CTSR","ACNF","ACNC","SCPS","BAUN","MYKU","NALI")
wd<-x[x$SPECIES %in% SPECIES_OF_INTEREST,]

#remove tiddlers from this!
wd[wd$SIZE_<0.2*wd$LMAX,]$COUNT<-0

wd<-wd[,c("DIVER", "SPECIES", "SIZE_", "COUNT")]

divers<-levels(wd$DIVER); divers

#set count to max of 25 ... so this doesnt get absurdly huge and slow
wd[wd$COUNT>25,]$COUNT<-25
wd<-wd[wd$COUNT>0,]
wd2<-wd[1, c("DIVER", "SPECIES", "SIZE_")]

# define working directory to save plots
setwd("T:/CruiseData/RA-22-01_LEG5/Project Group/Fish/data/")

if(wd[1, "COUNT"]>1)
  for (j in 2:wd[1, "COUNT"])
    wd2<-rbind(wd2,wd[1,c("DIVER", "SPECIES", "SIZE_")])

for(i in 2:dim(wd)[1])
{
  for (j in 1:wd[i, "COUNT"])
    wd2<-rbind(wd2,wd[i,c("DIVER", "SPECIES", "SIZE_")])
}

# create loop to make 1 plot for each species
for(i in 1:length(SPECIES_OF_INTEREST))
{
  sp<-SPECIES_OF_INTEREST[i]
  wd_sp<-wd2[wd2$SPECIES==sp,]
  
  tmp_name<-paste(sp, ".png", collapse="") # saves one plot for each species
  png(filename=tmp_name)
  #	boxplot(SIZE_ ~ DIVER, data=wd_sp, main=sp)
  
  p<-ggplot(wd_sp, aes(x = DIVER, y = SIZE_)) + 
    geom_boxplot()+ 
    theme_bw() +
    labs(y = "Size (cm)", x = "Diver") + 
    #scale_y_continuous(limits = c(0,max(wd_sp$SIZE_)*1.1), breaks=pretty(5))+
    geom_jitter(colour = "blue", size =2, alpha = 0.25)+
    ggtitle(sp)+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(size=12,color="black"), axis.text.y = element_text(size=12,color="black"))
  
  ggsave(filename=tmp_name)
  print(p)
  
  dev.off()
  graphics.off()
}

#### to combine: open adobe pdf, go to tools, combine files, add all graphs. Then go to print...multiple, and do 2x3 per page.


