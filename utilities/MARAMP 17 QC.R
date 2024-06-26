rm(list=ls())
setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Data Requests/GUFF")

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(scales)

source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/fish_team_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/Islandwide Mean&Variance Functions.R")

# #calc values which are means of sub-replicates per site (generally will be depth, coral cover, complexity etc...)
# Calc_REP_nSurveysArea<-function(x, survey_id_fields, rep_fields, count_fields, survey_data_fields){  
 
	# RETURN_FIELDS<-c("nCounts", "nReps")

	# #first average survey_data_fields for all replicate-counts
	# y<-aggregate(x[,survey_data_fields],by=x[,count_fields], mean)
	
# #	#idw if a rep has NAs in the data fields, then try to set them to the average of other counts in that rep
# #	y<-fill.NA.count(y, rep_fields, survey_data_fields)

	# y$nCounts<-1
	# y$nReps<-1
	# names(y)<-c(count_fields, survey_data_fields, RETURN_FIELDS)
	
	# return(y)
	
# } # end Calc_REP_nSurveysArea
# #################

# Calc_REP_Species_Richness<-function(x){  
  # # I would prefer this to be a function that can work with any value (species, family, genera.., am not there yet) 
  # # function returns a data frame with Site_VisitID, Method, and mean species_richness per Rep at the sute (Rep being an nSPC cylinder or a transect)	
  
  # y<-aggregate(x$COUNT,by=x[,c("SITEVISITID", "METHOD", "SITE", "REP", "DIVER", "REPLICATEID", "SPECIES")], sum)	#convert to count per species per rep
  # z<-aggregate(y$SPECIES,by=y[,c("SITEVISITID", "METHOD", "SITE", "REP", "DIVER", "REPLICATEID")], length)  		# count number of entries per rep	
  # dimnames(z)[[2]]<-c("SITEVISITID", "METHOD", "SITE", "REP","DIVER", "REPLICATEID", "SPECIESRICHNESS")
  
  # return(z)
  
# }
# # end Calc_REP_Species_Richness

Calc_REP_Bio<-function(x, grouping_field){  
	# function assumes that x is a data frame with at least the columns/elements listed in base_cols, plus the field_of_interest, in this case CommonFamily
	# function returns a data frame with Site_VisitID, Method, and mean site biomass(gm2) per each value of the field_of_interest (hard wired as CommonFamily for now!)
	#add an Abundance m2 field to x
	x$Bio_gm2<-Calc_Biomassgm2(x)

	x$GROUP<-x[,grouping_field]
	
	#Replicate ID is the base unit .. so pool up biomass at ReplicateID level, for the field of interest
	base_cols=c("SITEVISITID","METHOD","SITE", "REP", "DIVER", "REPLICATEID") # minimum set of fields to build up from
	pool_cols<-c(base_cols,"GROUP")                # minimum set, plus the one we are interested in
	
	#first calculate total biomass per rep for all values of this field
	y<-aggregate(x$Bio_gm2,by=x[,pool_cols], sum)
	names(y)<-c(pool_cols, "Bio_gm2")
	#now format this more or less as a crosstab, with field of interest as column variable
	y<-cast(y, SITEVISITID + METHOD + SITE + REP + DIVER + REPLICATEID ~ GROUP, value="Bio_gm2", fill=0)
		
	return(y)
	
} # end Calc_REP_Bio
                                                                                           

# FISH REA WORKINGS ----------------------------------------------------------------
###EXAMPLE CODE FOR MERGING WITH ACCESS DATABASE OUTPUT ....
x<-read.csv("REA FISH BASE_FOR_R.csv")    #NB... 'REA FISH BASE FOR R' IS THE ACCESS QUERY THAT GENERATES OUTPUT TO MATCH THE DATA_COLS ABOVE
x<-subset(x, x$METHOD == "nSPC")


#have to have same date types!  Somehow the formatting seems to change in every iteration. This might be due to how individuals have Access and/or Excel set up ... so probably need to be ready to fiddle this
# seems to be the case that Dates are Often weird, so this needs checking
# NB POSIXct formatting codes are defined at http://astrostatistics.psu.edu/su07/R/html/base/html/strptime.html
#x$DATE_<-as.POSIXct(strptime(x$DATE_, format="%d-%b-%y")) ... this is for abbrevaited month eg "mar"
#x$DATE_<-as.POSIXct(strptime(x$DATE_, format="%b-%d-%y")) ... this was month first
#x$DATE_<-as.POSIXct(strptime(x$DATE_, format="%m/%d/%Y"))  #... this is how it is for Adel's csv file
x$DATE_<-as.POSIXct(strptime(x$DATE_, format="%m/%d/%y"))  #... this is how it is for Ivor's csv file .. year is 2 digit
x$DATE_<-as.character(x$DATE_)

#select only after certain period (eg second leg)
#x<-x[x$DATE_ >"2015-06-24",]
x<-droplevels(x)

#DROP THE EMPTY RECORDS - FROM BENTHIC ONLY SITES
dim(x)
x<-x[x$SPECIES != "",]
dim(x)

# OPTIONAL, BUT PROBABLY A GOOD IDEA .. REMOVE SITES WHICH WERE TRAINING DIVES (AT LEAST ONE PARTICIPANT WAS TRAINING)
t_sites<-unique(x[x$TRAINING_YN,]$SITE); t_sites
x<-subset(x, !x$SITE %in% t_sites)
dim(x)

all.x<-x

# only use I, N, U (ie. instantaneous, non-instantaneous, and unknown) data for standard comparisons. This will filter out the fice minute and ten minute records that were only done in recent surveys
x<-subset(x, x$OBS_TYPE %in% c("U","I","N"))

#clean up data
DATA_COLS<-c("SITEVISITID", "METHOD", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION", 
"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH", "DATE_",
"HARD_CORAL", "MA",  "TA",  "CCA",  "SAND",  "SOFT_CORAL", "CLAM" , "SPONGE", "CORALLIMORPH", "CYANO", "TUNICATE", "ZOANTHID", "OTHER",
"SPECIES", "TAXONNAME", "COUNT", "SIZE_", "OBS_TYPE", 
"SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150", "MAX_HEIGHT",
"FAMILY" , "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

x<-droplevels(x)

## Update SITE to have three numeric digits (eg OAH-01 becomes OAH-001)
x$SITE<-SiteNumLeadingZeros(x$SITE)

sh_out<-CalcMeanSHMeanSHDiff(x)
x$MEAN_SH<-sh_out[[1]]
x$MEAN_SH_DIFF<-sh_out[[2]]
# remove the component SUBSTRATE_HEIGHT fields
x<-x[, setdiff(names(x),c("SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150"))]
############################################################################################

#generate a simple "Strata" field, by concatenating Stratum and Depth fields
x$STRATA<-as.factor(paste(x$REEF_ZONE, x$DEPTH_BIN, sep=''))
wd<-x

#################### IMPROVED CLEAN UP OF NAs IN WORKING DATA #############
tmp.lev<-levels(wd$HABITAT_CODE); head(tmp.lev)
levels(wd$HABITAT_CODE)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wd$FAMILY); head(tmp.lev)
levels(wd$FAMILY)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wd$COMMONFAMILYALL); head(tmp.lev)
levels(wd$COMMONFAMILYALL)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wd$TROPHIC_MONREP); head(tmp.lev)
levels(wd$TROPHIC_MONREP)<-c(tmp.lev, "UNKNOWN")

wd[is.na(wd$HABITAT_CODE),"HABITAT_TYPE"]<-"UNKNOWN"
wd[is.na(wd$FAMILY),"FAMILY"]<-"UNKNOWN"
wd[is.na(wd$COMMONFAMILYALL),"COMMONFAMILYALL"]<-"UNKNOWN"
wd[is.na(wd$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"

##wd[is.na(wd$COUNT),]$COUNT<-0
##wd[is.na(wd$COUNT),]$SIZE_<-0
##wd[is.na(wd$LMAX),]$LMAX<-999

wd<-droplevels(wd)
#####################################################################################

#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

#get base survey info, calculate average depth+complexity+so on
SURVEY_INFO<-c("OBS_YEAR", "REGION", "ISLAND", "SITE", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE", "STRATA", "SITEVISITID", "METHOD")
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)
island_table<-Aggregate_InputTable(wd, c("REGION","ISLAND"))
SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL", "MA", "CCA", "OTHER", "SAND", "MEAN_SH", "MEAN_SH_DIFF", "MAX_HEIGHT")
survey_est_benthos<-Calc_Site_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)   #Calc_Site_nSurveysArea deals better with situations where one REP has benthic data and other doesnt. 


## ADDED THIS::
#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)

## run through to create wd raw working data - ignore benthic stuff for now....
r1<-Calc_REP_Bio(wd, "FAMILY"); family.cols<-names(r1)[7:dim(r1)[2]]
r1$TotFish<-rowSums(r1[,family.cols])
r2<-Calc_REP_Species_Richness(wd)
r3<-Calc_REP_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA) 

COMPARE_ON<-c("SITEVISITID", "SITE", "REP", "DIVER", "REPLICATEID")
compdata<-merge(r1[,c(COMPARE_ON, "TotFish")], r2[,c(COMPARE_ON, "SPECIESRICHNESS")], by=COMPARE_ON, all.x=T)
compdata<-merge(compdata[,c(COMPARE_ON, "TotFish", "SPECIESRICHNESS")], r3[,c("REPLICATEID", "HARD_CORAL")], by=c("REPLICATEID"), all.x=T)
#NOW DEALING WITH THE SITUATION THAT SHOULDNT HAPPEN OF A DIVER HAVING TWO COUNTS AT SAME SITE ADN REP - eg SAME JAR-999, REP A, DIVER = AEG appears twice (with different REPLICATEIDs)
compdata<-aggregate(compdata[,c("TotFish", "SPECIESRICHNESS", "HARD_CORAL")], by=compdata[,c("SITEVISITID", "SITE", "REP", "DIVER")], FUN=mean)

# tack on year, region, island data
test<-merge(compdata, wd[,c("SITEVISITID", "OBS_YEAR", "REGION")], by="SITEVISITID", all.x=T)
compdata<-unique(test)

size_comp<-compdata
divers<-levels(size_comp$DIVER); divers
scr<-size_comp[,c("SITEVISITID", "DIVER", "TotFish")] ### stripped down
msd<-data.frame(with(scr, tapply(TotFish, list(SITEVISITID, DIVER), mean))) ## mean biomass estimate per diver per site

thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
toplot<-list()

for(i in 1:length(divers)){
  diver<-which(thenas[,i] == FALSE)
  
  ##select out the sites for diver from the dataframe
  diverdata<-msd[diver,i]
  
  ## get the other diver estimates per site (take mean because at some sites there are more than 2 divers)
  otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
  
  ## for diver 1 create a site level biomass ratio, the biomass of diver 1 / the biomass of the other divers
  x<-as.vector(diverdata - otherdiver)
  toplot[[i]]<-x
}

toplot<-(melt(toplot))

names(toplot)<-c("biomass_ratio", "diver")
toplot$diver<-as.factor(toplot$diver)
BIO_RANGE<-50

bio <- ggplot(toplot, aes(factor(diver), biomass_ratio))
bio <- bio + 
  geom_boxplot() + 
  coord_flip() + 
  geom_jitter(colour = "red", size =1, alpha = 0.4) + 
  theme_bw() +
  scale_y_continuous(limits=c(-BIO_RANGE,BIO_RANGE))+
  labs(y = expression(paste("Difference in biomass estimate (g ", m^-2,") relative to buddy")), x = "Diver") + 
  scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = divers)

suppressMessages(ggsave(filename =paste("divervsdiver_biomass","_.png", sep = ""), width=13, height = 12.0, units = c("cm")))     

######## diver vs diver for species richness (here number of species)

scr<-size_comp[,c("SITE", "DIVER", "SPECIESRICHNESS")] ### stripped down
msd<-data.frame(with(scr, tapply(SPECIESRICHNESS, list(SITE, DIVER), mean))) ## total number of species per diver at each site

thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
toplot<-list()

for(i in 1:length(divers)){
  diver<-which(thenas[,i] == FALSE)
  
  ##select out the sites for diver from the dataframe
  diverdata<-msd[diver,i]
  
  ## get the other diver estimates per site (take mean because at some sites there are more than 2 divers)
  otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
  
  ## for diver 1 create a site level biomass ratio, the biomass of diver 1 / the biomass of the other divers
  x<-as.vector(diverdata - otherdiver)
  toplot[[i]]<-x
}

toplot<-(melt(toplot))

names(toplot)<-c("species", "diver")
toplot$diver<-as.factor(toplot$diver)
RICH_RANGE<-10


species <- ggplot(toplot, aes(factor(diver), species))
species <- species + 
  geom_boxplot() + 
  coord_flip() + 
  geom_jitter(colour = "red", size =1, alpha = 0.4) + 
  theme_bw() + scale_y_continuous(limits=c(-RICH_RANGE,RICH_RANGE)) +
  labs(y = "Difference in species richness relative to buddy", x = "Diver") + 
  scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = divers)
suppressMessages(ggsave(filename = paste("divervsdiver_species",".png", sep = ""), width=13, height = 12.0, units = c("cm")))


######## diver vs diver for coral estimates
scr<-size_comp[,c("SITE", "DIVER", "HARD_CORAL")] ### stripped down
msd<-data.frame(with(scr, tapply(HARD_CORAL, list(SITE, DIVER), mean))) ## mean biomass estimate per diver per site

thenas<-data.frame(is.na(msd)) ## id all the locations of nas in dataset
toplot<-list()

for(i in 1:length(divers)){
  diver<-which(thenas[,i] == FALSE)
  
  ##select out the sites for diver from the dataframe
  diverdata<-msd[diver,i]
  
  ## get the other diver estimates per site (take mean because at some sites there are more than 2 divers)
  otherdiver<-rowMeans(msd[diver, !(colnames(msd) %in% divers[i])], na.rm = TRUE)
  
  ## for diver 1 create a site level biomass ratio, the biomass of diver 1 / the biomass of the other divers
  x<-as.vector(diverdata - otherdiver)
  toplot[[i]]<-x
}

toplot<-(melt(toplot))
names(toplot)<-c("hard_coral", "diver")
toplot$diver<-as.factor(toplot$diver)

coral <- ggplot(toplot, aes(factor(diver), hard_coral))
coral <- coral + 
  geom_boxplot() + 
  coord_flip() + 
  geom_jitter(colour = "red", size =1, alpha = 0.4) + 
  theme_bw() +
  labs(y = "Difference in coral cover estimate (%) relative to buddy", x = "Diver") + 
  scale_x_discrete(breaks = 1:length(levels(size_comp$DIVER)), labels = levels(size_comp$DIVER))
suppressMessages(ggsave(filename =paste("divervsdiver_coral", "_.png", sep = ""), width=13, height = 12.0, units = c("cm")))


####################################################
### SIZE COMPARISONS ###############################
####################################################
#SPECIES_OF_INTEREST<-c("ACNF", "ACOL", "ACDU", "ZEFL", "NALI", "APVI", "MENI", "CAME", "BOBI", "CEAR", "CHOR", "FOFL", "PAMU", "SCRU", "SUFR", "THDU", "SUBU", "PAAR")   		#HAWAII
#SPECIES_OF_INTEREST<-c("CAME", "SCRU", "SCFO", "CTBI", "ACNF", "CTSR", "LUBO", "APFU", "TROB", "SATI", "NALI", "MYBE", "APVI", "CTCY", "BAUN", "CEFL", "PSPA", "CHAG", "CALU")  #MARIAN
SPECIES_OF_INTEREST<-c("ACNC", "CTCY", "CTMA", "APFU", "CALU", "CAAB", "CAME", "CEAR", "CEUR", "CEBI", "CEFL", "CELO", "CHMA",  "POIM", "PLDI", "STFA", "STAU", "THQU", "THAM", "PSBA", "SCRU", "SCFO", "SCTR", "MEVI", "BAUN", "MYBE", "SATI", "PAIN")  #PRIA  - BUT IS A LONG LIST
wd<-all.x[all.x$SPECIES %in% SPECIES_OF_INTEREST,]

#wd[is.na(wd$LMAX), c("SPECIES", "LMAX"),]

#remove tiddlers from this!
wd[wd$SIZE_<0.4*wd$LMAX,]$COUNT<-0

wd<-wd[,c("DIVER", "SPECIES", "SIZE_", "COUNT")]

divers<-levels(wd$DIVER); divers


#set count to max of 50 ... so this doesnt get absurdly huge and slow
wd[wd$COUNT>50,]$COUNT<-50
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



quartz()

#cast data so that all comparisons 
comps<-cast(compdata, SITE + REP ~ DIVER, value="SPECIESRICHNESS")
compsR<-comps
compsD<-comps
divers<-names(comps)[3:dim(comps)[2]]
n_divers<-dim(comps)[2]-2
n_sites<-dim(comps)[1]


for(i in 1:n_divers){
	diver_data<-comps[,divers[i]]
	buddy_data<-comps[,-c(1,2,i+2)]
	buddy_data<-rowMeans(buddy_data, na.rm=TRUE)
	compsR[,2+i]<-log(diver_data/buddy_data,2)
	compsD[,2+i]<-diver_data-buddy_data
#	assign(divers[i], diver_ratio[!is.na(diver_ratio)])
}

boxplot(compsR[3:dim(compsR)[2]])
boxplot(compsD[3:dim(compsD)[2]])



levels(r1$DIVER)<-c("BUDDY", levels(r1$DIVER))
divers<-unique(wd$DIVER)
#Go through diver by diver comparing their performance with their buddy
for(i in 1:length(divers))
{
	d<-divers[i]
	#which sites
	diver_sites<-unique(r1[r1$DIVER==d,]$SITE)
	
	xx<-r1[r1$SITE %in% diver_sites,]
	r1[r1$DIVER != d,]$DIVER<-"BUDDY"
	cast(r1, DIVER ~ "TotFish")
}


