rm(list=ls())
setwd("/Users/ivor.williams/Documents/CRED/CRCP/NCRMP/Report Card Workshop/Fish Indicators 2017")
# SET UP ------------------------------------------------------------------
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below

source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/fish_team_functions.R")
source("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/lib/Islandwide Mean&Variance Functions.R")

Modified_Site_Species_Richness<-function(x){  
  # Modification fos tandard Calc_Site_Species_Richness to not count species with zero counts (as they can be left in data file to ensure that the site has data records at all) 
  y<-aggregate(x$COUNT,by=x[,c("SITEVISITID", "METHOD", "REP", "SPECIES")], sum)	#convert to count per species per rep
  y[y$x>1,]$x<-1																	#convert any non-zero count to 1, so we can sum those to get total number of species with count>0 
  z<-aggregate(y$x,by=y[,c("SITEVISITID", "METHOD", "REP")], sum)  		            # count number of species with non-zero counts this REP	
  xx<-aggregate(z$x,by=z[,c("SITEVISITID", "METHOD")], mean)				  		# count number of entries per rep	
  dimnames(xx)[[2]]<-c("SITEVISITID", "METHOD", "SPECIESRICHNESS")
  
  return(xx)  
} # end Modified_Site_Species_Richness


sm<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/data/SITE MASTER2016.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE)
sectors<-read.csv("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/data/Sectors-Strata-Areas2016.csv", stringsAsFactors=FALSE)


# FISH REA WORKINGS ----------------------------------------------------------------
load("/Users/ivor.williams/Documents/CRED/Fish Team/FishPaste/fish-paste/data/ALL_REA_FISH_RAW.rdata")
x<-df

# HOUSEKEEPING ------------------------------------------------------------
# clean up the data to only fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION" , "REGION_NAME", "SECTOR", "SPECIAL_AREA", "EXCLUDE_FLAG",
"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH", 
"HARD_CORAL", "MA",  "TA",  "CCA",  "SAND",  "SOFT_CORAL", "CLAM" , "SPONGE", "CORALLIMORPH", "CYANO", "TUNICATE", "ZOANTHID" , "OTHER", "COMPLEXITY", "TRAINING_YN", "VISIBILITY",
"SPECIES", "COUNT", "SIZE_", "OBS_TYPE", 
"SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150", "MAX_HEIGHT",
"RANK", "SCIENTIFIC_NAME",  "TAXONNAME", "COMMONNAME", "GENUS", "FAMILY", "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
head(x[,DATA_COLS])
x<-x[,DATA_COLS]

# by default, remove sites with EXCLUDE_FLAG set to TRUE
x[is.na(x$TRAINING_YN),]$TRAINING_YN<-FALSE   # Training flag of NA is equivalent to a FALSE .. as none of the odler data was 'training data'
x<-subset(x, x$TRAINING_YN==FALSE)
x<-subset(x, x$EXCLUDE_FLAG==0, drop=TRUE)
x<-subset(x, x$OBS_TYPE %in% c("U","I","N", "F","T"))
#x<-subset(x, x$REGION=="SAMOA")
x<-subset(x, x$REGION != "CT")
x<-subset(x, x$METHOD %in% c("nSPC"))
x<-subset(x, x$OBS_YEAR >2009)
x<-subset(x, x$REEF_ZONE %in% c("Forereef", "Protected Slope"))
x<-subset(x, x$ISLAND!="South Bank")
#x<-subset(x, x$OBS_YEAR != 2016)

x$SITE<-SiteNumLeadingZeros(x$SITE)
x<-droplevels(x)

#add SEC_NAME to x  
# this would be better if SECTOR field in database was up to date properly .. rather than merge with the site_Sectors spreadsheet
x<-merge(x, sm[,c("SITE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_STRATA", "ANALYSIS_SCHEME")], by="SITE", all.x=TRUE)

#for ones that are missing SEC_NAME, set it to ISLAND
no_secs<-is.na(x$SEC_NAME)
tmp<-as.character(x$SEC_NAME)
tmp[no_secs]<-as.character(x[no_secs,]$ISLAND)
x$SEC_NAME<-tmp

table(x$SEC_NAME)

############################################################################################
# remove the component SUBSTRATE_HEIGHT fields
sh_out<-CalcMeanSHMeanSHDiff(x)
x$MEAN_SH<-sh_out[[1]]
x$MEAN_SH_DIFF<-sh_out[[2]]
x<-x[, setdiff(names(x),c("SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150"))]
############################################################################################
x<-droplevels(x)


#######################
## CLEAN UP NAs #######
#######################
x[is.na(x$LMAX),]$SPECIES
x[is.na(x$LMAX) & x$SPECIES=="GYMI",]$LMAX<-45
x[is.na(x$TROPHIC_MONREP) & x$SPECIES=="ABNO",]$TROPHIC_MONREP<-"PLANKTIVORE"

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
tmp.lev<-levels(x$RANK); head(tmp.lev)
levels(x$RANK)<-c(tmp.lev, "UNKNOWN")

x[is.na(x$HABITAT_CODE),"HABITAT_CODE"]<-"UNKNOWN"
x[is.na(x$SCIENTIFIC_NAME),"SCIENTIFIC_NAME"]<-"UNKNOWN"
x[is.na(x$COMMONNAME),"COMMONNAME"]<-"UNKNOWN"
x[is.na(x$GENUS),"GENUS"]<-"UNKNOWN"
x[is.na(x$FAMILY),"FAMILY"]<-"UNKNOWN"
x[is.na(x$COMMONFAMILYALL),"COMMONFAMILYALL"]<-"UNKNOWN"
x[is.na(x$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"
x[is.na(x$RANK),"RANK"]<-"UNKNOWN"

x[is.na(x$COUNT),]$COUNT<-0
x[is.na(x$SIZE_),]$SIZE_<-0

#fixing unknown lat/long from a sites survyeted by Val Brown in Guam in 2015. These values are probably close
# .. putting this in here so that we do not have NAs in the LAT and LONG .. but we do nto want to save these to the actual master data file
x[x$SITE=="GUA-01310",]$LATITUDE<-13.24173
x[x$SITE=="GUA-01310",]$LONGITUDE<-144.70428

wd<-droplevels(x)

wd$ANALYSIS_YEAR<-wd$OBS_YEAR
wd$ANALYSIS_STRATA<-paste(wd$REEF_ZONE, wd$DEPTH_BIN, sep="")


#island_table<-Aggregate_InputTable(wd, c("REGION","ISLAND"))
OTHER_BENTHIC<-c("CLAM", "CORALLIMORPH", "ZOANTHID", "TUNICATE", "SPONGE", "OTHER", "CYANO", "TA")
wd$OTHER_BENTHIC<-rowSums(wd[,OTHER_BENTHIC],na.rm=T)
SURVEY_SITE_DATA<-c("DEPTH", "HARD_CORAL", "SAND", "MA", "CCA", "MEAN_SH")

#NOTE THAT BENTHOS DOES NOT ALWAYS SUM TO 100% .. I THINK BECAUSE OF ERRORS IN THE ORIGINAL DATA ENTERED INTO THE DATABASE. NEW CODE BELOW IS AN ATTEMPT TO FIX THAT
# Go through all surveys checking for situation where some reps have NAs in a particular BENTHIC_FIELDS, but other records have non-zeros - in that situation, we were recording a field but one other diver left it balnk - those should be zeros not NAs
# this is something that should really be fixed in the database rather than here (as its an error at time of data entry)
#### BELOW code does the job, but should be cleaned up and put in a function
### IDW- COULD GREATLY SPEED THIS UP BY DOING IT FOR A REGION AND YEAR .. AND LIMIT TO ONLY nSPC
# i.e make the checking for some data and some NAs at the levels of a full survey round .. and also use indixes into the wd structure, rather than create temp dfs (tmp_data)
BENTHIC_FIELDS<-c("HARD_CORAL", "MA", "TA", "CYANO", "CCA", "SAND", "OTHER")
UNIQUE_ROUND<-c("REGION", "OBS_YEAR", "METHOD")
round_table<-Aggregate_InputTable(wd, UNIQUE_ROUND)

wd$countBD<-apply(wd[,BENTHIC_FIELDS], 1, function(xx) length(which(!is.na(xx))))  #IDW 10-22-2013 checking for situation where there is NO benthic data at all
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

#generate Lm values
#Fish Species Table
# RAYFINNED FISHES: log10(Lm) = -0.1189 + 0.9157 * log10(Lmax) (Binholand and Froese J Appl Ichthyology 2009)
# ELASMOBRANCHES: log10(Lm)  = -0.1246 + 0.9924 * log10(Lmax)
# [Carcharhinidae, Dasyatidae, Ginglymostomatidae, Myliobatidae]
#######
wd$Lm<-10^(-0.1189+(0.9157*log10(wd$LMAX)))
ELASMO<-c("Carcharhinidae", "Dasyatidae", "Ginglymostomatidae", "Myliobatidae")
wd[wd$FAMILY %in% ELASMO,]$Lm<-10^(-0.1246+(0.9924*log10(wd[wd$FAMILY %in% ELASMO,]$LMAX)))
wd<-droplevels(wd)
WD_SAVE<-wd


# WORKING WITH POOLING READY DATA FROM HERE ON  -------------------------------------
#base information about the survey - field names should match those in input file (obviously!)
UNIQUE_SURVEY<-c("SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

#get base survey info, calculate average depth+complexity+so on
SURVEY_INFO<-c("OBS_YEAR", "REGION", "REGION_NAME", "ISLAND", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE", "SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_YEAR", "ANALYSIS_STRATA", "EXCLUDE_FLAG", "SITEVISITID", "METHOD")
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)
#write.csv(survey_table, file="tmpSamoaSites.csv")
survey_est_benthos<-Calc_Site_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)   #Calc_Site_nSurveysArea deals better with situations where one REP has benthic data and other doesnt. 
surveys<-merge(survey_table, survey_est_benthos, by=UNIQUE_SURVEY)
write.csv(surveys, file="tmpSurveys2010_16.csv")

#Pull all species information into a separate df, for later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC", "TROPHIC_MONREP", "LW_A", "LW_B", "LMAX", "LENGTH_CONVERSION_FACTOR", "Lm")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)
write.csv(species_table, file="tmpSpeciesTable.csv")

# GENERATE SUMMARY METRICS --------------------------------------------------

## MEAN SIZE OF TARGET SPECIES # MUST BE 15cm OR LARGER AND AT LEAST 40% of LMAX ##########################################
wd<-WD_SAVE

MINSIZE_PROP_CUT_OFF<-0.3
BIO_RANK_CUT_OFF<-25
MIN_TL<-15
SPATIAL_BASE<-c("REGION","ISLAND")

#head(bio)
TARGET_FAMILIES<-c("Acanthuridae", "Mullidae", "Scaridae", "Holocentridae")  #Siganidae? Priacanthidae? Carangidae? Serranidae? Lutjanidae? Leethrinidae?
#TARGET_FAMILIES<-c("Scaridae")  #FOR PURPOSES OF GENERATING MANAGEABLE SPECIES FOR DEMONSTRATION
tgt_species<-unique(wd[wd$FAMILY %in% TARGET_FAMILIES & wd$LMAX > 30,]$SPECIES)											# Perhaps ALSO use species that are only present at more than X% of sites in The region?  BEST TO HAVE have defined list for each region

#LIMIT THIS TO TOP SPECIES .. drop records of species not in top ranked speices from bio sampling data
#bio<-bio[!bio$RANK>BIO_RANK_CUT_OFF,]   
wd[!wd$SPECIES %in% tgt_species,]$COUNT<-0
#remove fishes that are very small (do not want to penalize location for having large recruitment!, also recruitment variability will add noise)
wd[wd$SIZE_ < wd$LMAX*MINSIZE_PROP_CUT_OFF,]$COUNT<-0
wd<-wd[wd$COUNT>0,]
wd<-droplevels(wd)

#Calculate portion of fishes at a location that are 'mature' - JUST FOR INTEREST!
wd$MCOUNT<-wd$COUNT
wd[wd$SIZE_ > wd$Lm,]$MCOUNT<-0
tmp<-aggregate(wd[,c("COUNT","MCOUNT")],by=wd[,c(SPATIAL_BASE,"SPECIES")],FUN=sum)
tmp$PMAT<-round(tmp$MCOUNT/tmp$COUNT,2);tmp
cast(tmp, SPECIES ~ ISLAND, value="PMAT", sum, fill=NA)

#Drop fishes below a min size and Calculate mean size per species and locations of remaining observations
wd[wd$SIZE_ < MIN_TL,]$COUNT<-0
#see how many we have
#tmp<-droplevels(wd[wd$COUNT>0,])
#tmp<-aggregate(tmp$COUNT, by=tmp[,c("SPECIES","TAXONNAME"),],FUN=sum)
#names(tmp)<-c("SPECIES","TAXONNAME","CREDobs")
#tmp<-merge(tmp, bio, by="TAXONNAME",all.y=T); tmp
#compare mean size in obs with mean size in biosamples
#tmp2<-wd[wd$ISLAND=="Tutuila",]
#tmp2$COUNTLEN<-tmp2$COUNT*tmp2$SIZE_
#tmp3<-aggregate(tmp2[,c("COUNT", "COUNTLEN")], by=tmp2[,c("TAXONNAME","SPECIES", "ISLAND")],FUN=sum)
#tmp3<-tmp3[tmp3$COUNT>0,]
#tmp3$MEANSIZE<-tmp3$COUNTLEN/tmp3$COUNT
#tmp3
#tmp4<-merge(tmp3,tmp,by="TAXONNAME")'tmp4

Calc_MeanSize<-function(x, spatial_base=c("ISLAND", "REP_CARD_UNIT", "SEC_NAME", "ANALYSIS_STRATA"), min_obs=1){  

	base_cols<-c(spatial_base, "SPECIES")
	x$CS<-x$COUNT*x$SIZE_
	y<-aggregate(x[,c("COUNT", "CS")], by=x[,base_cols],FUN=sum)
	y<-y[!y$COUNT<min_obs,]
	y$MEAN_SIZE<-y$CS/y$COUNT

	return(y[,c(base_cols, "MEAN_SIZE")])	
} # end Calc__MeanSize

ms<-Calc_MeanSize(wd[wd$COUNT>0,],spatial_base=SPATIAL_BASE, min_obs=1); head(ms);dim(ms)
ms<-merge(ms, species_table[,c("SPECIES","TAXONNAME","Lm")],by="SPECIES", all.x=T)
#ms<-merge(ms, bio[,c("TAXONNAME","RANK")],by="TAXONNAME", all.x=T)
ms$SZ_LM<-ms$MEAN_SIZE/ms$Lm
tmp<-cast(ms, SPECIES + Lm ~ ISLAND, value="SZ_LM", sum, fill=NA)
head(tmp)
write.csv(tmp,file="tmpMeanSizeWORKINGS.csv")	

ave.ms<-aggregate(ms$SZ_LM, by=ms[,c(SPATIAL_BASE)],FUN=mean)
write.csv(ave.ms, file="AggMeanSizeAsPropofLM.csv")

ave.ms3<-ave.ms
ave.ms3[,"x"]<-ave.ms3[,"x"]^3
write.csv(ave.ms3, file="RCAggMeanSizeAsPropofLMCubed.csv")


##INSTANANEOUS BIOMASS####################################################################################################
wd<-WD_SAVE

## CALCULATE INSTANTANEOUS BIOMASS MINUS SHARKS AND JACKS
wd[!wd$OBS_TYPE %in% c("I"),]$COUNT<-0
SHARKS_JACKS<-c("Carangidae", "Carcharhinidae", "Ginglymostomatidae", "Sphyrnidae")
wd[wd$FAMILY %in% SHARKS_JACKS,]$COUNT<-0

r1<-Calc_Site_Bio(wd, "TROPHIC_MONREP"); tmp.cols<-dimnames(r1)[[2]][3:dim(r1)[2]]
r1$TotINSTFishNoSJ<-rowSums(r1[,tmp.cols])

wd_rich<-WD_SAVE
wd_rich[!wd_rich$OBS_TYPE %in% c("U", "I", "N")  ,]$COUNT<-0

#remove gobies and blennies and sp. species 9assuming they are mostly juveniles of species that are already counted)

#r2X<-Modified_Site_Species_Richness(wd_rich)

wd_rich[wd_rich$FAMILY %in% c("Blenniidae", "Gobiidae"),]$COUNT<-0
unique(wd_rich$RANK)
#wd_rich[!wd_rich$RANK %in% c("Species", "Subspecies"),]$COUNT<-0  # not doing this for us, as we have lots of species that are clearly unique, just not identified to species level, so sp. data are probably not species taht are otherwise present in the cylinder

r2<-Modified_Site_Species_Richness(wd_rich)

wsd<-merge(surveys, r1, by=UNIQUE_SURVEY)
wsd<-merge(wsd, r2, by=UNIQUE_SURVEY)
data.cols<-c(tmp.cols, "TotINSTFishNoSJ", "SPECIESRICHNESS", SURVEY_SITE_DATA)

write.csv(wsd, file="tmp NCRMP working site data.csv")

####################################################################################################################################################################
#
#     POOL UP
#
####################################################################################################################################################################
## Nearly always DOING THIS ONLY WITH nSPC data ####
wsd<-subset(wsd, wsd$METHOD=="nSPC")
wsd<-droplevels(wsd)

## check which ISLANDS differ between sectors and working data..
setdiff(unique(sectors$SEC_NAME), unique(wsd$SEC_NAME))
setdiff(unique(wsd$ANALYSIS_SEC), unique(sectors$SEC_NAME))
setdiff(unique(wsd$SEC_NAME), unique(sectors$SEC_NAME))

#FOREREEF ONLY AND GREATER THAN 2012
wsd<-wsd[wsd$REEF_ZONE %in% c("Forereef", "Protected Slope"),] 
#wsd<-wsd[wsd$OBS_YEAR>2012,] 
head(wsd)       

# DETERMINE THE BASIC STRATIFICATION WITHIN SECTORS - DEFAULT IS REEF_ZONE AND DEPTH_BIN, BUT THIS CODE ALLOWS PSSIBILITY OF CHOOSING ANOTHER
sectors$ANALYSIS_STRATA<-paste(sectors$REEF_ZONE, sectors$DEPTH_BIN, sep='')

#generate table to be able to relate ANALYSIS_SEC to REP_CARD_UNIT (as we have one-off reporting units in this case)
#rcu<-aggregate(wsd$METHOD, by=wsd[,c("ISLAND", "REP_CARD_UNIT", "ANALYSIS_SEC")], FUN=length)

#.... Make Sarigan-Guguan-Alamagan be a single 'ISLAND' 
# there MUST already be appropraite records in the sectors table for the new 'ISLAND' name, in this case will be "AGS"
levels(wsd$ISLAND)<-c(levels(wsd$ISLAND), "AGS")
wsd[wsd$ISLAND %in% c("Sarigan", "Guguan", "Alamagan"),"ISLAND"]<-"AGS"
sectors[sectors$ISLAND %in% c("Sarigan", "Guguan", "Alamagan"),"ISLAND"]<-"AGS"

wsd<-droplevels(wsd)

WSD_SAVED<-wsd
SECTORS_SAVED<-sectors

SPATIAL_POOLING_BASE<-c("REGION","ISLAND", "ANALYSIS_SEC", "ANALYSIS_STRATA", "REEF_ZONE")    
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE)

CURRENT_SCHEMES<-c("RAMP_BASIC", "MARI2014", "AS_SANCTUARY")   #IGNORING YEAR = SO GOING TO THE FINEST SCALE IN EACH REGION

for(i in 1:length(CURRENT_SCHEMES)){
	wsd<-WSD_SAVED
	sectors<-SECTORS_SAVED
	wsd$ANALYSIS_SEC<-as.character(wsd$SEC_NAME)

	CURRENT_SCHEME<-CURRENT_SCHEMES[i]
	
	sectors$ANALYSIS_SEC<-sectors[,CURRENT_SCHEME]
	# DETERMINE THE BASIC STRATIFICATION WITHIN SECTORS - DEFAULT IS REEF_ZONE AND DEPTH_BIN, BUT THIS CODE ALLOWS PSSIBILITY OF CHOOSING ANOTHER
	sectors$ANALYSIS_STRATA<-paste(sectors$REEF_ZONE, sectors$DEPTH_BIN, sep='')
	
	#now deal with those missing sectors - either rename ANALYSIS_SEC OR remove
	if(CURRENT_SCHEME=="RAMP_BASIC") {
#		wsd[wsd$ANALYSIS_SEC %in% c("PATI_PT_MPA", "ACHANG_MPA", "TUMON_BAY_MPA", "PITI_BOMB_MPA", "GUAM_MP_MINUS_ACHANG"),]$ANALYSIS_SEC<-"GUAM_MP"
#		#in this case removing 2014 ACHANG_MPA sites (The shorebased ones) and changing ANALYSIS_SEC for all other GUAM MPA sectors to the RAMP base one "GUAM_MP", also remove SAMOA 2015 sites, they will run in AS_SANCTUARY 2015 and Tutuila 2010&012
#		wsd<-wsd[!(wsd$ANALYSIS_SEC == "ACHANG_MPA" & wsd$ANALYSIS_YEAR==2014),]
		wsd<-wsd[wsd$ISLAND != "Guam",]
		wsd<-wsd[!wsd$REGION == "SAMOA",]
	}
	if(CURRENT_SCHEME=="MARI2014") {
		wsd[wsd$ANALYSIS_SEC %in% c("PATI_PT_MPA", "TUMON_BAY_MPA", "PITI_BOMB_MPA"),]$ANALYSIS_SEC<-"GUAM_MP_MINUS_ACHANG"
		wsd<-wsd[wsd$ISLAND %in% "Guam",]
	}
	if(CURRENT_SCHEME=="AS_SANCTUARY") {
		wsd<-wsd[wsd$REGION == "SAMOA",]
		wsd[wsd$ISLAND=="Tau",]$ANALYSIS_SEC<-"Tau"    # Pooling Tau together HERE - I Think that there is no difference in management (its not actually closed)   IF Tau IS IMPORTANT     CHECK CHECK CHECK 
	}	#in this case remove everything that isnt SAMOA surveyed in 2015
	
	##DETERMINE WHICH SITES HAVE ANALYSIS STRATA THAT ARE NOT IN THIS 
	analysis_secs<-unique(wsd$ANALYSIS_SEC)
	missing_secs<-unique(analysis_secs[!analysis_secs %in% unique(sectors$ANALYSIS_SEC)])
	if(length(missing_secs)>0) {
		cat("ANALYSIS SECTORS missing from this scheme:", missing_secs)
	}
	tmp<-aggregate(wsd[,"TotINSTFishNoSJ"],by=wsd[,c("REGION", "ISLAND", "ANALYSIS_SEC")], sum, na.rm=FALSE)  
	tmp[tmp$ANALYSIS_SEC %in% missing_secs,]
	
	### CHECK REPLICATION WITHIN STRATA
	tmp<-aggregate(wsd[,"METHOD"], by=wsd[,c(POOLING_LEVEL ,"SITE")], length)
	tmp<-aggregate(tmp[,"x"], by=tmp[,c(POOLING_LEVEL)], length)
	tmp<-merge(sectors, tmp[,c("ANALYSIS_SEC", "ANALYSIS_STRATA","x")],by=c("ANALYSIS_SEC", "ANALYSIS_STRATA"),all.y=TRUE)
	names(tmp)[names(tmp)=="x"]<-"n_sites"
	a<-cast(tmp, REGION + ISLAND + ANALYSIS_SEC ~ ANALYSIS_STRATA, value="n_sites", sum, fill=NA)
	a
	
	#clean up the sectors table so pool all sub sectors within a scheme into a total for this scheme's sectors
	sectors<-aggregate(sectors[,"AREA_HA"], by=sectors[,c(SPATIAL_POOLING_BASE)], sum)
	names(sectors)[names(sectors)=="x"]<-"AREA_HA"
	
	#################################################################################################################################
	############################################# NOW DO THE CALCAULTION OF WINHIN-STRATA AND POOLED UP DATA VALUES #################
	#################################################################################################################################
	
	ADDITIONAL_POOLING_BY<-c("METHOD")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)
	#generate within strata means and vars
	POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
	data.per.strata<-Calc_PerStrata(wsd, data.cols, POOLING_LEVEL)
	write.csv(data.per.strata,file=paste(CURRENT_SCHEME, "tmp strata data.csv", sep=""))
	#save(data.per.strata, file=paste(CURRENT_SCHEME, "strata_data.rdata", sep=""))
	
	###### REMOVE STRATA with N=1 (cannot pool those up)
	data.per.strata$Mean<-data.per.strata$Mean[data.per.strata$Mean$N>1,]
	data.per.strata$SampleVar<-data.per.strata$SampleVar[data.per.strata$SampleVar$N>1,]
	data.per.strata$SampleSE<-data.per.strata$SampleSE[data.per.strata$SampleSE$N>1,]
	
	# e.g. SAVE BY ISLAND AND REEF ZONE PER YEAR
	AGGREGATION_LEVEL<-c("REGION","ISLAND","ANALYSIS_SEC")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
	dp<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
	write.csv(dp,file=paste(CURRENT_SCHEME, "data_pooled_SEC.csv", sep=""))
	save(dp, file=paste(CURRENT_SCHEME, "data_pooled_SEC.rdata", sep=""))
	
	# e.g. SAVE BY ISLAND PER YEAR
	AGGREGATION_LEVEL<-c("REGION","ISLAND")       # Spatial Level to agggregate output data to (eg per REGION or per (REGION, ISLAND) etc... 
	dp<-Calc_Pooled(data.per.strata$Mean, data.per.strata$SampleVar, data.cols, AGGREGATION_LEVEL, ADDITIONAL_POOLING_BY, SPATIAL_POOLING_BASE, sectors)
	write.csv(dp,file=paste(CURRENT_SCHEME, "data_pooled_is.csv", sep=""))
	save(dp, file=paste(CURRENT_SCHEME, "data_pooled_is.rdata", sep=""))
	
}

#LOAD THE data per SCHEME
load("RAMP_BASICdata_pooled_is.rdata")
x<-dp
load("MARI2014data_pooled_is.rdata")
g<-dp
load("AS_SANCTUARYdata_pooled_is.rdata")
as<-dp


X<-x$Mean
G<-g$Mean
AS<-as$Mean
Mean<-rbind(X, G, AS)

X<-x$PooledSE
G<-g$PooledSE
AS<-as$PooledSE
PooledSE<-rbind(X, G, AS)

dp<-list(Mean, PooledSE)
names(dp)<-list("Mean", "PooledSE")

write.csv(dp, file="CREP2010_16 RC Data.csv")
save(dp, file="CREP2010_16 RC Data.RData")



#################################################################### TOW WORKUP ###########################################################################
# IDW - NEED TO ADD IN SOME CHECKING OF THE MISSING DEPTHS ... AND MAYBE FILTERING AND SO ON ... CAN LIFT CODE FROM REPORT CARD I THINK 


##save(df, file="ALL_TOW_FISH_RAW.rdata")
#write.table(df, file = "ALL_TOW_FISH_RAW.csv", sep = ",", col.names = NA, qmethod = "double")
load(file="/Users/ivor.williams/Documents/CRED/Fish Team/Base R/Base Data Files/ALL_TOW_FISH_RAW.rdata")

 
# FISH TOW WORKINGS -------------------------------------------------------
wtd<-df
#wtd<-subset(wtd, wtd$REGION=="SAMOA", drop=TRUE)
wtd<-subset(wtd, wtd$OBS_YEAR > 2009, drop=TRUE)

wtd<-droplevels(wtd)

wtd$biomass_g<-wtd$LW_A*wtd$COUNT*((wtd$SIZE*wtd$LENGTH_CONVERSION_FACTOR)^wtd$LW_B)

## drop any rows which have NOSC and MISS in the species field, these are tows which were aborted part way through
## remove these so that the tow length is corrected....
nosc<-which(wtd$SPECIES == "NOSC")
wtd<-wtd[-nosc,]

miss<-which(wtd$SPECIES == "MISS")
wtd<-wtd[-nosc,]

#wtd<-merge(wtd, tow_ns[,c("DIVEID", "RCU")], by="DIVEID", all.x=T)

length(unique(wtd$DIVEID))

wtd[is.na(wtd$COUNT),]$COUNT<-0
wtd[is.na(wtd$DEPTH),]$DEPTH<-0	
wtd[is.na(wtd$SIZE_),]$SIZE_<-0	
wtd[is.na(wtd$CENTROIDLAT),]$CENTROIDLAT<-0	
wtd[is.na(wtd$CENTROIDLON),]$CENTROIDLON<-0	

tmp.lev<-levels(wtd$REEF_ZONE); head(tmp.lev)
levels(wtd$REEF_ZONE)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wtd$FAMILY); head(tmp.lev)
levels(wtd$FAMILY)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wtd$TAXONNAME); head(tmp.lev)
levels(wtd$TAXONNAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(wtd$TROPHIC_MONREP); head(tmp.lev)
levels(wtd$TROPHIC_MONREP)<-c(tmp.lev, "UNKNOWN")

wtd[is.na(wtd$REEF_ZONE),"REEF_ZONE"]<-"UNKNOWN"
wtd[is.na(wtd$TAXONNAME),"TAXONNAME"]<-"UNKNOWN"
wtd[is.na(wtd$FAMILY),"FAMILY"]<-"UNKNOWN"
wtd[is.na(wtd$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"

wtd$REP_CARD_UNIT<-wtd$ISLAND
#levels(wtd$REP_CARD_UNIT)<-c(levels(wtd$REP_CARD_UNIT), "TUT_N", "TUT_S")
#wtd[wtd$ISLAND=="Tutuila",]$REP_CARD_UNIT<-wtd[wtd$ISLAND=="Tutuila",]$RCU

#summarize tow information (length, depth, lat-long, date)
#    first do that by segment
TOW_DATA<-c("REGION", "ISLAND", "REP_CARD_UNIT", "CENTROIDLAT", "CENTROIDLON", "DATE_", "DEPTH", "STARTLOCALTIME", "STRATA", "PROJECTEDLENGTH", "DIVEID")   

SEGMENT_ID2<-c( "DIVEID", "SEGMENT")
SEGMENT_INFO<-c("REGION", "ISLAND", "REP_CARD_UNIT", "DATE_", "OBS_YEAR")
SEGMENT_INFO_TO_SUM<-c("PROJECTEDLENGTH")
SEGMENT_INFO_TO_AVE<-c("CENTROIDLAT", "CENTROIDLON", "DEPTH")
SEGMENT_INFO_TO_MIN<-c("STARTLOCALTIME")
SEGMENT_INFO_TO_MODE<-c("REEF_ZONE")

SEGMENT_FIELDS<-c(SEGMENT_INFO, SEGMENT_INFO_TO_SUM, SEGMENT_INFO_TO_AVE, SEGMENT_INFO_TO_MODE, SEGMENT_ID2)
DIVE_INFO<-c("DIVEID", SEGMENT_INFO)

WTD_SAVE<-wtd
MIN_SIZE<-100
#only sharks > 1m
SHARKS<-c("Carcharhinidae", "Ginglymostomatidae")
wtd[is.na(wtd$SIZE_) | !wtd$FAMILY %in% SHARKS,]$SIZE_<-0
wtd[wtd$SIZE_< MIN_SIZE | !wtd$FAMILY %in% SHARKS,]$COUNT<-0
wtd[wtd$SIZE_< MIN_SIZE | !wtd$FAMILY %in% SHARKS,]$biomass_g<-0
levels(wtd$FAMILY)<-c(levels(wtd$FAMILY), "OTHER")
wtd[wtd$SIZE_< MIN_SIZE | !wtd$FAMILY %in% SHARKS,]$FAMILY<-"OTHER"
wtd<-droplevels(wtd)
sum(wtd[!is.na(wtd$COUNT),]$COUNT)

length(unique(wtd$DIVEID))


#clean up the data file ## adel comment: this creates 14 warnings.... ### return to this, extract numeric only columns
##- invalid for factors with NA entries
#wtd[is.na(wtd$COUNT),]$COUNT<-0
#wtd[is.na(wtd$biomass_g),]$biomass_g<-0
#wtd[is.na(wtd$BIOMASS),]$BIOMASS<-0
#wtd[is.na(wtd$BIOMASS_G_M2),]$BIOMASS_G_M2<-0

segment.info<-aggregate(wtd$COUNT, by=wtd[,SEGMENT_FIELDS], sum, na.rm=F)## aggregate sums total count of all fishes per record, using field_list 
segment.info<-segment.info[,SEGMENT_FIELDS] # drop the count - was just using that to generate a summary table

length(unique(segment.info$DIVEID))
setdiff(wtd$DIVEID,segment.info$DIVEID)
	
#sum up to total length etc.. for the dive ID
#set depth, and centroid lat-long field to NaN if zero ... 
segment.info[segment.info$DEPTH==0,"DEPTH"]<-NaN
segment.info[segment.info$CENTROIDLAT==0,"CENTROIDLAT"]<-NaN
segment.info[segment.info$CENTROIDLON==0,"CENTROIDLON"]<-NaN

sum.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_SUM],by=segment.info[,DIVE_INFO], sum, na.rm=TRUE);
dimnames(sum.segments)[[2]]<-c(DIVE_INFO, SEGMENT_INFO_TO_SUM)
ave.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_AVE],by=segment.info[,DIVE_INFO], mean, na.rm=TRUE)  
med.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_AVE],by=segment.info[,DIVE_INFO], median, na.rm=TRUE)  
mode.segments<-aggregate(segment.info[,SEGMENT_INFO_TO_MODE],by=segment.info[,DIVE_INFO], Mode)
dimnames(mode.segments)[[2]]<-c(DIVE_INFO, SEGMENT_INFO_TO_MODE)

tt<-merge(ave.segments, mode.segments[,c("DIVEID",SEGMENT_INFO_TO_MODE)], by="DIVEID")
dive.info<-merge(tt, sum.segments[,c("DIVEID",SEGMENT_INFO_TO_SUM)], by="DIVEID")
dim(dive.info)
write.csv(dive.info, file="tmp Tows.csv")
############################################################
### Now sum abundance and biomass data per species per dive,
### and convert to gm2 and abund m2
############################################################

#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","FAMILY", "TAXONNAME", "TROPHIC_MONREP")
t.species.table<-aggregate(wtd$COUNT,by=wtd[,FISH_SPECIES_FIELDS], sum, na.rm=FALSE)

sum.abund.bio<-aggregate(wtd[,c("COUNT", "biomass_g")],by=wtd[,c("DIVEID", "FAMILY")], sum, na.rm=TRUE)
dim(sum.abund.bio)
t.fish.data<-merge(sum.abund.bio, dive.info[,c("DIVEID","PROJECTEDLENGTH")], by="DIVEID")
t.fish.data$BIOGM2<-t.fish.data$biomass_g / (10*t.fish.data$PROJECTEDLENGTH)
t.fish.data$ABUN2<-t.fish.data$COUNT / (10*t.fish.data$PROJECTEDLENGTH)
dim(t.fish.data)
## add consumer group to tow data, filter to forereef ONLY, add depth to give option to later filter by depth range .. then pool up by island & year and save SE

# add consumer group (and family, inc ase it is useful later) to t.fish.data
#x.fish.data<-merge(t.fish.data, t.species.table[, FISH_SPECIES_FIELDS], by="SPECIES")
# add data about the tow (island, zone, year, depth)
x.fish.data<-merge(t.fish.data, dive.info[, c("DIVEID", "REGION", "ISLAND", "REP_CARD_UNIT", "REEF_ZONE", "OBS_YEAR", "DEPTH")], by="DIVEID")
dim(x.fish.data)
write.csv(x.fish.data, file="TMPtowData.csv")


#filter out forereef tows only...!!!!!
#x.fish.data<-subset(x.fish.data, x.fish.data$REEF_ZONE=="Forereef", drop=TRUE)
t.fish.data<-(x.fish.data)

xx<-aggregate(t.fish.data$ABUN2, by=t.fish.data[,c("DIVEID", "REGION", "ISLAND", "REP_CARD_UNIT", "OBS_YEAR", "REEF_ZONE", "FAMILY")], sum, na.rm=TRUE)
dimnames(xx)[[2]]<-c("DIVEID", "REGION", "ISLAND", "REP_CARD_UNIT", "YEAR", "STRATA", "FAMILY", "ABUN2")
#now format this more or less as a crosstab, with field of interest as column variable
t.fish.abund<-cast(xx, DIVEID + REGION + ISLAND + REP_CARD_UNIT + YEAR + STRATA ~ FAMILY, value="ABUN2", fill=0)
t.fish.abund$TotSharkAbund<-rowSums(t.fish.abund[,levels(xx$FAMILY)])  
SHARK_COLS<-c("Carcharhinidae", "TotSharkAbund")
dim(t.fish.abund)
#aggregate - average per island/strata/year
t.fish.island.mean<-aggregate(t.fish.abund[,SHARK_COLS],by=t.fish.abund[,c("REGION", "ISLAND", "REP_CARD_UNIT", "STRATA", "YEAR")], mean, na.rm=TRUE)
t.fish.island.n<-aggregate(t.fish.abund[,SHARK_COLS],by=t.fish.abund[,c("REGION", "ISLAND", "REP_CARD_UNIT", "STRATA", "YEAR")], length)
t.fish.island.var<-aggregate(t.fish.abund[,SHARK_COLS],by=t.fish.abund[,c("REGION", "ISLAND", "REP_CARD_UNIT", "STRATA", "YEAR")], var, na.rm=TRUE)
t.fish.island.se<-sqrt(t.fish.island.var[,SHARK_COLS])/sqrt(t.fish.island.n$TotSharkAbund)

# add the N to the mean and se dfs before writing them
t.fish.island.mean$n<-t.fish.island.se$n<-t.fish.island.n$TotSharkAbund

write.csv(t.fish.island.mean, file="TMP tow_fish_shark abund100.csv")
write.csv(t.fish.island.se, file="tow_fish_shark abund_se.csv")

###################################################################
# using only 2010 onwards .. pool across any multiple years of surveys .. weighting each year's data equally
######## this is rough - but works for now! #############
island.data<-t.fish.island.mean
island.data<-subset(island.data, island.data$STRATA %in% c("Forereef", "Protected Slope"), drop=TRUE)
island.data<-subset(island.data, island.data$YEAR>2009, drop=TRUE)
island.data<-droplevels(island.data)
idw<-aggregate(island.data[,SHARK_COLS],by=island.data[,c("REGION","ISLAND", "REP_CARD_UNIT")], mean, na.rm=TRUE)
#convert abund in m2 to Ha
idw[,SHARK_COLS]<-idw[,SHARK_COLS]*10000
write.csv(idw, file="RCtow fish 2010on forereef equallyweighted.csv")




## GENERATE COUNTS PER REP FROM THE BASE WORKING DATA ####################################################################################################
wd<-WD_SAVE
## CALCULATE INSTANTANEOUS BIOMASS MINUS SHARKS AND JACKS
wd[!wd$OBS_TYPE  %in%  c("I", "U", "N"),]$COUNT<-0
wd<-droplevels(wd)
tmp<-cast(wd, OBS_YEAR + ISLAND + REP_CARD_UNIT + ANALYSIS_STRATA + LATITUDE + LONGITUDE + SITE + REP + REPLICATEID + DIVER  ~ SPECIES, value="COUNT", sum, fill=0); head(tmp)

write.csv(tmp, file="tmp AS Counts data.csv")




































