# Calculate variance based off old functions using Ault Formulas .. but modified to not use secondary unit variance (there is not real replication at that level) and assuming different input structure - more in keeping with structure of data coming directly from REA_FISH_BASE and standard manipulations
#


# gdata library inclues the drop.levels() function used below
library(gdata)

#############################################################################################################################
# Function reads a strata areas csv, and converts those to weightings per strata per island, and returns a matrix with weights
# Assumes data structure looks a bit like this ..
# island	strata1	strata2	strata3 etc..
# Guam		1230	3444	98
# Saipan	245		1990	4450
# output file has same structure and names, but with areas converted to weights ..
# hence island names, and strata names must match thsoe used in data files for subsequent functions to work
# nb also requires there to be a file "tmpAreaPerStrata.csv" in the working directory
#############################################################################################################################
GetIslandWeights <- function ()
{
	Wh.data = read.csv("tmpAreaPerStrata.CSV")              # Habitat Per Strata Per Island 
	Wh<-Wh.data                                             # Convert that to weights i.e. - proportion of total habitat per island by strata (total comes to 1 each row)
	Wh[,2:(dim(Wh.data)[2]-1)]<- Wh.data[,2:(dim(Wh.data)[2]-1)]/rowSums(Wh.data[,2:(dim(Wh.data)[2]-1)])

# IDW need to convert these to same format as calculated data files e.g. 2 dimensional array ("Island" "Strata") with data being strata weight
	n.islands<-dim(Wh)[1]
	n.strata<-dim(Wh)[2]-1
	island.names<-Wh[1]
	strata.names<-list(names(Wh)[2:dim(Wh)[2]])
	output.array<-array(dim=c(n.islands, n.strata), data=0, dimnames=c(island.names, strata.names))
	for (i in 1:n.islands){
		for (j in 1:n.strata) {
			output.array[i,j]<-Wh[i,j+1] 	
		}
	}
	 	
	return(output.array)             
	
} # end GetIslandWeights


#############################################################################################################################
# Function calculate stderr - this didnt seem to be in the basic R library
#############################################################################################################################
stderr <- function(x) sqrt(var(x)/length(x))

#############################################################################################################################
# Function calculates primary unit variance
# Calculate primary unit variance - this is based exactly on Jerry's Ault's Formula on p95 of the NWHI report, and seems to return same as var()
# Eq 4.20
#############################################################################################################################
pu.var <- function (x)
{
  x.tot<-0
  x.len <- length(x)
  x.ave<-mean(x)
  for (i in 1: x.len)
  {
    x.tot<-x.tot+((x[i]-x.ave)^2)
  }
  return (x.tot/(x.len-1))
} # end pu.var

############################################################################################################################
# Function uses the standard sector areas table (area per hectare per strata per sector per island) and a pooling_fields parameter, and generates an output that is total_area and total_area_with_samples (ie where there is data at that pooling_level) 
#
# for example .. sectors input table, currently has fields REGION, ISLAND, SEC NAME, DEPTH_ZONE, REEF_ZONE, AREA_HA
# .... assume pooling_fields of c("REGION", "ISLAND", "SEC_NAME" , "DEPTH_BIN", "REEF_ZONE") 
#       and that means_data contains all the pooling fields, plus a variable "N", followed by various data_cols
#
#     output will have one record per each level of the pooling_fields with TotalArea (sum of AREA_HA for all sectors with that output_level) and TotalAreaWithSamples (sum of AREA_HA for all sectors records with that output_level and for which there are also records in means_data)
#
#################################################################################################################################
Calc_SampleAreas<-function (means_data, pooling_fields=c("REGION", "ISLAND", "SEC_NAME", "REEF_ZONE","DEPTH_BIN"), strata_weights)
{
#means_data<-data.per.strata$Mean
#pooling_fields<-c("REGION", "ISLAND", "SEC_NAME", "REEF_ZONE","DEPTH_BIN")
#strata_weights<-sectors

	#aggregate the means data and the sectors data to match the output_level and strata_fields
	tmp.areas<-aggregate(list(TotalArea=strata_weights$AREA_HA), by=strata_weights[,pooling_fields],sum)
	tmp.means<-aggregate(list(N=means_data$N), by=means_data[,pooling_fields],sum)

	#now calculate the actual area per output_level that there are means_data for
	
	tmp.areas<-merge(tmp.areas, tmp.means, by=pooling_fields, all.x=TRUE)
	tmp.areas$TotalAreaWithSamples<-0
	tmp.areas[!is.na(tmp.areas$N),]$TotalAreaWithSamples<-tmp.areas[!is.na(tmp.areas$N),]$TotalArea
	
	return(tmp.areas)
  
} #end Calc_SampleAreas



############################################################################################################################
# Function FieldConcat concatenates columns in a dataframe .. input is the data frame x, plus a list of field names (which can be any length, including length 1) .. all values in field_names must be names of variables in the df x
#   I know this seems a very basic function, but I couldnt find an existing R version
#################################################################################################################################
FieldConcat<-function (x, field_names)
{
	if(length(field_names)>1)
	{
		out.val<-apply(x[,field_names], 1, FUN=function(x){return(paste(x,collapse=""))})
	
	} else {
		out.val<-x[,field_names]
	}
	return(out.val)
} # FieldConcat



############################################################################################################################
# Function calculates weighted mean and SE for data pooled up at level of aggregation_level
# Requires input of dfs with means and sample variance data per strata, data_cols (names of data columns), output_level (eg level to pool data up to [eg if we have data for REGION, ISLAND, REEF_ZONE and DEPTH_BIN, we can pool up to REGION or to c(REGION, ISLAND)]), 
#   The parameter pooling_fields (is the combination of fields used to build the means_data and the vars_data and also the habitat_areas df .. ie are the fields that defined the base strata level
#   code is written with the assumption that structure of means, vars, and habitat_areas dfs are the same, although habitat_areas can contain strata for which there are no means or vars data.
#       means and vars data dfs must contain data_cols, pooling_fields and a column "N" (number of samples that strata)  
#       habitat_areas must contain the fields pooling_fields, and a variable "AREA_HA" (area in hectares)
#       ouput_level fields must be fields within pooling_fields
#    .. in addition, this function will calculate total area that has samples AND total area of all matching reords in the habitat_areas input table 
#    ........ reason for that is to provide a means to show when there are no means and vars data but there is a habitat_area record ... eg there could be a sector within an island that we have no samples from, or a strata within a sector that has no data
# returns a list with 2 dfs, pooled_means and pooled_se whcih is weighted and pooled mean and sampleSE data
#################################################################################################################################
Calc_Pooled<-function (means_data, var_data, data_cols, aggregation_level=c("REGION","ISLAND"), by_fields=c("METHOD", "OBS_YEAR"), pooling_fields=c("REGION", "ISLAND", "SEC_NAME", "REEF_ZONE","DEPTH_BIN"), habitat_areas_table)
{

#means_data<-data.per.strata$Mean
#var_data<-data.per.strata$SampleVar
#data_cols<-data.cols
#aggregation_level<-AGGREGATION_LEVEL
#by_fields<-ADDITIONAL_POOLING_BY
#pooling_fields<-SPATIAL_POOLING_BASE
#habitat_areas_table<-sectors


GRID_CELL_SIZE<-50*50   #grid cells are 50*50
AREA_UNITS<-100*100     # units of the area numbers in csv file are hectares


	#Generate data frame to hold total area for each of the pooling_fields i.e sums total area of all records that match the pooling_fields
	habitat_areas<-aggregate(list(TotalArea=habitat_areas_table$AREA_HA), by=habitat_areas_table[,pooling_fields], sum)             # this is the size of each of the base sample areas [eg szie of FOREREEF within ISLAND within REGION


	#Create "OL" "AL" amd "PL" fields, which concatentates the various sets of fields above .. will make it much easier to match between the different data structures
	output_level<-c(aggregation_level, by_fields)
	means_data$OL<-FieldConcat(means_data,output_level)
	var_data$OL<-FieldConcat(var_data,output_level)

	means_data$AL<-FieldConcat(means_data,aggregation_level)
	var_data$AL<-FieldConcat(var_data,aggregation_level)
	habitat_areas$AL<-FieldConcat(habitat_areas,aggregation_level)
	
	means_data$PL<-FieldConcat(means_data,pooling_fields)
	var_data$PL<-FieldConcat(var_data,pooling_fields)
	habitat_areas$PL<-FieldConcat(habitat_areas,pooling_fields)

	#add the right habitat area to the means_data and var_data record (matching on pooling_fields) THIS ASSUMES THERE IS no more than one habitat_area record for each means_data record 
	# which will be the case if habitat_area and means_data were both built using those same pooling fields
	means_data<-merge(means_data, habitat_areas[,c(pooling_fields, "TotalArea")], by=pooling_fields, all.x=T)
	var_data<-merge(var_data, habitat_areas[,c(pooling_fields, "TotalArea")], by=pooling_fields, all.x=T)

	
	#Generate output structures
	pooled.means<-aggregate(means_data[,c("N", data_cols)],by=means_data[,c(output_level, "OL", "AL")],sum)
	pooled.means[,data_cols]<-NA
	pooled.se<-pooled.means
	
	#generate a simplifed habitat_area output table, pooled at the output_level, to be passed out of the function
	hab.areas.out<-aggregate(list(TotalArea=means_data$TotalArea), by=means_data[,c(output_level,"OL", "AL")], sum)
#	hab.areas.out$TotalAreaWithSamples<-0
	

	#go through the output structures row by row pooling and weighting the mean and vars data for that output level
	for(i in 1:dim(pooled.means)[1]){
		md<-subset(means_data, means_data$OL == pooled.means[i,"OL"])		
		vd<-subset(var_data, var_data$OL == pooled.means[i,"OL"])
		
		tot.area<-sum(md$TotalArea)
#		hab.areas.out[hab.areas.out$OL==pooled.means[i,"OL"],]$TotalAreaWithSamples<-tot.area
		md$wt<-md$TotalArea / tot.area
		vd$wt<-vd$TotalArea / tot.area
		
		vd$pctSampled<-(vd$N*GRID_CELL_SIZE)/(vd$TotalArea*AREA_UNITS)

    	#TMP FIDDLE, SHOULD NEVER HAPPEN, BUT pctSampled should not be above 1
    	if(max(vd$pctSampled, na.rm=T)>1)
    	{
    		cat("pctSampled is greater than 1: ", max(vd$pctSampled))
    		cat(" Sample is ", vd[vd$pctSampled==max(vd$pctSampled),]$OL, vd[vd$pctSampled==max(vd$pctSampled),]$PL)
    		cat(" N is ", vd[vd$pctSampled==max(vd$pctSampled),"N"])
    		cat(" Area(Ha) is ", vd[vd$pctSampled==max(vd$pctSampled),"TotalAreaWithSamples"])
    		vd[vd$pctSampled>1,]$pctSampled<-1
    	}

		#now do the weighting ...
		pooled.means[i,data_cols]<-colSums(md[,data_cols]*md$wt)  
		# multiply sample variance values by square of strata weight and divided by number of samples this strata, and adjust for proportion of total area sampled (I am using Krebs formula 8.18 p276)
		pooled.se[i,data_cols]<-colSums(vd[,data_cols]*((vd$wt^2)/vd$N)*(1-vd$pctSampled))     #IDW this is effectively variance of sample means for entire domain
		pooled.se[i,data_cols]<-sqrt(pooled.se[i,data_cols])                                   #now converting this to standard deviation of sample means for entire domain (=equiv to sample SE)
	}


	#also generate a total area table for total area at the level that 
	habitat_areas_table$AL<-FieldConcat(habitat_areas_table,aggregation_level)
	AL_habitat_area<-aggregate(list(TotalArea=habitat_areas_table$AREA_HA), by=habitat_areas_table[,c(aggregation_level, "AL")], sum)		# this is the total area of habitat at the aggregation level (eg all habitat within the REGION or the ISLAND within REGION or whatever)
	hab.areas.out$TotalAreaWithSamples<-hab.areas.out$TotalArea
	hab.areas.out$TotalArea<-NULL
	hab.areas.out<-merge(hab.areas.out,AL_habitat_area,by=c(aggregation_level, "AL"),all.x=TRUE)

	#add the area sampled and total area to pooled.means using merge
	pooled.means<-merge(pooled.means, hab.areas.out[,c("OL", "TotalArea", "TotalAreaWithSamples")], by="OL", all.x=TRUE)

	#order pooled.means and pooled.se by OL, then delete that field	
	pooled.means<-pooled.means[order(pooled.means$OL),]
	pooled.se<-pooled.se[order(pooled.se$OL),]
	pooled.means$OL<-pooled.se$OL<-NULL
	pooled.means$AL<-pooled.se$AL<-NULL

  	out.x<-list(pooled.means, pooled.se)
  	names(out.x)<-list("Mean", "PooledSE")
  	return(out.x)
  
} #end Calc_Pooled


#############################################################################################################################
# Function calculates mean, variance and N for passed-in data_cols at whatever requested pooling_level (can be single or multiple fields)
	# This funcrion is similat to Calc_PooledStrata_MeanVarianceCount, but is much less restricted .. allows user to pool at any level 
		# SITEVISITID is base sample level (ie generally a SURVEY DIVE)
	# returns a list with three dfs: - 1st df is means per data col per strata, 2nd is var, 3rd is SE
#############################################################################################################################
Calc_PerStrata <- function (sample_data, data_cols, pooling_level = c("ISLAND", "STRATA", "OBS_YEAR", "METHOD"))
{
  BASE_DATA_COLS<-c(pooling_level, "SITEVISITID", data_cols)
  
  #first clean up sample data to just have the data that will be used for generating islandwide mean, var, N values (per strata, method, year)
  sample_data<-sample_data[,BASE_DATA_COLS]
  
  # Calculate aggregate Mean, Var, N	
  strata.means<-aggregate(sample_data[,data_cols],by=sample_data[,pooling_level], mean)
  strata.vars<-aggregate(sample_data[,data_cols],by=sample_data[,pooling_level], var)
  N<-aggregate(sample_data[,"SITEVISITID"],by=sample_data[,pooling_level], length)$x
  strata.means$N<-strata.vars$N<-N
  strata.se<-strata.vars
  strata.se[,data_cols]<-sqrt(strata.vars[,data_cols])/sqrt(N)
  
  out.x<-list(strata.means[,c(pooling_level,"N", data_cols)], strata.se[,c(pooling_level,"N", data_cols)], strata.vars[,c(pooling_level,"N", data_cols)])
  names(out.x)<-list("Mean", "SampleSE", "SampleVar")
  return(out.x)
  
}
# end Calc_PerStrata


############################################################################################################################
# Function calculates weighted mean and SE for data pooled up at level of output_level
# Requires input of df with means, df with sample variance data, data_cols (names of data columns), output_level (eg pool up to this level), strata_field (the strata within the output_level), and wh (a weights table)
#       data_cols, output_level, strata_fields and a column "N" (number of samples that strata) must be within the means and SE dfs
#       wh must have weights per strata within the target output_level (and field names must be identical) 
#       NB format of wh must be like a crosstab output with values of the strata_field as column variable, output_level as row variables, area in whatever units as the value field)
#
#    .. note also that this function will fall over if Wh is missing information .. i.e. if there are pooling or strata levels in the data input tables that arent in Wh 
#    .. in addition, this function will ignore strata in Wh that have no associated data in the means and variance inputs (eg if there is a strata 'OTHER" with no samples, then the ouput_level data will not include that strata)
#    IDW would be good if this also generated an output total area per output_level df - IDW NOT IMPLEMENTED YET
# returns a list with 2 dfs, each with same columns as in the input files 1st df has pooled means for data_cols, 2nd has pooled se's
#################################################################################################################################
Calc_Weighted<-function (means_data, var_data, data_cols, output_level=c("REGION","ISLAND"), strata_field="STRATA", wh)
{
  
GRID_CELL_SIZE<-50*50   #grid cells are 50*50
AREA_UNITS<-100*100     # units of the area numbers in csv file are hectares
   
	#in means_data, var_data, and wh .. do not treat strings as factors .. NB this is a rather crude fix, it would probably be better if they were not created as factors to being with
	for(i in 1:length(output_level))
	{
		means_data[,output_level[i]]<-as.character(means_data[,output_level[i]],stringsAsFactors=FALSE)
		var_data[,output_level[i]]<-as.character(var_data[,output_level[i]],stringsAsFactors=FALSE)
		wh[,output_level[i]]<-as.character(wh[,output_level[i]],stringsAsFactors=FALSE)
	}

	# CREATE 'OL' variable in means_data, var_data, and wh ... that will be a single field that is a concatenation of the various fields in output_level (so that matching beteween eg means_data and wh rows becomes possible)
	means_data$OL<-var_data$OL<-"NONE"
	wh$OL<-"NONE"
	for(i in 1:dim(means_data)[1]) 
	{
		means_data[i,]$OL<-paste(means_data[i, output_level], collapse="")
		var_data[i,]$OL<-paste(var_data[i, output_level], collapse="")
	}
	for(i in 1:dim(wh)[1])
	{
		wh[i,]$OL<-paste(wh[i, output_level], collapse="")
	}
 
 	#create structure weighting.data to put weighting data into
  	weighting.data<-means_data[,c(output_level, "OL", strata_field, "N")]
  	names(weighting.data)<-c(output_level, "OL", strata_field, "Area")
  	num.rows<-dim(weighting.data)[1]

  	for(i in 1:num.rows){
    	x.unit<-weighting.data[i,"OL"]
    	x.strata<-as.vector(weighting.data[i,strata_field])
    	weighting.data[i,"Area"]<-wh[wh[,"OL"]==x.unit,x.strata]   
	}
  
	#calculate total area sampled per survey year, per island, per methods
	TotAreaSampled<-aggregate(weighting.data[,"Area"],by=weighting.data[,c(output_level,"OL")], sum)
	names(TotAreaSampled)<-c(output_level, "OL", "TotArea")

	#also calculate total area in all strata at that output_level (whether or not there are samples there)
	sw.names<-names(wh)
	wh$TotAreaALL<-rowSums(wh[,(length(output_level)+1):(length(sw.names)-1)])
	TotAreaSampled$TotAreaALL<-TotAreaSampled$TotArea	
	# add TotAreaALL to TotAreaSampled
	for(i in 1:dim(TotAreaSampled)[1])
	{
		TotAreaSampled[i,]$TotAreaALL<-wh[wh$OL==TotAreaSampled[i,]$OL,"TotAreaALL"]
	}

	#generate structure for actual weights
	weightings<-weighting.data
	names(weightings)<-c(output_level, "OL", strata_field, "StrataWeight")
  	for(i in 1:num.rows){
    	x.unit<-as.vector(weighting.data[i, "OL"])
    	x.strata<-as.vector(weighting.data[i, strata_field])  
    
    	x.tot.area<-TotAreaSampled[TotAreaSampled[,"OL"]==x.unit, ]$TotArea    
    	
		#calculate proportion of area sampled
    	x.n<-means_data[means_data[,"OL"]==x.unit & means_data[,strata_field]==x.strata, "N"]
    	x.f<-x.n/(weighting.data[i, "Area"]*AREA_UNITS/GRID_CELL_SIZE)
    	#TMP FIDDLE, SHOULD NEVER HAPPEN, BUT x.f should not be above 1
    	if(x.f>1)
    	{
    		cat("x.f is greater than 1: ", x.f)
    		cat("Sample is ", x.unit, x.strata)
    		cat("N is ", x.n)
    		x.f<-1
    	}
    	
    	weight<-weighting.data[i,]$Area/x.tot.area
    	weightings[i,"StrataWeight"]<-weight	
    
    	# multiple means this strata by correct strata weight 
    	strata.means<-(means_data[means_data[,"OL"]==x.unit & means_data[,strata_field]==x.strata, data_cols])
    	means_data[means_data[,"OL"]==x.unit & means_data[,strata_field]==x.strata, data_cols]<-strata.means*weight
    	
    	# multiply sample variance values by square of strata weight and divided by number of samples this strata, and adjust for proprotion of total area sampled (using Krebs formula 8.18 p276)
    	strata.sample.vars<-(var_data[var_data[,"OL"]==x.unit & var_data[,strata_field]==x.strata, data_cols])
    	strata.mean.vars<-(strata.sample.vars*((weight^2)/x.n))*(1-x.f)
    	var_data[var_data[,"OL"]==x.unit & var_data[, strata_field]==x.strata, data_cols]<-strata.mean.vars  
  	}
  
  	#sum means data per island, method, srvy.yr and do same for var_data BUT square root the sum of the weighted sevar_data_data to get pooled_SE
  	xxx<-names(means_data)
  	output.cols<-xxx[!xxx %in% c(data_cols, strata_field, "N")]
  	pooled.means<-aggregate(means_data[,c("N", data_cols)],by=means_data[,c(output.cols)], sum)
  	pooled.se<-aggregate(var_data[,c("N", data_cols)],by=var_data[,c(output.cols)], sum)  #IDW this is effectively variance of sample means for entire domain
  	pooled.se[,data_cols]<-sqrt(pooled.se[,data_cols])	                                # converting this to standard deviation of sample means for entire domain (=equiv to sample SE)

 	#sort means, se, and TotArea by OL 
 	pooled.means<-pooled.means[order(pooled.means$OL), ]
 	pooled.se<-pooled.se[order(pooled.se$OL), ]
 	TotAreaSampled<-TotAreaSampled[order(TotAreaSampled$OL), ]
	
	pooled.means$OL<-pooled.se$OL<-NULL
	TotAreaSampled$OL<-NULL
	 
  	out.x<-list(pooled.means, pooled.se, TotAreaSampled)
  	names(out.x)<-list("Mean", "Pooled SE", "TotAreaSampled")
  	return(out.x)
  
} #end Calc_Weighted





