setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs")
rm(list=ls())

## fish status report standard figures
## island section
library(ggplot2)
library(reshape)
library(gdata)
library(grid)

## data for island graphs - FOREREEF ONLY

load("MONREPdata_pooled_is_yr_RZ.rdata") ### island estimates mean over cruises since spc after 2007
rd<-as.data.frame(data_pooled_is_yr)

# change year to factor and OBS_YEAR to match code
rd$Mean.OBS_YEAR<-as.factor(rd$Mean.ANALYSIS_YEAR)

## data for region reference lines
load("MONREPdata_pooled_reg_FRF.rdata")
ref<-as.data.frame(data_pooled_reg) # forereef only reference

# NO ref lines for lagoon and backreef

# for pooled benthic data FOREREEF ONLY 
load("MONREPdata_pooled_is_RZ.rdata")
ben<-as.data.frame(data_pooled_is)


# batch fish benthic graphs -----------------------------------------------

rdd<-rd
## with the reference lines, need to run each region separately...

# MAINS -----------------------------------------------

## create reference lines for region
# consumer group 
mhi_ref_cons<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI",c("Mean.PRIMARY","Mean.SECONDARY","Mean.PLANKTIVORE","Mean.PISCIVORE")]))

# for biomass of parrots in 2 size classes
mhi_ref_sc<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI",c("Mean.10_30","Mean.30_plus")]))

# for  all fish biomass
mhi_ref_tb<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI",c("Mean.TotFish")]))

# mean size
mhi_ref_sz<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI","Mean.MEAN_SIZE"]))

## references lines for plots, use ..._m (mean), sep (m plus se) and sem (m minus se)
hline.data.MHI_cons_m <- data.frame(z = mhi_ref_cons, Consumergroup = c("Pri. cons.","Sec. cons.","Planktivores","Piscivores")) ## needs to match the graph type

hline.data.MHI_sc_m <- data.frame(z = mhi_ref_sc, Size_class = c("Parrots 10-30 cm", "Parrots >30 cm")) ## needs to match the graph type

hline.data.MHI_tb_m <- data.frame(z = mhi_ref_tb, Size_class = "All fishes") ## needs to match the graph type

hline.data.MHI_sz_m <- data.frame(z = mhi_ref_sz, mean_size = "All fishes mean size") ## needs to match the graph type

## subset graphing data

## with the reference lines, need to run each region separately...
rd<-subset(rdd, Mean.REGION == "MHI")

### need to manually change the loop code below to pull the correct reference lines for each region

max(rd$Mean.TotFish+rd$PooledSE.TotFish) ## check to manually set the ylims on graphs ## set at 70
max(rd$Mean.MEAN_SIZE+rd$PooledSE.MEAN_SIZE) # 25
max(rd$Mean.PRIMARY+rd$PooledSE.PRIMARY) ## set at 40
max(rd$Mean.10_30+rd$PooledSE.10_30)
max(rd$Mean.30_plus+rd$PooledSE.30_plus) # set at 6
max(rd$Mean.SECONDARY+rd$PooledSE.SECONDARY)
max(rd$Mean.PISCIVORE+rd$PooledSE.PISCIVORE)
max(rd$Mean.PLANKTIVORE+rd$PooledSE.PLANKTIVORE)


# save graphs in figures folder
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures")
## batch make the graphs
for(i in 1:length(rd$Mean.ISLAND)){
  
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  #TEST
  #data<-subset(rd, Mean.ISLAND == "Kahoolawe")
  #data<-drop.levels(data)
  
  ##########  mean size graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
  levels(dt$mean_size)<-"All fishes mean size"
  #levels(dt$Year)<-c( "2009",   "2010"  , "2011" ,  "2011.1", "2012" ,  "2014",   "2014.1" ,"2015","2016") 
   #dt$Year<-as.character(dt$Year)
    #dt[dt$Year %in% "2011.1",]$Year<-"2010 & 2012"
    #dt[dt$Year %in% "2014.1",]$Year<-"2013 & 2015"
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"MEAN SIZE" 
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    geom_hline(aes(yintercept = z), hline.data.MHI_sz_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 25))+
    theme(axis.text.x = element_text(angle = 25, hjust = 1))+
    #theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    theme(legend.position = "none")+
    labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))
  
  ##########  PARROT size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
  dt$Size_class<-drop.levels(dt$Size_class)
  #levels(dt$Year)<-c( "2009",   "2010"  , "2011" ,  "2011.1", "2012" ,  "2014",   "2014.1" ,"2015"  ) 
  #dt$Year<-as.character(dt$Year)
  #dt[dt$Year %in% "2011.1",]$Year<-"2010 & 2012"
  #dt[dt$Year %in% "2014.1",]$Year<-"2013 & 2015"
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"PARROTFISH BIOMASS" 
  ## define labels so to have line breaks
 
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") + 
    geom_hline(aes(yintercept = z), hline.data.MHI_sc_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))+
    #theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+   
    coord_cartesian(ylim = c(0, 6))+
    theme(legend.position = "none")+
    labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))+
    theme(plot.title = element_text(hjust = 0.5))

  
##########  total fish biomass graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
levels(dt$mean_tot)<-"All fishes biomass"
#levels(dt$Year)<-c( "2009",   "2010"  , "2011" ,  "2011.1", "2012" ,  "2014",   "2014.1" ,"2015"  ) 
#dt$Year<-as.character(dt$Year)
 #dt[dt$Year %in% "2011.1",]$Year<-"2010 & 2012"
 #dt[dt$Year %in% "2014.1",]$Year<-"2013 & 2015"
#dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"ALL FISH" 
## define labels so to have line breaks

meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
  geom_hline(aes(yintercept = z), hline.data.MHI_sz_m, colour = "darkred")+
  theme_bw() + theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 70))+
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  #theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+ 
  theme(legend.position = "none")+
  labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))
         
         
#total biomass per consumer group island per year ----------------------------
         
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
  levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
  dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
  #levels(dt$Year)<-c( "2009",   "2010"  , "2011" ,  "2011.1", "2012" ,  "2014",   "2014.1" ,"2015"  ) 
  #dt$Year<-as.character(dt$Year)
  #dt[dt$Year %in% "2011.1",]$Year<-"2010 & 2012"
  #dt[dt$Year %in% "2014.1",]$Year<-"2013 & 2015"
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  
  ## define labels so to have line breaks
  consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
    facet_grid(~Consumergroup, scales ="fixed") + 
    theme_bw() +
    geom_hline(aes(yintercept = z), hline.data.MHI_cons_m, colour = "darkred")+
    theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 40))+
    theme(axis.text.x = element_text(angle = 25, hjust = 1))+
    #theme(axis.text.x = element_blank(),axis.ticks.x=element_blank()) +
    theme(legend.position = "none")+
    labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))+
    theme(plot.title = element_text(hjust = 0.5))
#consgrp  

  #   ####### bentic cover per island per year
  #   
  benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
  benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
  names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
  names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
  #   
  benthic$Year<-as.factor(benthic$Year)
  benthic_se$Year<-as.factor(benthic_se$Year)
  #   
  test<-melt(benthic)
  test_se<-melt(benthic_se)
  #   
  benthic<-cbind(test, test_se$value)
  names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
  #   
  #levels(benthic$Year)<-c( "2009",   "2010"  , "2011" ,  "2011.1", "2012" ,  "2014",   "2014.1" ,"2015"  ) 
  #benthic$Year<-as.character(benthic$Year)
  #benthic[benthic$Year %in% "2011.1",]$Year<-"2010 & 2012"
  #benthic[benthic$Year %in% "2014.1",]$Year<-"2013 & 2015"
  #   
  #   ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
  dodge <- position_dodge(width=0.9)
  #   
  #   ## define labels so to have line breaks
  cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
    facet_grid(~Benthos, scales ="fixed") + 
    theme_bw() +
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 25, hjust = 1))+
    #theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks.x=element_blank())+
    labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")+
    theme(plot.title = element_text(hjust = 0.5))
  #
  png(filename = paste("island_",s,"_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 800, restoreConsole = TRUE)
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
  print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
  print(meanbio,vp=vplayout(1,1))
  print(meansize, vp = vplayout(1, 4))
  print(consgrp, vp = vplayout(2, 1:4))
  print(cover, vp = vplayout(3, 1:4))
  
  dev.off()   

}


# PRIAS -----------------------------------------------

## create reference lines for region
# to check col nums
head(ref)
colnames(ref)

# consumer group 
prias_ref_cons<-as.numeric(as.vector(ref[ref$Mean.REGION=="PRIAs",c("Mean.PRIMARY","Mean.SECONDARY","Mean.PLANKTIVORE","Mean.PISCIVORE")]))

# biomass of parrots in 2 size classes
prias_ref_sc<-as.numeric(as.vector(ref[ref$Mean.REGION=="PRIAs",c("Mean.10_30","Mean.30_plus")]))

# biomass of all fishes
prias_ref_tb<-as.numeric(as.vector(ref[ref$Mean.REGION=="PRIAs","Mean.TotFish"]))

# mean size
prias_ref_sz<-as.numeric(as.vector(ref[ref$Mean.REGION=="PRIAs","Mean.MEAN_SIZE"]))

## references lines for plots, use ..._m (mean), sep (m plus se) and sem (m minus se)
hline.data.PRIAs_cons_m <- data.frame(z = prias_ref_cons, Consumergroup = c("Planktivores","Pri. cons.","Sec. cons.","Piscivores")) ## needs to match the graph type

hline.data.PRIAs_sc_m <- data.frame(z = prias_ref_sc, Size_class = c("Parrots 10-30 cm", "Parrots >30 cm")) ## needs to match the graph type

hline.data.PRIAs_tb_m <- data.frame(z = prias_ref_tb, Size_class = "All fishes") ## needs to match the graph type

hline.data.PRIAs_sz_m <- data.frame(z = prias_ref_sz, mean_size = "All fishes mean size") ## needs to match the graph type

## with the reference lines, need to run each region separately...
rd<-subset(rdd, Mean.REGION == "PRIAs")
rd<-droplevels(rd)
ben.pria<-subset(ben, Mean.REGION == "PRIAs")
#### FOREREEF
ben.pria<-subset(ben.pria, Mean.REEF_ZONE == "Forereef")
### need to manually change the loop code below to pull the correct reference lines for each region

max(rd$Mean.TotFish+rd$PooledSE.TotFish) ## check to manually set the ylims on graphs ## 350
max(rd$Mean.MEAN_SIZE+rd$PooledSE.MEAN_SIZE) # 25
max(rd$Mean.10_30+rd$PooledSE.10_30)
max(rd$Mean.30_plus+rd$PooledSE.30_plus) # 20
max(rd$Mean.PRIMARY+rd$PooledSE.PRIMARY) ## 225
max(rd$Mean.SECONDARY+rd$PooledSE.SECONDARY)
max(rd$Mean.PISCIVORE+rd$PooledSE.PISCIVORE)
max(rd$Mean.PLANKTIVORE+rd$PooledSE.PLANKTIVORE)

## subset graphing data
#------------------------Wake------------------------------------------------------
# ## single island graph for WAKE - surveyed without other PRIAs
#   data<-subset(rd,Mean.ISLAND=="Wake")
#   data<-drop.levels(data)
#  
#   benth<-subset(ben.pria,Mean.ISLAND=="Wake")
#   benth<-drop.levels(benth)
#   
# ##########  size class group graph
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish", "Mean.10_30", "Mean.30_plus")]
# test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish", "PooledSE.10_30", "PooledSE.30_plus")]
# test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
# 
# dt<-as.data.frame(cbind(test, test2$SE))
# names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
# levels(dt$Size_class)<-c("All fishes", "Parrots 10-30 cm", "Parrots >30 cm")
# levels(dt$Year)<-c("2009","2011", "2014")
# #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
# 
# ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
# dodge <- position_dodge(width=0.9)
# colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
# title<-"FISH BIOMASS: TOTAL AND PARROTS" 
# ## define labels so to have line breaks
# 
# sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
#   facet_grid(~Size_class, scales ="fixed") + 
#   geom_hline(aes(yintercept = z), hline.data.MHI_sc_m, colour = "darkred")+
#   theme_bw() + theme(axis.title.x = element_blank()) +
#   coord_cartesian(ylim = c(0, 350))+
#   theme(legend.position = "none")+
#   labs(title = "FISH BIOMASS: TOTAL AND PARROTS", y = expression(paste("Fish biomass (g ", m^-2,")")))
# 
# ##########  mean size graph
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
# test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
# test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
# 
# dt<-as.data.frame(cbind(test, test2$SE))
# names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
# levels(dt$mean_size)<-"All fishes mean size"
# levels(dt$Year)<-c("2009", "2011", "2014")
# #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
# 
# ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
# dodge <- position_dodge(width=0.9)
# colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
# title<-"MEAN SIZE" 
# ## define labels so to have line breaks
# 
# meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
#   geom_hline(aes(yintercept = z), hline.data.MHI_sz_m, colour = "darkred")+
#   theme_bw() + theme(axis.title.x = element_blank()) +
#   coord_cartesian(ylim = c(0, 20))+
#   theme(legend.position = "none")+
#   labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))
# 
# 
# #   -----total biomass per consumer group island per year 
# 
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
# test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
# test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
# 
# dt<-as.data.frame(cbind(test, test2$SE))
# names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
# levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
# dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
# levels(dt$Year)<-c("2009", "2011", "2014")
# 
# ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
# dodge <- position_dodge(width=0.9)
# 
# ## define labels so to have line breaks
# consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Oranges") +
#   facet_grid(~Consumergroup, scales ="fixed") + 
#   theme_bw() +
#   geom_hline(aes(yintercept = z), hline.data.MHI_cons_m, colour = "darkred")+
#   geom_hline(aes(yintercept = z), hline.data.MHI_cons_sep, colour = "darkred", lty=2)+
#   geom_hline(aes(yintercept = z), hline.data.MHI_cons_sem, colour = "darkred", lty=2)+
#   theme(axis.title.x = element_blank()) +
#   coord_cartesian(ylim = c(0, 225))+
#   theme(legend.position = "none")+
#   labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))
# 
# #   ####### bentic cover per island per year
# #   
# benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
# benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
# names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
# names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
# #   
# benthic$Year<-as.factor(benthic$Year)
# benthic_se$Year<-as.factor(benthic_se$Year)
# #   
# test<-melt(benthic)
# test_se<-melt(benthic_se)
# #   
# benthic<-cbind(test, test_se$value)
# names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
# #   
# levels(benthic$Year)<-c("2009","2011","2014")
# #   
# #   ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
# dodge <- position_dodge(width=0.9)
# #   
# #   ## define labels so to have line breaks
# cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Greens") +
#   facet_grid(~Benthos, scales ="fixed") + 
#   theme_bw() +
#   theme(legend.position = "none")+
#   labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")
# #
# png(filename = paste("island_",s,"_2015.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
# 
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(3, 3))) # 3 rows, 3 columns
# print(sizeclass, vp = vplayout(1, 1:2))  # this plot covers row 1 and cols 1:2
# print(meansize, vp = vplayout(1, 3))
# print(consgrp, vp = vplayout(2, 1:3))
# print(cover, vp = vplayout(3, 1:3))
# 
# dev.off()

# save graphs in figures folder
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures")

## batch make the graphs

#-----------------------------Other Prias----------------------------------------------------------------
# save graphs in figures folder
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures")
################### FOREREEF PRIA #####################################
rd<-rd[rd$Mean.REEF_ZONE=="Forereef",]
for(i in 1:length(rd$Mean.ISLAND)){
  
  #i<-"Jarvis"
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  #data<-subset(rd,Mean.ISLAND=="Jarvis")
  data<-drop.levels(data)
  
  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
  levels(dt$Year)<-c("2010","2012", "2015","2016") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"PARROTFISH BIOMASS" 
  ## define labels so to have line breaks
  
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") + 
    geom_hline(aes(yintercept = z), hline.data.PRIAs_sc_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
 theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    coord_cartesian(ylim = c(0, 20))+
    theme(legend.position = "none")+
    labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))+
    theme(plot.title = element_text(hjust = 0.5))
  
  ##########  mean size graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
  levels(dt$mean_size)<-"All fishes mean size"
  levels(dt$Year)<-c("2010","2012", "2015","2016") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"MEAN SIZE" 
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    geom_hline(aes(yintercept = z), hline.data.PRIAs_sz_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    coord_cartesian(ylim = c(0, 25))+
    theme(legend.position = "none")+
    labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))+
    theme(plot.title = element_text(hjust = 0.5))
  
##########  total fish biomass graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
  levels(dt$mean_tot)<-"All fishes biomass"
  levels(dt$Year)<-c("2010","2012", "2015","2016") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"ALL FISH" 
  ## define labels so to have line breaks
  
  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
    geom_hline(aes(yintercept = z), hline.data.PRIAs_tb_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    coord_cartesian(ylim = c(0, 355))+
    theme(legend.position = "none")+
    labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  ######total biomass per consumer group island per year
  
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
  levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
  dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
  levels(dt$Year)<-c("2010","2012", "2015","2016") # CHANGE FOR RELEVANT YEARS
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  
  ## define labels so to have line breaks
  consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
    facet_grid(~Consumergroup, scales ="fixed") + 
    theme_bw() +
    geom_hline(aes(yintercept = z), hline.data.PRIAs_cons_m, colour = "darkred")+
    theme(axis.title.x = element_blank()) +
 theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    coord_cartesian(ylim = c(0, 230))+
    theme(legend.position = "none")+
    labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))+
    theme(plot.title = element_text(hjust = 0.5))
  
  #   ####### bentic cover per island per year
  #   
  benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
  benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
  names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
  names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
  #   
  benthic$Year<-as.factor(benthic$Year)
  benthic_se$Year<-as.factor(benthic_se$Year)
  #   
  test<-melt(benthic)
  test_se<-melt(benthic_se)
  #   
  benthic<-cbind(test, test_se$value)
  names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
  #   
  levels(dt$Year)<-c("2010","2012", "2015","2016") # CHANGE FOR RELEVANT YEARS
  #   
  #   ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
  dodge <- position_dodge(width=0.9)
  #   
  #   ## define labels so to have line breaks
  cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
    facet_grid(~Benthos, scales ="fixed") + 
    theme_bw() +
    theme(legend.position = "none")+
    labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")+
    theme(plot.title = element_text(hjust = 0.5))
  #
  png(filename = paste("island_",s,"FRF_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 1200, restoreConsole = TRUE)
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
  print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
  print(meanbio,vp=vplayout(1,1))
  print(meansize, vp = vplayout(1, 4))
  print(consgrp, vp = vplayout(2, 1:4))
  print(cover, vp = vplayout(3, 1:4))
  
  dev.off()   
  
}

################### Lagoon PRIA #######################################

rd<-subset(rdd, Mean.REGION == "PRIAs")
rd<-droplevels(rd)
ben.pria<-subset(ben, Mean.REGION == "PRIAs")
ben.pria<-subset(ben.pria, Mean.REEF_ZONE == "Lagoon")
rd<-rd[rd$Mean.REEF_ZONE=="Lagoon",]
for(i in 1:length(rd$Mean.ISLAND)){
  
  #i<-"Jarvis"
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  #data<-subset(rd,Mean.ISLAND=="Jarvis")
  data<-drop.levels(data)
  
  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"PARROTFISH BIOMASS" 
  ## define labels so to have line breaks
  
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") + 
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_sc_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 20))+
    theme(legend.position = "none")+
    labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  ##########  mean size graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
  levels(dt$mean_size)<-"All fishes mean size"
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"MEAN SIZE" 
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_sz_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 25))+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    theme(legend.position = "none")+
    labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))
  
  ##########  total fish biomass graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
  levels(dt$mean_tot)<-"All fishes biomass"
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"ALL FISH" 
  ## define labels so to have line breaks
  
  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_tb_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 355))+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    theme(legend.position = "none")+
    labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  
  ######total biomass per consumer group island per year 
  
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PISCIVORE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
  levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
  dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  
  ## define labels so to have line breaks
  consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
    facet_grid(~Consumergroup, scales ="fixed") + 
    theme_bw() +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_cons_m, colour = "darkred")+
    theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 230))+
    theme(legend.position = "none")+
    labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  #   ####### bentic cover per island per year
  #   
  benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
  benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
  names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
  names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
  #   
  benthic$Year<-as.factor(benthic$Year)
  benthic_se$Year<-as.factor(benthic_se$Year)
  #   
  test<-melt(benthic)
  test_se<-melt(benthic_se)
  #   
  benthic<-cbind(test, test_se$value)
  names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
  #   
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #   
  #   ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
  dodge <- position_dodge(width=0.9)
  #   
  #   ## define labels so to have line breaks
  cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
    facet_grid(~Benthos, scales ="fixed") + 
    theme_bw() +
    theme(legend.position = "none")+
    labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")
  #
  png(filename = paste("island_",s,"LAG_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
  print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
  print(meanbio,vp=vplayout(1,1))
  print(meansize, vp = vplayout(1, 4))
  print(consgrp, vp = vplayout(2, 1:4))
  print(cover, vp = vplayout(3, 1:4))
  
  dev.off()   
  
}

################### BACKREEF PRIA ##########################
rd<-subset(rdd, Mean.REGION == "PRIAs")
rd<-droplevels(rd)
ben.pria<-subset(ben, Mean.REGION == "PRIAs")
ben.pria<-subset(ben.pria, Mean.REEF_ZONE == "Backreef")
rd<-rd[rd$Mean.REEF_ZONE=="Backreef",]
for(i in 1:length(rd$Mean.ISLAND)){
  
  #i<-"Jarvis"
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  #data<-subset(rd,Mean.ISLAND=="Jarvis")
  data<-drop.levels(data)
  
  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"PARROTFISH BIOMASS" 
  ## define labels so to have line breaks
  
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") + 
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_sc_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 20))+
    theme(legend.position = "none")+
    labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  ##########  mean size graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
  levels(dt$mean_size)<-"All fishes mean size"
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"MEAN SIZE" 
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_sz_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 25))+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    theme(legend.position = "none")+
    labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))
  
  ##########  total fish biomass graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
  levels(dt$mean_tot)<-"All fishes biomass"
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"ALL FISH" 
  ## define labels so to have line breaks
  
  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_tb_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 355))+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    theme(legend.position = "none")+
    labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  
  
  ######total biomass per consumer group island per year
  
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
  levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
  dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  
  ## define labels so to have line breaks
  consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
    facet_grid(~Consumergroup, scales ="fixed") + 
    theme_bw() +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_cons_m, colour = "darkred")+
    theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 230))+
    theme(legend.position = "none")+
    labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  #   ####### bentic cover per island per year
  #   
  benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
  benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
  names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
  names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
  #   
  benthic$Year<-as.factor(benthic$Year)
  benthic_se$Year<-as.factor(benthic_se$Year)
  #   
  test<-melt(benthic)
  test_se<-melt(benthic_se)
  #   
  benthic<-cbind(test, test_se$value)
  names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
  #   
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #   
  #   ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
  dodge <- position_dodge(width=0.9)
  #   
  #   ## define labels so to have line breaks
  cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
    facet_grid(~Benthos, scales ="fixed") + 
    theme_bw() +
    theme(legend.position = "none")+
    labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")
  #
  png(filename = paste("island_",s,"BRF_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
  print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
  print(meanbio,vp=vplayout(1,1))
  print(meansize, vp = vplayout(1, 4))
  print(consgrp, vp = vplayout(2, 1:4))
  print(cover, vp = vplayout(3, 1:4))
  
  dev.off()   
  
}
# SAMOA -----------------------------------------------

## create reference lines for region
# to check col nums
head(ref)
colnames(ref)

# consumer group 
SAM_ref_cons<-as.numeric(as.vector(ref[ref$Mean.REGION=="SAMOA",c("Mean.PLANKTIVORE","Mean.PRIMARY","Mean.SECONDARY","Mean.PISCIVORE")]))

# biomass of parrots in 2 size classes
SAM_ref_sc<-as.numeric(as.vector(ref[ref$Mean.REGION=="SAMOA",c("Mean.10_30","Mean.30_plus")]))

# biomass of all fishes
SAM_ref_tb<-as.numeric(as.vector(ref[ref$Mean.REGION=="SAMOA","Mean.TotFish"]))

# mean size
SAM_ref_sz<-as.numeric(as.vector(ref[ref$Mean.REGION=="SAMOA","Mean.MEAN_SIZE"]))

## references lines for plots, use ..._m (mean), sep (m plus se) and sem (m minus se)
hline.data.SAM_cons_m <- data.frame(z = SAM_ref_cons, Consumergroup = c("Planktivores","Pri. cons.","Sec. cons.","Piscivores")) ## needs to match the graph type

hline.data.SAM_sc_m <- data.frame(z = SAM_ref_sc, Size_class = c("Parrots 10-30 cm", "Parrots >30 cm")) ## needs to match the graph type

hline.data.SAM_tb_m <- data.frame(z = SAM_ref_tb, Size_class = "All fishes") ## needs to match the graph type

hline.data.SAM_sz_m <- data.frame(z = SAM_ref_sz, mean_size = "All fishes mean size") ## needs to match the graph type

## with the reference lines, need to run each region separately...

rd<-subset(rdd, Mean.REGION == "SAMOA")
rd<-droplevels(rd)
ben.sam<-subset(ben, Mean.REGION == "SAMOA")
### need to manually change the loop code below to pull the correct reference lines for each region

max(rd$Mean.TotFish+rd$PooledSE.TotFish) ## check to manually set the ylims on graphs ## totbiomass 110
max(rd$Mean.MEAN_SIZE+rd$PooledSE.MEAN_SIZE) # meansize 25
max(rd$Mean.10_30+rd$PooledSE.10_30)
max(rd$Mean.30_plus+rd$PooledSE.30_plus) # sizeclass 12
max(rd$Mean.PRIMARY+rd$PooledSE.PRIMARY) ## consumergrp 45
max(rd$Mean.SECONDARY+rd$PooledSE.SECONDARY)
max(rd$Mean.PISCIVORE+rd$PooledSE.PISCIVORE)
max(rd$Mean.PLANKTIVORE+rd$PooledSE.PLANKTIVORE)

# save graphs in figures folder
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures")
################### FOREREEF ONLY
rd<-rd[rd$Mean.REEF_ZONE=="Forereef",]
for(i in 1:length(rd$Mean.ISLAND)){
  
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  #data<-subset(rd,Mean.ISLAND=="Tutuila")
  data<-drop.levels(data)
  
  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
  levels(dt$Year)<-c("2010","2012", "2015","2016") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"PARROTFISH BIOMASS" 
  ## define labels so to have line breaks
  
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") + 
    geom_hline(aes(yintercept = z), hline.data.SAM_sc_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    coord_cartesian(ylim = c(0, 12))+
    theme(legend.position = "none")+
    labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))+
    theme(plot.title = element_text(hjust = 0.5))
  
  ##########  mean size graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
  levels(dt$mean_size)<-"All fishes mean size"
  levels(dt$Year)<-c("2010","2012", "2015","2016") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"MEAN SIZE" 
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    geom_hline(aes(yintercept = z), hline.data.SAM_sz_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 25))+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    theme(legend.position = "none")+
    labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))+
    theme(plot.title = element_text(hjust = 0.5))
  
  ##########  total fish biomass graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
  levels(dt$mean_tot)<-"All fishes biomass"
  levels(dt$Year)<-c("2010","2012", "2015","2016") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"ALL FISH" 
  ## define labels so to have line breaks
  
  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
    geom_hline(aes(yintercept = z), hline.data.SAM_tb_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 110))+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    theme(legend.position = "none")+
    labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  ######total biomass per consumer group island per year
  
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
  levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
  dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
  levels(dt$Year)<-c("2010","2012", "2015","2016") # CHANGE FOR RELEVANT YEARS
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  
  ## define labels so to have line breaks
  consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
    facet_grid(~Consumergroup, scales ="fixed") + 
    theme_bw() +
    geom_hline(aes(yintercept = z), hline.data.SAM_cons_m, colour = "darkred")+
    theme(axis.title.x = element_blank()) +
theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    coord_cartesian(ylim = c(0, 45))+
    theme(legend.position = "none")+
    labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))+
    theme(plot.title = element_text(hjust = 0.5))
  
  #   ####### bentic cover per island per year
  #   
  benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
  benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
  names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
  names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
  #   
  benthic$Year<-as.factor(benthic$Year)
  benthic_se$Year<-as.factor(benthic_se$Year)
  #   
  test<-melt(benthic)
  test_se<-melt(benthic_se)
  #   
  benthic<-cbind(test, test_se$value)
  names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
  #   
  levels(dt$Year)<-c("2010","2012", "2015","2016") # CHANGE FOR RELEVANT YEARS
  #   
  #   ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
  dodge <- position_dodge(width=0.9)
  #   
  #   ## define labels so to have line breaks
  cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
    facet_grid(~Benthos, scales ="fixed") + 
    theme_bw() +
    theme(legend.position = "none")+
    labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")+
    theme(plot.title = element_text(hjust = 0.5))
  #
  png(filename = paste("island_",s,"FRF_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
  print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
  print(meanbio,vp=vplayout(1,1))
  print(meansize, vp = vplayout(1, 4))
  print(consgrp, vp = vplayout(2, 1:4))
  print(cover, vp = vplayout(3, 1:4))
  
  dev.off()   
  
}

################### Lagoon SAMOA #####################################
rd<-subset(rdd, Mean.REGION == "SAMOA")
rd<-droplevels(rd)
ben.sam<-subset(ben, Mean.REGION == "SAMOA")
ben.sam<-subset(ben.sam, Mean.REEF_ZONE == "Lagoon")
rd<-rd[rd$Mean.REEF_ZONE=="Lagoon",]
# save graphs in figures folder
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures")

for(i in 1:length(rd$Mean.ISLAND)){
  
  #i<-"Jarvis"
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  #data<-subset(rd,Mean.ISLAND=="Jarvis")
  data<-drop.levels(data)
  
  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
  levels(dt$Year)<-c("2010","2015","2016") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"PARROTFISH BIOMASS" 
  ## define labels so to have line breaks
  
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") + 
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 10))+
    theme(legend.position = "none")+
    labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  ##########  mean size graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
  levels(dt$mean_size)<-"All fishes mean size"
  levels(dt$Year)<-c("2010","2015","2016") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"MEAN SIZE" 
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 25))+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    theme(legend.position = "none")+
    labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))
  
  ##########  total fish biomass graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
  levels(dt$mean_tot)<-"All fishes biomass"
  levels(dt$Year)<-c("2010","2015","2016") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"ALL FISH" 
  ## define labels so to have line breaks
  
  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0,110))+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    theme(legend.position = "none")+
    labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  
  
  ######total biomass per consumer group island per year 
  
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
  levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
  dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
  levels(dt$Year)<-c("2010","2015","2016") # CHANGE FOR RELEVANT YEARS
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  
  ## define labels so to have line breaks
  consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
    facet_grid(~Consumergroup, scales ="fixed") + 
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 45))+
    theme(legend.position = "none")+
    labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  #   ####### bentic cover per island per year
  #   
  benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
  benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
  names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
  names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
  #   
  benthic$Year<-as.factor(benthic$Year)
  benthic_se$Year<-as.factor(benthic_se$Year)
  #   
  test<-melt(benthic)
  test_se<-melt(benthic_se)
  #   
  benthic<-cbind(test, test_se$value)
  names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
  #   
  levels(dt$Year)<-c("2010","2015","2016") # CHANGE FOR RELEVANT YEARS
  #   
  #   ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
  dodge <- position_dodge(width=0.9)
  #   
  #   ## define labels so to have line breaks
  cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
    facet_grid(~Benthos, scales ="fixed") + 
    theme_bw() +
    theme(legend.position = "none")+
    labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")
  #
  png(filename = paste("island_",s,"LAG_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
  print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
  print(meanbio,vp=vplayout(1,1))
  print(meansize, vp = vplayout(1, 4))
  print(consgrp, vp = vplayout(2, 1:4))
  print(cover, vp = vplayout(3, 1:4))
  
  dev.off()   
  
}

################### Backreef SAMOA #######################################
rd<-subset(rdd, Mean.REGION == "SAMOA")
rd<-droplevels(rd)
ben.sam<-subset(ben, Mean.REGION == "SAMOA")
ben.sam<-subset(ben.sam, Mean.REEF_ZONE == "Backreef")
rd<-rd[rd$Mean.REEF_ZONE=="Backreef",]
# save graphs in figures folder
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures")

for(i in 1:length(rd$Mean.ISLAND)){
  
  #i<-"Jarvis"
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  #data<-subset(rd,Mean.ISLAND=="Jarvis")
  data<-drop.levels(data)
  
  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"PARROTFISH BIOMASS" 
  ## define labels so to have line breaks
  
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") + 
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 10))+
    theme(legend.position = "none")+
    labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  ##########  mean size graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
  levels(dt$mean_size)<-"All fishes mean size"
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"MEAN SIZE" 
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 22))+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    theme(legend.position = "none")+
    labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))
  
  ##########  total fish biomass graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
  levels(dt$mean_tot)<-"All fishes biomass"
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"ALL FISH" 
  ## define labels so to have line breaks
  
  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 110))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  
  
  ######total biomass per consumer group island per year 
  
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
  levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
  dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  
  ## define labels so to have line breaks
  consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
    facet_grid(~Consumergroup, scales ="fixed") + 
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 45))+
    theme(legend.position = "none")+
    labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  #   ####### bentic cover per island per year
  #   
  benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
  benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
  names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
  names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
  #   
  benthic$Year<-as.factor(benthic$Year)
  benthic_se$Year<-as.factor(benthic_se$Year)
  #   
  test<-melt(benthic)
  test_se<-melt(benthic_se)
  #   
  benthic<-cbind(test, test_se$value)
  names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
  #   
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #   
  #   ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
  dodge <- position_dodge(width=0.9)
  #   
  #   ## define labels so to have line breaks
  cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
    facet_grid(~Benthos, scales ="fixed") + 
    theme_bw() +
    theme(legend.position = "none")+
    labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")
  #
  png(filename = paste("island_",s,"BRF_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
  print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
  print(meanbio,vp=vplayout(1,1))
  print(meansize, vp = vplayout(1, 4))
  print(consgrp, vp = vplayout(2, 1:4))
  print(cover, vp = vplayout(3, 1:4))
  
  dev.off()   
  
}
# NMARIANAS -----------------------------------------------

## create reference lines for region
marn_ref_cons<-as.numeric(as.vector(ref[ref$Mean.REGION=="N.MARIAN",c("Mean.PLANKTIVORE","Mean.PRIMARY","Mean.SECONDARY","Mean.PISCIVORE")]))
 # consumer groups

# for biomass of parrots in 2 size classes
marn_ref_sc<-as.numeric(as.vector(ref[ref$Mean.REGION=="N.MARIAN",c("Mean.10_30","Mean.30_plus")]))

# for  all fish biomass
marn_ref_tb<-as.numeric(as.vector(ref[ref$Mean.REGION=="N.MARIAN",c("Mean.TotFish")]))

# mean size
marn_ref_sz<-as.numeric(as.vector(ref[ref$Mean.REGION=="N.MARIAN","Mean.MEAN_SIZE"]))


## references lines for plots, use ..._m (mean), sep (m plus se) and sem (m minus se)
hline.data.MARN_cons_m <- data.frame(z = marn_ref_cons, Consumergroup = c("Planktivores","Pri. cons.", "Sec. cons.","Piscivores")) ## needs to match the graph type

hline.data.MARN_sc_m <- data.frame(z = marn_ref_sc, Size_class = c("All fishes", "0-20 cm TL", "20-50 cm TL", ">50 cm TL")) ## needs to match the graph type

hline.data.MARN_tb_m <- data.frame(z = marn_ref_tb, Size_class = "All fishes") ## needs to match the graph type

hline.data.MARN_sz_m <- data.frame(z = marn_ref_sz, mean_size = "All fishes mean size") ## needs to match the graph type

## with the reference lines, need to run each region separately...
rd<-subset(rdd, Mean.REGION == "N.MARIAN")
rd<-drop.levels(rd)
ben.nmari<-subset(ben, Mean.REGION == "N.MARIAN")
ben.nmari<-subset(ben.nmari, Mean.REEF_ZONE == "Forereef")
ben.nmari<-drop.levels(ben.nmari)

#levels(rd$Mean.ISLAND)<-c("Agrihan","AGS","Asuncion","FDP","Maug","Pagan")
#levels(ben.nmari$Mean.ISLAND)<-c("Agrihan","AGS","Asuncion","FDP","Maug","Pagan")

### need to manually change the loop code below to pull the correct reference lines for each region
#colnames(rd)
max(rd$Mean.TotFish+rd$PooledSE.TotFish) ## check to manually set the ylims on graphs ## set at 180
max(rd$Mean.MEAN_SIZE+rd$PooledSE.MEAN_SIZE) # 25
max(rd$Mean.10_35+rd$PooledSE.10_35)
max(rd$Mean.35_plus+rd$PooledSE.35_plus) # 15
max(rd$Mean.PRIMARY+rd$PooledSE.PRIMARY) ## set to 65
max(rd$Mean.SECONDARY+rd$PooledSE.SECONDARY)
max(rd$Mean.PISCIVORE+rd$PooledSE.PISCIVORE)
max(rd$Mean.PLANKTIVORE+rd$PooledSE.PLANKTIVORE)

# save graphs in figures folder
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures")

## batch make the graphs 
for(i in 1:length(rd$Mean.ISLAND)){
s<-rd$Mean.ISLAND[i]
data<-subset(rd, Mean.ISLAND == s) ## change back to s
#data<-subset(rd,Mean.ISLAND=="Pagan")
data<-drop.levels(data)

  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
  levels(dt$Year)<-c("2010","2012", "2015","2016") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"PARROTFISH BIOMASS" 
  ## define labels so to have line breaks
  
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") + 
    geom_hline(aes(yintercept = z), hline.data.PRIAs_sc_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 15))+
    theme(legend.position = "none")+
    labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  ##########  mean size graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
  levels(dt$mean_size)<-"All fishes mean size"
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"MEAN SIZE" 
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    geom_hline(aes(yintercept = z), hline.data.PRIAs_sz_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 25))+
  theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    theme(legend.position = "none")+
    labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))
  
  ##########  total fish biomass graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
  levels(dt$mean_tot)<-"All fishes biomass"
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"ALL FISH" 
  ## define labels so to have line breaks
  
  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
    geom_hline(aes(yintercept = z), hline.data.PRIAs_tb_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 355))+
  theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    theme(legend.position = "none")+
    labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  
  ######total biomass per consumer group island per year
  
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
  levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
  dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  
  ## define labels so to have line breaks
  consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
    facet_grid(~Consumergroup, scales ="fixed") + 
    theme_bw() +
    geom_hline(aes(yintercept = z), hline.data.PRIAs_cons_m, colour = "darkred")+
    theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 230))+
    theme(legend.position = "none")+
    labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  #   ####### bentic cover per island per year
  #   
  benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
  benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
  names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
  names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
  #   
  benthic$Year<-as.factor(benthic$Year)
  benthic_se$Year<-as.factor(benthic_se$Year)
  #   
  test<-melt(benthic)
  test_se<-melt(benthic_se)
  #   
  benthic<-cbind(test, test_se$value)
  names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
  #   
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #   
  #   ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
  dodge <- position_dodge(width=0.9)
  #   
  #   ## define labels so to have line breaks
  cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
    facet_grid(~Benthos, scales ="fixed") + 
    theme_bw() +
    theme(legend.position = "none")+
    labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")
  #
  png(filename = paste("island_",s,"2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
  print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
  print(meanbio,vp=vplayout(1,1))
  print(meansize, vp = vplayout(1, 4))
  print(consgrp, vp = vplayout(2, 1:4))
  print(cover, vp = vplayout(3, 1:4))
  
  dev.off()   
  
}


# SMARIANAS -----------------------------------------------

## create reference lines for region
## with the reference lines, need to run each region separately...
rd<-subset(rdd, Mean.REGION == "S.MARIAN")
ben.smari<-subset(ben, Mean.REGION == "S.MARIAN")
ben.smari<-subset(ben.smari, Mean.REEF_ZONE == "Forereef")

## create reference lines for region
mars_ref_cons<-as.numeric(as.vector(ref[ref$Mean.REGION=="S.MARIAN",c("Mean.PLANKTIVORE","Mean.PRIMARY","Mean.SECONDARY","Mean.PISCIVORE")]))
# consumer groups

# for biomass of parrots in 2 size classes
mars_ref_sc<-as.numeric(as.vector(ref[ref$Mean.REGION=="S.MARIAN",c("Mean.10_30","Mean.30_plus")]))

# for  all fish biomass
mars_ref_tb<-as.numeric(as.vector(ref[ref$Mean.REGION=="S.MARIAN",c("Mean.TotFish")]))

# mean size
mars_ref_sz<-as.numeric(as.vector(ref[ref$Mean.REGION=="S.MARIAN","Mean.MEAN_SIZE"]))


## references lines for plots, use ..._m (mean), sep (m plus se) and sem (m minus se)
hline.data.MARS_cons_m <- data.frame(z = mars_ref_cons, Consumergroup = c("Planktivores","Pri. cons.", "Sec. cons.","Piscivores")) ## needs to match the graph type

hline.data.MARS_sc_m <- data.frame(z = mars_ref_sc, Size_class = c("All fishes", "0-20 cm TL", "20-50 cm TL", ">50 cm TL")) ## needs to match the graph type

hline.data.MARS_tb_m <- data.frame(z = mars_ref_tb, Size_class = "All fishes") ## needs to match the graph type

hline.data.MARS_sz_m <- data.frame(z = mars_ref_sz, mean_size = "All fishes mean size") ## needs to match the graph type

## with the reference lines, need to run each region separately...
rd<-subset(rdd, Mean.REGION == "N.MARIAN")
rd<-drop.levels(rd)
ben.nmari<-subset(ben, Mean.REGION == "N.MARIAN")
ben.nmari<-subset(ben.nmari, Mean.REEF_ZONE == "Forereef")
ben.nmari<-drop.levels(ben.nmari)

#levels(rd$Mean.ISLAND)<-c("Agrihan","AGS","Asuncion","FDP","Maug","Pagan")
#levels(ben.nmari$Mean.ISLAND)<-c("Agrihan","AGS","Asuncion","FDP","Maug","Pagan")

### need to manually change the loop code below to pull the correct reference lines for each region
#colnames(rd)
max(rd$Mean.TotFish+rd$PooledSE.TotFish) ## check to manually set the ylims on graphs ## set at 180
max(rd$Mean.MEAN_SIZE+rd$PooledSE.MEAN_SIZE) # 25
max(rd$Mean.10_35+rd$PooledSE.10_35)
max(rd$Mean.35_plus+rd$PooledSE.35_plus) # 20
max(rd$Mean.PRIMARY+rd$PooledSE.PRIMARY) ## set to 65
max(rd$Mean.SECONDARY+rd$PooledSE.SECONDARY)
max(rd$Mean.PISCIVORE+rd$PooledSE.PISCIVORE)
max(rd$Mean.PLANKTIVORE+rd$PooledSE.PLANKTIVORE)

# save graphs in figures folder
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures")

## batch make the graphs 
for(i in 1:length(rd$Mean.ISLAND)){
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  #data<-subset(rd,Mean.ISLAND=="Guam")
  data<-drop.levels(data)
  
  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"PARROTFISH BIOMASS" 
  ## define labels so to have line breaks
  
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") + 
    geom_hline(aes(yintercept = z), hline.data.PRIAs_sc_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 20))+
    theme(legend.position = "none")+
    labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  ##########  mean size graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
  levels(dt$mean_size)<-"All fishes mean size"
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"MEAN SIZE" 
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    geom_hline(aes(yintercept = z), hline.data.PRIAs_sz_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 25))+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    theme(legend.position = "none")+
    labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))
  
  ##########  total fish biomass graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
  levels(dt$mean_tot)<-"All fishes biomass"
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"ALL FISH" 
  ## define labels so to have line breaks
  
  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
    geom_hline(aes(yintercept = z), hline.data.PRIAs_tb_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 355))+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    theme(legend.position = "none")+
    labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  
  ######total biomass per consumer group island per year
  
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
  levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
  dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  
  ## define labels so to have line breaks
  consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
    facet_grid(~Consumergroup, scales ="fixed") + 
    theme_bw() +
    geom_hline(aes(yintercept = z), hline.data.PRIAs_cons_m, colour = "darkred")+
    theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 230))+
    theme(legend.position = "none")+
    labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  #   ####### bentic cover per island per year
  #   
  benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
  benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
  names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
  names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
  #   
  benthic$Year<-as.factor(benthic$Year)
  benthic_se$Year<-as.factor(benthic_se$Year)
  #   
  test<-melt(benthic)
  test_se<-melt(benthic_se)
  #   
  benthic<-cbind(test, test_se$value)
  names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
  #   
  levels(dt$Year)<-c("2010","2012", "2015") # CHANGE FOR RELEVANT YEARS
  #   
  #   ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
  dodge <- position_dodge(width=0.9)
  #   
  #   ## define labels so to have line breaks
  cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
    facet_grid(~Benthos, scales ="fixed") + 
    theme_bw() +
    theme(legend.position = "none")+
    labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")
  #
  png(filename = paste("island_",s,"2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
  print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
  print(meanbio,vp=vplayout(1,1))
  print(meansize, vp = vplayout(1, 4))
  print(consgrp, vp = vplayout(2, 1:4))
  print(cover, vp = vplayout(3, 1:4))
  
  dev.off()   
  
}

# NWHI forereef-----------------------------------------------
# Different islands surveyed in different years, run each separately: ffs, lisi, midway for 2014

# consumer group 
NWHI_ref_cons<-as.numeric(as.vector(ref[ref$Mean.REGION=="NWHI",c("Mean.PLANKTIVORE","Mean.PRIMARY","Mean.SECONDARY","Mean.PISCIVORE")]))

# biomass of parrots in 2 size classes
NWHI_ref_sc<-as.numeric(as.vector(ref[ref$Mean.REGION=="NWHI",c("Mean.10_30","Mean.30_plus")]))

# biomass of all fishes
NWHI_ref_tb<-as.numeric(as.vector(ref[ref$Mean.REGION=="NWHI","Mean.TotFish"]))

# mean size
NWHI_ref_sz<-as.numeric(as.vector(ref[ref$Mean.REGION=="NWHI","Mean.MEAN_SIZE"]))

## references lines for plots, use ..._m (mean), sep (m plus se) and sem (m minus se)
hline.data.NWHI_cons_m <- data.frame(z = NWHI_ref_cons, Consumergroup = c("Planktivores","Pri. cons.","Sec. cons.","Piscivores")) ## needs to match the graph type

hline.data.NWHI_sc_m <- data.frame(z = NWHI_ref_sc, Size_class = c("Parrots 10-30 cm", "Parrots >30 cm")) ## needs to match the graph type

hline.data.NWHI_tb_m <- data.frame(z = NWHI_ref_tb, Size_class = "All fishes") ## needs to match the graph type

hline.data.NWHI_sz_m <- data.frame(z = NWHI_ref_sz, mean_size = "All fishes mean size") ## needs to match the graph type

## with the reference lines, need to run each region separately...

rd<-subset(rdd, Mean.REGION == "NWHI")
rd<-droplevels(rd)
ben.nwhi<-subset(ben, Mean.REGION == "NWHI")
### need to manually change the loop code below to pull the correct reference lines for each region
#colnames(rd)
max(rd$Mean.TotFish+rd$PooledSE.TotFish) ## check to manually set the ylims on graphs ## set at 300
max(rd$Mean.MEAN_SIZE+rd$PooledSE.MEAN_SIZE) # 27
max(rd$Mean.10_30+rd$PooledSE.10_30)
max(rd$Mean.30_plus+rd$PooledSE.30_plus) # 20
max(rd$Mean.PRIMARY+rd$PooledSE.PRIMARY) ## set to 195
max(rd$Mean.SECONDARY+rd$PooledSE.SECONDARY)
max(rd$Mean.PISCIVORE+rd$PooledSE.PISCIVORE)
max(rd$Mean.PLANKTIVORE+rd$PooledSE.PLANKTIVORE)
# determine what islands were surveyed this year
names(rd)
a<-rd[rd$Mean.ANALYSIS_YEAR==2016,]
b<-rd[rd$Mean.OBS_YEAR==2016,]
unique(a$Mean.ISLAND) # surveyed in 2015: FFS, Kure, laysan, Lisi, Maro, Midway, P&H

## subset graphing data for French Frigate
# save graphs in figures folder
setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures")

################### FOREREEF ONLY
rd<-rd[rd$Mean.REEF_ZONE=="Forereef",]
## do each island separately, different survey years - ffs
#----------------FFS---------------------------------------------------------------
data<-subset(rd,Mean.ISLAND=="French Frigate") #"Lisianski", "Midway","French Frigate"
data<-drop.levels(data)

benth<-subset(ben.nwhi,Mean.ISLAND=="French Frigate")#"Lisianski", "Midway"
benth<-drop.levels(benth)

##########  parrot size class group graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
levels(dt$Year)<-c("2010-12","2013-15", "2016") # CHANGE FOR RELEVANT YEARS
#dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"PARROTFISH BIOMASS" 
## define labels so to have line breaks

sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
  facet_grid(~Size_class, scales ="fixed") + 
  geom_hline(aes(yintercept = z), hline.data.NWHI_sc_m, colour = "darkred")+
  theme_bw() + theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim = c(0, 20))+
  theme(legend.position = "none")+
  labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))

##########  mean size graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
levels(dt$mean_size)<-"All fishes mean size"
levels(dt$Year)<-c("2010-12","2013-15", "2016")  # CHANGE FOR RELEVANT YEARS
#dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"MEAN SIZE" 
## define labels so to have line breaks

meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
  geom_hline(aes(yintercept = z), hline.data.NWHI_sz_m, colour = "darkred")+
  theme_bw() + theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 27))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))+
  theme(plot.title = element_text(hjust = 0.5))

##########  total fish biomass graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
levels(dt$mean_tot)<-"All fishes biomass"
levels(dt$Year)<-c("2010-12","2013-15", "2016") # CHANGE FOR RELEVANT YEARS
#dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"ALL FISH" 
## define labels so to have line breaks

meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
  geom_hline(aes(yintercept = z), hline.data.NWHI_tb_m, colour = "darkred")+
  theme_bw() + theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 300))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))



######total biomass per consumer group island per year

tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR","Mean.PLANKTIVORE", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PLANKTIVORE","PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
levels(dt$Consumergroup)<-c("Planktivores","Pri. cons.", "Sec. cons.", "Piscivores")
#dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
levels(dt$Year)<-c("2010-12","2013-15", "2016")  # CHANGE FOR RELEVANT YEARS

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)

## define labels so to have line breaks
consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
  facet_grid(~Consumergroup, scales ="fixed") + 
  theme_bw() +
  geom_hline(aes(yintercept = z), hline.data.NWHI_cons_m, colour = "darkred")+
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim = c(0, 195))+
  theme(legend.position = "none")+
  labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))

#   ####### bentic cover per island per year
#   
benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
#   
benthic$Year<-as.factor(benthic$Year)
benthic_se$Year<-as.factor(benthic_se$Year)
#   
test<-melt(benthic)
test_se<-melt(benthic_se)
#   
benthic<-cbind(test, test_se$value)
names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
#   
levels(dt$Year)<-c("2010-12","2013-15", "2016") # CHANGE FOR RELEVANT YEARS
#   
#   ##Define the top and bottom of the errorbars 
limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
dodge <- position_dodge(width=0.9)
#   
#   ## define labels so to have line breaks
cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
  facet_grid(~Benthos, scales ="fixed") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")+
  theme(plot.title = element_text(hjust = 0.5))
#
png(filename = paste("island_","FFS_","FRF_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 800, restoreConsole = TRUE)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
print(meanbio,vp=vplayout(1,1))
print(meansize, vp = vplayout(1, 4))
print(consgrp, vp = vplayout(2, 1:4))
print(cover, vp = vplayout(3, 1:4))

dev.off()   

#---------------- KURE-------------------------------------------------------

data<-subset(rd,Mean.ISLAND=="Kure") #"Lisianski", "Midway","French Frigate"
data<-drop.levels(data)

benth<-subset(ben.nwhi,Mean.ISLAND=="Kure")#"Lisianski", "Midway"
benth<-drop.levels(benth)

##########  parrot size class group graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
levels(dt$Year)<-c( "2007-09", "2010-12" ,  "2013-15" ,  "2016") # CHANGE FOR RELEVANT YEARS
#dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"PARROTFISH BIOMASS" 
## define labels so to have line breaks

sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
  facet_grid(~Size_class, scales ="fixed") + 
  geom_hline(aes(yintercept = z), hline.data.NWHI_sc_m, colour = "darkred")+
  theme_bw() + theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim = c(0, 20))+
  theme(legend.position = "none")+
  labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))

##########  mean size graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
levels(dt$mean_size)<-"All fishes mean size"
levels(dt$Year)<-c( "2007-09", "2010-12" ,  "2013-15" ,  "2016") # CHANGE FOR RELEVANT YEARS
#dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"MEAN SIZE" 
## define labels so to have line breaks

meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
  geom_hline(aes(yintercept = z), hline.data.NWHI_sz_m, colour = "darkred")+
  theme_bw() + theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 27))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))+
  theme(plot.title = element_text(hjust = 0.5))
#meansize
##########  total fish biomass graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
levels(dt$mean_tot)<-"All fishes biomass"
levels(dt$Year)<-c( "2007-09", "2010-12" ,  "2013-15" ,  "2016") # CHANGE FOR RELEVANT YEARS
#dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"ALL FISH" 
## define labels so to have line breaks

meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
  geom_hline(aes(yintercept = z), hline.data.NWHI_tb_m, colour = "darkred")+
  theme_bw() + theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 300))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))
# meanbio


######total biomass per consumer group island per year

tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
levels(dt$Year)<-c( "2007-09", "2010-12" ,  "2013-15" ,  "2016") # CHANGE FOR RELEVANT YEARS

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)

## define labels so to have line breaks
consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
  facet_grid(~Consumergroup, scales ="fixed") + 
  theme_bw() +
  geom_hline(aes(yintercept = z), hline.data.NWHI_cons_m, colour = "darkred")+
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim = c(0, 195))+
  theme(legend.position = "none")+
  labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))
# consgrp

#   ####### bentic cover per island per year
#   
benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
benthic_se<-benthic_se[benthic_se$Year != "2007-2009",] 
benthic<-benthic[benthic$Year != "2007-2009",] 
benthic$Year<-as.factor(benthic$Year)
benthic_se$Year<-as.factor(benthic_se$Year)
test<-melt(benthic)
test_se<-melt(benthic_se)
benthic<-cbind(test, test_se$value)
names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
benthic<-na.omit(benthic)
benthic<-droplevels(benthic)
#levels(dt$Year)<-c( "2010-12" ,  "2013-15" ,  "2016") # CHANGE FOR RELEVANT YEARS
 
##Define the top and bottom of the errorbars 
limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
dodge <- position_dodge(width=0.9)
   
## define labels so to have line breaks
cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
  facet_grid(~Benthos, scales ="fixed") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")+
  theme(plot.title = element_text(hjust = 0.5))

png(filename = paste("island_","KUR_","FRF_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 800, restoreConsole = TRUE)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
print(meanbio,vp=vplayout(1,1))
print(meansize, vp = vplayout(1, 4))
print(consgrp, vp = vplayout(2, 1:4))
print(cover, vp = vplayout(3, 1:4))

dev.off()   


# #----------------------------------LAY AND MARO - same years -------------------------------
# 
# data<-subset(rd,Mean.ISLAND=="Laysan") #"Maro", "Laysan"
# data<-drop.levels(data)
# 
# benth<-subset(ben.nwhi,Mean.ISLAND=="Laysan") #"Maro","Laysan"
# benth<-drop.levels(benth)
# 
# ##########  parrot size class group graph
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
# test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
# test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
# 
# dt<-as.data.frame(cbind(test, test2$SE))
# names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
# levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
# levels(dt$Year)<-c("2009","2011","2015") # CHANGE FOR RELEVANT YEARS
# #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
# 
# ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
# dodge <- position_dodge(width=0.9)
# colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
# title<-"PARROTFISH BIOMASS" 
# ## define labels so to have line breaks
# 
# sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
#   facet_grid(~Size_class, scales ="fixed") + 
#   geom_hline(aes(yintercept = z), hline.data.NWHI_sc_m, colour = "darkred")+
#   theme_bw() + theme(axis.title.x = element_blank()) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   coord_cartesian(ylim = c(0, 26))+
#   theme(legend.position = "none")+
#   labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))
# 
# ##########  mean size graph
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
# test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
# test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
# 
# dt<-as.data.frame(cbind(test, test2$SE))
# names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
# levels(dt$mean_size)<-"All fishes mean size"
# levels(dt$Year)<-c("2009","2011","2015") # CHANGE FOR RELEVANT YEARS
# #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
# 
# ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
# dodge <- position_dodge(width=0.9)
# colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
# title<-"MEAN SIZE" 
# ## define labels so to have line breaks
# 
# meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
#   geom_hline(aes(yintercept = z), hline.data.NWHI_sz_m, colour = "darkred")+
#   theme_bw() + theme(axis.title.x = element_blank()) +
#   coord_cartesian(ylim = c(0, 27))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(legend.position = "none")+
#   labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))
# 
# ##########  total fish biomass graph
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
# test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
# test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")
# 
# dt<-as.data.frame(cbind(test, test2$SE))
# names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
# levels(dt$mean_tot)<-"All fishes biomass"
# levels(dt$Year)<-c("2009","2011","2015") # CHANGE FOR RELEVANT YEARS
# #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
# 
# ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
# dodge <- position_dodge(width=0.9)
# colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
# title<-"ALL FISH" 
# ## define labels so to have line breaks
# 
# meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
#   geom_hline(aes(yintercept = z), hline.data.NWHI_tb_m, colour = "darkred")+
#   theme_bw() + theme(axis.title.x = element_blank()) +
#   coord_cartesian(ylim = c(0, 270))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(legend.position = "none")+
#   labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))
# 
# 
# 
# ######total biomass per consumer group island per year
# 
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
# test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
# test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
# 
# dt<-as.data.frame(cbind(test, test2$SE))
# names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
# levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
# dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
# levels(dt$Year)<-c("2009","2011","2015") # CHANGE FOR RELEVANT YEARS
# 
# ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
# dodge <- position_dodge(width=0.9)
# 
# ## define labels so to have line breaks
# consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
#   facet_grid(~Consumergroup, scales ="fixed") + 
#   theme_bw() +
#   geom_hline(aes(yintercept = z), hline.data.NWHI_cons_m, colour = "darkred")+
#   theme(axis.title.x = element_blank()) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   coord_cartesian(ylim = c(0, 180))+
#   theme(legend.position = "none")+
#   labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))
# 
# #   ####### bentic cover per island per year
# #   
# benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
# benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
# names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
# names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
# #   
# benthic$Year<-as.factor(benthic$Year)
# benthic_se$Year<-as.factor(benthic_se$Year)
# #   
# test<-melt(benthic)
# test_se<-melt(benthic_se)
# #   
# benthic<-cbind(test, test_se$value)
# names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
# #   
# benthic<-benthic[benthic$Year=="2011"|benthic$Year=="2015",]
# #levels(benthic$Year)<-c("2011","2015") # CHANGE FOR RELEVANT YEARS
# benthic<-drop.levels(benthic)
# 
# #   
# #   ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
# dodge <- position_dodge(width=0.9)
# #   
# #   ## define labels so to have line breaks
# cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
#   facet_grid(~Benthos, scales ="fixed") + 
#   theme_bw() +
#   #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(legend.position = "none")+
#   labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")
# 
# #change name to MAR/LAY below
# 
# png(filename = paste("island_","LAY_","FRF_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
# 
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
# print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
# print(meanbio,vp=vplayout(1,1))
# print(meansize, vp = vplayout(1, 4))
# print(consgrp, vp = vplayout(2, 1:4))
# print(cover, vp = vplayout(3, 1:4))
# 
# dev.off() 
# 
# #----------------------------MIDWAY-----------------------------------
# data<-subset(rd,Mean.ISLAND=="Midway") 
# data<-drop.levels(data)
# 
# ##########  parrot size class group graph
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
# test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
# test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
# 
# dt<-as.data.frame(cbind(test, test2$SE))
# names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
# levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
# levels(dt$Year)<-c("2009","2011","2014","2015") # CHANGE FOR RELEVANT YEARS
# #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
# 
# ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
# dodge <- position_dodge(width=0.9)
# colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
# title<-"PARROTFISH BIOMASS" 
# ## define labels so to have line breaks
# 
# sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
#   facet_grid(~Size_class, scales ="fixed") + 
#   geom_hline(aes(yintercept = z), hline.data.NWHI_sc_m, colour = "darkred")+
#   theme_bw() + theme(axis.title.x = element_blank()) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   coord_cartesian(ylim = c(0, 26))+
#   theme(legend.position = "none")+
#   labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))
# 
# ##########  mean size graph
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
# test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
# test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
# 
# dt<-as.data.frame(cbind(test, test2$SE))
# names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
# levels(dt$mean_size)<-"All fishes mean size"
# levels(dt$Year)<-c("2009","2011","2014","2015") # CHANGE FOR RELEVANT YEARS
# #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
# 
# ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
# dodge <- position_dodge(width=0.9)
# colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
# title<-"MEAN SIZE" 
# ## define labels so to have line breaks
# 
# meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
#   geom_hline(aes(yintercept = z), hline.data.NWHI_sz_m, colour = "darkred")+
#   theme_bw() + theme(axis.title.x = element_blank()) +
#   coord_cartesian(ylim = c(0, 27))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(legend.position = "none")+
#   labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))
# 
# ##########  total fish biomass graph
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
# test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
# test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")
# 
# dt<-as.data.frame(cbind(test, test2$SE))
# names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
# levels(dt$mean_tot)<-"All fishes biomass"
# levels(dt$Year)<-c("2009","2011","2014","2015") # CHANGE FOR RELEVANT YEARS
# #dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])
# 
# ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
# dodge <- position_dodge(width=0.9)
# colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
# title<-"ALL FISH" 
# ## define labels so to have line breaks
# 
# meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
#   geom_hline(aes(yintercept = z), hline.data.NWHI_tb_m, colour = "darkred")+
#   theme_bw() + theme(axis.title.x = element_blank()) +
#   coord_cartesian(ylim = c(0, 270))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(legend.position = "none")+
#   labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))
# 
# 
# 
# ######total biomass per consumer group island per year
# 
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
# test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
# tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
# test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
# names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
# 
# dt<-as.data.frame(cbind(test, test2$SE))
# names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
# levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
# dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
# levels(dt$Year)<-c("2009","2011","2014","2015") # CHANGE FOR RELEVANT YEARS
# 
# ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
# dodge <- position_dodge(width=0.9)
# 
# ## define labels so to have line breaks
# consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
#   facet_grid(~Consumergroup, scales ="fixed") + 
#   theme_bw() +
#   geom_hline(aes(yintercept = z), hline.data.NWHI_cons_m, colour = "darkred")+
#   theme(axis.title.x = element_blank()) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   coord_cartesian(ylim = c(0, 180))+
#   theme(legend.position = "none")+
#   labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))
# 
# #   ####### bentic cover per island per year
# #   
# benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
# benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
# names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
# names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
# #   
# benthic$Year<-as.factor(benthic$Year)
# benthic_se$Year<-as.factor(benthic_se$Year)
# #   
# test<-melt(benthic)
# test_se<-melt(benthic_se)
# #   
# benthic<-cbind(test, test_se$value)
# names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
# #   
# benthic<-benthic[benthic$Year=="2011"|benthic$Year=="2015"|benthic$Year=="2014",]
# #levels(benthic$Year)<-c("2011","2015") # CHANGE FOR RELEVANT YEARS
# benthic<-drop.levels(benthic)
# 
# #   
# #   ##Define the top and bottom of the errorbars 
# limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
# dodge <- position_dodge(width=0.9)
# #   
# #   ## define labels so to have line breaks
# cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
#   geom_bar(position="dodge", stat="identity", colour="black") + 
#   geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
#   facet_grid(~Benthos, scales ="fixed") + 
#   theme_bw() +
#   #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(legend.position = "none")+
#   labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")
# 
# 
# png(filename = paste("island_","MID_","FRF_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
# 
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
# print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
# print(meanbio,vp=vplayout(1,1))
# print(meansize, vp = vplayout(1, 4))
# print(consgrp, vp = vplayout(2, 1:4))
# print(cover, vp = vplayout(3, 1:4))
# 
# dev.off() 

#----------------------------------PEARL & HERMES-----------------------------------
data<-subset(rd,Mean.ISLAND=="Pearl & Hermes") 
data<-drop.levels(data)
head(data)
##########  parrot size class group graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
levels(benthic$Year)<-c("2010-12","2013-15","2016") # CHANGE FOR RELEVANT YEARS
#dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"PARROTFISH BIOMASS" 
## define labels so to have line breaks

sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
  facet_grid(~Size_class, scales ="fixed") + 
  geom_hline(aes(yintercept = z), hline.data.NWHI_sc_m, colour = "darkred")+
  theme_bw() + theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim = c(0, 20))+
  theme(legend.position = "none")+
  labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))

##########  mean size graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
levels(dt$mean_size)<-"All fishes mean size"
levels(benthic$Year)<-c("2010-12","2013-15","2016") # CHANGE FOR RELEVANT YEARS
#dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"MEAN SIZE" 
## define labels so to have line breaks

meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
  geom_hline(aes(yintercept = z), hline.data.NWHI_sz_m, colour = "darkred")+
  theme_bw() + theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 27))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))+
  theme(plot.title = element_text(hjust = 0.5))

##########  total fish biomass graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
levels(dt$mean_tot)<-"All fishes biomass"
levels(benthic$Year)<-c("2010-12","2013-15","2016") # CHANGE FOR RELEVANT YEARS
#dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"ALL FISH" 
## define labels so to have line breaks

meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
  geom_hline(aes(yintercept = z), hline.data.NWHI_tb_m, colour = "darkred")+
  theme_bw() + theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 300))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))



######total biomass per consumer group island per year

tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
levels(benthic$Year)<-c("2010-12","2013-15","2016") # CHANGE FOR RELEVANT YEARS

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)

## define labels so to have line breaks
consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
  facet_grid(~Consumergroup, scales ="fixed") + 
  theme_bw() +
  geom_hline(aes(yintercept = z), hline.data.NWHI_cons_m, colour = "darkred")+
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim = c(0, 195))+
  theme(legend.position = "none")+
  labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))

#   ####### bentic cover per island per year
#   
benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
benthic$Year<-as.factor(benthic$Year)
benthic_se$Year<-as.factor(benthic_se$Year)
test<-melt(benthic)
test_se<-melt(benthic_se)
benthic<-cbind(test, test_se$value)
names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
#benthic<-benthic[benthic$Year=="2011"|benthic$Year=="2015"|benthic$Year=="2014",]
levels(benthic$Year)<-c("2010-12","2013-15","2016") # CHANGE FOR RELEVANT YEARS
benthic<-drop.levels(benthic)

#   ##Define the top and bottom of the errorbars 
limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
dodge <- position_dodge(width=0.9)
  
#   ## define labels so to have line breaks
cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
  facet_grid(~Benthos, scales ="fixed") + 
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")+
  theme(plot.title = element_text(hjust = 0.5))


png(filename = paste("island_","P&H_","FRF_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 800, restoreConsole = TRUE)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
print(meanbio,vp=vplayout(1,1))
print(meansize, vp = vplayout(1, 4))
print(consgrp, vp = vplayout(2, 1:4))
print(cover, vp = vplayout(3, 1:4))

dev.off() 

#----------------------------------LISIANSKI-----------------------------------
data<-subset(rd,Mean.ISLAND=="Lisianski") 
data<-drop.levels(data)
head(data)
##########  parrot size class group graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
levels(dt$Year)<-c("2007-09","2010-12","2013-15","2016") # CHANGE FOR RELEVANT YEARS
#dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"PARROTFISH BIOMASS" 
## define labels so to have line breaks

sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
  facet_grid(~Size_class, scales ="fixed") + 
  geom_hline(aes(yintercept = z), hline.data.NWHI_sc_m, colour = "darkred")+
  theme_bw() + theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim = c(0, 20))+
  theme(legend.position = "none")+
  labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))

##########  mean size graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
levels(dt$mean_size)<-"All fishes mean size"
levels(dt$Year)<-c("2007-09","2010-12","2013-15","2016") # CHANGE FOR RELEVANT YEARS
#dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"MEAN SIZE" 
## define labels so to have line breaks

meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
  geom_hline(aes(yintercept = z), hline.data.NWHI_sz_m, colour = "darkred")+
  theme_bw() + theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 27))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))+
  theme(plot.title = element_text(hjust = 0.5))

##########  total fish biomass graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
levels(dt$mean_tot)<-"All fishes biomass"
levels(dt$Year)<-c("2007-09","2010-12","2013-15","2016") # CHANGE FOR RELEVANT YEARS
#dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(3,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"ALL FISH" 
## define labels so to have line breaks

meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
  geom_hline(aes(yintercept = z), hline.data.NWHI_tb_m, colour = "darkred")+
  theme_bw() + theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 300))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))



######total biomass per consumer group island per year

tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
levels(dt$Year)<-c("2007-09","2010-12","2013-15","2016") # CHANGE FOR RELEVANT YEARS

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)

## define labels so to have line breaks
consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
  facet_grid(~Consumergroup, scales ="fixed") + 
  theme_bw() +
  geom_hline(aes(yintercept = z), hline.data.NWHI_cons_m, colour = "darkred")+
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim = c(0, 195))+
  theme(legend.position = "none")+
  labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))

#   ####### bentic cover per island per year
#   
benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
#   
benthic$Year<-as.factor(benthic$Year)
benthic_se$Year<-as.factor(benthic_se$Year)
#   
test<-melt(benthic)
test_se<-melt(benthic_se)
#   
benthic<-cbind(test, test_se$value)
names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
#   
#benthic<-benthic[benthic$Year=="2011"|benthic$Year=="2015"|benthic$Year=="2014"|benthic$Year=="2010"|benthic$Year=="2012",]
#levels(benthic$Year)<-c("2010","2011","2012","2014","2015") # CHANGE FOR RELEVANT YEARS
benthic<-benthic[benthic$Year != "2007-2009",]
benthic<-drop.levels(benthic)

#   
#   ##Define the top and bottom of the errorbars 
limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
dodge <- position_dodge(width=0.9)
#   
#   ## define labels so to have line breaks
cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
  facet_grid(~Benthos, scales ="fixed") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")+
  theme(plot.title = element_text(hjust = 0.5))


png(filename = paste("island_","Lis_","FRF_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 800, restoreConsole = TRUE)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
print(meanbio,vp=vplayout(1,1))
print(meansize, vp = vplayout(1, 4))
print(consgrp, vp = vplayout(2, 1:4))
print(cover, vp = vplayout(3, 1:4))

dev.off() 

# -------------------------- NWHI LAGOON, BACKREEF, PROTECTED SLOPE ---------------------
### LAGOON FOR FFS, MID, P&H, KUR - only for reef zones surveyed in this year 2016: FFS, P&H
### BACKREEF FOR PH, MID, FFS, KUR; NONE 2016
### PROTECTED SLOPE FOR FFS

# Subset data FOR REEF ZONE
rd<-subset(rdd, Mean.REGION == "NWHI")
rd<-subset(rd, Mean.REEF_ZONE == "Protected Slope") # "Backreef"  "Lagoon" "Protected Slope"
rd<-drop.levels(rd)
ben.nwhi<-subset(ben, Mean.REGION == "NWHI")
ben.nwhi<-subset(ben.nwhi, Mean.REEF_ZONE == "Protected Slope")
ben.nwhi<-drop.levels(ben.nwhi)

# Set graph limits for each reef zone
max(rd$Mean.TotFish+rd$PooledSE.TotFish) ## check to manually set the ylims on graphs ## set at LAGOON: 150, PS: 294
max(rd$Mean.MEAN_SIZE+rd$PooledSE.MEAN_SIZE) # LAGOON: 25, PS: 25
max(rd$Mean.10_30+rd$PooledSE.10_30)
max(rd$Mean.30_plus+rd$PooledSE.30_plus) # SIZE CLASS: LAGOON: 20, PS: 10
max(rd$Mean.PRIMARY+rd$PooledSE.PRIMARY) ## set to LAGOON: 125, PS: 200
max(rd$Mean.SECONDARY+rd$PooledSE.SECONDARY)
max(rd$Mean.PISCIVORE+rd$PooledSE.PISCIVORE)
max(rd$Mean.PLANKTIVORE+rd$PooledSE.PLANKTIVORE)

##### subset for each island
data<-subset(rd,Mean.ISLAND=="French Frigate") # "French Frigate" "Pearl & Hermes"
data<-drop.levels(data)
head(data)

##########  parrot size class group graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.10_30", "Mean.30_plus")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.10_30", "PooledSE.30_plus")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"PARROTFISH BIOMASS" 
## define labels so to have line breaks

sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
  facet_grid(~Size_class, scales ="fixed") + 
  theme_bw() + theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  ### USE FOR FFS
  coord_cartesian(ylim = c(0, 10))+
  theme(legend.position = "none")+
  labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))

##########  mean size graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
levels(dt$mean_size)<-"All fishes mean size"

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"MEAN SIZE" 
## define labels so to have line breaks

meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
  theme_bw() + theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 25))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))+
  theme(plot.title = element_text(hjust = 0.5))

##########  total fish biomass graph
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
levels(dt$mean_tot)<-"All fishes biomass"

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
title<-"ALL FISH" 
## define labels so to have line breaks

meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
  theme_bw() + theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(0, 295))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")+
  labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))

######total biomass per consumer group island per year

tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)

## define labels so to have line breaks
consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
  facet_grid(~Consumergroup, scales ="fixed") + 
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  # NEED FOR FFS
  coord_cartesian(ylim = c(0, 200))+
  theme(legend.position = "none")+
  labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))+
  theme(plot.title = element_text(hjust = 0.5))

#   ####### bentic cover per island per year
benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")

benthic$Year<-as.factor(benthic$Year)
benthic_se$Year<-as.factor(benthic_se$Year)   
test<-melt(benthic)
test_se<-melt(benthic_se)

benthic<-cbind(test, test_se$value)
names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
benthic<-drop.levels(benthic)

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
dodge <- position_dodge(width=0.9)

#define labels so to have line breaks
cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
  geom_bar(position="dodge", stat="identity", colour="black") + 
  geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
  facet_grid(~Benthos, scales ="fixed") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # NEED FOR FFS
  theme(legend.position = "none")+
  labs(title = "MAJOR BENTHIC GROUPS", y = "Cover (%)")+
  theme(plot.title = element_text(hjust = 0.5))

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!CHANGE FILE NAME FOR EACH ISLAND!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"P&H_","MID_"
png(filename = paste("island_","FFS_","PS_2016.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
print(meanbio,vp=vplayout(1,1))
print(meansize, vp = vplayout(1, 4))
print(consgrp, vp = vplayout(2, 1:4))
print(cover, vp = vplayout(3, 1:4))

dev.off() 
# GO BACK TO LAGOON, BACKREEF, PROTECTED SLOPE



