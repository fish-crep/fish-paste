### LOAD DATA #####
setwd("D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/data/Data Outputs")
rm(list=ls())

## fish status report standard figures
## island section
library(reshape)
library(gdata)
library(grid)
library(gridExtra)
library(gridBase)
library(ggplot2)

## data for island graphs - FOREREEF ONLY

load("MONREPdata_pooled_is_yr_RZ.rdata") ### island estimates mean over cruises since spc after 2007
rd<-as.data.frame(dp)

# change year to factor and OBS_YEAR to match code
rd$Mean.OBS_YEAR<-as.factor(rd$Mean.ANALYSIS_YEAR)

## data for region reference lines
load("MONREPdata_pooled_reg_FRF.rdata")
ref<-as.data.frame(dpR) # forereef only reference

# NO ref lines for lagoon and backreef

# for pooled benthic data FOREREEF ONLY 
load("MONREPdata_pooled_is_RZ.rdata")
ben<-as.data.frame(dpI)

# save all data as rdd so can pull from that file for each region
rdd<-rd

## with the reference lines, need to run each region separately...
# what areas:
year<-rdd[rdd$Mean.ANALYSIS_YEAR == "2019",]
year$Mean.ISLAND

# MHI -----------------------------------------------

## create reference lines for region

# consumer group 
mhi_ref_cons<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI",c("Mean.PRIMARY","Mean.SECONDARY","Mean.PLANKTIVORE","Mean.PISCIVORE")]))

# biomass of parrots in 2 size classes
mhi_ref_sc<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI",c("Mean.P10_30","Mean.P30_plus")]))

# biomass of all fishes
mhi_ref_tb<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI","Mean.TotFish"]))

# mean size
mhi_ref_sz<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI","Mean.MEAN_SIZE"]))

## references lines for plots
hline.data.MHI_cons_m <- data.frame(z = mhi_ref_cons, Consumergroup = c("Pri. cons.","Sec. cons.","Planktivores","Piscivores")) ## needs to match the graph type

hline.data.MHI_sc_m <- data.frame(z = mhi_ref_sc, Size_class = c("Parrots 10-30 cm", "Parrots >30 cm")) ## needs to match the graph type

hline.data.MHI_tb_m <- data.frame(z = mhi_ref_tb, Size_class = "All fishes") ## needs to match the graph type

hline.data.MHI_sz_m <- data.frame(z = mhi_ref_sz, mean_size = "All fishes mean size") ## needs to match the graph type

## with the reference lines, need to run each region separately...
rd<-subset(rdd, Mean.REGION == "MHI")
rd<-droplevels(rd)
ben.pria<-subset(ben, Mean.REGION == "MHI")

# save graphs in figures folder
setwd("D:/CRED/fish_cruise_routine_report/monitoring_report/2019_status_report/figures")

################### MHI Forereef #####################################
rd<-subset(rdd, Mean.REGION == "MHI")
rd<-droplevels(rd)
ben.pria<-subset(ben, Mean.REGION == "MHI")
ben.pria<-subset(ben.pria, Mean.REEF_ZONE == "Forereef")
rd<-rd[rd$Mean.REEF_ZONE=="Forereef",]
# add okina to island names
levels(rd$Mean.ISLAND)
levels(rd$Mean.ISLAND)<-c("Hawai`i","Kaho`olawe","Kaua`i","Lana`i", "Maui","Moloka`i","Ni`ihau","Oah`u")

# batch make the graphs
for(i in 1:length(rd$Mean.ISLAND)){
  
  #i<-"Jarvis"
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  #data<-subset(rd,Mean.ISLAND=="Jarvis")
  data<-drop.levels(data)

  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.P10_30", "Mean.P30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.P10_30", "PooledSE.P30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
   
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  title<-"PARROTFISH BIOMASS" 
  ## define labels so to have line breaks
  
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") + 
    geom_hline(aes(yintercept = z), hline.data.MHI_sc_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 6))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
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
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
  dodge <- position_dodge(width=0.9)
  title<-"MEAN SIZE" 
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    geom_hline(aes(yintercept = z), hline.data.MHI_sz_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 18))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
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
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"ALL FISH" 
  ## define labels so to have line breaks
  
  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
    geom_hline(aes(yintercept = z), hline.data.MHI_tb_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 35))+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
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
    #coord_cartesian(ylim = c(0, 20))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
    labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
####### bentic cover per island per year
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
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
  dodge <- position_dodge(width=0.9)
    
  ## define labels so to have line breaks
  cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
    facet_grid(~Benthos, scales ="fixed") + 
    theme_bw() +
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
    labs(title = "VISUALLY ESTIMATED BENTHIC GROUPS", y = "Cover (%)")
  
  png(filename = paste(s,"FRF_2019.png", sep = "_"), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  grid.newpage() #plot.new()
  pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
  print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
  print(meanbio,vp=vplayout(1,1))
  print(meansize, vp = vplayout(1, 4))
  print(consgrp, vp = vplayout(2, 1:4))
  print(cover, vp = vplayout(3, 1:4))
  
  dev.off()   
  
}

###################  PRIAs Lagoon #######################################

rd<-subset(rdd, Mean.REGION == "PRIAs")
rd<-droplevels(rd)
ben.pria<-subset(ben, Mean.REGION == "PRIAs")
ben.pria<-subset(ben.pria, Mean.REEF_ZONE == "Lagoon")
rd<-rd[rd$Mean.REEF_ZONE=="Lagoon",]
#for(i in 1:length(rd$Mean.ISLAND)){

  i<-"Kingman" # only did Kingman lagoon in 2018
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  data<-subset(rd,Mean.ISLAND=="Kingman")
  data<-drop.levels(data)

  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.P10_30", "Mean.P30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.P10_30", "PooledSE.P30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")

  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
 
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
  dodge <- position_dodge(width=0.9)
  title<-"PARROTFISH BIOMASS"
  ## define labels so to have line breaks

  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_sc_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    coord_cartesian(ylim = c(0, 20))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
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
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE)
  dodge <- position_dodge(width=0.9)
  title<-"MEAN SIZE"
  ## define labels so to have line breaks

  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_sz_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 25))+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
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
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
  dodge <- position_dodge(width=0.9)
  title<-"ALL FISH"
  ## define labels so to have line breaks

  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_tb_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 355))+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
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
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
  dodge <- position_dodge(width=0.9)

  ## define labels so to have line breaks
  consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
    facet_grid(~Consumergroup, scales ="fixed") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_cons_m, colour = "darkred")+
    theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 230))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
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
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(title = "VISUALLY ESTIMATED BENTHIC GROUPS", y = "Cover (%)")
  #
  png(filename = paste("Kingman","LAG_2018.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)

  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
  print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
  print(meanbio,vp=vplayout(1,1))
  print(meansize, vp = vplayout(1, 4))
  print(consgrp, vp = vplayout(2, 1:4))
  print(cover, vp = vplayout(3, 1:4))

  dev.off()

#}

################### PRIAs Backreef ##########################
rd<-subset(rdd, Mean.REGION == "PRIAs")
rd<-droplevels(rd)
ben.pria<-subset(ben, Mean.REGION == "PRIAs")
ben.pria<-subset(ben.pria, Mean.REEF_ZONE == "Backreef")
rd<-rd[rd$Mean.REEF_ZONE=="Backreef",]
#for(i in 1:length(rd$Mean.ISLAND)){

  i<-"Kingman"
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  data<-subset(rd,Mean.ISLAND=="Kingman")
  data<-drop.levels(data)

  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.P10_30", "Mean.P30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.P10_30", "PooledSE.P30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")

  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-35 cm", "Parrots >35 cm")
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
  dodge <- position_dodge(width=0.9)
  title<-"PARROTFISH BIOMASS"
  ## define labels so to have line breaks

  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_sc_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 20))+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
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
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE)
  dodge <- position_dodge(width=0.9)
  title<-"MEAN SIZE"
  ## define labels so to have line breaks

  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_sz_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 25))+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
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
 
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
  dodge <- position_dodge(width=0.9)
  title<-"ALL FISH"
  ## define labels so to have line breaks

  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_tb_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 355))+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
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
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    #coord_cartesian(ylim = c(0, 230))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
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
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(title = "VISUALLY ESTIMATED BENTHIC GROUPS", y = "Cover (%)")
  #
  png(filename = paste("Kingman","BRF_2018.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)

  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
  print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
  print(meanbio,vp=vplayout(1,1))
  print(meansize, vp = vplayout(1, 4))
  print(consgrp, vp = vplayout(2, 1:4))
  print(cover, vp = vplayout(3, 1:4))

  dev.off()

#}
  ################### PRIAs Protected Slope ##########################
  rd<-subset(rdd, Mean.REGION == "PRIAs")
  rd<-droplevels(rd)
  ben.pria<-subset(ben, Mean.REGION == "PRIAs")
  ben.pria<-subset(ben.pria, Mean.REEF_ZONE == "Protected Slope")
  rd<-rd[rd$Mean.REEF_ZONE=="Protected Slope",]
  #for(i in 1:length(rd$Mean.ISLAND)){
  
  i<-"Kingman"
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  data<-subset(rd,Mean.ISLAND=="Kingman")
  data<-drop.levels(data)
  
  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.P10_30", "Mean.P30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.P10_30", "PooledSE.P30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-35 cm", "Parrots >35 cm")
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
  dodge <- position_dodge(width=0.9)
  title<-"PARROTFISH BIOMASS"
  ## define labels so to have line breaks
  
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_sc_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 20))+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
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
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE)
  dodge <- position_dodge(width=0.9)
  title<-"MEAN SIZE"
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_sz_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 25))+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
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
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
  dodge <- position_dodge(width=0.9)
  title<-"ALL FISH"
  ## define labels so to have line breaks
  
  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
    #geom_hline(aes(yintercept = z), hline.data.PRIAs_tb_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 355))+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
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
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    #coord_cartesian(ylim = c(0, 230))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
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
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(legend.position = "none")+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(title = "VISUALLY ESTIMATED BENTHIC GROUPS", y = "Cover (%)")
  #
  png(filename = paste("Kingman","PS_2018.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
  print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
  print(meanbio,vp=vplayout(1,1))
  print(meansize, vp = vplayout(1, 4))
  print(consgrp, vp = vplayout(2, 1:4))
  print(cover, vp = vplayout(3, 1:4))
  
  dev.off()

  # SAMOA -----------------------------------------------

## create reference lines for region
# to check col nums
head(ref)
colnames(ref)

# consumer group
SAM_ref_cons<-as.numeric(as.vector(ref[ref$Mean.REGION=="SAMOA",c("Mean.PRIMARY","Mean.SECONDARY","Mean.PLANKTIVORE","Mean.PISCIVORE")]))

# biomass of parrots in 2 size classes
SAM_ref_sc<-as.numeric(as.vector(ref[ref$Mean.REGION=="SAMOA",c("Mean.P10_30","Mean.P30_plus")]))

# biomass of all fishes
SAM_ref_tb<-as.numeric(as.vector(ref[ref$Mean.REGION=="SAMOA","Mean.TotFish"]))

# mean size
SAM_ref_sz<-as.numeric(as.vector(ref[ref$Mean.REGION=="SAMOA","Mean.MEAN_SIZE"]))

hline.data.SAM_sc_m <- data.frame(z = SAM_ref_sc, Size_class = c("Parrots 10-30 cm", "Parrots >30 cm")) ## needs to match the graph type
hline.data.SAM_tb_m <- data.frame(z = SAM_ref_tb, Size_class = "All fishes") ## needs to match the graph type
hline.data.SAM_sz_m <- data.frame(z = SAM_ref_sz, mean_size = "All fishes mean size") ## needs to match the graph type
hline.data.SAM_cons_m <- data.frame(z = SAM_ref_cons, Consumergroup = c("Pri. cons.","Sec. cons.","Planktivores","Piscivores")) 
## with the reference lines, need to run each region separately...

rd<-subset(rdd, Mean.REGION == "SAMOA")
rd<-droplevels(rd)
ben.sam<-subset(ben, Mean.REGION == "SAMOA")

# save graphs in figures folder
setwd("D:/CRED/fish_cruise_routine_report/monitoring_report/2018_status_report/figures")


###################  SAMOA Forereef ##################
rd<-rd[rd$Mean.REEF_ZONE=="Forereef",]
for(i in 1:length(rd$Mean.ISLAND)){

  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  #data<-subset(rd,Mean.ISLAND=="Tutuila")
  data<-drop.levels(data)

  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.P10_30", "Mean.P30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.P10_30", "PooledSE.P30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  title<-"PARROTFISH BIOMASS" 
  ## define labels so to have line breaks
  
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") + 
    geom_hline(aes(yintercept = z), hline.data.SAM_sc_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 6))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
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
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
  dodge <- position_dodge(width=0.9)
  title<-"MEAN SIZE" 
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    geom_hline(aes(yintercept = z), hline.data.SAM_sz_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 18))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
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
    #coord_cartesian(ylim = c(0, 35))+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
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
    #coord_cartesian(ylim = c(0, 20))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
    labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  ####### bentic cover per island per year
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
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
  dodge <- position_dodge(width=0.9)
  
  ## define labels so to have line breaks
  cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
    facet_grid(~Benthos, scales ="fixed") + 
    theme_bw() +
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
    labs(title = "VISUALLY ESTIMATED BENTHIC GROUPS", y = "Cover (%)")
  
  png(filename = paste(s,"FRF_2018.png", sep = "_"), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)

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

# ###################  SAMOA Lagoon #####################################
# rd<-rd[rd$Mean.REEF_ZONE=="Lagoon",]
# for(i in 1:length(rd$Mean.ISLAND)){
#   
#   s<-rd$Mean.ISLAND[i]
#   data<-subset(rd, Mean.ISLAND == s) ## change back to s
#   #data<-subset(rd,Mean.ISLAND=="Rose")
#   data<-drop.levels(data)
#   
#   ##########  parrot size class group graph
#   tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.P10_30", "Mean.P30_plus")]
#   test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
#   names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
#   tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.P10_30", "PooledSE.P30_plus")]
#   test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
#   names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
#   
#   dt<-as.data.frame(cbind(test, test2$SE))
#   names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
#   levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
#   
#   ##Define the top and bottom of the errorbars 
#   limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
#   dodge <- position_dodge(width=0.9)
#   title<-"PARROTFISH BIOMASS" 
#   ## define labels so to have line breaks
#   
#   sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
#     geom_bar(position="dodge", stat="identity", colour="black") + 
#     geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
#     facet_grid(~Size_class, scales ="fixed") + 
#     #geom_hline(aes(yintercept = z), hline.data.SAM_sc_m, colour = "darkred")+
#     theme_bw() + theme(axis.title.x = element_blank()) +
#     #coord_cartesian(ylim = c(0, 6))+
#     theme(legend.position = "none")+
#     theme(axis.text.x = element_text(angle = 90, vjust = .5))+
#     theme(plot.title = element_text(hjust = 0.5))+ 
#     labs(title = "PARROTFISH BIOMASS", y = expression(paste("Fish biomass (g ", m^-2,")")))
#   
#   ##########  mean size graph
#   tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.MEAN_SIZE")]
#   test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
#   names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
#   tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.MEAN_SIZE")]
#   test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
#   names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
#   
#   dt<-as.data.frame(cbind(test, test2$SE))
#   names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
#   levels(dt$mean_size)<-"All fishes mean size"
#   
#   ##Define the top and bottom of the errorbars 
#   limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
#   dodge <- position_dodge(width=0.9)
#   title<-"MEAN SIZE" 
#   ## define labels so to have line breaks
#   
#   meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
#     geom_bar(position="dodge", stat="identity", colour="black") + 
#     geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
#     #geom_hline(aes(yintercept = z), hline.data.SAM_sz_m, colour = "darkred")+
#     theme_bw() + theme(axis.title.x = element_blank()) +
#     #coord_cartesian(ylim = c(0, 18))+
#     theme(legend.position = "none")+
#     theme(axis.text.x = element_text(angle = 90, vjust = .5))+
#     theme(plot.title = element_text(hjust = 0.5))+ 
#     labs(title = "MEAN SIZE", y = expression(paste("Total length (cm)")))
#   
#   ##########  total fish biomass graph
#   tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.TotFish")]
#   test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
#   names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
#   tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.TotFish")]
#   test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
#   names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")
#   
#   dt<-as.data.frame(cbind(test, test2$SE))
#   names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
#   levels(dt$mean_tot)<-"All fishes biomass"
#   
#   ##Define the top and bottom of the errorbars 
#   limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
#   dodge <- position_dodge(width=0.9)
#   colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
#   title<-"ALL FISH" 
#   ## define labels so to have line breaks
#   
#   meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
#     geom_bar(position="dodge", stat="identity", colour="black") + 
#     geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
#     #geom_hline(aes(yintercept = z), hline.data.SAM_tb_m, colour = "darkred")+
#     theme_bw() + theme(axis.title.x = element_blank()) +
#     #coord_cartesian(ylim = c(0, 35))+
#     theme(axis.text.x = element_text(angle = 90, vjust = .5))+
#     theme(plot.title = element_text(hjust = 0.5))+ 
#     theme(legend.position = "none")+
#     labs(title = "ALL FISH", y = expression(paste("Fish biomass (g ", m^-2,")")))
#   
#   
#   ######total biomass per consumer group island per year
#   
#   tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
#   test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
#   names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
#   tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
#   test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
#   names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
#   
#   dt<-as.data.frame(cbind(test, test2$SE))
#   names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
#   levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores")
#   dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
#   
#   ##Define the top and bottom of the errorbars 
#   limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
#   dodge <- position_dodge(width=0.9)
#   
#   ## define labels so to have line breaks
#   consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
#     geom_bar(position="dodge", stat="identity", colour="black") + 
#     geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
#     facet_grid(~Consumergroup, scales ="fixed") + 
#     theme_bw() +
#     #geom_hline(aes(yintercept = z), hline.data.SAM_cons_m, colour = "darkred")+
#     theme(axis.title.x = element_blank()) +
#     #coord_cartesian(ylim = c(0, 20))+
#     theme(legend.position = "none")+
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
#     theme(plot.title = element_text(hjust = 0.5))+ 
#     labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))
#   
#   ####### bentic cover per island per year
#   benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
#   benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
#   names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
#   names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
#   
#   benthic$Year<-as.factor(benthic$Year)
#   benthic_se$Year<-as.factor(benthic_se$Year)
#   
#   test<-melt(benthic)
#   test_se<-melt(benthic_se)
#   
#   benthic<-cbind(test, test_se$value)
#   names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
#   
#   ##Define the top and bottom of the errorbars 
#   limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
#   dodge <- position_dodge(width=0.9)
#   
#   ## define labels so to have line breaks
#   cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
#     geom_bar(position="dodge", stat="identity", colour="black") + 
#     geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
#     facet_grid(~Benthos, scales ="fixed") + 
#     theme_bw() +
#     theme(legend.position = "none")+
#     theme(axis.text.x = element_text(angle = 90, vjust = .5))+
#     theme(plot.title = element_text(hjust = 0.5))+ 
#     labs(title = "VISUALLY ESTIMATED BENTHIC GROUPS", y = "Cover (%)")
#   
#   png(filename = paste(s,"LAG_2018.png", sep = "_"), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
#   
#   vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
#   
#   grid.newpage()
#   pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
#   print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
#   print(meanbio,vp=vplayout(1,1))
#   print(meansize, vp = vplayout(1, 4))
#   print(consgrp, vp = vplayout(2, 1:4))
#   print(cover, vp = vplayout(3, 1:4))
#   
#   dev.off()
#   
# }

# 
# ###################  SAMOA Backreef #######################################
rd<-subset(rdd, Mean.REGION == "SAMOA")
rd<-droplevels(rd)
ben.sam<-subset(ben, Mean.REGION == "SAMOA")
ben.sam<-subset(ben.sam, Mean.REEF_ZONE == "Backreef")
rd<-rd[rd$Mean.REEF_ZONE=="Backreef",]
# save graphs in figures folder
setwd("D:/CRED/fish_cruise_routine_report/monitoring_report/2018_status_report/figures")

rd<-rd[rd$Mean.REEF_ZONE=="Backreef",]
for(i in 1:length(rd$Mean.ISLAND)){
  
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  #data<-subset(rd,Mean.ISLAND=="Rose")
  data<-drop.levels(data)
  
  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.P10_30", "Mean.P30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.P10_30", "PooledSE.P30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.OBS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  title<-"PARROTFISH BIOMASS" 
  ## define labels so to have line breaks
  
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Reds") +
    facet_grid(~Size_class, scales ="fixed") + 
    #geom_hline(aes(yintercept = z), hline.data.SAM_sc_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 6))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
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
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE) 
  dodge <- position_dodge(width=0.9)
  title<-"MEAN SIZE" 
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(210/255,60/255,0/255)) +
    #geom_hline(aes(yintercept = z), hline.data.SAM_sz_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 18))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
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
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  colours <-c("#E60000", "#A7DBD8", "#E0E4CC", "#F38630")
  title<-"ALL FISH" 
  ## define labels so to have line breaks
  
  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=rgb(168/255,0/255,0/255)) +
    #geom_hline(aes(yintercept = z), hline.data.SAM_tb_m, colour = "darkred")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 35))+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
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
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
  dodge <- position_dodge(width=0.9)
  
  ## define labels so to have line breaks
  consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_brewer(palette = "Blues") +
    facet_grid(~Consumergroup, scales ="fixed") + 
    theme_bw() +
    #geom_hline(aes(yintercept = z), hline.data.SAM_cons_m, colour = "darkred")+
    theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 20))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
    labs(title = "FISH BIOMASS BY CONSUMER GROUP", y = expression(paste("Fish biomass (g ", m^-2,")")))
  
  ####### bentic cover per island per year
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
  
  ##Define the top and bottom of the errorbars 
  limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
  dodge <- position_dodge(width=0.9)
  
  ## define labels so to have line breaks
  cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
    geom_bar(position="dodge", stat="identity", colour="black") + 
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
    facet_grid(~Benthos, scales ="fixed") + 
    theme_bw() +
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+ 
    labs(title = "VISUALLY ESTIMATED BENTHIC GROUPS", y = "Cover (%)")
  
  png(filename = paste(s,"BRF_2018.png", sep = "_"), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
  
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

#   # NMARIANAS -----------------------------------------------
# 
# ## create reference lines for region
# marn_ref_cons<-as.numeric(as.vector(ref[ref$Mean.REGION=="N.MARIAN",c("Mean.PRIMARY","Mean.SECONDARY","Mean.PLANKTIVORE","Mean.PISCIVORE")]))
#  # consumer groups
# 
# # for biomass of parrots in 2 size classes
# marn_ref_sc<-as.numeric(as.vector(ref[ref$Mean.REGION=="N.MARIAN",c("Mean.P10_30","Mean.P30_plus")]))
# 
# # for  all fish biomass
# marn_ref_tb<-as.numeric(as.vector(ref[ref$Mean.REGION=="N.MARIAN",c("Mean.TotFish")]))
# 
# # mean size
# marn_ref_sz<-as.numeric(as.vector(ref[ref$Mean.REGION=="N.MARIAN","Mean.MEAN_SIZE"]))
# 
# 
# ## references lines for plots, use ..._m (mean), sep (m plus se) and sem (m minus se)
# hline.data.MARN_cons_m <- data.frame(z = marn_ref_cons, Consumergroup = c("Pri. cons.", "Sec. cons.","Planktivores","Piscivores")) ## needs to match the graph type
# 
# hline.data.MARN_sc_m <- data.frame(z = marn_ref_sc, Size_class = c("Parrots 10-30 cm", "Parrots >30 cm")) ## needs to match the graph type
# 
# hline.data.MARN_tb_m <- data.frame(z = marn_ref_tb, Size_class = "All fishes") ## needs to match the graph type
# 
# hline.data.MARN_sz_m <- data.frame(z = marn_ref_sz, mean_size = "All fishes mean size") ## needs to match the graph type
# 
# ## with the reference lines, need to run each region separately...
# rd<-subset(rdd, Mean.REGION == "N.MARIAN")
# rd<-drop.levels(rd)
# ben.nmari<-subset(ben, Mean.REGION == "N.MARIAN")
# ben.nmari<-subset(ben.nmari, Mean.REEF_ZONE == "Forereef")
# ben.nmari<-drop.levels(ben.nmari)
# 
# # save graphs in figures folder
# setwd("D:/CRED/fish_cruise_routine_report/monitoring_report/201_status_report/figures")
# 
# ## batch make the graphs !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! come back to run for Agrihan, no benthic data for 2014, didn't go there that year
# for(i in 1:length(rd$Mean.ISLAND)){
# s<-rd$Mean.ISLAND[i]
# data<-subset(rd, Mean.ISLAND == s) ## change back to s
# #data<-subset(rd,Mean.ISLAND=="Agrihan")
# data<-drop.levels(data)
# 
#   ##########  parrot size class group graph
#     #   ####### bentic cover per island per year
#   #   
#   benthic<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "Mean.HARD_CORAL", "Mean.MA", "Mean.CCA")]
#   benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
#   # get rid of 2009, no data !!!!!!!! and 2014 for Agrihan
#   benthic<-benthic[2:4,]
#   benthic<-drop.levels(benthic)
#   benthic_se<-data[,c("Mean.ISLAND","Mean.OBS_YEAR", "PooledSE.HARD_CORAL", "PooledSE.MA", "PooledSE.CCA")]
#   # ## !!!!!!!! and 2014 for Agrihan
#   # benthic<-benthic[1:2,]
#   # benthic<-drop.levels(benthic)
#   
#   names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
#   benthic_se<-benthic_se[2:4,]
#   benthic_se<-drop.levels(benthic_se)
#   names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
#   
#   # ## !!!!!!!! and 2014 for Agrihan
#   # benthic_se<-benthic_se[1:2,]
#   # benthic_se<-drop.levels(benthic_se)
#   
#   names(benthic)<-c("Island", "Year", "Hard coral", "Macroalgae", "Encrusting algae")
#   names(benthic_se)<-c("Island", "Year", "HC.SE", "MA.SE", "CCA.SE")
#   #   
#   benthic$Year<-as.factor(benthic$Year)
#   benthic_se$Year<-as.factor(benthic_se$Year)
#   #   
#   test<-melt(benthic)
#   test_se<-melt(benthic_se)
#   #   
#   benthic<-cbind(test, test_se$value)
#   names(benthic)<-c("Island", "Year", "Benthos", "Cover", "Cover_se")
#   
#   #   ##Define the top and bottom of the errorbars 
#   limits <- aes(ymax = Cover + Cover_se, ymin=Cover - Cover_se) 
#   dodge <- position_dodge(width=0.9)
#   #   
#   #   ## define labels so to have line breaks
#   cover <- ggplot(benthic, aes(fill=Benthos, y=Cover, x=Year)) + 
#     geom_bar(position="dodge", stat="identity", colour="black") + 
#     geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=c(rgb(209/255,255/255,115/255),rgb(152/255,230/255,0/255),rgb(89/255,150/255,0/255))) +
#     facet_grid(~Benthos, scales ="fixed") + 
#     theme_bw() +
#     theme(legend.position = "none")+
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#     theme(plot.title = element_text(hjust = 0.5))+ 
#     labs(title = "VISUALLY ESTIMATED BENTHIC GROUPS", y = "Cover (%)")
#   #
#   png(filename = paste("island_",s,"2017.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
#  # ### !!!!!!! Agrihan: 
#  #  png(filename = paste("Agrihan","2017.png", sep = ""), width = 6.5, height = 8, units = "in",  bg = "white", res = 600, restoreConsole = TRUE)
#   vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
#   
#   grid.newpage()
#   pushViewport(viewport(layout = grid.layout(3, 4))) # 3 rows, 3 columns
#   print(sizeclass, vp = vplayout(1, 2:3))  # this plot covers row 1 and cols 1:2
#   print(meanbio,vp=vplayout(1,1))
#   print(meansize, vp = vplayout(1, 4))
#   print(consgrp, vp = vplayout(2, 1:4))
#   print(cover, vp = vplayout(3, 1:4))
#   
#   dev.off()   
#   
# }


# SMARIANAS -----------------------------------------------

## create reference lines for region
## with the reference lines, need to run each region separately...
rd<-subset(rdd, Mean.REGION == "S.MARIAN")
ben.smari<-subset(ben, Mean.REGION == "S.MARIAN")
ben.smari<-subset(ben.smari, Mean.REEF_ZONE == "Forereef")

## create reference lines for region
mars_ref_cons<-as.numeric(as.vector(ref[ref$Mean.REGION=="S.MARIAN",c("Mean.PRIMARY","Mean.SECONDARY","Mean.PLANKTIVORE","Mean.PISCIVORE")]))
# consumer groups

# for biomass of parrots in 2 size classes
mars_ref_sc<-as.numeric(as.vector(ref[ref$Mean.REGION=="S.MARIAN",c("Mean.P10_30","Mean.P30_plus")]))

# for  all fish biomass
mars_ref_tb<-as.numeric(as.vector(ref[ref$Mean.REGION=="S.MARIAN",c("Mean.TotFish")]))

# mean size
mars_ref_sz<-as.numeric(as.vector(ref[ref$Mean.REGION=="S.MARIAN","Mean.MEAN_SIZE"]))


## references lines for plots, use ..._m (mean), sep (m plus se) and sem (m minus se)
hline.data.MARS_cons_m <- data.frame(z = mars_ref_cons, Consumergroup = c("Pri. cons.", "Sec. cons.","Planktivores","Piscivores")) ## needs to match the graph type

hline.data.MARS_sc_m <- data.frame(z = mars_ref_sc, Size_class = c("Parrots 10-30 cm", "Parrots >30 cm")) ## needs to match the graph type

hline.data.MARS_tb_m <- data.frame(z = mars_ref_tb, Size_class = "All fishes") ## needs to match the graph type

hline.data.MARS_sz_m <- data.frame(z = mars_ref_sz, mean_size = "All fishes mean size") ## needs to match the graph type


# save graphs in figures folder
setwd("D:/CRED/fish_cruise_routine_report/monitoring_report/201_status_report/figures")

## batch make the graphs 
for(i in 1:length(rd$Mean.ISLAND)){
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  #data<-subset(rd,Mean.ISLAND=="Rota")
  data<-drop.levels(data)
  
  ##########  parrot size class group graph
  

# # NWHI forereef-----------------------------------------------
# # Different islands surveyed in different years, run each separately: ffs, lisi, midway for 2014
# 
# # consumer group 
# NWHI_ref_cons<-as.numeric(as.vector(ref[ref$Mean.REGION=="NWHI",c("Mean.PLANKTIVORE","Mean.PRIMARY","Mean.SECONDARY","Mean.PISCIVORE")]))
# 
# # biomass of parrots in 2 size classes
# NWHI_ref_sc<-as.numeric(as.vector(ref[ref$Mean.REGION=="NWHI",c("Mean.P10_30","Mean.P30_plus")]))
# 
# # biomass of all fishes
# NWHI_ref_tb<-as.numeric(as.vector(ref[ref$Mean.REGION=="NWHI","Mean.TotFish"]))
# 
# # mean size
# NWHI_ref_sz<-as.numeric(as.vector(ref[ref$Mean.REGION=="NWHI","Mean.MEAN_SIZE"]))
# 
# ## references lines for plots, use ..._m (mean), sep (m plus se) and sem (m minus se)
# hline.data.NWHI_cons_m <- data.frame(z = NWHI_ref_cons, Consumergroup = c("Planktivores","Pri. cons.","Sec. cons.","Piscivores")) ## needs to match the graph type
# 
# hline.data.NWHI_sc_m <- data.frame(z = NWHI_ref_sc, Size_class = c("Parrots 10-30 cm", "Parrots >30 cm")) ## needs to match the graph type
# 
# hline.data.NWHI_tb_m <- data.frame(z = NWHI_ref_tb, Size_class = "All fishes") ## needs to match the graph type
# 
# hline.data.NWHI_sz_m <- data.frame(z = NWHI_ref_sz, mean_size = "All fishes mean size") ## needs to match the graph type
# 
# ## with the reference lines, need to run each region separately...
# 
# rd<-subset(rdd, Mean.REGION == "NWHI")
# rd<-droplevels(rd)
# ben.nwhi<-subset(ben, Mean.REGION == "NWHI")
# 
# names(rd)
# a<-rd[rd$Mean.ANALYSIS_YEAR== 2016-17,]
# b<-rd[rd$Mean.OBS_YEAR==2017,]
# unique(a$Mean.ISLAND) # surveyed in 2017: French Frigate, Kure,Laysan,Lisianski,Midway,Pearl & Hermes
# 
# ## subset graphing data for French Frigate
# # save graphs in figures folder
# setwd("D:/CRED/fish_cruise_routine_report/monitoring_report/201_status_report/figures")
# 
# ################### FOREREEF ONLY
# rd<-rd[rd$Mean.REEF_ZONE == "Forereef",]
# 
# # batch make the graphs - year shouldn't be a factor    
#       for(i in 1:length(rd$Mean.ISLAND)){
#       s<-rd$Mean.ISLAND[i]
#       data<-subset(rd, Mean.ISLAND == s) ## change back to s
#       #data<-subset(rd,Mean.ISLAND=="FFS")
#       data<-drop.levels(data)    
# 
#       ##########  parrot size class group graph

  