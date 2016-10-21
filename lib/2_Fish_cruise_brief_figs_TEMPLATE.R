rm(list=ls())
###################################################
##### run Fish_cruise_brief_cleaning first ########
###################################################

# set wd to folder for cruise brief
setwd("/Volumes/TOSHIBA EXT/CRED/fish_cruise_routine_report/monitoring_brief/2016_sunnary_brief_cruise_reports/MHI2016")
load("working_site_data.rdata") # site level data
load("data_pooled_is_yr.rdata") # island level data

ard_m<-dp$Mean
ard_se<-dp$PooledSE

deets<-wsd
rdt<-deets

# get list of islands that were surveyed and correct names for text
levels(deets$ISLAND)
# MHI: change island names to include okina 
# "Hawai`i","Kaho`olawe","Kaua`i","Lana`i", "Maui","Moloka`i","Ni`ihau","O`ahu"
levels(deets$ISLAND)<-c("Hawai`i","Kaho`olawe","Kaua`i","Lana`i", "Maui","Moloka`i","Ni`ihau","O`ahu")
levels(deets$REGION_NAME)<-c("main Hawaiian Islands")  

a<-paste("2016") # select year
b<-paste("MHI") # select region MARIAN MHI NWHI PRIAs SAMOA

rdt<-deets[deets$OBS_YEAR == a,] # select year
rdt<-rdt[rdt$REGION == b,] # select region MARIAN MHI NWHI PRIAs SAMOA

rdt<-droplevels(rdt)

reg<-levels(rdt$REGION)
### need to fix this to correct the region names for tech edit
reg<-"main Hawaiian Islands"
#reg<-"Pacific Remote Island Areas"
#reg<-"American Samoa"

yr<-mean(rdt$OBS_YEAR)
rdt<-droplevels(rdt)
nsites<-length(levels((rdt$SITE)))
is.n<-as.vector(with(rdt, table(ISLAND), length(levels((rdt$SITE)))))
islands<-as.vector(levels(rdt$ISLAND))

islandn<-NULL
for(i in 1:length(islands)){
  islandn[i]<-paste(islands[[i]], " (n=",is.n[[i]],")", sep="")
}

islandn<-as.vector(islandn)
allislandsminuslast<-islandn[1:length(islandn)-1]
lastisland<-islandn[length(islandn)]

rd_m<-ard_m[ard_m$OBS_YEAR == a,] # select year
rd_m<-ard_m[ard_m$REGION == b,] # select year

rd_se<-ard_se[ard_se$OBS_YEAR == a,]
rd_se<-ard_se[ard_se$REGION == b,] # select region

rd_m<-droplevels(rd_m)
rd_se<-droplevels(rd_se)

#----------------- site information for paragraph 2 -----------------
nsites #256
islandn
# ----------------- consumer group graph -----------------
library(ggplot2)
library(reshape)

tmr<-rd_m[c("ISLAND", "PRIMARY", "SECONDARY", "PISCIVORE", "PLANKTIVORE", "TotFish")]
test<-melt(tmr)
names(test)<-c("Island", "Consumer group", "Biomass")
tmr<-rd_se[c("ISLAND","PRIMARY", "SECONDARY", "PISCIVORE", "PLANKTIVORE", "TotFish")]
suppressMessages(test2<-melt(tmr))
names(test2)<-c("Island", "Consumer group", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Consumergroup", "Biomass", "SE")
levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores", "All fishes")
dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(3,2,1,4,5)])
# ORDER ISLANDS GEOGRAPHICALLY - !!!!! change numbers in Island levels!!!!! 
levels(dt$Island)
levels(dt$Island)<-c("Hawai`i","Kaho`olawe","Kaua`i","Lana`i", "Maui","Moloka`i","Ni`ihau","O`ahu")
dt$Island<-factor(dt$Island, levels(dt$Island)[c(7,3,8,6,4,5,2,1)])

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)  
base_size<-12
colours <- c("#69D2E7", "#A7DBD8", "#E0E4CC", "#F38630", "#FA6900")

# plot
p <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Island)) + geom_bar(position="dodge", stat="identity", colour="black") + scale_y_continuous(breaks=c(0,50)) +
geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values = colours) + facet_grid(Consumergroup ~ ., scales ="free") +
theme_bw()+theme(panel.border = element_rect(colour="dark gray"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none", axis.text.x = element_text(angle = 45, vjust= 1, hjust=1, size = base_size*0.8), strip.text.y = element_text(angle = 0, size = base_size*0.8)) + labs(y = expression(paste("Fish biomass (g ", m^-2,")")))## or yellow red?? YlOrRd + facet_grid()
suppressMessages(ggsave(filename ="sig_findings_graph_trophic.pdf", width=9.5, height = 11.0, units = c("cm")))
p
## --------------------size class group graph------------------------
tmr<-rd_m[c("ISLAND", "0_20", "20_50", "50_plus")]
suppressMessages(test<-melt(tmr))
names(test)<-c("Island", "Size_class", "Biomass")
tmr<-rd_se[c("ISLAND", "0_20", "20_50", "50_plus")]
suppressMessages(test2<-melt(tmr))
names(test2)<-c("Island", "Size_class", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Island", "Size_class", "Biomass", "SE")
levels(dt$Size_class)<-c("0-20 cm TL", "20-50 cm TL", ">50 cm TL")
dt$Size_class<-factor(dt$Size_class, levels(dt$Size_class)[c(1,2,3)])
#!!!!!!!!!!!!! CHANGE ISLAND LEVELS !!!!!!!!!!!!!!!!!
levels(dt$Island)
levels(dt$Island)<-c("Hawai`i","Kaho`olawe","Kaua`i","Lana`i", "Maui","Moloka`i","Ni`ihau","O`ahu")
dt$Island<-factor(dt$Island, levels(dt$Island)[c(7,3,8,6,4,5,2,1)])
##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)
colours <-c("#A7DBD8", "#E0E4CC", "#F38630")
## define labels so to have line breaks

d <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Island)) + geom_bar(position="dodge", stat="identity", colour="black") + scale_y_continuous(breaks=c(0,20,40)) + # CHANGE FOR Y-AXIS SCALE
geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values = colours) + facet_grid(Size_class ~ ., scales ="fixed") +
theme_bw()+theme(panel.border = element_rect(colour="dark gray"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position="none", axis.text.x = element_text(angle = 45, vjust=1, hjust=1, size = base_size*0.8), strip.text.y = element_text(angle = 0, size = base_size*0.8)) + labs(y = expression(paste("Fish biomass (g ", m^-2,")")))## or yellow red?? YlOrRd + facet_grid()
suppressMessages(ggsave(filename ="sig_findings_graph_size.pdf", width=9.5, height = 11.0, units = c("cm")))
d
