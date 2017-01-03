setwd("E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/data/Data Outputs")

rm(list=ls())

## fish status report standard figures
## pacific section
library(ggplot2)
library(reshape)
library(gdata)


# data selection ----------------------------------------------------------

load("MONREPdata_pooled_is_RZ.rdata") ### island estimates mean over cruises since spc after 2007
wd<-as.data.frame(data_pooled_is)
rd<-as.data.frame(data_pooled_is)

# only want to compare forereef for Pacific graphs, subset
rd<-rd[rd$Mean.REEF_ZONE=="Forereef",]
# remove SOB 
rd<-rd[rd$Mean.ISLAND != "South Bank",]
rd<-drop.levels(rd)

# consumer group graph ----------------------------------------------------

## consumer group graph
tmr<-rd[c("Mean.REGION", "Mean.ISLAND","Mean.PRIMARY","Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE", "Mean.TotFish")]
test<-melt(tmr)
names(test)<-c("REGION", "Island", "Consumer group", "Biomass")
tmr<-rd[c("Mean.REGION", "Mean.ISLAND", "PooledSE.PRIMARY","PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE", "PooledSE.TotFish")]
test2<-melt(tmr)
names(test2)<-c("Region", "Island", "Consumer group", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Region","Island","Consumergroup", "Biomass", "SE")
levels(dt$Consumergroup)<-c("Pri. cons.", "Sec. cons.", "Piscivores", "Planktivores", "All fishes")
dt$Consumergroup<-reorder(dt$Consumergroup, new.order=c("Piscivores","Sec. cons.","Pri. cons.","Planktivores", "All fishes"))
dt<-dt[order(dt$Consumergroup),]

levels(dt$Region)<-c("MHI", "N.Mariana", "NWHI", "PRIA", "S.Mariana", "Samoa")
dt$Region<-reorder(dt$Region, new.order=c("NWHI","MHI","N.Mariana","S.Mariana","PRIA", "Samoa"))
dt<-dt[order(dt$Region),]

####  !!!!!! May need to update island names !!!!!!!!!!!
levels(dt$Island)<-c( "Agrihan","AGS","Aguijan","Asuncion","Baker","FDP","FFS","Gardner","Guam", "Hawaii","Howland","Jarvis","Johnston","Kahoolawe","Kauai","Kingman","Kure","Lanai","Laysan","Lisianski","Maro","Maug","Maui","Midway","Molokai","Necker","Nihoa","Niihau" ,"Oahu","O&O","Pagan","Palmyra","P&H","Rose","Rota","Saipan","Swains","Tau","Tinian","Tutuila" ,"Wake")

dt$Island<-reorder(dt$Island,new.order=c("Kure", "Midway", "P&H", "Lisianski", "Laysan", "Gardner", "Maro","FFS", "Necker", "Nihoa", "Niihau", "Kauai", "Oahu", "Molokai","Lanai", "Maui","Kahoolawe","Hawaii", "FDP", "Maug", "Asuncion", "Agrihan", "Pagan", "AGS", "Saipan", "Tinian", "Aguijan", "Rota","Guam", "Wake", "Johnston", "Kingman", "Palmyra","Howland", "Baker", "Jarvis",  "Swains", "O&O", "Tau", "Tutuila", "Rose"))
dt<-dt[order(dt$Island),]

#### theme for the pacific graphs

theme_bw <- function(base_size = 12,  base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text         = element_text(size = rel(0.8)),
      axis.ticks        = element_line(colour = "black"),
      axis.ticks.x      = element_blank(),
      axis.text.x       = element_text(angle = 90, hjust =1, vjust =0.4),
      legend.key        = element_rect(colour = "grey80"),
      legend.position   = "none",
      panel.background  = element_rect(fill = "white"),
      panel.border      = element_rect(fill = NA, colour = "grey50"),
      panel.grid.major  = element_line(colour = "grey90", size = 0.2),
      panel.grid.minor  = element_line(colour = "grey98", size = 0.5),
      strip.background  = element_rect(fill = "grey80", colour = "grey50")
    )
}


##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)  

p <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Island)) + 
  geom_bar(width = 0.85, position= position_dodge(width=0.5), stat="identity", colour="black") +
  geom_errorbar(limits, position=dodge, width=0.25) + 
  scale_fill_brewer(palette = "Blues") + 
  facet_grid(Consumergroup ~ Region, scales = "free", space="free_x") + 
  theme_bw() +
  labs(y = expression(paste("Fish biomass (g ", m^-2,")")))

p

ggsave(filename ="E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures/pacific_graph_consumers.png", width=7, height = 5.8, units = 'in')
#tiff(file="E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures/pacific_graph_consumersTIFF.tiff",width=7,height=5.8,pointsize=1/300,units='in',res=300)
#p
#dev.off()

# size class graph --------------------------------------------------------

tmr<-rd[c("Mean.REGION", "Mean.ISLAND","Mean.0_20","Mean.20_50", "Mean.50_plus")]
test<-melt(tmr)
names(test)<-c("REGION", "Island", "Size class", "Biomass")
tmr<-rd[c("Mean.REGION", "Mean.ISLAND", "PooledSE.0_20","PooledSE.20_50", "PooledSE.50_plus")]
test2<-melt(tmr)
names(test2)<-c("Region", "Island", "Size class", "SE")

dt<-as.data.frame(cbind(test, test2$SE))
names(dt)<-c("Region","Island","Size_class", "Biomass", "SE")
levels(dt$Size_class)<-c("0-20 cm TL", "20-50 cm TL", ">50 cm TL")
levels(dt$Region)<-c("MHI", "N.Mariana", "NWHI", "PRIA", "S.Mariana", "Samoa")

dt$Region<-reorder(dt$Region, new.order=c("NWHI","MHI","N.Mariana","S.Mariana","PRIA", "Samoa"))
dt<-dt[order(dt$Region),]

### !!!! May need to update island names !!!!!
levels(dt$Island)<-c( "Agrihan","AGS","Aguijan","Asuncion","Baker","FDP","FFS","Gardner","Guam", "Hawaii","Howland","Jarvis","Johnston","Kahoolawe","Kauai","Kingman","Kure","Lanai","Laysan","Lisianski","Maro","Maug","Maui","Midway","Molokai","Necker","Nihoa","Niihau" ,"Oahu","O&O","Pagan","Palmyra","P&H","Rose","Rota","Saipan","Swains","Tau","Tinian","Tutuila" ,"Wake")

dt$Island<-reorder(dt$Island,new.order=c("Kure", "Midway", "P&H", "Lisianski", "Laysan", "Gardner", "Maro","FFS", "Necker", "Nihoa", "Niihau", "Kauai", "Oahu", "Molokai","Lanai", "Maui","Kahoolawe","Hawaii", "FDP", "Maug", "Asuncion", "Agrihan", "Pagan", "AGS", "Saipan", "Tinian", "Aguijan", "Rota","Guam", "Wake", "Johnston", "Kingman", "Palmyra","Howland", "Baker", "Jarvis",  "Swains", "O&O", "Tau", "Tutuila", "Rose"))
dt<-dt[order(dt$Island),]

##Define the top and bottom of the errorbars 
limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE) 
dodge <- position_dodge(width=0.9)  

p <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Island)) + 
  geom_bar(width = 0.85, position= position_dodge(width=0.5), stat="identity", colour="black") +
  geom_errorbar(limits, position=dodge, width=0.25) + 
  scale_fill_brewer(palette = "YlOrRd") + 
  facet_grid(Size_class ~ Region, scales = "free", space="free_x") + 
  theme_bw() +
  labs(y = expression(paste("Fish biomass (g ", m^-2,")")))
p
ggsave(filename ="E:/CRED/fish_cruise_routine_report/monitoring_report/2016_status_report/figures/pacific_graph_size_class.png", width=7, height = 5.5, units = c("in"))

