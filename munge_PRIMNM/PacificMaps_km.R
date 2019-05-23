# Code to make Pacific-wide plots with raster underlay for PRIMNM report

# Load libraries ----------------------------------------------------------

library(rerddap)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(raster)
library(colorRamps)
library(scales)
library(ggpubr)
library(maps)
library(PBSmapping) # for addBubbles
library(cowplot) # for arranging plots


# Get PRIMNM and world map data -----------------------------------

# Get island coordinates  !!! changed island names to archipelago names to get hawaii, samoa, and marianas labels
islands = read.csv("D:/CRED/PRIMNM_REPORT/data/islands_midpoints_ARCH_EDIT.csv")

# Convert to 0-360 longitude (easier to plot)
islands$LONGITUDE = ifelse(islands$LONGITUDE < 0,
                           islands$LONGITUDE + 360,
                           islands$LONGITUDE)

# Get world map
WorldData <- map_data('world')

# Convert to 0-360 longitude
WorldData$long360 = ifelse(WorldData$long < 0, WorldData$long + 360, WorldData$long)

# Subset to Pacific box and prepare for plotting
WorldData.pac <-
  WorldData[which(WorldData$lat >= -30 &
                    WorldData$lat <= 45 &
                    WorldData$long >= 130 & WorldData$long360 < 230), ]
WorldData.pac <- WorldData.pac[order(WorldData.pac$order), ]
WorldData.pac <- fortify(WorldData.pac)

### Get Tow sightings of ESA listed species -----------------------------------

wd<-read.csv("D:/CRED/PRIMNM_REPORT/Pacific wide/pac_wide_tow/ESA_SIGHTINGS_ISLAND_COORD.csv")

# Convert to 0-360 longitude (easier to plot)
wd$LONGITUDE = ifelse(wd$LONGITUDE < 0,
                           wd$LONGITUDE + 360,
                           wd$LONGITUDE)

### Make base map ---------------------------------------------------------------
map = ggplot() +
  geom_point(data = islands, aes(x = LONGITUDE, y = LATITUDE)) +
  geom_text_repel(
    data = subset(islands, RegionCode == "PRIA"),
    aes(x = LONGITUDE, y = LATITUDE, label = Name),
    box.padding = unit(0.4, 'lines'),
    point.padding = 0.6,
    force = 5
  ) +
  geom_polygon(
    data = WorldData.pac,
    aes(x = long360, y = lat, group = group),
    fill = "white",
    colour = "black",
    size = 0.5
  ) +
  theme_bw() +
  ylab("") +
  xlab("") +
  scale_x_continuous(
    labels = seq(140, 210, 10),#
    breaks = seq(140, 210, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = seq(-20, 35, 10),
    breaks = seq(-20, 35, 10),
    expand = c(0, 0)
  ) +
  coord_equal(xlim = c(140, 210), ylim = c(-20, 35)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'right',
    legend.key = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank()
  )


# add BOMU bubbles to map-------------
#points(pch=21,mhi$LONGITUDE, mhi$LATITUDE,col="red",cex=1)
bomu<-wd[wd$BOMU_10>0,]
bomup<-map+
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = BOMU_10), color='turquoise3',fill='turquoise3', shape = 21,data=bomu)+
  scale_size_continuous(breaks=c(1,49), name="Number \nPer 10 km")+
  # postion legend: 0,0 = bottom left, 1,1 = top right
  theme(legend.justification=c(-0.1,-0.1), legend.position=c(0,0),legend.background = element_rect(color = "black", size = .3,linetype = "solid"))+
  theme(plot.margin = unit(c(0,0,0,0),"cm"))
bomup

# save file
suppressMessages(ggsave(filename ="D:/CRED/PRIMNM_REPORT/Pacific wide/BOMU3.tiff", width=14.0, height = 10.0, units = c("cm")))

# add CHUD bubbles to map-------------
#points(pch=21,mhi$LONGITUDE, mhi$LATITUDE,col="red",cex=1)
chud<-wd[wd$CHUD_10>0,]
chudp<-map+
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = CHUD_10), color='black',fill='dodgerblue2', shape = 21, alpha=0.85,data=chud)+
  scale_size_continuous(breaks=c(4,8,12), name="Number \nPer 10 km")+
  # postion legend: 0,0 = bottom left, 1,1 = top right
  theme(legend.justification=c(-0.1,-0.1), legend.position=c(0,0),legend.background = element_rect(color = "black", size = .3,linetype = "solid"))+
  labs(title = "Humphead wrasse")+
  # adjust title position: t=top, r=right, b=bottom, l=left
  theme(plot.title = element_text(margin = margin(t = 10, r=-10, b=-20, l=20)))+
  theme(plot.margin = unit(c(0,0.5,0,0),"cm"))
chudp
# save file
suppressMessages(ggsave(filename ="D:/CRED/PRIMNM_REPORT/Pacific wide/CHUD.tiff", width=14.0, height = 10.0, units = c("cm")))


# add hammerhead bubbles to map-------------
#points(pch=21,mhi$LONGITUDE, mhi$LATITUDE,col="red",cex=1)
sple<-wd[wd$SPLE_10>0,]
splep<-map+
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = SPLE_10), color='black',fill='red2', shape = 21, alpha=0.85,data=sple)+
  scale_size_continuous(breaks=c(1,2,3), name="Number \nPer 10 km")+
  # postion legend: 0,0 = bottom left, 1,1 = top right
  theme(legend.justification=c(-0.1,-0.1), legend.position=c(0,0),legend.background = element_rect(color = "black", size = .3,linetype = "solid"))+
  labs(title = "Scalloped hammerhead shark")+
  # adjust title position: t=top, r=right, b=bottom, l=left
  theme(plot.title = element_text(margin = margin(t = 10, r=-10, b=-20, l=20)))+
  theme(plot.margin = unit(c(0,0.5,0,0),"cm"))

# save file
suppressMessages(ggsave(filename ="D:/CRED/PRIMNM_REPORT/Pacific wide/SPLE.tiff", width=14.0, height = 10.0, units = c("cm")))

# add mantas bubbles to map-------------
#points(pch=21,mhi$LONGITUDE, mhi$LATITUDE,col="red",cex=1)
mabi<-wd[wd$MABI_10>0,]
mabip<-map+
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = MABI_10), color='black',fill='darkorchid4', shape = 21, alpha=0.85,data=mabi)+
  scale_size_continuous(breaks=c(1,3,5), name="Number \nPer 10 km")+
  # postion legend: 0,0 = bottom left, 1,1 = top right
  theme(legend.justification=c(-0.1,-0.1), legend.position=c(0,0),legend.background = element_rect(color = "black", size = .3,linetype = "solid"))+
  labs(title = "Manta ray")+
  # adjust title position: t=top, r=right, b=bottom, l=left
  theme(plot.title = element_text(margin = margin(t = 10, r=-10, b=-20, l=20)))+
  theme(plot.margin = unit(c(0,0.5,0,0),"cm"))

# save file
suppressMessages(ggsave(filename ="D:/CRED/PRIMNM_REPORT/Pacific wide/MABI.tiff", width=14.0, height = 10.0, units = c("cm")))

# add GRTU bubbles to map-------------
#points(pch=21,mhi$LONGITUDE, mhi$LATITUDE,col="red",cex=1)
GRTU<-wd[wd$GRTU_10>0,]
grtup<-map+
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = GRTU_10), color='black',fill='green3', shape = 21, alpha=0.85,data=GRTU)+
  scale_size_continuous(breaks=c(8,16,24),name="Number \nPer 10 km")+
  # postion legend: 0,0 = bottom left, 1,1 = top right
  theme(legend.justification=c(-0.1,-0.01), legend.position=c(0,0),legend.background = element_rect(color = "black", size = .3,linetype = "solid"))+
  labs(title = "Green turtle")+
  # adjust title position: t=top, r=right, b=bottom, l=left
  theme(plot.title = element_text(margin = margin(t = 10, r=-10, b=-20, l=20)))+
  theme(plot.margin = unit(c(0,0.5,0,0),"cm"))
grtup
# save file
suppressMessages(ggsave(filename ="D:/CRED/PRIMNM_REPORT/Pacific wide/GRTU.tiff", width=14.0, height = 10.0, units = c("cm")))

# add HKTU bubbles to map-------------
#points(pch=21,mhi$LONGITUDE, mhi$LATITUDE,col="red",cex=1)
HKTU<-wd[wd$HKTU_10>0,]
hktup<-map+
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = HKTU_10), color='black',fill='gold3', shape = 21, alpha=0.85,data=HKTU)+
  scale_size_continuous(breaks=c(1,2), name="Number \nPer 10 km")+
  # postion legend: 0,0 = bottom left, 1,1 = top right
  theme(legend.justification=c(-0.1,-0.1), legend.position=c(0,0),legend.background = element_rect(color = "black", size = .3,linetype = "solid"))+
  labs(title = "Hawksbill turtle")+
  # adjust title position: t=top, r=right, b=bottom, l=left
  theme(plot.title = element_text(margin = margin(t = 10, r=-10, b=-20, l=20)))+
  theme(plot.margin = unit(c(0,0.5,0,0),"cm"))

# save file
suppressMessages(ggsave(filename ="D:/CRED/PRIMNM_REPORT/Pacific wide/HKTU.tiff", width=14.0, height = 10.0, units = c("cm")))

# add TURT bubbles to map-------------
#points(pch=21,mhi$LONGITUDE, mhi$LATITUDE,col="red",cex=1)
TURT<-wd[wd$TURT_10>0,]
turtp<-map+
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = TURT_10), color='black',fill='grey69', shape = 21, data=TURT)+
  scale_size_continuous(breaks=c(6,12,18), name="Number \nPer 10 km")+
  # postion legend: 0,0 = bottom left, 1,1 = top right
  theme(legend.justification=c(-0.1,-0.1), legend.position=c(0,0),legend.background = element_rect(color = "black", size = .3,linetype = "solid"))+
  labs(title = "Turtle species")+
  # adjust title position: t=top, r=right, b=bottom, l=left
  theme(plot.title = element_text(margin = margin(t = 10, r=-10, b=-20, l=20)))+
  theme(plot.margin = unit(c(0,0.5,0,0),"cm"))

# save file
suppressMessages(ggsave(filename ="D:/CRED/PRIMNM_REPORT/Pacific wide/TURT.tiff", width=14.0, height = 10.0, units = c("cm")))

# combine plots ----------------------------
ggarrange(chudp,mabip,splep,hktup,grtup, turtp, ncol=2, nrow=3, align= "hv")

suppressMessages(ggsave(filename ="D:/CRED/PRIMNM_REPORT/Pacific wide/ESA2.tiff", width=18.0, height = 24.0, units = c("cm")))


#plot_grid(chudp,mabip,splep,hktup,grtup, turtp,labels=c('A','B','C','D','E','F'))
# 3 vertical plots
ggarrange(chudp,mabip,splep, ncol=1, nrow=3, align= "v")
ggarrange(hktup,grtup, turtp, ncol=1, nrow=3, align= "v")

# 3 horizontal plots
ggarrange(chudp,mabip,splep, ncol=3, nrow=1, align= "h")
suppressMessages(ggsave(filename ="D:/CRED/PRIMNM_REPORT/Pacific wide/ESA_fish.tiff", width=30.0, height = 8.0, units = c("cm")))
ggarrange(hktup,grtup, turtp, ncol=3, nrow=1, align= "h")
suppressMessages(ggsave(filename ="D:/CRED/PRIMNM_REPORT/Pacific wide/ESA_turtle.tiff", width=30.0, height = 8.0, units = c("cm")))

# 3 grouped plots
ggarrange(chudp,mabip,splep, ncol=2, nrow=2, align= "h")
