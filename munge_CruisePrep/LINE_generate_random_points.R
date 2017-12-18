rm(list=ls(all=TRUE))
library(plyr)
library(dplyr)

# NOTES: upload the latest version of island grid files. Using the allocation, generate ~3x the number of target points. Put sites on a map, and eliminate sites that are too close together. 

#---------------------------Jarvis-----------------------------------------------
t<-read.csv("T:/Fish/GIS/Projects/Gridding/JAR/JAR_REA_grid.csv")
summary(t)
test<-t
# Create a field for samplable, make it no for mesophotic
test$Samplable<-"YES"

for(i in 1:dim(test)[1])
{
  if(test$DepthBin[i]== "Mesophotic" | test$DepthBin[i]== "NoData")
  {
    test$Samplable[i]<-"NO" 
  }
}
summary(test)

# change "Site" to past_site
names(test)
colnames(test)[14]
colnames(test)[14]<-"past_site"

# take out un samplable points
temp<-test[test$Samplable=="YES",]
temp<-droplevels(temp)
#save file
write.csv(temp, file="T:/Fish/GIS/Projects/Gridding/JAR/JAR_REA_Grid_samplable.csv")

library(plyr)
library(dplyr)

# select sites
t<-read.csv("T:/Fish/GIS/Projects/Gridding/TAU/TAU_REA_Grid_samplable.csv")

# Select random sites
# EXAMPLE BELOW
#p1<-sample_n(t[t$DepthBin=="Deep" & t$SECTOR=="NW" & t$Zone=="Forereef",],n_samp)
head(t)
a<-sample_n(t[t$DepthBin=="Deep",],25)
b<-sample_n(t[t$DepthBin=="Moderate",],55)
c<-sample_n(t[t$DepthBin=="Shallow",],40)

#combine 
jar<-rbind(a,b,c)

# temporarily names sites consecutive numbers just to visualize on a map. If sites are too close, note number, come back and delete those numbers, number of obs.
jar$site_temp<-seq(1,120)

head(jar)

write.csv(jar,file="T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/JAR_sites.csv")

t<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/JAR_sites.csv")

test<-t[!(t$site_temp==65 |t$site_temp==46 |t$site_temp==7|t$site_temp==19 |t$site_temp==73 |t$site_temp==76 |t$site_temp==10 |t$site_temp==52 |t$site_temp==169 |t$site_temp==38|t$site_temp==62|t$site_temp==42|t$site_temp==49|t$site_temp==106|t$site_temp==95 |t$site_temp==47 |t$site_temp==69|t$site_temp==70 |t$site_temp==67 |t$site_temp==102 |t$site_temp==110 |t$site_temp==117 |t$site_temp==89 |t$site_temp==104|t$site_temp==91|t$site_temp==14|t$site_temp==8|t$site_temp==37),]

# make sure there are still enough for the allocation x 2
head(test)
ddply(test, .(Zone,DepthBin),summarize,length(site_temp))

# name sites
test$site_no<-seq(731,823)
test$site<-paste("JAR",test$site_no,sep="-")
head(test)

test$X<-NULL
names(test)[18]
names(test)[18]<-"X"
head(test)

write.csv(test,file="T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/JAR_sites.csv")
