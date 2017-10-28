rm(list=ls(all=TRUE))
library(plyr)
library(dplyr)

# NOTES: upload the latest version of island grid files. Using the allocation, generate ~3x the number of target points. Put sites on a map, and eliminate sites that are too close together. 

#---------------------- TAU---------------------------------------
# t<-read.csv("T:/Fish/GIS/Projects/Gridding/TAU/TAU_REA_Grid_OLD.csv")
# summary(t)
# # get rid of columns that have no value or are not needed
# test<-t[,c("TARGET_FID","XMIN","XMAX","YMIN","YMAX","Depth","DepthBin","DepthBin","DepthQual","HardSoft","HrdSftQual","BioGeoZone","Zone","Site","DiverDepth","Label","Sanctuary","Datum","Area_KM","Area_SM","Area_NM","Area_Acres","X","Y")]
# 
# # Create a field for samplable, make it no for soft bottom, lagoon, and backreef for Tau
# test$Samplable<-"YES"
# 
# for(i in 1:dim(test)[1])
# {
#   if(test$HardSoft[i]== "Soft" | test$Zone[i] == "Backreef" | test$Zone[i] == "Lagoon")
#   {
#     test$Samplable[i]<-"NO" 
#   }
# }
# summary(test)
# 
# # Save file
# write.csv(test, file="T:/Fish/GIS/Projects/Gridding/TAU/TAU_REA_Grid.csv")
# 
# # take out un samplable points
# temp<-test[test$Samplable=="YES",]
# temp<-droplevels(temp)
# #save file
# write.csv(temp, file="T:/Fish/GIS/Projects/Gridding/TAU/TAU_REA_Grid_samplable.csv")

library(plyr)
library(dplyr)

# select sites
t<-read.csv("T:/Fish/GIS/Projects/Gridding/TAU/TAU_REA_Grid_samplable.csv")

# Select random sites
# EXAMPLE BELOW
#p1<-sample_n(t[t$DepthBin=="Deep" & t$SECTOR=="NW" & t$Zone=="Forereef",],n_samp)
head(t)
a<-sample_n(t[t$DepthBin=="Deep",],55)
b<-sample_n(t[t$DepthBin=="Moderate",],80)
c<-sample_n(t[t$DepthBin=="Shallow",],20)

#combine 
tau<-rbind(a,b,c)

# temporarily names sites consecutive numbers just to visualize on a map. If sites are too close, note number, come back and delete those numbers, number of obs.
tau$site_temp<-seq(1,155)

head(tau)

tau$X<-NULL
names(tau)[23]
names(tau)[23]<-"X"
write.csv(tau,file="T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/TAU_sites.csv")

t<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/TAU_sites.csv")

test<-t[!(t$site_temp==92 |t$site_temp==88 |t$site_temp==49|t$site_temp==26 |t$site_temp==33 |t$site_temp==46 |t$site_temp==96 |t$site_temp==90 |t$site_temp==78 |t$site_temp==129|t$site_temp==51|t$site_temp==68|t$site_temp==101|t$site_temp==27|t$site_temp==44 |t$site_temp==109 |t$site_temp==130|t$site_temp==36 |t$site_temp==37 |t$site_temp==53 |t$site_temp==79 |t$site_temp==21 |t$site_temp==147 |t$site_temp==116|t$site_temp==25|t$site_temp==98|t$site_temp==155|t$site_temp==133|t$site_temp==135|t$site_temp==121|t$site_temp==96|t$site_temp==132|t$site_temp==112),]

# make sure there are still enough for the allocation x 2
head(test)
ddply(test, .(Zone,DepthBin),summarize,length(site_temp))

# name sites
test$site_no<-seq(766,888)
test$site<-paste("TAU",test$site_no,sep="-")
head(test)

test$X<-NULL
names(test)[23]
names(tau)[23]<-"X"

write.csv(test,file="T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/TAU_sites.csv")



#---------------------------Tutuila--------------------------------------
t<-read.csv("T:/Fish/GIS/Projects/Gridding/TUT/TUT_REA_Grid.csv")
summary(t)
# take out un samplable points
temp<-t[t$Samplable=="YES",]
#make sure hardsoft category is not soft or land
df<-ddply(temp,.(HardSoft),count)
unique(df$freq)
# change 'site' field to 'past_site'
names(temp)
names(temp)[17]<-"past_site"
#save file
write.csv(temp, file="T:/Fish/GIS/Projects/Gridding/TUT/TUT_REA_Grid_samplable.csv")

library(plyr)

# select sites
t<-read.csv("T:/Fish/GIS/Projects/Gridding/TUT/TUT_REA_Grid_samplable.csv")
head(t)
t$X<-NULL
names(t)[24]<-"X"

# change below code for each Sector (if applicable), Reef zone (if applicable), and depth bin, and number of sites to generate for that strata
a<-sample_n(t[t$Zone_=="NW" & t$DepthBin=="Deep" ,],27)
b<-sample_n(t[t$Zone_=="NW" & t$DepthBin=="Moderate" ,],21)
c<-sample_n(t[t$Zone_=="NW" & t$DepthBin=="Shallow" ,],9)
d<-sample_n(t[t$Zone_=="NE" & t$DepthBin=="Deep" ,],12)
e<-sample_n(t[t$Zone_=="NE" & t$DepthBin=="Moderate" ,],18)
f<-sample_n(t[t$Zone_=="NE" & t$DepthBin=="Shallow" ,],12)
g<-sample_n(t[t$Zone_=="SW" & t$DepthBin=="Deep" ,],21)
h<-sample_n(t[t$Zone_=="SW" & t$DepthBin=="Moderate" ,],33)
i<-sample_n(t[t$Zone_=="SW" & t$DepthBin=="Shallow" ,],21)
j<-sample_n(t[t$Zone_=="SE" & t$DepthBin=="Deep" ,],30)
k<-sample_n(t[t$Zone_=="SE" & t$DepthBin=="Moderate" ,],42)
l<-sample_n(t[t$Zone_=="SE" & t$DepthBin=="Shallow" ,],24)

# Combine all sites
tut<-rbind(a,b,c,d,e,f,g,h,i,j,k,l)

# temporarily names sites consecutive numbers just to visualize on a map. If sites are too close, note number, come back and delete those numbers 
tut$site_temp<-seq(1,270)

write.csv(tut,file="T:/Fish/GIS/Projects/AS_RFS_2016/TUT/TUT_sites.csv")
t<-tut
t<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/TUT/TUT_sites.csv")

test<-t[!(t$site_temp==159 |t$site_temp==101 |t$site_temp==248|t$site_temp==230 |t$site_temp==259 |t$site_temp==183 |t$site_temp==62 |t$site_temp==66 |t$site_temp==13 |t$site_temp==38 |t$site_temp==25 |t$site_temp==9 |t$site_temp==21 |t$site_temp==34 |t$site_temp==113 |t$site_temp==164 |t$site_temp==133 |t$site_temp==145),]

# make sure there are still enough for the allocation x 2
ddply(test, .(Zone_,DepthBin),summarize,length(site_temp))

# name sites
test$site_no<-seq(2256,2507)
test$site<-paste("TUT",test$site_no,sep="-")
head(test)

write.csv(test,file="T:/Fish/GIS/Projects/AS_RFS_2016/TUT/TUT_sites.csv")
tut<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/TUT/TUT_sites.csv")
head(tut)
# fix x.1 to read x
tut$X<-NULL
names(tut)[23]<-"X"
write.csv(test,file="T:/Fish/GIS/Projects/AS_RFS_2016/TUT/TUT_sites.csv")

# FAGAMALO SITES
# select sites
t<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/TUT/Fagamalo_grid.csv")
head(t)

# calc area for each strata
t<-t[t$Samplable=="YES",]
a<-ddply(t,.(DepthBin),summarize, "area"=sum(Area_SM))
a$percent_area<-a$area/sum(a$area)
a$targ<-12*a$percent_area
a

b<-sample_n(t[t$DepthBin=="Deep",],10)
c<-sample_n(t[t$DepthBin=="Moderate",],6)
d<-sample_n(t[t$DepthBin=="Shallow",],6)

#combine 
faga<-rbind(b,c,d)
# name sites
faga$site_no<-seq(2525,2546)
faga$site<-paste("TUT",faga$site_no,sep="-")
head(faga)

write.csv(faga,file="T:/Fish/GIS/Projects/AS_RFS_2016/TUT/Faga_sites.csv")



#-------------------------------Ofu&Olosega---------------------------------------------
library(plyr)
t<-read.csv("T:/Fish/GIS/Projects/Gridding/OFU/OFU_REA_Grid.csv")
summary(t)

# # create Samplable field
# t$Samplable<-"YES"
# 
# # change samplable hard bottom to yes
# for(i in 1:dim(t)[1])
# {
#   if(t$HardSoft[i]== "Soft"|t$HardSoft[i]== "Land"| t$Zone[i]=="Backreef"|t$Zone[i]=="Lagoon")
#   {
#     t$Samplable[i]<-"NO" 
#   }
# }
# # Change mesophotic detph no Not samplable
# for(i in 1:dim(t)[1])
# {
#   if(t$DepthBin[i]== "Mesophotic")
#   {
#     t$Samplable[i]<-"NO" 
#   }
# }
# 
# # take out un samplable points
# temp<-t[t$Samplable=="YES",]
# #make sure hardsoft category is not soft or land, and zone is not lagoon or backreef
# df<-ddply(temp,.(HardSoft),count)
# unique(df$freq) # should be 1
# df<-ddply(temp,.(Zone),count)
# unique(df$freq) # should be 1
# 
# # change 'site' field to 'past_site'
# names(temp)
# names(temp)[16] # to make sure this is the right field
# names(temp)[16]<-"past_site"
# 
# #save file
# write.csv(temp, file="T:/Fish/GIS/Projects/Gridding/OFU/OFU_REA_Grid_samplable.csv")

library(plyr)
library(dplyr)

# select sites
t<-read.csv("T:/Fish/GIS/Projects/Gridding/OFU/OFU_REA_Grid_samplable.csv")

# Select random sites
# EXAMPLE BELOW
#p1<-sample_n(t[t$DepthBin=="Deep" & t$SECTOR=="NW" & t$Zone=="Forereef",],n_samp)

a<-sample_n(t[t$DepthBin=="Deep",],40)
b<-sample_n(t[t$DepthBin=="Moderate",],50)
c<-sample_n(t[t$DepthBin=="Shallow",],16)

#combine 
ofu<-rbind(a,b,c)

# temporarily names sites consecutive numbers just to visualize on a map. If sites are too close, note number, come back and delete those numbers, number of obs.
ofu$site_temp<-seq(1,106)

head(ofu)

ofu$X<-NULL
names(ofu)[20]
names(ofu)[20]<-"X"
write.csv(ofu,file="T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/OFU_sites.csv")

t<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/OFU_sites.csv")

test<-t[!(t$site_temp==65 |t$site_temp==37 |t$site_temp==27|t$site_temp==25 |t$site_temp==69 |t$site_temp==73 |t$site_temp==24 |t$site_temp==16 |t$site_temp==82 |t$site_temp==100|t$site_temp==4|t$site_temp==9|t$site_temp==81|t$site_temp==62),]

# make sure there are still enough for the allocation x 2
head(test)
ddply(test, .(Zone,DepthBin),summarize,length(site_temp))

# name sites
test$site_no<-seq(788,879)
test$site<-paste("OFU",test$site_no,sep="-")
head(test)

write.csv(test,file="T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/OFU_sites.csv")

#-------------------------------Rose-----------------------------------------
t<-read.csv("T:/Fish/GIS/Projects/Gridding/ROS/ROS_REA_Grid.csv")
summary(t)

# create Samplable field
t$Samplable<-"YES"

# change samplable hard bottom to yes
for(i in 1:dim(t)[1])
{
  if(t$HardSoft[i]== "Soft"|t$HardSoft[i]== "Land")
  {
    t$Samplable[i]<-"NO" 
  }
}
# Change mesophotic detph no Not samplable
for(i in 1:dim(t)[1])
{
  if(t$DepthBin[i]== "Mesophotic")
  {
    t$Samplable[i]<-"NO" 
  }
}

# take out un samplable points
temp<-t[t$Samplable=="YES",]

#make sure hardsoft category is not soft or land, and zone is not lagoon or backreef
summary(temp)

# change 'site' field to 'past_site'
names(temp)
names(temp)[17] # to make sure this is the right field
names(temp)[17]<-"past_site"

# change Fore reef to Forereef 
str(temp)
temp$Zone<-as.character(temp$Zone)
temp[temp$Zone %in% "Fore reef",]$Zone<-"Forereef"
temp$Zone<-as.factor(temp$Zone)
summary(temp)

#save file
write.csv(temp, file="T:/Fish/GIS/Projects/Gridding/ROS/ROS_REA_Grid_samplable.csv")

library(plyr)
library(dplyr)

# select sites
t<-read.csv("T:/Fish/GIS/Projects/Gridding/ROS/ROS_REA_Grid_samplable.csv")

# Select random sites
# EXAMPLE BELOW
#p1<-sample_n(t[t$DepthBin=="Deep" & t$SECTOR=="NW" & t$Zone=="Forereef",],n_samp)
head(t)
# forereef
a<-sample_n(t[t$Zone=="Forereef" & t$DepthBin=="Deep",],13)
b<-sample_n(t[t$Zone=="Forereef" & t$DepthBin=="Moderate",],50)
c<-sample_n(t[t$Zone=="Forereef" & t$DepthBin=="Shallow",],19)
# #Lagoon
# d<-sample_n(t[t$Zone=="Lagoon" & t$DepthBin=="Deep",],40)
# e<-sample_n(t[t$Zone=="Lagoon" & t$DepthBin=="Moderate",],50)
# f<-sample_n(t[t$Zone=="Lagoon" & t$DepthBin=="Shallow",],16)
# #Backreef
# g<-sample_n(t[t$Zone=="Backreef" & t$DepthBin=="Deep",],40)
# h<-sample_n(t[t$Zone=="Backreef" & t$DepthBin=="Moderate",],50)
# i<-sample_n(t[t$Zone=="Backreef" & t$DepthBin=="Shallow",],16)

#combine 
ros<-rbind(a,b,c)

# temporarily names sites consecutive numbers just to visualize on a map. If sites are too close, note number, come back and delete those numbers, number of obs.
ros$site_temp<-seq(1,82)

head(ros)

ros$X<-NULL
names(ros)[20]
names(ros)[20]<-"X"
write.csv(ros,file="T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/ROS_sites.csv")

t<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/ROS_sites.csv")

test<-t[!(t$site_temp==13 |t$site_temp==31 |t$site_temp==39|t$site_temp==36 |t$site_temp==46 |t$site_temp==81 |t$site_temp==77 |t$site_temp==44 |t$site_temp==78 |t$site_temp==66|t$site_temp==69|t$site_temp==9|t$site_temp==68|t$site_temp==48|t$site_temp==33 |t$site_temp==49 |t$site_temp==32|t$site_temp==28 |t$site_temp==51 |t$site_temp==56 |t$site_temp==57 |t$site_temp==29 |t$site_temp==63 |t$site_temp==17|t$site_temp==27|t$site_temp==42),]

# make sure there are still enough for the allocation x 2
head(test)
ddply(test, .(Zone,DepthBin),summarize,length(site_temp))

# name sites
test$site_no<-seq(744,799) # CHECK TROY'S EMAIL
test$site<-paste("ROS",test$site_no,sep="-")
head(test)

write.csv(test,file="T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/ROS_sites.csv")

#--------------------master site list-----------------------------------------

tau<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/TAU_sites.csv")
tut<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/TUT/TUT_sites.csv")
fa<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/TUT/Faga_sites.csv")
ros<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/ROS_sites.csv")
ofu<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/OFU_sites.csv")
jar<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/OTHER AS ISLANDS/JAR_sites.csv")

# get relevant fields
names(tau)
tau<-tau[,c("site","site_no","DepthBin","Zone","X","Y")]
tau$Island<-"TAU"
names(tut)
tut$X<-NULL
names(tut)[24]<-"X"
tut<-tut[,c("site","site_no","DepthBin","Zone","X","Y")]
tut$Island<-"TUTUILA"
names(fa)
fa$X<-NULL
names(fa)[21]<-"X"
fa<-fa[,c("site","site_no","DepthBin","Zone","X","Y")]
fa$Island<-"TUTILA"
names(ofu)
ofu$X<-NULL
names(ofu)[21]<-"X"
ofu<-ofu[,c("site","site_no","DepthBin","Zone","X","Y")]
ofu$Island<-"OFU"
names(ros)
ros$X<-NULL
names(ros)[21]<-"X"
ros<-ros[,c("site","site_no","DepthBin","Zone","X","Y")]
ros$Island<-"ROSE"

jar$X<-NULL
names(jar)
names(jar)[18]
names(jar)[18]<-"X"
jar$Zone<-"Forereef"
jar<-jar[,c("site","site_no","DepthBin","Zone","X","Y")]
jar$Island<-"JARVIS"
head(jar)
# Combine
ml<-rbind(tau,tut,fa,ros,jar) #jar
head(ml)
#save file
write.csv(ml,file="T:/Fish/GIS/Projects/AS_RFS_2016/ASRFS_master_site.csv")
wtf<-read.csv("T:/Fish/GIS/Projects/AS_RFS_2016/ASRFS_master_site.csv")
wtf$X<-NULL
names(wtf)[5]<-"X"
head(wtf)
