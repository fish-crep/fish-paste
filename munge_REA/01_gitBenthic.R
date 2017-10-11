
  rm(list=ls())
#Load raw data
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/ReportCard")#set directory
st<- read.csv("MHI13-16_adultstrata.csv")
is<- read.csv("MHI13-16_adultisland.csv")
s <- read.csv("MHI13-16_adultsite.csv")
