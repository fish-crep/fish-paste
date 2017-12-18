# Randomly generate sites

# load appropriate grid file

# ------------------MHI-------------------------------
# connect to T drive, load grid file
df<-read.csv("/Volumes/general/Fish/GIS/Projects/Gridding/MHI_grid_points.csv")

# Function to randomly select rows:
sample.df<-function(df,n) df[sample(nrow(df),n), ,drop=FALSE]

# Niihau:
#!!!!!! CHANGE NUMBER AT END OF THE LINE TO MATCH ALLOCATION !!!!!!!!!!!
nii.shal<-sample.df(df[(df$ISLAND == "Niihau") & (df$DEPTH_BIN=="SHAL"), ], 3)
nii.mid<-sample.df(df[(df$ISLAND == "Niihau") & (df$DEPTH_BIN=="MID"), ], 3)
nii.deep<-sample.df(df[(df$ISLAND == "Niihau") & (df$DEPTH_BIN=="DEEP"), ], 3)

nii<-rbind(nii.shal,nii.mid,nii.deep)

# Kauai:
kau.shal<-sample.df(df[(df$ISLAND == "Kauai") & (df$DEPTH_BIN=="SHAL"), ], 3)
kau.mid<-sample.df(df[(df$ISLAND == "Kauai") & (df$DEPTH_BIN=="MID"), ], 3)
kau.deep<-sample.df(df[(df$ISLAND == "Kauai") & (df$DEPTH_BIN=="DEEP"), ], 3)

kau<-rbind(kau.shal,kau.mid,kau.deep)

# Oahu:
oah.shal<-sample.df(df[(df$ISLAND == "Oahu") & (df$DEPTH_BIN=="SHAL"), ], 3)
oah.mid<-sample.df(df[(df$ISLAND == "Oahu") & (df$DEPTH_BIN=="MID"), ], 3)
oah.deep<-sample.df(df[(df$ISLAND == "Oahu") & (df$DEPTH_BIN=="DEEP"), ], 3)

oah<-rbind(oah.shal,oah.mid,oah.deep)

# Molokai:



