

################################################
#CLEAN UP THE DATA .. for sites with incomplete replicate pairs OR mislabelled reps

#Sites where either two SPCs at REP B only OR where there is one SPC at REP A and one SPC at REP B
CHANGE_FROM_B_TO_A<-c("GUA-0002", "GUA-0003", "GUA-0004", "GUA-0005", "GUA-0007", "GUA-0009", "GUA-0010", "GUA-0012", "GUA-0013", "HOW-0212", "FFS-0185", "JAR-0112", "TUT-0299", "TUT-0481", "TUT-0245", "TUT-0320", "TUT-0367", "PAG-0057")
x[x$SITE %in% CHANGE_FROM_B_TO_A,]$REP<-"A"

# drop sites with only one SPC cylinder
ONE_REP_ID<-c("TUT-0304", "TUT-0457", "JAR-0105", "PAL-0154")


#tmp<-droplevels(x[x$SITE %in% ONE_REP_ID,])
#table(tmp$SITE, tmp$REPLICATEID)
x<-subset(x, !x$SITE %in% ONE_REP_ID)

#drop the B for sites with only one B Replicate
ONE_B_TWO_A<-c("TUT-0358", "FFS-0160", "FFS-0168", "JAR-0100", "JAR-0103", "JAR-0120", "JAR-0121", "JAR-0101", "JAR-0102", "JAR-0109", "JAR-0111", "JAR-0119", "PAL-0106", "PAL-0108", "PAL-0418", "PAL-0427")
length(unique(x$REPLICATEID))
x<-x[!(x$SITE %in% ONE_B_TWO_A & x$REP=="B"),]
length(unique(x$REPLICATEID))

#drop the A for sites with only one A Rep, and change the B to A for remaining data
ONE_A_TWO_B<-c("TUT-0202", "TUT-0218", "TUT-0220", "TUT-0234", "TUT-0261", "TUT-0265", "TUT-0283", "TUT-0285", "TUT-0298", "TUT-0310", "TUT-0314", "TUT-0329", "TUT-0349", "TUT-0350", "HAW_190")


#tmp<-droplevels(x[x$SITE %in% ONE_A_TWO_B,]); head(tmp)
#table(paste(tmp$SITE,tmp$REPLICATEID, sep=""), tmp$REP)

length(unique(x$REPLICATEID))
x<-x[!(x$SITE %in% ONE_A_TWO_B & x$REP=="A"),]
length(unique(x$REPLICATEID))
x[x$SITE %in% ONE_A_TWO_B,]$REP<-"A"

#change sites with one A and one B replicate to both be A
ONE_A_ONE_B<-c("TUT-0481", "TUT-0245", "TUT-0249", "TUT-0299", "TUT-0320", "TUT-0367")
#tmp<-droplevels(x[x$SITE %in% ONE_A_ONE_B,]); head(tmp)
#table(paste(tmp$SITE,tmp$REPLICATEID, sep=""), tmp$REP)
x[x$SITE %in% ONE_A_ONE_B,]$REP<-"A"

################################################
