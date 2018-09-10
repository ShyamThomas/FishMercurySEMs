
library(PerformanceAnalytics)

setwd("D:/Database")

ALL.MERGED.data=read.csv("HG_WATERCHEM_LAKECHAR_LULC_CLIM.csv") ## This database is the final version of data merging 
head(ALL.MERGED.data)
colnames(ALL.MERGED.data)

chart.Correlation(ALL.MERGED.data[,c(6,7,13,16,18,20:24,26,28,30,34:36,41,43)], pch=21)

WAT_LID=as.character(ALL.MERGED.data$WATERBODY_LID)
LAT=ALL.MERGED.data$lat
LON=ALL.MERGED.data$lon

###Transforming data to get as close as possible to a normal distribution
logTEMP=log(ALL.MERGED.data$avgtmp)
logRAIN=log(ALL.MERGED.data$avgrain)
hist(ALL.MERGED.data$GDD8110)
hist(log(ALL.MERGED.data$GDD8110))
logGDD=log(ALL.MERGED.data$GDD8110)

STRAHLER=ALL.MERGED.data$STRAHLER
logWETLAND=log(ALL.MERGED.data$SHORE.WETLANDS+0.001)
hist(logWETLAND)
hist(ALL.MERGED.data$AGRICULTURE)
logAGRI=log(ALL.MERGED.data$AGRICULTURE+0.001)
hist(ALL.MERGED.data$FORESTS)
asinFOREST=asin(ALL.MERGED.data$FORESTS)
hist(log(ALL.MERGED.data$DISTURBANCE+0.001))
logDIST=log(ALL.MERGED.data$DISTURBANCE+0.001)
logURB=log(ALL.MERGED.data$URBAN+0.001)
hist(logURB)

sqMINDO=sqrt(ALL.MERGED.data$MINDO)
sqAVGDO=sqrt(ALL.MERGED.data$AVGDO)
sqDOC=sqrt(ALL.MERGED.data$DOC)
PH=ALL.MERGED.data$PH
logSULF=log(ALL.MERGED.data$SSO4UR)

logWEIGHT=log(ALL.MERGED.data$WEIGHT_GRAM)
logLENGTH=log(ALL.MERGED.data$LENGTH_CM)

logHG=log(ALL.MERGED.data$VALUE)

logALLSEMDATA=as.data.frame(cbind(LON, LAT, logTEMP,logRAIN,logGDD,STRAHLER,logWETLAND,logAGRI,asinFOREST,logDIST,logURB,sqMINDO,sqAVGDO,sqDOC,PH,logSULF,logWEIGHT,logLENGTH, logHG))
head(logALLSEMDATA)
length(logALLSEMDATA)
length(logALLSEMDATA[,1])
length(unique(logALLSEMDATA[,1]))
logALLSEMDATA <- data.frame(sapply(logALLSEMDATA, function(x) as.numeric(as.character(x))))
head(WAT_LID)
logALLSEMDATA$WAT_LID=WAT_LID
str(logALLSEMDATA)
head(logALLSEMDATA)
length(logALLSEMDATA)

logALLSEMDATA$SPECIES=ALL.MERGED.data$SPECIES_NAME

chart.Correlation(logALLSEMDATA[,c(3:18)], pch=21)
write.csv(logALLSEMDATA,"logALLSEMDATA.csv")
                                   
                                   






