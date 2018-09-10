#############################################################################WALLEYE PIECEWISE SEM ANALYSES##################################################
library("nlme", lib.loc="D:/R/R-3.5.0/library")
library("piecewiseSEM", lib.loc="D:/R/R-3.5.0/library")

###FIRST SEM ANALYSIS AT LAKE-LEVEL
SEM.transdata.WE=subset(logALLSEMDATA,logALLSEMDATA$SPECIES=="Walleye")
head(SEM.transdata.WE)
tail(SEM.transdata.WE)
chart.Correlation(SEM.transdata.WE[,c(7:17,19)], pch=21)

mod1.list= psem(
lme(logHG~ PH + sqDOC+logWEIGHT, random = ~1|WAT_LID, data = SEM.transdata.WE),
lme(logWEIGHT~ sqDOC, random = ~1|WAT_LID, data = SEM.transdata.WE),
sqDOC%~~%PH,
PH%~~%logSULF)
summary(mod1.list)

sink("./WalleyeResults/lakemodel.summary.txt")
print(summary(mod1.list))
sink()

sink("./WalleyeResults/lakemodel.coefs.txt")
coefs(mod1.list)
sink()

###ANOTHER LOOK AT LAKE-LEVEL SEM WITH DISSOLVED OXYGEN INCLUDED#####
mod1a.list= psem(
lme(logHG~ PH + sqMINDO+ sqDOC+logWEIGHT, random = ~1|WAT_LID, data = SEM.transdata.WE),
lme(logWEIGHT~ sqDOC+sqAVGDO, random = ~1|WAT_LID, data = SEM.transdata.WE),
sqDOC%~~%PH,
PH%~~%logSULF,
sqAVGDO%~~%sqDOC,
sqMINDO%~~%sqDOC)

summary(mod1a.list)
sink("./WalleyeResults/lakemodel_1a.summary.txt")
print(summary(mod1.list))
sink()
sink("./WalleyeResults/lakemodel_1a.coefs.txt")
print(coefs(mod1.list))
sink()

###SECOND SEM ANALYSIS AT LAKESHED-LEVEL
mod2.list= psem(
lme(logHG~STRAHLER+ PH + sqDOC +logWEIGHT, random = ~1|WAT_LID, data = SEM.transdata.WE),
lme(logWEIGHT~ sqDOC+logDIST+STRAHLER, random = ~1|WAT_LID, data = SEM.transdata.WE),
lm(sqDOC~STRAHLER+asinFOREST +logAGRI + logDIST + logWETLAND , data = SEM.transdata.WE),
lm(PH~STRAHLER+asinFOREST +logAGRI +logDIST, data = SEM.transdata.WE),
lm(asinFOREST~ STRAHLER, data = SEM.transdata.WE),
lm(logAGRI~ STRAHLER, data = SEM.transdata.WE),
sqDOC%~~%PH,
PH%~~%logSULF,
asinFOREST%~~%logAGRI,
logWETLAND%~~%logDIST,
asinFOREST%~~%logDIST,
asinFOREST%~~%logWETLAND,
logAGRI%~~%logWETLAND,
logDIST%~~%logAGRI)
summary(mod2.list, conditional = TRUE)

sink("./WalleyeResults/lakeshedmodel.coefs.txt")
coefs(mod2.list)
sink()
sink("./WalleyeResults/lakeshedmodel.summary.txt")
summary(mod2.list)
sink()

###SECOND SEM ANALYSIS AT LAKESHED-LEVEL WITH URBAN LANDUSE INCLUDED
mod2b.list= psem(
lme(logHG~STRAHLER+ PH + sqDOC +logWEIGHT +logURB, random = ~1|WAT_LID, data = SEM.transdata.WE),
lme(logWEIGHT~ sqDOC+STRAHLER+logURB+logDIST +logWETLAND, random = ~1|WAT_LID, data = SEM.transdata.WE),
lm(sqDOC~STRAHLER+asinFOREST +logAGRI + logDIST +logURB+logWETLAND , data = SEM.transdata.WE),
lm(PH~STRAHLER+asinFOREST +logAGRI +logDIST+logURB, data = SEM.transdata.WE),
lm(asinFOREST~ STRAHLER, data = SEM.transdata.WE),
lm(logAGRI~ STRAHLER, data = SEM.transdata.WE),
sqDOC%~~%PH,
PH%~~%logSULF,
asinFOREST%~~%logAGRI,
logWETLAND%~~%logDIST,
asinFOREST%~~%logDIST,
asinFOREST%~~%logWETLAND,
logAGRI%~~%logWETLAND,
logDIST%~~%logAGRI,
asinFOREST%~~%logURB,
logWETLAND%~~%logURB,
logAGRI%~~%logURB)
summary(mod2b.list, conditional = TRUE)
sink("./WalleyeResults/lakeshed_urbanmodel.summary.txt")
summary(mod2b.list, conditional = TRUE)
sink()
sink("./WalleyeResults/lakeshed_urbanmodel.coefs.txt")
coefs(mod2b.list)
sink()

###THIRD SEM ANALYSIS AT CLIMATE-LEVEL
mod3.list= psem(
lme(logHG~ logTEMP+ logRAIN+ PH + sqDOC + logWEIGHT, random = ~1|WAT_LID, data = SEM.transdata.WE),
lme(logWEIGHT~ sqDOC +PH  + logRAIN+ logGDD + logTEMP, random = ~1|WAT_LID, data = SEM.transdata.WE),
lm(sqDOC~logTEMP+ logRAIN + logGDD, data = SEM.transdata.WE),
lm(PH~logTEMP+ logRAIN +logGDD, data = SEM.transdata.WE),
sqDOC%~~%PH,
PH%~~%logSULF,sqDOC%~~%logSULF,
logTEMP%~~%logGDD,
logTEMP%~~%logRAIN,
logGDD%~~%logRAIN)
summary(mod3.list,conditional = TRUE)

sink("./WalleyeResults/climatemodel.summary.txt")
print(summary(mod3.list))
sink()
sink("./WalleyeResults/climatemodel.coefs.txt")
print(coefs(mod3.list))
sink()


###FOURTH SEM ANALYSIS AT ALL-LEVELS
mod4.list= psem(
lme(logHG~ logRAIN+ STRAHLER+logTEMP+asinFOREST+logDIST+ PH + sqDOC +logWEIGHT, random = ~1|WAT_LID, data = SEM.transdata.WE),
lme(logWEIGHT~ logRAIN+sqDOC+logAGRI +STRAHLER, random = ~1|WAT_LID, data = SEM.transdata.WE),
lm(sqDOC~logTEMP+ logRAIN + logGDD+STRAHLER+asinFOREST +logAGRI + logDIST + logWETLAND , data = SEM.transdata.WE),
lm(PH~logTEMP+ logRAIN + logGDD+STRAHLER+asinFOREST +logAGRI +logDIST, data = SEM.transdata.WE),
lm(asinFOREST~ STRAHLER, data = SEM.transdata.WE),
lm(logAGRI~ STRAHLER, data = SEM.transdata.WE),
sqDOC%~~%PH,
PH%~~%logSULF,
asinFOREST%~~%logAGRI,
logWETLAND%~~%logDIST,
asinFOREST%~~%logDIST,
asinFOREST%~~%logWETLAND,
logAGRI%~~%logWETLAND,
logDIST%~~%logAGRI,
logTEMP%~~%STRAHLER,
logAGRI%~~%logRAIN,
logAGRI%~~%logGDD,
asinFOREST%~~%logTEMP,
logAGRI%~~%logTEMP,
logTEMP%~~%logRAIN,
logTEMP%~~%logGDD,
asinFOREST%~~%logRAIN,
asinFOREST%~~%logGDD)
summary(mod4.list,conditional = TRUE)

