library(Rcmdr)
library(epiDisplay) ###Para analise de risco relativo, Odds Ratio e razao de prevalencias
library(car)
rm(list=ls(all=TRUE))
dfine <-read.csv("/Volumes/armazenamento dois/Tese/ArticleSurrey/D-FINESfinal2006AND2010_Short_region.csv")
buffer1000 <-read.csv("/Volumes/armazenamento dois/Tese/ArticleSurrey/Buffer1000m.csv")
dietvd <-read.csv("/Volumes/armazenamento dois/Tese/ArticleSurrey/DFINES_dietvitD_summer.csv")

dfine <- merge (dfine,buffer1000,by="Subject")
dfine <- merge (dfine,dietvd,by="Subject")
View(dfine)

summary(dfine$SummerVitaminDintakeMicrograms.d)
summary(dfine$SumMEAN)
summary(dfine$SumWTotTime)
summary(dfine$PTHSum06)
summary(dfine$IMD)
summary(dfine$X_mean)

###################################Exploratory analysis
par(mfrow=c(2,2))

Boxplot(X_mean~fVitD, data=dfine, xlab="")
Boxplot(SumMEAN~fVitD, data=dfine, xlab="")
Boxplot(PTHSum06~fVitD, data=dfine, xlab="")
Boxplot(IMD~fVitD, data=dfine, xlab="")
Boxplot(SumWTotTime~fVitD, data=dfine, xlab="")

plotMeans(dfine$X_mean, dfine$fVitD, error.bars="conf.int", level=0.95, connect=FALSE, xlab="")
plotMeans(dfine$SumMEAN, dfine$fVitD, error.bars="conf.int", level=0.95, connect=FALSE, xlab="")
plotMeans(dfine$PTHSum06, dfine$fVitD, error.bars="conf.int", level=0.95, connect=FALSE, xlab="")
plotMeans(dfine$IMD, dfine$fVitD, error.bars="conf.int", level=0.95, connect=FALSE, xlab="")
plotMeans(dfine$SumWTotTime, dfine$fVitD, error.bars="conf.int", level=0.95, connect=FALSE, xlab="")
plotMeans(dfine$SummerVitaminDintakeMicrograms.d, dfine$fVitD, error.bars="conf.int", level=0.95, connect=FALSE, xlab="")

# Criando um fator com variaveis quantitativas
dfine$VitD <- ifelse((dfine$SumVitD >= 50), 0, 1) ## vitamin D deficience
dfine$Land1000 <- ifelse((dfine$X_mean > 0.2551), 0, 1) ###Exposicao ao baixo ndvi
dfine$SumSED <- ifelse((dfine$SumMEAN > 2), 0, 1) ##Exposicao: Baixa exposicao a RUV
dfine$PTHSum<- ifelse((dfine$PTHSum06 < 3.9), 0, 1)####Alto PTH
dfine$IMD2 <- ifelse((dfine$IMD > 14), 0, 1) ###Confusao: Vulnerabilidade social
dfine$walk <- ifelse((dfine$SumWTotTime >= 60), 0, 1) ###Exposicao: low walking
dfine$dietvd <- ifelse((dfine$SummerVitaminDintakeMicrograms.d >= 1.86), 0, 1)

# Transformando as variaveis qualitativas em fatores
dfine$Menopause2 <- factor(dfine$Menopause, levels=c(1,2), labels=c("Premenopausal","Postmenopausal"))
dfine$Ethnic2 <- factor(dfine$Ethnic, levels=c(1,2), labels=c("Caucasian","Asian")) 
dfine$fVitD <- factor(dfine$VitD, levels=c(0,1), labels=c("Suficience","Deficience")) 
dfine$fLand1000 <- factor(dfine$Land1000, levels=c(0,1), labels=c("High NDVI","Low NDVI"))
dfine$fSumSED <- factor(dfine$SumSED, levels=c(0,1), labels=c("High RUV","Low RUV"))
dfine$fPTHSum <- factor(dfine$PTHSum, levels=c(0,1), labels=c("Low PTH","High PTH"))
dfine$fIMD2 <- factor(dfine$IMD2, levels=c(0,1), labels=c("Low IMD","High IMD"))
dfine$fwalk <- factor(dfine$walk, levels=c(0,1), labels=c("High walk","Low walk"))
dfine$fdietvd <- factor(dfine$dietvd, levels=c(0,1), labels=c("High vdingest","Low vdingest"))

###########################Qui-quadrado
tabland1000 <- xtabs(~fLand1000+fVitD, data=dfine)
tabpct(dfine$fLand1000, dfine$fVitD, percent ="row", graph = FALSE )
chisq.test(tabland1000, correct=FALSE) ####Ho = Nao existe associação entre as variaveis 

tabSumSED <- xtabs(~fSumSED+fVitD, data=dfine)
tabpct(dfine$fSumSED, dfine$fVitD, percent ="row", graph = FALSE )
chisq.test(tabSumSED, correct=FALSE) 

tabwalk <- xtabs(~fwalk+fVitD, data=dfine)
tabpct(dfine$fwalk, dfine$fVitD, percent ="row", graph = FALSE )
chisq.test(tabwalk, correct=FALSE) ####Ho = Nao existe associação entre as variaveis

tabIMD2 <- xtabs(~fIMD2+fVitD, data=dfine)
tabpct(dfine$fIMD2, dfine$fVitD, percent ="row", graph = FALSE )
chisq.test(tabIMD2, correct=FALSE) 

tabMenopause2 <- xtabs(~Menopause2+fVitD, data=dfine)
tabpct(dfine$Menopause2, dfine$fVitD, percent ="row", graph = FALSE )
chisq.test(tabMenopause2, correct=FALSE) ####Ho = Nao existe associação entre as variaveis

tabEthnic2 <- xtabs(~Ethnic2+fVitD, data=dfine)
tabpct(dfine$Ethnic2, dfine$fVitD, percent ="row", graph = FALSE )
chisq.test(tabEthnic2, correct=FALSE) 

tabPTHSum <- xtabs(~fPTHSum+fVitD, data=dfine)
tabpct(dfine$fPTHSum, dfine$fVitD, percent ="row", graph = FALSE )
chisq.test(tabPTHSum, correct=FALSE) 

tabdietvd <- xtabs(~fdietvd+fVitD, data=dfine)
tabpct(dfine$fdietvd, dfine$fVitD, percent ="row", graph = FALSE )
chisq.test(tabdietvd, correct=FALSE) 

# Risco relativo (em estudos de coorte)

cs(dfine$fVitD, dfine$fLand1000)      
cs(dfine$fVitD, dfine$fSumSED)
cs(dfine$fVitD, dfine$fwalk)
cs(dfine$fVitD, dfine$fIMD2)
cs(dfine$VitD, dfine$Menopause2)
cs(dfine$VitD, dfine$Ethnic2)
cs(dfine$VitD, dfine$PTHSum)
cs(dfine$VitD, dfine$dietvd)

####Modelo de regressao logistica multiplo (stepwise forward)
mod.1 <- glm(fVitD ~ fLand1000, family=binomial(logit), data=dfine); 
summary(mod.1); exp(coef(mod.1)); exp(confint(mod.1))

mod.2 <- update(mod.1, .~. + SumSED, data=dfine)
summary(mod.2); exp(coef(mod.2)); exp(confint(mod.2))

mod.3 <- update(mod.2, .~. + dietvd, data=dfine)
summary(mod.3); exp(coef(mod.3)); exp(confint(mod.3))

mod.4 <- update(mod.3, .~. + Ethnic2, data=dfine)
summary(mod.4); exp(coef(mod.4)); exp(confint(mod.4))

mod.5 <- update(mod.4, .~. + PTHSum, data=dfine)
summary(mod.5); exp(coef(mod.5)); exp(confint(mod.5))

