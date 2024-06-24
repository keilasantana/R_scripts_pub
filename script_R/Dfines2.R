library(Rcmdr)
library(lattice)
library(epiDisplay)
library(rstatix)
install.packages("lifecycle")
install.packages("pillar")
install.packages("tidyselect")
library(dplyr)
###reg1 - guildford / reg2 - woking
rm(list=ls(all=TRUE))

############Databank
dfine <-read.csv("./ArticleSurrey/Dados/D-FINESfinal2006AND2010_Short_region.csv")
buffer1000 <-read.csv("/Volumes/armazenamento dois/Tese/ArticleSurrey/Buffer1000m.csv")
dfine <- mutate (dfine, SumHrsDayPaidHouseWk = SumHrsDayPaidWk + SumHrsDayHousewk, 
                 AutHrsDayPaidHouseWk = AutHrsDayPaidWk + AutHrsDayHousewk,
                 WinPaidHouseWkHrsDay = WinPaidWkHrsDay + WinHousewkHrsDay,
                 SprPaidHouseWkHrsDay = SprPaidWkHrsDay + SprHousewkHrsDay,
                 SumHrsDayVeryModActive = SumHrsDayVeryActive + SumHrsDayModActive,
                 AutHrsDayWkVeryModActive = AutHrsDayWkVeryActive + AutHrsDayWkModActive,
                 WinWkHrsDayVerModActive = WinWkHrsDayVerActive + WinWkHrsDayModActive,
                 SprWkHrsDayVeryModActive = SprWkHrsDayVeryActive + SprWkHrsDayModActive,
                 SumWkHrsNonWkDayHrsBed = (SumWkHrsDayHrsBed + SumNonWkHrsDayBed)/2,
                 AutWkNonWkHrsDayBed = (AutWkHrsDayBed + AutNonWkHrsDayBed)/2,
                 WinWkNonWkHrsDayBed = (WinWkHrsDayBed + WinNonWkHrsDayBed)/2,
                 SprWkNonWkHrsDayBed = (SprWkHrsDayBed + SprNonWkHrsDayBed)/2,
                 SumWkNonWkBreathless = SumWkBreathless + SumNonWkBreathless,
                 AutWkNonWkBreathless = AutWkBreathless + AutNonWkBreathless,
                 WinWkNonWkBreathless = WinWkBreathless + WinNonWkBreathless,
                 SprWkNonWkBreathless = SprWkBreathless + SprNonWkBreathless)

View(buffer1000)
View(dfine)
str(dfine)
dfine <- merge (dfine,buffer1000,by="Subject")
dfine$Ethnic <- as.factor(dfine$Ethnic)
class(dfine$Ethnic)

############################################Cumulative Frequency 
space <- seq(from= 0, to= 164.5, by= 25)
space
classe <- c("0-25","25-50","50-75","75-100","100-125","125-150")
classe

#############################################Reg1+2
############################Summer
dfinec<- dfine[dfine$Ethnic=="1",]
dfinea<- dfine[dfine$Ethnic=="2",]
summary(dfinec$SumVitD)
summary(dfinea$SumVitD)
dfinecfvd<- table(cut(dfinec$SumVitD,breaks = space,include.lowest = TRUE, right = FALSE,labels = classe))
dfinecfvd
cumdfinecfvd <- cumsum(dfinecfvd)
cumdfinecfvd

dfineafvd<- table(cut(dfinea$SumVitD,breaks = space,include.lowest = TRUE, right = FALSE,labels = classe))
dfineafvd
cumdfineafvd <- cumsum(dfineafvd)
cumdfineafvd

###################################Autumn
summary(dfinec$AutVitD)
summary(dfinea$AutVitD)
dfinecfvdAtumn<- table(cut(dfinec$AutVitD,breaks = space,include.lowest = TRUE, right = FALSE,labels = classe))
dfinecfvdAtumn
cumdfinecfvdAtumn <- cumsum(dfinecfvdAtumn)
cumdfinecfvdAtumn

dfineafvdAtumn<- table(cut(dfinea$AutVitD,breaks = space,include.lowest = TRUE, right = FALSE,labels = classe))
dfineafvdAtumn
cumdfineafvdAtumn <- cumsum(dfineafvdAtumn)
cumdfineafvdAtumn

###################################Winter
summary(dfinec$WinVitD)
summary(dfinea$WinVitD)
dfinecfvdWint<- table(cut(dfinec$WinVitD,breaks = space,include.lowest = TRUE, right = FALSE,labels = classe))
dfinecfvdWint
cumdfinecfvdWint <- cumsum(dfinecfvdWint)
cumdfinecfvdWint

dfineafvdWint<- table(cut(dfinea$WinVitD,breaks = space,include.lowest = TRUE, right = FALSE,labels = classe))
dfineafvdWint
cumdfineafvdWint <- cumsum(dfineafvdWint)
cumdfineafvdWint

###################################Spring
summary(dfinec$Spr07VitD)
summary(dfinea$Spr07VitD)
dfinecfvdspr7<- table(cut(dfinec$Spr07VitD,breaks = space,include.lowest = TRUE, right = FALSE,labels = classe))
dfinecfvdspr7
cumdfinecfvdspr7 <- cumsum(dfinecfvdspr7)
cumdfinecfvdspr7

dfineafvdspr7<- table(cut(dfinea$Spr07VitD,breaks = space,include.lowest = TRUE, right = FALSE,labels = classe))
dfineafvdspr7
cumdfineafvdspr7 <- cumsum(dfineafvdspr7)
cumdfineafvdspr7

#############################################AGE, NDVI and IMD
wilcox.test(Age2006 ~ Region, alternative='greater', data = dfine)
dfine %>% group_by(Region) %>%
  get_summary_stats(Age2006, type = "full")

wilcox.test(IMD ~ Region, alternative='less', data = dfine)
dfine %>% group_by(Region) %>%
  get_summary_stats(IMD, type = "full")
seq(from)

shapiro.test(dfine$X_mean)
dfine$logX_mean <- log(dfine$X_mean)
shapiro.test(dfine$logX_mean)
wilcox.test(X_mean ~ Region.x, alternative='greater', data = dfine)
tapply(dfine$X_mean, dfine$Region.x, summary)

shapiro.test(dfine$X_mean)
dfine$logX_mean <- log(dfine$X_mean)
shapiro.test(dfine$logX_mean)
wilcox.test(X_mean ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$X_mean, dfine$Ethnic, summary)

shapiro.test(reg1$X_mean)
reg1$logX_mean <- log(reg1$X_mean)
shapiro.test(reg1$logX_mean)
wilcox.test(X_mean ~ Ethnic, alternative='less', data = reg1)
tapply(reg1$X_mean, reg1$Ethnic, summary)

shapiro.test(reg2$X_mean)
reg2$logX_mean <- log(reg2$X_mean)
shapiro.test(reg2$logX_mean)
wilcox.test(X_mean ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$X_mean, reg2$Ethnic, summary)

####chi square test
Ethn <- xtabs(~Ethnic+Region, data=dfine)
tabpct(dfine$Ethnic, dfine$Region, percent ="row", graph = FALSE )
chisq.test(tabghq, correct=FALSE)

######################################################reg1 - Guildford
###############################Test t student #Mann Whitney
#######################Serum 25(OH)D (nmol/L)

shapiro.test(reg1$SumVitD)
reg1$logSumVitD <- log(reg1$SumVitD)
shapiro.test(reg1$logSumVitD)
wilcox.test(SumVitD ~ Ethnic, alternative='greater', data = reg1)
reg1 %>% group_by(Ethnic) %>%
  get_summary_stats(SumVitD, type = "full")

shapiro.test(dfine$SumVitD)
dfine$logSumVitD <- log(dfine$SumVitD)
shapiro.test(dfine$logSumVitD)
wilcox.test(SumVitD ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SumVitD, dfine$Ethnic, summary)

shapiro.test(reg2$SumVitD)
reg2$logSumVitD <- log(reg2$SumVitD)
shapiro.test(reg2$logSumVitD)
wilcox.test(SumVitD ~ Ethnic, alternative='greater', data = reg2)
reg2 %>% group_by(Ethnic) %>%
  get_summary_stats(SumVitD, type = "full")

shapiro.test(reg1$AutVitD)
reg1$logAutVitD <- log(reg1$AutVitD)
shapiro.test(reg1$logAutVitD)
wilcox.test(AutVitD ~ Ethnic, alternative='greater', data = reg1)
reg1 %>% group_by(Ethnic) %>%
  get_summary_stats(AutVitD, type = "full")

shapiro.test(dfine$AutVitD)
dfine$logAutVitD <- log(dfine$AutVitD)
shapiro.test(dfine$logAutVitD)
wilcox.test(AutVitD ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$AutVitD, dfine$Ethnic, summary)

shapiro.test(reg2$AutVitD)
reg2$logAutVitD <- log(reg2$AutVitD)
shapiro.test(reg2$logAutVitD)
wilcox.test(AutVitD ~ Ethnic, alternative='greater', data = reg2)
reg2 %>% group_by(Ethnic) %>%
  get_summary_stats(AutVitD, type = "full")

shapiro.test(reg1$WinVitD)
reg1$logWinVitD <- log(reg1$WinVitD)
shapiro.test(reg1$logWinVitD)
tapply(reg1$WinVitD, reg1$Ethnic, ci.numeric)
normalityTest(logWinVitD ~ Ethnic, test="shapiro.test", data=reg1)
tapply(reg1$logWinVitD, reg1$Ethnic,  var, na.rm=TRUE)
var.test(logWinVitD ~ Ethnic, alternative='two.sided', conf.level=.95, data=reg1) # nao rejeita Ho (p = 0,06) # as variancias sao iguais
t.test(logWinVitD~Ethnic, alternative='greater', conf.level=.95, var.equal=TRUE, data=reg1) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

shapiro.test(dfine$WinVitD)
dfine$logWinVitD <- log(dfine$WinVitD)
shapiro.test(dfine$logWinVitD)
wilcox.test(WinVitD ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$WinVitD, dfine$Ethnic, summary)

shapiro.test(reg2$WinVitD)
reg2$logWinVitD <- log(reg2$WinVitD)
shapiro.test(reg2$logWinVitD)
wilcox.test(WinVitD ~ Ethnic, alternative='greater', data = reg2)
reg2 %>% group_by(Ethnic) %>%
  get_summary_stats(WinVitD, type = "full")

shapiro.test(reg1$Spr07VitD)
reg1$logSpr07VitD <- log(reg1$Spr07VitD)
shapiro.test(reg1$logSpr07VitD)
tapply(reg1$Spr07VitD, reg1$Ethnic, ci.numeric)
normalityTest(logSpr07VitD ~ Ethnic, test="shapiro.test", data=reg1)
tapply(reg1$logSpr07VitD, reg1$Ethnic,  var, na.rm=TRUE)
var.test(logSpr07VitD ~ Ethnic, alternative='two.sided', conf.level=.95, data=reg1) # nao rejeita Ho (p = 0,21) # as variancias sao iguais
t.test(logSpr07VitD~Ethnic, alternative='greater', conf.level=.95, var.equal=TRUE, data=reg1) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

shapiro.test(dfine$Spr07VitD)
dfine$logSpr07VitD <- log(dfine$Spr07VitD)
shapiro.test(dfine$logSpr07VitD)
wilcox.test(Spr07VitD ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$Spr07VitD, dfine$Ethnic, summary)

shapiro.test(reg2$Spr07VitD)
reg2$logSpr07VitD <- log(reg2$Spr07VitD)
shapiro.test(reg2$logSpr07VitD)
tapply(reg2$Spr07VitD, reg2$Ethnic, ci.numeric)
normalityTest(logSpr07VitD ~ Ethnic, test="shapiro.test", data=reg2)
tapply(reg2$logSpr07VitD, reg2$Ethnic,  var, na.rm=TRUE)
var.test(logSpr07VitD ~ Ethnic, alternative='two.sided', conf.level=.95, data=reg2) # nao rejeita Ho (p = 0,06) # as variancias sao iguais
t.test(logSpr07VitD~Ethnic, alternative='greater', conf.level=.95, var.equal=TRUE, data=reg2) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

shapiro.test(reg1$Spr08VitD)
reg1$logSpr08VitD <- log(reg1$Spr08VitD)
shapiro.test(reg1$logSpr08VitD)
tapply(reg1$Spr08VitD, reg1$Ethnic, ci.numeric)
normalityTest(logSpr08VitD ~ Ethnic, test="shapiro.test", data=reg1)
tapply(reg1$logSpr08VitD, reg1$Ethnic,  var, na.rm=TRUE)
var.test(logSpr08VitD ~ Ethnic, alternative='two.sided', conf.level=.95, data=reg1) # nao rejeita Ho (p = 0,21) # as variancias sao iguais
t.test(logSpr08VitD~Ethnic, alternative='greater', conf.level=.95, var.equal=TRUE, data=reg1) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

shapiro.test(dfine$Spr08VitD)
dfine$logSpr08VitD <- log(dfine$Spr08VitD)
shapiro.test(dfine$logSpr08VitD)
wilcox.test(Spr08VitD ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$Spr08VitD, dfine$Ethnic, summary)

shapiro.test(reg2$Spr08VitD)
reg2$logSpr08VitD <- log(reg2$Spr08VitD)
shapiro.test(reg2$logSpr08VitD)
tapply(reg2$Spr08VitD, reg2$Ethnic, ci.numeric)
normalityTest(logSpr08VitD ~ Ethnic, test="shapiro.test", data=reg2)
tapply(reg2$logSpr08VitD, reg2$Ethnic,  var, na.rm=TRUE)
var.test(logSpr08VitD ~ Ethnic, alternative='two.sided', conf.level=.95, data=reg2) # nao rejeita Ho (p = 0,06) # as variancias sao iguais
t.test(logSpr08VitD~Ethnic, alternative='greater', conf.level=.95, var.equal=TRUE, data=reg2) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

############################RUV
shapiro.test(reg1$SumMEAN)
reg1$logSumMEAN <- log(reg1$SumMEAN)
shapiro.test(reg1$logSumMEAN)
wilcox.test(SumMEAN ~ Ethnic, alternative='greater', data = reg1)
reg1 %>% group_by(Ethnic) %>%
  get_summary_stats(SumMEAN, type = "full")

shapiro.test(dfine$SumMEAN)
wilcox.test(SumMEAN ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SumMEAN, dfine$Ethnic, summary)

shapiro.test(reg2$SumMEAN)
reg2$logSumMEAN <- log(reg2$SumMEAN)
shapiro.test(reg2$logSumMEAN)
wilcox.test(SumMEAN ~ Ethnic, alternative='greater', data = reg2)
reg2 %>% group_by(Ethnic) %>%
  get_summary_stats(SumMEAN, type = "full")

shapiro.test(reg1$AutMEAN)
wilcox.test(AutMEAN ~ Ethnic, alternative='greater', data = reg1)
reg1 %>% group_by(Ethnic) %>%
  get_summary_stats(AutMEAN, type = "full")

shapiro.test(dfine$AutMEAN)
wilcox.test(AutMEAN ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$AutMEAN, dfine$Ethnic, summary)

shapiro.test(reg2$AutMEAN)
wilcox.test(AutMEAN ~ Ethnic, alternative='greater', data = reg2)
reg2 %>% group_by(Ethnic) %>%
  get_summary_stats(AutMEAN, type = "full")

shapiro.test(reg1$WinMEAN)
wilcox.test(WinMEAN ~ Ethnic, alternative='greater', data = reg1)
reg1 %>% group_by(Ethnic) %>%
  get_summary_stats(WinMEAN, type = "full")

shapiro.test(dfine$WinMEAN)
wilcox.test(WinMEAN ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$WinMEAN, dfine$Ethnic, summary)

shapiro.test(reg2$WinMEAN)
wilcox.test(WinMEAN ~ Ethnic, alternative='greater', data = reg2)
reg2 %>% group_by(Ethnic) %>%
  get_summary_stats(WinMEAN, type = "full")

shapiro.test(reg1$SprMEAN)
wilcox.test(SprMEAN ~ Ethnic, alternative='greater', data = reg1)
reg1 %>% group_by(Ethnic) %>%
  get_summary_stats(SprMEAN, type = "full")

shapiro.test(dfine$SprMEAN)
wilcox.test(SprMEAN ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SprMEAN, dfine$Ethnic, summary)

shapiro.test(reg2$SprMEAN)
wilcox.test(SprMEAN ~ Ethnic, alternative='greater', data = reg2)
reg2 %>% group_by(Ethnic) %>%
  get_summary_stats(SprMEAN, type = "full")

################################PTH
shapiro.test(reg1$PTHSum06)
reg1$logPTHSum06 <- log(reg1$PTHSum06)
shapiro.test(reg1$logPTHSum06)
wilcox.test(PTHSum06 ~ Ethnic, alternative='less', data = reg1)
reg1 %>% group_by(Ethnic) %>%
  get_summary_stats(PTHSum06, type = "full")

shapiro.test(dfine$PTHSum06)
dfine$logPTHSum06 <- log(dfine$PTHSum06)
shapiro.test(dfine$logPTHSum06)
wilcox.test(PTHSum06 ~ Ethnic, alternative='less', data = dfine)
tapply(dfine$PTHSum06, dfine$Ethnic, summary)

shapiro.test(reg2$PTHSum06)
reg2$logPTHSum06 <- log(reg2$PTHSum06)
shapiro.test(reg2$logPTHSum06)
tapply(reg2$PTHSum06, reg2$Ethnic, ci.numeric)
normalityTest(logPTHSum06 ~ Ethnic, test="shapiro.test", data=reg2)
tapply(reg2$logPTHSum06, reg2$Ethnic,  var, na.rm=TRUE)
var.test(logPTHSum06 ~ Ethnic, alternative='two.sided', conf.level=.95, data=reg2) # nao rejeita Ho (p = 0,06) # as variancias sao iguais
t.test(logPTHSum06~Ethnic, alternative='less', conf.level=.95, var.equal=TRUE, data=reg2) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

shapiro.test(reg1$PTHAut06)
reg1$logPTHAut06 <- log(reg1$PTHAut06)
shapiro.test(reg1$logPTHAut06)
tapply(reg1$PTHAut06, reg1$Ethnic, ci.numeric)
normalityTest(logPTHAut06 ~ Ethnic, test="shapiro.test", data=reg1)
tapply(reg1$logPTHAut06, reg1$Ethnic,  var, na.rm=TRUE)
var.test(logPTHAut06 ~ Ethnic, alternative='two.sided', conf.level=.95, data=reg1) # nao rejeita Ho (p = 0,06) # as variancias sao iguais
t.test(logPTHAut06~Ethnic, alternative='less', conf.level=.95, var.equal=TRUE, data=reg1) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

shapiro.test(dfine$PTHAut06)
dfine$logPTHAut06 <- log(dfine$PTHAut06)
shapiro.test(dfine$logPTHAut06)
tapply(dfine$PTHAut06, dfine$Ethnic, ci.numeric)
normalityTest(logPTHAut06 ~ Ethnic, test="shapiro.test", data=dfine)
tapply(dfine$logPTHAut06, dfine$Ethnic,  var, na.rm=TRUE)
var.test(logPTHAut06 ~ Ethnic, alternative='two.sided', conf.level=.95, data=dfine) # nao rejeita Ho (p = 0,31) # as variancias sao iguais
t.test(logPTHAut06~Ethnic, alternative='less', conf.level=.95, var.equal=TRUE, data=dfine) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

shapiro.test(reg2$PTHAut06)
reg2$logPTHAut06 <- log(reg2$PTHAut06)
shapiro.test(reg2$logPTHAut06)
tapply(reg2$PTHAut06, reg2$Ethnic, ci.numeric)
normalityTest(logPTHAut06 ~ Ethnic, test="shapiro.test", data=reg2)
tapply(reg2$logPTHAut06, reg2$Ethnic,  var, na.rm=TRUE)
var.test(logPTHAut06 ~ Ethnic, alternative='two.sided', conf.level=.95, data=reg2) # nao rejeita Ho (p = 0,06) # as variancias sao iguais
t.test(logPTHAut06~Ethnic, alternative='less', conf.level=.95, var.equal=TRUE, data=reg2) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

shapiro.test(reg1$PTHWin06)
reg1$logPTHWin06 <- log(reg1$PTHWin06)
shapiro.test(reg1$logPTHWin06)
tapply(reg1$PTHWin06, reg1$Ethnic, ci.numeric)
normalityTest(logPTHWin06 ~ Ethnic, test="shapiro.test", data=reg1)
tapply(reg1$logPTHWin06, reg1$Ethnic,  var, na.rm=TRUE)
var.test(logPTHWin06 ~ Ethnic, alternative='two.sided', conf.level=.95, data=reg1) # nao rejeita Ho (p = 0,06) # as variancias sao iguais
t.test(logPTHWin06~Ethnic, alternative='less', conf.level=.95, var.equal=TRUE, data=reg1) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

shapiro.test(dfine$PTHWin06)
dfine$logPTHWin06 <- log(dfine$PTHWin06)
shapiro.test(dfine$logPTHWin06)
tapply(dfine$PTHWin06, dfine$Ethnic, ci.numeric)
normalityTest(logPTHWin06 ~ Ethnic, test="shapiro.test", data=dfine)
tapply(dfine$logPTHWin06, dfine$Ethnic,  var, na.rm=TRUE)
var.test(logPTHWin06 ~ Ethnic, alternative='two.sided', conf.level=.95, data=dfine) # nao rejeita Ho (p = 0,87) # as variancias sao iguais
t.test(logPTHWin06~Ethnic, alternative='less', conf.level=.95, var.equal=TRUE, data=dfine) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

shapiro.test(reg2$PTHWin06)
reg2$logPTHWin06 <- log(reg2$PTHWin06)
shapiro.test(reg2$logPTHWin06)
tapply(reg2$PTHWin06, reg2$Ethnic, ci.numeric)
normalityTest(logPTHWin06 ~ Ethnic, test="shapiro.test", data=reg2)
tapply(reg2$logPTHWin06, reg2$Ethnic,  var, na.rm=TRUE)
var.test(logPTHWin06 ~ Ethnic, alternative='two.sided', conf.level=.95, data=reg2) # nao rejeita Ho (p = 0,06) # as variancias sao iguais
t.test(logPTHWin06~Ethnic, alternative='less', conf.level=.95, var.equal=TRUE, data=reg2) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

shapiro.test(reg1$PTHSpr07)
reg1$logPTHSpr07 <- log(reg1$PTHSpr07)
shapiro.test(reg1$logPTHSpr07)
tapply(reg1$PTHSpr07, reg1$Ethnic, ci.numeric)
normalityTest(logPTHSpr07 ~ Ethnic, test="shapiro.test", data=reg1)
tapply(reg1$logPTHSpr07, reg1$Ethnic,  var, na.rm=TRUE)
var.test(logPTHSpr07 ~ Ethnic, alternative='two.sided', conf.level=.95, data=reg1) # nao rejeita Ho (p = 0,06) # as variancias sao iguais
t.test(logPTHSpr07~Ethnic, alternative='less', conf.level=.95, var.equal=TRUE, data=reg1) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

shapiro.test(dfine$PTHSpr07)
dfine$logPTHSpr07 <- log(dfine$PTHSpr07)
shapiro.test(dfine$logPTHSpr07)
tapply(dfine$PTHSpr07, dfine$Ethnic, ci.numeric)
normalityTest(logPTHSpr07 ~ Ethnic, test="shapiro.test", data=dfine)
tapply(dfine$logPTHSpr07, dfine$Ethnic,  var, na.rm=TRUE)
var.test(logPTHSpr07 ~ Ethnic, alternative='two.sided', conf.level=.95, data=dfine) # nao rejeita Ho (p = 0,87) # as variancias sao iguais
t.test(logPTHSpr07~Ethnic, alternative='less', conf.level=.95, var.equal=TRUE, data=dfine) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

shapiro.test(reg2$PTHSpr07)
reg2$logPTHSpr07 <- log(reg2$PTHSpr07)
shapiro.test(reg2$logPTHSpr07)
tapply(reg2$PTHSpr07, reg2$Ethnic, ci.numeric)
normalityTest(logPTHSpr07 ~ Ethnic, test="shapiro.test", data=reg2)
tapply(reg2$logPTHSpr07, reg2$Ethnic,  var, na.rm=TRUE)
var.test(logPTHSpr07 ~ Ethnic, alternative='two.sided', conf.level=.95, data=reg2) # nao rejeita Ho (p = 0,06) # as variancias sao iguais
t.test(logPTHSpr07~Ethnic, alternative='less', conf.level=.95, var.equal=TRUE, data=reg2) # nao rejeita Ho (p = 0,26) # nao existe diferenca entre as medias

#################Activities
#SumHrsDayPaidHouseWk, AutHrsDayPaidHouseWk,WinPaidHouseWkHrsDay,SprPaidHouseWkHrsDay,
#SumDaysWeekPaidHouseWk, AutDaysWeekPaidHouseWk, WinPaidHouseWkDaysWeek,SprPaidHouseWkDaysWeek
#SumHrsDayVeryModActive,AutHrsDayWkVeryModActive,WinWkHrsDayVerModActive,SprWkHrsDayVeryModActive,
#SumWkHrsNonWkDayHrsBed, AutWkNonWkHrsDayBed,WinWkNonWkHrsDayBed,SprWkNonWkHrsDayBed,
#SumWkNonWkBreathless,AutWkNonWkBreathless, WinWkNonWkBreathless, SprWkNonWkBreathless
########Walking
shapiro.test(reg1$SumWTotTime)
wilcox.test(SumWTotTime ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$SumWTotTime, reg1$Ethnic, summary)

shapiro.test(reg2$SumWTotTime)###log = NA (value = 0)
wilcox.test(SumWTotTime ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$SumWTotTime, reg2$Ethnic, summary)

shapiro.test(dfine$SumWTotTime)
wilcox.test(SumWTotTime ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SumWTotTime, dfine$Ethnic, summary)

shapiro.test(reg1$AutWTotTime)
wilcox.test(AutWTotTime ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$AutWTotTime, reg1$Ethnic, summary)

shapiro.test(reg2$AutWTotTime)###log = NA (value = 0)
wilcox.test(AutWTotTime ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$AutWTotTime, reg2$Ethnic, summary)

shapiro.test(dfine$AutWTotTime)
wilcox.test(AutWTotTime ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$AutWTotTime, dfine$Ethnic, summary)

shapiro.test(reg1$WinWTotTime)
wilcox.test(WinWTotTime ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$WinWTotTime, reg1$Ethnic, summary)

shapiro.test(reg2$WinWTotTime)###log = NA (value = 0)
wilcox.test(WinWTotTime ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$WinWTotTime, reg2$Ethnic, summary)

shapiro.test(dfine$WinWTotTime)
wilcox.test(WinWTotTime ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$WinWTotTime, dfine$Ethnic, summary)

shapiro.test(reg1$SprWTotTime)
wilcox.test(SprWTotTime ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$SprWTotTime, reg1$Ethnic, summary)

shapiro.test(reg2$SprWTotTime)###log = NA (value = 0)
wilcox.test(SprWTotTime ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$SprWTotTime, reg2$Ethnic, summary)

shapiro.test(dfine$SprWTotTime)
wilcox.test(SprWTotTime ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SprWTotTime, dfine$Ethnic, summary)

########Hours per day in paid/house work
shapiro.test(reg1$SumHrsDayPaidHouseWk)
wilcox.test(SumHrsDayPaidHouseWk ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$SumHrsDayPaidHouseWk, reg1$Ethnic, summary)

shapiro.test(reg2$SumHrsDayPaidHouseWk)###log = NA (value = 0)
wilcox.test(SumHrsDayPaidHouseWk ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$SumHrsDayPaidHouseWk, reg2$Ethnic, summary)

shapiro.test(dfine$SumHrsDayPaidHouseWk)
wilcox.test(SumHrsDayPaidHouseWk ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SumHrsDayPaidHouseWk, dfine$Ethnic, summary)

shapiro.test(reg1$AutHrsDayPaidHouseWk)
wilcox.test(AutHrsDayPaidHouseWk ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$AutHrsDayPaidHouseWk, reg1$Ethnic, summary)

shapiro.test(reg2$AutHrsDayPaidHouseWk)###log = NA (value = 0)
wilcox.test(AutHrsDayPaidHouseWk ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$AutHrsDayPaidHouseWk, reg2$Ethnic, summary)

shapiro.test(dfine$AutHrsDayPaidHouseWk)
wilcox.test(AutHrsDayPaidHouseWk ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$AutHrsDayPaidHouseWk, dfine$Ethnic, summary)

shapiro.test(reg1$WinPaidHouseWkHrsDay)
wilcox.test(WinPaidHouseWkHrsDay ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$WinPaidHouseWkHrsDay, reg1$Ethnic, summary)

shapiro.test(reg2$WinPaidHouseWkHrsDay)###log = NA (value = 0)
wilcox.test(WinPaidHouseWkHrsDay ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$WinPaidHouseWkHrsDay, reg2$Ethnic, summary)

shapiro.test(dfine$WinPaidHouseWkHrsDay)
wilcox.test(WinPaidHouseWkHrsDay ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$WinPaidHouseWkHrsDay, dfine$Ethnic, summary)

shapiro.test(reg1$SprPaidHouseWkHrsDay)
wilcox.test(SprPaidHouseWkHrsDay ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$SprPaidHouseWkHrsDay, reg1$Ethnic, summary)

shapiro.test(reg2$SprPaidHouseWkHrsDay)###log = NA (value = 0)
wilcox.test(SprPaidHouseWkHrsDay ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$SprPaidHouseWkHrsDay, reg2$Ethnic, summary)

shapiro.test(dfine$SprPaidHouseWkHrsDay)
wilcox.test(SprPaidHouseWkHrsDay ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SprPaidHouseWkHrsDay, dfine$Ethnic, summary)

#############Hours per day in house work
shapiro.test(reg1$SumHrsDayHousewk)
wilcox.test(SumHrsDayHousewk ~ Ethnic, alternative='less', data = reg1)
tapply(reg1$SumHrsDayHousewk, reg1$Ethnic, summary)

shapiro.test(reg2$SumHrsDayHousewk)###log = NA (value = 0)
wilcox.test(SumHrsDayHousewk ~ Ethnic, alternative='less', data = reg2)
tapply(reg2$SumHrsDayHousewk, reg2$Ethnic, summary)

shapiro.test(dfine$SumHrsDayHousewk)
wilcox.test(SumHrsDayHousewk ~ Ethnic, alternative='less', data = dfine)
tapply(dfine$SumHrsDayHousewk, dfine$Ethnic, summary)

shapiro.test(reg1$AutHrsDayHousewk)
wilcox.test(AutHrsDayHousewk ~ Ethnic, alternative='less', data = reg1)
tapply(reg1$AutHrsDayHousewk, reg1$Ethnic, summary)

shapiro.test(reg2$AutHrsDayHousewk)###log = NA (value = 0)
wilcox.test(AutHrsDayHousewk ~ Ethnic, alternative='less', data = reg2)
tapply(reg2$AutHrsDayHousewk, reg2$Ethnic, summary)

shapiro.test(dfine$AutHrsDayHousewk)
wilcox.test(AutHrsDayHousewk ~ Ethnic, alternative='less', data = dfine)
tapply(dfine$WinHousewkHrsDay, dfine$Ethnic, summary)

shapiro.test(reg1$WinHousewkHrsDay)
wilcox.test(WinHousewkHrsDay ~ Ethnic, alternative='less', data = reg1)
tapply(reg1$WinHousewkHrsDay, reg1$Ethnic, summary)

shapiro.test(reg2$WinHousewkHrsDay)###log = NA (value = 0)
wilcox.test(WinHousewkHrsDay ~ Ethnic, alternative='less', data = reg2)
tapply(reg2$WinHousewkHrsDay, reg2$Ethnic, summary)

shapiro.test(dfine$AutHrsDayHousewk)
wilcox.test(AutHrsDayHousewk ~ Ethnic, alternative='less', data = dfine)
tapply(dfine$AutHrsDayHousewk, dfine$Ethnic, summary)

shapiro.test(reg1$SprHousewkHrsDay)
wilcox.test(SprHousewkHrsDay ~ Ethnic, alternative='less', data = reg1)
tapply(reg1$SprHousewkHrsDay, reg1$Ethnic, summary)

shapiro.test(reg2$SprHousewkHrsDay)###log = NA (value = 0)
wilcox.test(SprHousewkHrsDay ~ Ethnic, alternative='less', data = reg2)
tapply(reg2$SprHousewkHrsDay, reg2$Ethnic, summary)

shapiro.test(dfine$SprHousewkHrsDay)
wilcox.test(SprHousewkHrsDay ~ Ethnic, alternative='less', data = dfine)
tapply(dfine$SprHousewkHrsDay, dfine$Ethnic, summary)

######################Hours per day in paid work
shapiro.test(reg1$SumDaysWeekHousewk)
wilcox.test(SumDaysWeekHousewk ~ Ethnic, alternative='less', data = reg1)
tapply(reg1$SumDaysWeekHousewk, reg1$Ethnic, summary)

shapiro.test(reg2$SumDaysWeekHousewk)###log = NA (value = 0)
wilcox.test(SumDaysWeekHousewk ~ Ethnic, alternative='less', data = reg2)
tapply(reg2$SumDaysWeekHousewk, reg2$Ethnic, summary)

shapiro.test(dfine$SumDaysWeekHousewk)
wilcox.test(SumDaysWeekHousewk ~ Ethnic, alternative='less', data = dfine)
tapply(dfine$SumDaysWeekHousewk, dfine$Ethnic, summary)

shapiro.test(reg1$AutDaysWeekHousewk)
wilcox.test(AutDaysWeekHousewk ~ Ethnic, alternative='less', data = reg1)
tapply(reg1$AutDaysWeekHousewk, reg1$Ethnic, summary)

shapiro.test(reg2$AutDaysWeekHousewk)###log = NA (value = 0)
wilcox.test(AutDaysWeekHousewk ~ Ethnic, alternative='less', data = reg2)
tapply(reg2$AutDaysWeekHousewk, reg2$Ethnic, summary)

shapiro.test(dfine$AutDaysWeekHousewk)
wilcox.test(AutDaysWeekHousewk ~ Ethnic, alternative='less', data = dfine)
tapply(dfine$AutDaysWeekHousewk, dfine$Ethnic, summary)

shapiro.test(reg1$WinHousewkDaysWeek)
wilcox.test(WinHousewkDaysWeek ~ Ethnic, alternative='less', data = reg1)
tapply(reg1$WinHousewkDaysWeek, reg1$Ethnic, summary)

shapiro.test(reg2$WinHousewkDaysWeek)###log = NA (value = 0)
wilcox.test(WinHousewkDaysWeek ~ Ethnic, alternative='less', data = reg2)
tapply(reg2$WinHousewkDaysWeek, reg2$Ethnic, summary)

shapiro.test(dfine$WinHousewkDaysWeek)
wilcox.test(WinHousewkDaysWeek ~ Ethnic, alternative='less', data = dfine)
tapply(dfine$WinHousewkDaysWeek, dfine$Ethnic, summary)

shapiro.test(reg1$SprHousewkDaysWeek)
wilcox.test(SprHousewkDaysWeek ~ Ethnic, alternative='less', data = reg1)
tapply(reg1$SprHousewkDaysWeek, reg1$Ethnic, summary)

shapiro.test(reg2$SprHousewkDaysWeek)###log = NA (value = 0)
wilcox.test(SprHousewkDaysWeek ~ Ethnic, alternative='less', data = reg2)
tapply(reg2$SprHousewkDaysWeek, reg2$Ethnic, summary)

shapiro.test(dfine$SprHousewkDaysWeek)
wilcox.test(SprHousewkDaysWeek ~ Ethnic, alternative='less', data = dfine)
tapply(dfine$SprHousewkDaysWeek, dfine$Ethnic, summary)

########Days per week paid work
shapiro.test(reg1$SumDaysWeekPaidWk)
wilcox.test(SumDaysWeekPaidWk ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$SumDaysWeekPaidWk, reg1$Ethnic, summary)

shapiro.test(reg2$SumDaysWeekPaidWk)###log = NA (value = 0)
wilcox.test(SumDaysWeekPaidWk ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$SumDaysWeekPaidWk, reg2$Ethnic, summary)

shapiro.test(dfine$SumDaysWeekPaidWk)###log = NA (value = 0)
wilcox.test(SumDaysWeekPaidWk ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SumDaysWeekPaidWk, dfine$Ethnic, summary)

shapiro.test(reg1$AutDaysWeekPaidWk)
wilcox.test(AutDaysWeekPaidWk ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$AutDaysWeekPaidWk, reg1$Ethnic, summary)

shapiro.test(reg2$AutDaysWeekPaidWk)###log = NA (value = 0)
wilcox.test(AutDaysWeekPaidWk ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$AutDaysWeekPaidWk, reg2$Ethnic, summary)

shapiro.test(dfine$AutDaysWeekPaidWk)###log = NA (value = 0)
wilcox.test(AutDaysWeekPaidWk ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$AutDaysWeekPaidWk, dfine$Ethnic, summary)

shapiro.test(reg1$WinPaidWkDaysWeek)
wilcox.test(WinPaidWkDaysWeek ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$WinPaidWkDaysWeek, reg1$Ethnic, summary)

shapiro.test(reg2$WinPaidWkDaysWeek)###log = NA (value = 0)
wilcox.test(WinPaidWkDaysWeek ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$WinPaidWkDaysWeek, reg2$Ethnic, summary)

shapiro.test(dfine$WinPaidWkDaysWeek)
wilcox.test(WinPaidWkDaysWeek ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$WinPaidWkDaysWeek, dfine$Ethnic, summary)

shapiro.test(reg1$SprPaidWkDaysWeek)
wilcox.test(SprPaidWkDaysWeek ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$SprPaidWkDaysWeek, reg1$Ethnic, summary)

shapiro.test(reg2$SprPaidWkDaysWeek)###log = NA (value = 0)
wilcox.test(SprPaidWkDaysWeek ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$SprPaidWkDaysWeek, reg2$Ethnic, summary)

shapiro.test(dfine$SprPaidWkDaysWeek)
wilcox.test(SprPaidWkDaysWeek ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SprPaidWkDaysWeek, dfine$Ethnic, summary)

########Hours per day 
#SumHrsDayVeryModActive,AutHrsDayWkVeryModActive,WinWkHrsDayVerModActive,SprWkHrsDayVeryModActive,
shapiro.test(reg1$SumHrsDayVeryModActive)
wilcox.test(SumHrsDayVeryModActive ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$SumHrsDayVeryModActive, reg1$Ethnic, summary)

shapiro.test(reg2$SumHrsDayVeryModActive)###log = NA (value = 0)
wilcox.test(SumHrsDayVeryModActive ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$SumHrsDayVeryModActive, reg2$Ethnic, summary)

shapiro.test(dfine$SumHrsDayVeryModActive)###log = NA (value = 0)
wilcox.test(SumHrsDayVeryModActive ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SumHrsDayVeryModActive, dfine$Ethnic, summary)

shapiro.test(reg1$SumHrsDayInactiveWork)
wilcox.test(SumHrsDayInactiveWork ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$SumHrsDayInactiveWork, reg1$Ethnic, summary)

shapiro.test(reg2$SumHrsDayInactiveWork)###log = NA (value = 0)
wilcox.test(SumHrsDayInactiveWork ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$SumHrsDayInactiveWork, reg2$Ethnic, summary)

shapiro.test(dfine$SumHrsDayInactiveWork)
wilcox.test(SumHrsDayInactiveWork ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SumHrsDayInactiveWork, dfine$Ethnic, summary)

shapiro.test(reg1$AutHrsDayWkVeryModActive)
wilcox.test(AutHrsDayWkVeryModActive ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$AutHrsDayWkVeryModActive, reg1$Ethnic, summary)

shapiro.test(reg2$AutHrsDayWkVeryModActive)###log = NA (value = 0)
wilcox.test(AutHrsDayWkVeryModActive ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$AutHrsDayWkVeryModActive, reg2$Ethnic, summary)

shapiro.test(reg1$AutHrsDayWkInactive)
wilcox.test(AutHrsDayWkInactive ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$AutHrsDayWkInactive, reg1$Ethnic, summary)

shapiro.test(reg2$AutHrsDayWkInactive)###log = NA (value = 0)
wilcox.test(AutHrsDayWkInactive ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$AutHrsDayWkInactive, reg2$Ethnic, summary)

shapiro.test(dfine$AutHrsDayWkInactive)
wilcox.test(AutHrsDayWkInactive ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$AutHrsDayWkInactive, dfine$Ethnic, summary)

shapiro.test(reg1$WinWkHrsDayVerModActive)
wilcox.test(WinWkHrsDayVerModActive ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$WinWkHrsDayVerModActive, reg1$Ethnic, summary)

shapiro.test(reg2$WinWkHrsDayVerModActive)###log = NA (value = 0)
wilcox.test(WinWkHrsDayVerModActive ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$WinWkHrsDayVerModActive, reg2$Ethnic, summary)

shapiro.test(dfine$WinWkHrsDayVerModActive)
wilcox.test(WinWkHrsDayVerModActive ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$WinWkHrsDayVerModActive, dfine$Ethnic, summary)

shapiro.test(reg1$WinWkHrsDayInactive)
wilcox.test(WinWkHrsDayInactive ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$WinWkHrsDayInactive, reg1$Ethnic, summary)

shapiro.test(reg2$WinWkHrsDayInactive)###log = NA (value = 0)
wilcox.test(WinWkHrsDayInactive ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$WinWkHrsDayInactive, reg2$Ethnic, summary)

shapiro.test(dfine$WinWkHrsDayInactive)
wilcox.test(WinWkHrsDayInactive ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$WinWkHrsDayInactive, dfine$Ethnic, summary)

shapiro.test(reg1$SprWkHrsDayVeryModActive)
wilcox.test(SprWkHrsDayVeryModActive ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$SprWkHrsDayVeryModActive, reg1$Ethnic, summary)

shapiro.test(reg2$SprWkHrsDayVeryModActive)###log = NA (value = 0)
wilcox.test(SprWkHrsDayVeryModActive ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$SprWkHrsDayVeryModActive, reg2$Ethnic, summary)

shapiro.test(dfine$SprWkHrsDayVeryModActive)
wilcox.test(SprWkHrsDayVeryModActive ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SprWkHrsDayVeryModActive, dfine$Ethnic, summary)

shapiro.test(reg1$SprWkHrsDayInactive)
wilcox.test(SprWkHrsDayInactive ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$SprWkHrsDayInactive, reg1$Ethnic, summary)

shapiro.test(reg2$SprWkHrsDayInactive)###log = NA (value = 0)
wilcox.test(SprWkHrsDayInactive ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$SprWkHrsDayInactive, reg2$Ethnic, summary)

shapiro.test(dfine$SprWkHrsDayInactive)
wilcox.test(SprWkHrsDayInactive ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SprWkHrsDayInactive, dfine$Ethnic, summary)

#########################Hours spent in bed on work/non-work day

shapiro.test(reg1$SumWkHrsNonWkDayHrsBed)
wilcox.test(SumWkHrsNonWkDayHrsBed ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$SumWkHrsNonWkDayHrsBed, reg1$Ethnic, summary)

shapiro.test(reg2$SumWkHrsNonWkDayHrsBed)###log = NA (value = 0)
wilcox.test(SumWkHrsNonWkDayHrsBed ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$SumWkHrsNonWkDayHrsBed, reg2$Ethnic, summary)

shapiro.test(dfine$SumWkHrsNonWkDayHrsBed)
wilcox.test(SumWkHrsNonWkDayHrsBed ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SumWkHrsNonWkDayHrsBed, dfine$Ethnic, summary)

shapiro.test(reg1$AutWkNonWkHrsDayBed)
wilcox.test(AutWkNonWkHrsDayBed ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$AutWkNonWkHrsDayBed, reg1$Ethnic, summary)

shapiro.test(reg2$AutWkNonWkHrsDayBed)###log = NA (value = 0)
wilcox.test(AutWkNonWkHrsDayBed ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$AutWkNonWkHrsDayBed, reg2$Ethnic, summary)

shapiro.test(dfine$AutWkNonWkHrsDayBed)
wilcox.test(AutWkNonWkHrsDayBed ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$AutWkNonWkHrsDayBed, dfine$Ethnic, summary)

shapiro.test(reg1$WinWkNonWkHrsDayBed)
wilcox.test(WinWkNonWkHrsDayBed ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$WinWkNonWkHrsDayBed, reg1$Ethnic, summary)

shapiro.test(reg2$WinWkNonWkHrsDayBed)###log = NA (value = 0)
wilcox.test(WinWkNonWkHrsDayBed ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$WinWkNonWkHrsDayBed, reg2$Ethnic, summary)

shapiro.test(dfine$WinWkNonWkHrsDayBed)
wilcox.test(WinWkNonWkHrsDayBed ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$WinWkNonWkHrsDayBed, dfine$Ethnic, summary)

shapiro.test(reg1$SprWkNonWkHrsDayBed)
wilcox.test(SprWkNonWkHrsDayBed ~ Ethnic, alternative='greater', data = reg1)
tapply(reg1$SprWkNonWkHrsDayBed, reg1$Ethnic, summary)

shapiro.test(reg2$SprWkNonWkHrsDayBed)###log = NA (value = 0)
wilcox.test(SprWkNonWkHrsDayBed ~ Ethnic, alternative='greater', data = reg2)
tapply(reg2$SprWkNonWkHrsDayBed, reg2$Ethnic, summary)

shapiro.test(dfine$SprWkNonWkHrsDayBed)
wilcox.test(SprWkNonWkHrsDayBed ~ Ethnic, alternative='greater', data = dfine)
tapply(dfine$SprWkNonWkHrsDayBed, dfine$Ethnic, summary)

#########################breathlesness working / non-working
shapiro.test(reg1$SumWkNonWkBreathless)
wilcox.test(SumWkNonWkBreathless ~ Ethnic, alternative='less', data = reg1)
tapply(reg1$SumWkNonWkBreathless, reg1$Ethnic, summary)

shapiro.test(reg2$SumWkNonWkBreathless)###log = NA (value = 0)
wilcox.test(SumWkNonWkBreathless ~ Ethnic, alternative='less', data = reg2)
tapply(reg2$SumWkNonWkBreathless, reg2$Ethnic, summary)

shapiro.test(dfine$SumWkNonWkBreathless)
wilcox.test(SumWkNonWkBreathless ~ Ethnic, alternative='less', data = dfine)
tapply(dfine$SumWkNonWkBreathless, dfine$Ethnic, summary)

shapiro.test(reg1$AutWkNonWkBreathless)
wilcox.test(AutWkNonWkBreathless ~ Ethnic, alternative='less', data = reg1)
tapply(reg1$AutWkNonWkBreathless, reg1$Ethnic, summary)

shapiro.test(reg2$AutWkNonWkBreathless)###log = NA (value = 0)
wilcox.test(AutWkNonWkBreathless ~ Ethnic, alternative='less', data = reg2)
tapply(reg2$AutWkNonWkBreathless, reg2$Ethnic, summary)

shapiro.test(dfine$AutWkNonWkBreathless)
wilcox.test(AutWkNonWkBreathless ~ Ethnic, alternative='less', data = dfine)
tapply(dfine$AutWkNonWkBreathless, dfine$Ethnic, summary)

shapiro.test(reg1$WinWkNonWkBreathless)
wilcox.test(WinWkNonWkBreathless ~ Ethnic, alternative='less', data = reg1)
tapply(reg1$WinWkNonWkBreathless, reg1$Ethnic, summary)

shapiro.test(reg2$WinWkNonWkBreathless)###log = NA (value = 0)
wilcox.test(WinWkNonWkBreathless ~ Ethnic, alternative='less', data = reg2)
tapply(reg2$WinWkNonWkBreathless, reg2$Ethnic, summary)

shapiro.test(dfine$WinWkNonWkBreathless)
wilcox.test(WinWkNonWkBreathless ~ Ethnic, alternative='less', data = dfine)
tapply(dfine$WinWkNonWkBreathless, dfine$Ethnic, summary)

shapiro.test(reg1$SprWkNonWkBreathless)
wilcox.test(SprWkNonWkBreathless ~ Ethnic, alternative='less', data = reg1)
tapply(reg1$SprWkNonWkBreathless, reg1$Ethnic, summary)

shapiro.test(reg2$SprWkNonWkBreathless)###log = NA (value = 0)
wilcox.test(SprWkNonWkBreathless ~ Ethnic, alternative='less', data = reg2)
tapply(reg2$SprWkNonWkBreathless, reg2$Ethnic, summary)

shapiro.test(dfine$SprWkNonWkBreathless)
wilcox.test(SprWkNonWkBreathless ~ Ethnic, alternative='less', data = dfine)
tapply(dfine$SprWkNonWkBreathless, dfine$Ethnic, summary)

