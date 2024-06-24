library(ggm)
library(rstatix)
library(readxl)
library(epiDisplay) ## Fornece o intervalo de confianca
library(Rcmdr) ## Importa banco de dados e faz grafico de qqplot
library(nortest) ## Testar normalidade
library(lattice) # Graficos
library(tidyverse)
library (epitools)
library (dplyr)
library (car)
library (multcomp)
library (emmeans)

##Artigo Tese
vd <- read_excel("/Volumes/armazenamento dois/Tese/Dados/BancoDados_Arara_VD.xlsx")
View(vd)
vd$id.menos50 <- ifelse(vd$idade > 50, 1, 0)

##Estatistica descritiva
ci.numeric(vd$vitaminad)
numSummary(vd$vitaminad, groups=vd$nivelvd, statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))
ci.numeric(vd$vitaminad [vd$nivelvd==1])
ci.numeric(vd$vitaminad [vd$nivelvd==0])
ci.numeric(vd$imc)
ci.numeric(vd$cintura)
summary(vd$idade)
summary(vd$sed)
summary(vd$sol)
summary(vd$ghq2)

### Normalidade - grafico
par(mfrow=c(1,2))
boxplot(vd$vitaminad,ylab="Níveis de vitamina D (ng/ml)", col=c("gray"))
hist(vd$vitaminad, main="",ylab="frequência", xlab = "Niveis de vitamina D (ng/ml)",col=c("gray"))
qqPlot(vd$vitaminad,xlab = "",ylab="Niveis de vitamina D (ng/ml)", dist="norm")

#################################Teste t student e Mann Witney (>50 ou ≤50 anos)
vd$sedlog <- log(vd$sed) ## Tranformacao logaritimica
ci.numeric(vd$sedlog)
class(vd$idade2)
vd$idade2 <- as.factor(vd$idade2)
tapply(vd$sedlog, vd$idade2, ci.numeric)
normalityTest(sedlog ~ idade2, test="shapiro.test", data=vd)
tapply(vd$sedlog, vd$idade2,  var, na.rm=TRUE)
var.test(sedlog ~ idade2, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
t.test(sedlog~idade2, alternative='less', conf.level=.95, var.equal=TRUE, data=vd)

class(vd$idade2)
vd$idade2 <- as.factor(vd$idade2)
tapply(vd$vitaminad, vd$idade2, ci.numeric)
normalityTest(vitaminad ~ idade2, test="shapiro.test", data=vd)
tapply(vd$vitaminad, vd$idade2,  var, na.rm=TRUE)
var.test(vitaminad ~ idade2, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
t.test(vitaminad~idade2, alternative='less', conf.level=.95, var.equal=TRUE, data=vd)

tapply(vd$imc, vd$idade2, ci.numeric)
normalityTest(imc ~ idade2, test="shapiro.test", data=vd)
tapply(vd$imc, vd$idade2,  var, na.rm=TRUE)
var.test(imc ~ idade2, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
t.test(imc~idade2, alternative='greater', conf.level=.95, var.equal=TRUE, data=vd)

tapply(vd$cintura, vd$idade2, ci.numeric)
normalityTest(cintura ~ idade2, test="shapiro.test", data=vd)
tapply(vd$cintura, vd$idade2,  var, na.rm=TRUE)
var.test(cintura ~ idade2, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
t.test(cintura~idade2, alternative='greater', conf.level=.95, var.equal=TRUE, data=vd)

tapply(vd$sol, vd$idade2,  var, na.rm=TRUE)
var.test(vd$sol ~ idade2, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
wilcox.test(sol ~ idade2, alternative='greater', data = vd)
vd %>% group_by(idade2) %>%
  get_summary_stats(sol, type = "full")

tapply(vd$ghq2, vd$idade2,  var, na.rm=TRUE)
var.test(vd$ghq2 ~ idade2, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
wilcox.test(ghq2 ~ idade2, alternative='greater', data = vd)
vd %>% group_by(idade2) %>%
  get_summary_stats(ghq2, type = "full")

###############################Anova e Kruskal Wallis com tercis de idade
quantile(vd$idade,c(0.33, 0.66, 1))
vd$idadecut <- cut(aravd$idade, breaks=c(0,44,55,72))### grupo 1 inclui 44, grupo 2 - 55, grupo 3 - 72
table(idadecut)

class(vd$idadecut)
tapply(vd$vitaminad, vd$idadecut, ci.numeric)
normalityTest(vitaminad ~ idadecut, test="shapiro.test", data=vd)
tapply(vd$vitaminad, vd$idadecut,  var, na.rm=TRUE)
bartlett.test(vd$vitaminad ~ vd$idadecut)
idvd <- aov(vd$vitaminad ~ vd$idadecut)
summary (idvd)
TukeyHSD(idvd)

tapply(vd$imc, vd$idadecut, ci.numeric)
normalityTest(imc ~ idadecut, test="shapiro.test", data=vd)
tapply(vd$imc, vd$idadecut,  var, na.rm=TRUE)
bartlett.test(vd$imc ~ vd$idadecut)
idvd <- aov(vd$imc ~ vd$idadecut)
summary (idvd)
TukeyHSD(idvd)

tapply(vd$cintura, vd$idadecut, ci.numeric)
normalityTest(cintura ~ idadecut, test="shapiro.test", data=vd)
tapply(vd$cintura, vd$idadecut,  var, na.rm=TRUE)
bartlett.test(vd$cintura ~ vd$idadecut)
idvd <- aov(vd$cintura ~ vd$idadecut)
summary (idvd)
TukeyHSD(idvd)

###Teste de Kruskal-Wallis com tercis de idade
tapply(vd$sed, vd$idadecut,  var, na.rm=TRUE)
bartlett.test(vd$sed ~ vd$idadecut)
kruskal.test(sed ~ idadecut, data = vd)
##Teste de post-hoc
#Teste de Dunn com ajuste de valore de p
dunn_test(sed ~ idadecut, data = vd, p.adjust.method = "bonferroni")
#Analise descritiva dos dados
vd %>% group_by(idadecut) %>%
  get_summary_stats(sed, type = "full")

tapply(vd$ghq2, vd$idadecut,  var, na.rm=TRUE)
bartlett.test(vd$ghq2 ~ vd$idadecut)
kruskal.test(ghq2 ~ idadecut, data = vd)
##Teste de post-hoc
#Teste de Dunn com ajuste de valore de p
dunn_test(ghq2 ~ idadecut, data = vd, p.adjust.method = "bonferroni")
#Analise descritiva dos dados
vd %>% group_by(idadecut) %>%
  get_summary_stats(ghq2, type = "full")

######################Nivel de exposicao solar (Mann-Whitney / Kruskal-Wallis)
class(vd$imc2)
vd$imc2 <- as.factor(vd$imc2)
tapply(vd$sed, vd$imc2,  var, na.rm=TRUE)
var.test(vd$sed ~ imc2, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
wilcox.test(sed ~ imc2, alternative='greater', data = vd)
vd %>% group_by(imc2) %>%
  get_summary_stats(sed, type = "full")

class(vd$fuma)
vd$fuma <- as.factor(vd$fuma)
tapply(vd$sed, vd$fuma,  var, na.rm=TRUE)
var.test(sed ~ fuma, alternative='two.sided', conf.level=.95, data=vd)
wilcox.test(sed ~ fuma, alternative='less', data = vd)
vd %>% group_by(fuma) %>%
  get_summary_stats(sed, type = "full")

class(vd$bebe)
vd$bebe <- as.factor(vd$bebe)
tapply(vd$sed, vd$bebe,  var, na.rm=TRUE)
var.test(sed ~ bebe, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
wilcox.test(sed ~ bebe, alternative='greater', data = vd)
vd %>% group_by(bebe) %>%
  get_summary_stats(sed, type = "full")

class(vd$sol20)
vd$sol20 <- as.factor(vd$sol20)
vd %>% group_by(sol20) %>%
  get_summary_stats(sed, type = "full")
tapply(vd$sed, vd$sol20,  var, na.rm=TRUE)
var.test(sed ~ sol20, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
wilcox.test(sed ~ sol20, alternative='less', data = vd)

class(vd$menopausa)
vd$menopausa <- as.factor(vd$menopausa)
tapply(vd$sed, vd$menopausa,  var, na.rm=TRUE)
var.test(sed ~ menopausa, alternative='two.sided', conf.level=.95, data=vd)
wilcox.test(sed ~ menopausa, alternative='less', data = vd)
vd %>% group_by(menopausa) %>%
  get_summary_stats(sed, type = "full")
par(mfrow=c(1,2))
hist(vd$sed[vd$menopausa == 1],
     ylab="Frequência", xlab="Exposição solar", main="Mulheres na menopausa")
hist(vd$sed[vd$menopausa == 0],
     ylab="Frequência", xlab="Exposição solar", main="Mulheres sem menopausa")

class(vd$ghq)
vd$ghq <- as.factor(vd$ghq)
tapply(vd$sed, vd$ghq,  var, na.rm=TRUE)
var.test(sed ~ ghq, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
wilcox.test(sed ~ ghq, alternative='greater', data = vd)
vd %>% group_by(ghq) %>%
  get_summary_stats(sed, type = "full")

#####################Kruskal-Wallis com RUV
tapply(vd$sed, vd$cor,  var, na.rm=TRUE)
bartlett.test(vd$sed ~ vd$cor)
kruskal.test(sed ~ cor, data = vd)
vd %>% group_by(cor) %>%
  get_summary_stats(sed, type = "full")

tapply(vd$sed, vd$ipaq,  var, na.rm=TRUE)
bartlett.test(vd$sed ~ vd$ipaq)
kruskal.test(sed ~ ipaq, data = vd)
vd %>% group_by(ipaq) %>%
  get_summary_stats(sed, type = "full")

tapply(vd$sed, vd$ocupacao2,  var, na.rm=TRUE)
bartlett.test(vd$sed ~ vd$ocupacao2)
kruskal.test(sed ~ ocupacao2, data = vd)
##Teste de post-hoc
#Teste de Dunn com ajuste de valore de p
dunn_test(sed ~ ocupacao2, data = vd, p.adjust.method = "bonferroni")
#Analise descritiva dos dados
vd %>% group_by(ocupacao2) %>%
  get_summary_stats(sed, type = "full")

##########################Concentracao de 25(OH)D:Testes T-student/Man Witney e Anova
class(vd$fuma)
vd$fuma <- as.factor(vd$fuma)
tapply(vd$vitaminad, vd$fuma, summary) ## Sumario da variavel dependente por outra variavel categorica
tapply(vd$vitaminad, vd$fuma, ci.numeric)
normalityTest(vitaminad ~ fuma, test="shapiro.test", data=vd)
tapply(vd$vitaminad, vd$fuma,  var, na.rm=TRUE)
var.test(vitaminad ~ fuma, alternative='two.sided', conf.level=.95, data=vd) # as variancias nao sao iguais
t.test(vitaminad~fuma, alternative='greater', conf.level=.95, var.equal=FALSE, data=vd)

class(vd$imc2)
vd$imc2 <- as.factor(vd$imc2)
tapply(vd$vitaminad, vd$imc2, ci.numeric)
normalityTest(vitaminad ~ imc2, test="shapiro.test", data=vd)
tapply(vd$vitaminad, vd$imc2,  var, na.rm=TRUE)
var.test(vitaminad ~ imc2, alternative='two.sided', conf.level=.95, data=vd) # as variancias nao sao iguais
t.test(vitaminad~imc2, alternative='greater', conf.level=.95, var.equal=FALSE, data=vd)

class(vd$menopausa)
vd$menopausa <- as.factor(vd$menopausa)
tapply(vitaminad, vd$menopausa, ci.numeric)
normalityTest(vitaminad ~ vd$menopausa, test="shapiro.test", data=vd)
tapply(vitaminad, vd$menopausa,  var, na.rm=TRUE)
var.test(vitaminad ~ vd$menopausa, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
t.test(vitaminad~vd$menopausa, alternative='less', conf.level=.95, var.equal=TRUE, data=vd)

class(vd$bebe)
vd$bebe <- as.factor(vd$bebe)
tapply(vitaminad, vd$bebe, ci.numeric)
normalityTest(vitaminad ~ bebe, test="shapiro.test", data=vd)
tapply(vitaminad, vd$bebe,  var, na.rm=TRUE)
var.test(vitaminad ~ bebe, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
t.test(vitaminad~bebe, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=vd)

summary(vd$sol)
vd$sol20 <- ifelse(vd$sol >= 20, 1, 0)
class(vd$sol20)
vd$sol20 <- as.factor(vd$sol20)
normalityTest(vitaminad ~ sol20, test="shapiro.test", data=vd)
tapply(vd$vitaminad, vd$sol20,  var, na.rm=TRUE)
var.test(vitaminad ~ sol20, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
t.test(vitaminad~sol20, alternative='less', conf.level=.95, var.equal=TRUE, data=vd)
tapply(vd$vitaminad, vd$bebe, ci.numeric)

class(vd$sed3)
vd$sed3 <- as.factor(vd$sed3)
tapply(vd$vitaminad, vd$sed3, ci.numeric)
normalityTest(vitaminad ~ sed3, test="shapiro.test", data=vd)
tapply(vd$vitaminad, vd$sed3,  var, na.rm=TRUE)
var.test(vitaminad ~ sed3, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
t.test(vitaminad~sed3, alternative='less', conf.level=.95, var.equal=TRUE, data=vd)

class(vd$proteor1)
vd$proteor1 <- as.factor(vd$proteor1)
tapply(vd$vitaminad, vd$proteor1, ci.numeric)
normalityTest(vitaminad ~ proteor1, test="shapiro.test", data=vd)
tapply(vd$vitaminad, vd$proteor1,  var, na.rm=TRUE)
var.test(vitaminad ~ proteor1, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
t.test(vitaminad~proteor1, alternative='greater', conf.level=.95, var.equal=TRUE, data=vd)

class(vd$ghq)
vd$ghq <- as.factor(vd$ghq)
tapply(vd$vitaminad, vd$ghq, ci.numeric)
normalityTest(vitaminad ~ ghq, test="shapiro.test", data=vd)
tapply(vd$vitaminad, vd$ghq,  var, na.rm=TRUE)
var.test(vitaminad ~ ghq, alternative='two.sided', conf.level=.95, data=vd) # as variancias sao iguais
t.test(vitaminad~ghq, alternative='greater', conf.level=.95, var.equal=TRUE, data=vd)

###Teste ANOVA com vitamina D
class(vd$cor)
vd$cor <- as.factor(vd$cor)
tapply(vd$vitaminad, vd$cor, ci.numeric)
normalityTest(vitaminad ~ cor, test="shapiro.test", data=vd)
tapply(vd$vitaminad, vd$cor,  var, na.rm=TRUE)
bartlett.test(vd$vitaminad ~ vd$cor)
vdcor <- aov(vd$vitaminad ~ vd$cor)
summary (vdcor)
TukeyHSD(vdcor)
help("bartlett.test")

class(vd$ipaq)
vd$ipaq <- as.factor(vd$ipaq)
tapply(vd$vitaminad, vd$ipaq, ci.numeric)
normalityTest(vitaminad ~ ipaq, test="shapiro.test", data=vd)
tapply(vd$vitaminad, vd$ipaq,  var, na.rm=TRUE)
bartlett.test(vd$vitaminad ~ vd$ipaq)
vdipaq <- aov(vd$vitaminad ~ vd$ipaq)
summary (vdipaq)
TukeyHSD(vdipaq)

class(vd$ocupacao2)
vd$ocupacao2 <- as.factor(vd$ocupacao2)
tapply(vd$vitaminad, vd$ocupacao2, ci.numeric)
normalityTest(vitaminad ~ ocupacao2, test="shapiro.test", data=vd)
tapply(vd$vitaminad, vd$ocupacao2,  var, na.rm=TRUE)
bartlett.test(vd$vitaminad ~ vd$ocupacao2)
vdocup <- aov(vd$vitaminad ~ vd$ocupacao2)
summary (vdocup)
TukeyHSD(vdocup)

###Teste de Kruskal-Wallis com vitamina D
tapply(vd$vitaminad, vd$cor,  var, na.rm=TRUE)
bartlett.test(vd$vitaminad ~ vd$cor)
kruskal.test(vitaminad ~ cor, data = vd)
##Teste de post-hoc
#Teste de Dunn com ajuste de valore de p
dunn_test(vitaminad ~ cor, data = vd, p.adjust.method = "bonferroni")
#Analise descritiva dos dados
vd %>% group_by(cor) %>%
  get_summary_stats(vitaminad, type = "median_iqr")

###############################Status vitamina D: Anova e Kruskal Wallis
vd$sedlog <- log(vd$sed) ## Tranformacao logaritimica
view(vd)
class(vd$nivelvd2)
vd$nivelvd2 <- as.factor(vd$nivelvd2)
tapply(vd$sedlog, vd$nivelvd2, ci.numeric)
normalityTest(sedlog ~ nivelvd2, test="shapiro.test", data=vd)
tapply(vd$sedlog, vd$nivelvd2,  var, na.rm=TRUE)
bartlett.test(vd$sedlog ~ vd$nivelvd2)
vdimc <- aov(vd$sedlog ~ vd$nivelvd2)
summary (vdimc)
TukeyHSD(vdimc)

class(vd$nivelvd2)
vd$nivelvd2 <- as.factor(vd$nivelvd2)
tapply(vd$vitaminad, vd$nivelvd2, ci.numeric)
normalityTest(vitaminad ~ nivelvd2, test="shapiro.test", data=vd)
tapply(vd$vitaminad, vd$nivelvd2,  var, na.rm=TRUE)
bartlett.test(vd$vitaminad ~ vd$nivelvd2)
vdimc <- aov(vd$vitaminad ~ vd$nivelvd2)
summary (vdimc)
TukeyHSD(vdimc)

tapply(vd$imc, vd$nivelvd2, ci.numeric)
normalityTest(imc ~ nivelvd2, test="shapiro.test", data=vd)
tapply(vd$imc, vd$nivelvd2,  var, na.rm=TRUE)
bartlett.test(vd$imc ~ vd$nivelvd2)
vdimc <- aov(vd$imc ~ vd$nivelvd2)
summary (vdimc)
TukeyHSD(vdimc)

tapply(vd$cintura, vd$nivelvd2, ci.numeric)
normalityTest(cintura ~ nivelvd2, test="shapiro.test", data=vd)
tapply(vd$cintura, vd$nivelvd2,  var, na.rm=TRUE)
bartlett.test(vd$cintura ~ vd$nivelvd2)
vdimc <- aov(vd$cintura ~ vd$nivelvd2)
summary (vdimc)
TukeyHSD(vdimc)

###Teste de Kruskal-Wallis nivel vitamina D
tapply(vd$sed, vd$nivelvd2,  var, na.rm=TRUE)
bartlett.test(vd$sed ~ vd$nivelvd2)
kruskal.test(sed ~ nivelvd2, data = vd)
##Teste de post-hoc
#Teste de Dunn com ajuste de valore de p
dunn_test(sed ~ nivelvd2, data = vd, p.adjust.method = "bonferroni")
#Analise descritiva dos dados
vd %>% group_by(nivelvd2) %>%
  get_summary_stats(sed, type = "full")

tapply(vd$ghq2, vd$nivelvd2,  var, na.rm=TRUE)
bartlett.test(vd$ghq2 ~ vd$nivelvd2)
kruskal.test(ghq2 ~ nivelvd2, data = vd)
##Teste de post-hoc
#Teste de Dunn com ajuste de valore de p
dunn_test(ghq2 ~ nivelvd2, data = vd, p.adjust.method = "bonferroni")
#Analise descritiva dos dados
vd %>% group_by(nivelvd2) %>%
  get_summary_stats(ghq2, type = "full")

tapply(vd$idade, vd$nivelvd2,  var, na.rm=TRUE)
bartlett.test(vd$idade ~ vd$nivelvd2)
kruskal.test(idade ~ nivelvd2, data = vd)
##Teste de post-hoc
#Teste de Dunn com ajuste de valore de p
dunn_test(idade ~ nivelvd2, data = vd, p.adjust.method = "bonferroni")
#Analise descritiva dos dados
vd %>% group_by(nivelvd2) %>%
  get_summary_stats(idade, type = "full")

tapply(vd$sol, vd$nivelvd2,  var, na.rm=TRUE)
bartlett.test(vd$sol ~ vd$nivelvd2)
kruskal.test(sol ~ nivelvd2, data = vd)
##Teste de post-hoc
#Teste de Dunn com ajuste de valore de p
dunn_test(sol ~ nivelvd2, data = vd, p.adjust.method = "bonferroni")
#Analise descritiva dos dados
vd %>% group_by(nivelvd2) %>%
  get_summary_stats(sol, type = "full")

###############################################Ancova
#############################Pressuposto: Independência entre VI e a covariavel
ancova <- aov(imc ~ vitaminad, data = vd)
summary (ancova) ############ P< 0,05: nao eh possivel realizar Ancova

##########################################Diagrama de dispersao
vd$sedlog <- log(vd$sed) ## Tranformacao logaritimica
shapiro.test(vd$ghq2)
shapiro.test(vd$vitaminad)
par(mfrow=c(1,1))
scatterplot(vitaminad~sedlog,xlab = "Nível de exposição solar", ylab = "Vitamina D", regLine=TRUE, smooth=FALSE, boxplot=FALSE, data = vd)
scatterplot(vitaminad~imc,xlab = "IMC", ylab = "Vitamina D", regLine=TRUE, smooth=FALSE, boxplot=FALSE, data = vd)
scatterplot(vitaminad~cintura,xlab = "Circunferência da cintura", ylab = "Vitamina D", regLine=TRUE, smooth=FALSE, boxplot=FALSE, data = vd)
scatterplot(vitaminad~idade,xlab = "Idade", ylab = "Vitamina D", regLine=TRUE, smooth=FALSE, boxplot=FALSE, data = vd)
scatterplot(vitaminad~sol,xlab = "Banho de sol", ylab = "Vitamina D", regLine=TRUE, smooth=FALSE, boxplot=FALSE, data = vd)
scatterplot(vitaminad~ghq2,xlab = "Angústia", ylab = "Vitamina D", regLine=TRUE, smooth=FALSE, boxplot=FALSE, data = vd)

scatterplotMatrix(~vitaminad+sedlog+idade+imc+cintura+sol, reg.line=FALSE, smooth=FALSE, spread=FALSE, diagonal = '', data=vd)

### Ha correlacao entre variaveis
cor.test(vd$imc,vd$cintura) ###Alta correlacao 0,83, p < 0,001
cor.test(vd$imc,vd$idade) ###Baixa correlacao - 0,213, p = 0,032
cor.test(vd$imc,vd$sedlog) ###Baixa correlacao 0,228, p = 0,026
cor.test(vd$idade,vd$sedlog) ### Baixa correlacao 0,218, p = 0,032
cor.test(vd$idade, vd$sol) ###Sem correlacao -0.017,  p-value = 0.8751
cor.test(vd$idade, vd$cintura) ###Sem correlacao -0.105,  p-value = 0.298
cor.test(vd$sol, vd$cintura) ###Sem correlacao ,  p-value = 
cor.test(vd$imc,vd$sol) ###Sem correlacao -0.056, p-value = 0.603
cor.test(vd$sedlog,vd$sol) ###Sem correlacao -0.121, p-value = 0.2774
cor.test(vd$sedlog,vd$cintura) ###Sem correlacao -0.047, p-value = 0.648
cor.test(vd$ghq2,vd$cintura)
cor.test(vd$ghq2,vd$sedlog)
cor.test(vd$ghq2,vd$sol)
cor.test(vd$ghq2,vd$imc)
cor.test(vd$ghq2,vd$idade)

# correlação VD, RUV, ghq controlando o IMC e idade
vd2 <- vd [c(-1,-2,-3,-4,-5),]
view (vd2)
rp1<-pcor(c("vitaminad","ghq2","idade","imc"),var(vd))  # r=0,214 p=0,034
pcor.test(rp1,2,length(vd$vitaminad))
rp2<-pcor(c("vitaminad","sedlog","idade","imc"),var(vd2))  # r=0,214 p=0,034
pcor.test(rp,2,length(vd2$vitaminad))

### Correlacao com vitamina D
cor.test(vd$sedlog,vd$vitaminad) ###Correlacao baixa de 0,294/ p = 0,004
cor.test(vd$imc,vd$vitaminad) ###Correlacao baixa de -0,326/ p < 0,001
cor.test(vd$cintura,vd$vitaminad) ###Correlacao baixa de -0,271/ p = 0,006
cor.test(vd$idade,vd$vitaminad) ###Corelacao baixa de 0,224/ p = 0,02
cor.test(vd$sol,vd$vitaminad) ###Sem correlacao significativa
cor.test(vd$ghq2,vd$vitaminad) ###Corelacao baixa de 0,309/ p = 0,002
rcorr.adjust(vd [,c("vitaminad","sedlog","idade", "imc","cintura","sol")], type = "pearson", use = "complete")

#################################Regressao linear
## Os valores de y tem que ser independentes (sorteio aleatorio)
class(vd$ocupacao3)
vd$ocupacao3 <- as.factor(vd$ocupacao3)
vd$sedlog <- log(vd$sed)
vd2 <- vd [c(-1,-2,-3,-4,-5),]
view (vd2)
sedreg <- lm(vitaminad~sedlog + imc + ghq2 + menopausa, data=vd2)
summary(sedreg)
sedreg <- lm(vitaminad~sedlog + imc + ghq2 + idade, data=vd2)
summary(sedreg)
sedreg <- lm(sedlog ~ imc + menopausa + bebe + ocupacao3, data=vd2)
summary(sedreg)
sedreg <- lm(sedlog ~ imc + ocupacao3 + idade + bebe, data=vd2)
summary(sedreg)
Anova(sedreg)



