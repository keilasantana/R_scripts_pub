###Projeto vitamina D

## Instalando programas
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("digest")
library(digest)
install.packages("epitools")
library (epitools)

## Nomeando e verificando banco de dados
aravd<-ParticipantesAraraquara_Dados_Completos
names(aravd)
str(aravd)
summary(aravd)

## Transformando variáveis
aravd$escolaridade <-as.factor(aravd$escolaridade)

aravd$cor <- as.character (aravd$cor)
str(aravd)

### Verificando correlação graficamente (sem verificar a normalidade)
ggplot(data = aravd, aes (x = idade, y = vitaminad)) + geom_point() + geom_smooth(method=lm)
ggplot(data = aravd, aes (x = cor, y = vitaminad)) + geom_point() + geom_smooth(method=lm)
ggplot(data = aravd, aes (x = escolaridade, y = vitaminad)) + geom_point() + geom_smooth(method=lm)
ggplot(data = aravd, aes (x = idade, y = vitaminad,colour = escolaridade)) + geom_point() 
ggplot(data = aravd, aes (x = idade, y = vitaminad,colour = cor)) + geom_point() 
ggplot(data = aravd, aes (x = idade, y = vitaminad,colour = cor,size=escolaridade)) + geom_point() 

### Verificando suposição de normalidade das variáveis
hist(aravd$vitaminad)
hist(aravd$idade)
qqnorm(aravd$vitaminad,main="QQplot Vitamina D", xlab="Quantis teóricos N(0,1)",pch=20)
qqline(aravd$vitaminad,tly=15,col="red")
qqnorm(aravd$idade,main="QQplot Idade", xlab="Quantis teóricos N(0,1)",pch=20)
qqline(aravd$idade,tly=15,col="red")

#Variavel Categórica (frequencias absoluta e relativa)
# escolaridade, cor da pele
table(aravd$cor)
100*table(aravd$cor)/sum(table(aravd$cor))

table(vacinacompl$vacinado)
100*table(vacinacompl$vacinado)/sum(table(vacinacompl$vacinado))

table(vacinacompl$escola)
100*table(vacinacompl$escola)/sum(table(vacinacompl$escola))
















