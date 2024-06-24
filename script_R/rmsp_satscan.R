library(reshape2)
library(dplyr) #mutate
library(janitor)
library(tidyverse) #pivot

###Organizar os dados da populacao RMSP
rmsp <- read.table("./fapesp/pandemia/ibge/pop_aps_rmsp_2010.csv", header=T,dec = ',')
rmsp <- mutate(rmsp, "M_60+" = M_60a69+M_70a79+M_80a89, M_90a99, M_100m,
               "F_60+" = F_60a69+F_70a79+F_80a89, F_90a99, F_100m)
rmsp <- rmsp[,-c(8:12,19:35)]
rmsp <- rmsp %>% pivot_longer( cols = 'M_0a9':'F_60+', names_to = "Faixa", values_to = "pop")
rmsp <- data.frame(rmsp, colsplit(rmsp$Faixa, pattern="_", names=paste("sep", 1:2)))
rmsp <- rmsp[,-c(2)]
colnames (rmsp) <- c ("cidade", "populacao","sexo","faixa") 
rmsp <- rmsp %>% mutate(sexo = recode(sexo, "M" = "MASCULINO","F" = "FEMININO"))

write.csv(rmsp,file = './fapesp/dados/satscan/rmsp/pop_aps_rmsp_2010_longer.csv')
rmsp2$AREA_POND<-as.character(rmsp2$AREA_POND)

###Organizar os dados de casos RMSP
cov.rmsp <- read.table("./fapesp/dados/satscan/rmsp/casos_aps_rmsp_2020_2022.csv", header=T,sep = ';')
cov.rmsp$CASOS <- as.integer(cov.rmsp$CASOS)

# FILTRANDO PELO OPERADOR DOS COLCHETES
df_filtrado <- cov.rmsp[cov.rmsp$CASOS > 0, ]

####Exportando a tabela
write.csv(df_filtrado,file="./fapesp/dados/satscan/rmsp/casos_aps_rmsp_2020_2022.csv")


sat.hiraq <- read.table("./fapesp/dados/satscan/teste/rmsp_espacial_hierarq.col.dbf", header=T, sep = ',')

###################################################################################DBF
library(foreign)

# Caminho para o seu arquivo DBF
arquivo_dbf <- "./fapesp/dados/satscan/teste/rmsp_espacial_hierarq.col.dbf"

# Ler o arquivo DBF
dados <- read.dbf(arquivo_dbf)

# Salvar os dados como CSV
write.csv(dados, file='./fapesp/dados/satscan/teste/rmsp_espacial_hierarq.col.csv', row.names = FALSE)
dados.csv <- read.csv("./fapesp/dados/satscan/teste/rmsp_espacial_hierarq.col.csv")

################
dados.gis <- read.dbf("./fapesp/dados/satscan/teste/rmsp_espacial_gini.gis.dbf")
write.csv(dados.gis, file='./fapesp/dados/satscan/teste/rmsp_espacial_gini.gis.csv', row.names = FALSE)
dados.csv.gis <- read.csv("./fapesp/dados/satscan/teste/rmsp_espacial_gini.gis.csv")
dados.csv.gis2 <- dados.csv.gis[-(280:285), ]
write.csv(dados.csv.gis2, file='./fapesp/dados/satscan/teste/rmsp_espacial_gini.gis2.csv', row.names = FALSE)


densi.rmsp <- read.table("./fapesp/dados/satscan/rmsp/densidade_rmsp.csv", header=T,dec=',',sep=';')

rmsp.casos <- read.table("./fapesp/dados/satscan/rmsp/casos_aps_rmsp_2020_2022.csv", header=T,sep = ',')


################
dados.gis <- read.dbf("./fapesp/dados/satscan/teste/rmsp_espaco_tempo.gis.dbf")
write.csv(dados.gis, file='./fapesp/dados/satscan/teste/rmsp_espaco_tempo.gis.csv', row.names = FALSE)
dados.csv.gis <- read.csv("./fapesp/dados/satscan/teste/rmsp_espaco_tempo.gis.csv")






