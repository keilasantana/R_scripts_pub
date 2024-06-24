###Carregando as bibliotecas
pacman::p_load(dplyr,reshape2,tidyverse,janitor, epitools) 


####Juntando bancos de dados
diadema <- read.csv('./Diadema/Diadema_2010_indicadores.csv', header = T)


####Pop setor censitario de Diadema por sexo e por faixa etaria
diadema <- read.csv('/Volumes/DiscoD/fapesp/dados/Pessoa03_SP.csv', sep = ';', header = T,na.strings=c("X"))
diadema$cidade <- str_sub(diadema$Cod_setor, start = 1, end = 7)
diadema <- filter(diadema, cidade == 3513801)
diadema <- diadema %>% rowwise() %>% 
  mutate(M_05a09 = sum(c_across(V087:V096)),
         M_10a19 = sum(c_across(V097:V106)),
         M_20a29 = sum(c_across(V117:V126)),
         M_30a39 = sum(c_across(V127:V136)),
         M_40a49 = sum(c_across(V137:V146)),
         M_50a59 = sum(c_across(V147:V156)),
         M_maior60 = sum(c_across(V157:V166)),
         F_05a09 = sum(c_across(V167:V176)),
         F_10a19 = sum(c_across(V177:V186)),
         F_20a29 = sum(c_across(V197:V206)),
         F_30a39 = sum(c_across(V207:V216)),
         F_40a49 = sum(c_across(V217:V226)),
         F_50a59 = sum(c_across(V227:V236)),
         F_maior60 = sum(c_across(V237:V246)))
diadema <- diadema[,-c(2:254)]###excluindo colunas com ambos os sexos, alfabetizacao e demais faixas etarias 

diadema0_4 <- read.csv('/Volumes/DiscoD/fapesp/dados/Pessoa05_SP.csv', sep = ';', 
                       header = T,na.strings=c("X"))
diadema0_4$cidade <- str_sub(diadema0_4$Cod_setor, start = 1, end = 7)
diadema0_4 <- filter(diadema0_4, cidade == 3513801)
diadema0_4 <- diadema0_4 %>% rowwise() %>% 
  mutate(M_0a4 = sum(c_across(V001:V005)),
         F_0a4 = sum(c_across(V006:V010)))
diadema0_4 <- diadema0_4[,-c(2:13)]

diadema1 <-  diadema%>% left_join(diadema0_4, by = c("Cod_setor")) ###Juntando df

diadema <- mutate(diadema1, M_0a9 = M_0a4+M_05a09,F_0a9 = F_0a4,F_05a09)
diadema <- diadema[,-c(2,9,16,17)]
diadema <- diadema %>% pivot_longer( cols = 'M_10a19':'F_0a9', 
                                     names_to = "Faixa", values_to = "pop")

diadema <- data.frame(diadema, colsplit(diadema$Faixa, pattern="_", 
                                        names=paste("sep", 1:2)))
diadema <- diadema[,c(-2)]
colnames (diadema) <- c ("codigo", "populacao","sexo","faixa") 
diadema <- diadema %>% mutate(sexo = recode(sexo, "M" = "MASCULINO","F" = "FEMININO"))
diadema <- diadema %>% mutate(faixa = recode(faixa, '0a9'='0 a 9','10a19'='10 a 19', '20a29'='20 a 29',
                                         '30a39'='30 a 39', '40a49'='40 a 49', '50a59'='50 a 59',
                                         'maior60'= '60 ou mais'))
diadema$codigo<-as.character(diadema$codigo)
view(diadema)
write.table(diadema, "pop_diadema_FE_SC.csv", sep=";", row.names = FALSE)

########Populacao por AA, sexo e faixa etaria
pop_dia <- read.csv("/Volumes/DiscoD/fapesp/dados/pop_diadema_FE_SC.csv",
                    sep = ";",header=T)
pop_dia$codigo<-as.character(pop_dia$codigo)
pop_dia <- mutate_at(pop_dia, c("populacao"), ~replace(., is.na(.), 0))
view(pop_dia)

ubs <- read.table("/Volumes/DiscoD/fapesp/dados/corresp_sc_ubs.csv",sep = ',',
                  header=T)
ubs$CD_GEOCODI <-as.character(ubs$CD_GEOCODI)
colnames (ubs) <- c ("codigo", "nome","UBS") 
pop_dia_FE_ubs <- left_join(pop_dia, ubs, by=c('codigo'))

pop_dia_FE_ubs <- pop_dia_FE_ubs %>% group_by(nome,faixa,sexo) %>%
  summarise(populacao = sum(populacao))
write.table(pop_dia_FE_ubs, "pop_dia_FE_ubs.csv", sep=";", row.names = FALSE)

###################Populacao por area da UBS
diadema <- read.csv('/Volumes/DiscoD/fapesp/dados/Pessoa03_SP.csv', sep = ';', 
                    header = T,na.strings=c("X"))
diadema <- diadema[,-c(4:254)]
diadema$cidade <- str_sub(diadema$Cod_setor, start = 1, end = 7)
diadema <- filter(diadema, cidade == 3513801)
diadema$Cod_setor <- as.character(diadema$Cod_setor)
diadema <- diadema[,-c(2,4)]
colnames (diadema) <- c ("codigo","populacao")

popubsdia <- left_join(diadema, ubs, by=c('codigo'))
popubsdia <- popubsdia %>% group_by(nome) %>%
  summarise(populacao = sum(populacao))
view(popubsdia)

write.table(popubsdia, "popubsdia.csv", sep=";", row.names = FALSE, dec = ".")

### Notidicacao de covid em 2020 a 2022 por sc
cov_int20 <- read.csv("/Volumes/DiscoD/fapesp/dados/20221122_Dados_Diadema_NotifTransf_LC.csv",
                      sep = ";",header=T)
view(cov_int20)
cov_int20 <- cov_int20[,-c(1,5:7,9,11:17,20:26,30:33,35:37)]
cov_int20 <- cov_int20[,-c(3:6,8,9,11,13)]
cov_int20$CD_SETOR_2010 <- as.character(cov_int20$CD_SETOR_2010)
cov_int20 <- cov_int20 %>% mutate(FX.ETÁRIA = recode(FX.ETÁRIA, '0 a 4' = '0 a 9',
                                                     '05 a 09' = '0 a 9',
                                                     '60 a 69' = '60 ou mais',
                                                     '70 a 79' = '60 ou mais',
                                                     '80 +' = '60 ou mais'))
cov_int20 <- cov_int20 %>% mutate(GÊNERO = recode(GÊNERO, "M" = "MASCULINO","F" = "FEMININO"))
write.table(cov_int20, "diadema_covid_2020_2022.csv", sep=";",
            row.names = FALSE, dec = ".")

## Cria uma nova coluna para o CV
rmsp_mun$mes <- filtro
## A estrutura de repetição fica
for(i in rmsp_mun){
  print('i'+i)
  for (j in filtro){
    print('i'+i)
    print('j'+j)
  }
  
}
filtro <-
  data.frame(Date = seq(as.Date("2020-03-01"), as.Date("2022-04-30"), by="day"))
filtro$MES <- format(filtro$Date, "%Y-%m")
filtro <- filtro %>% group_by(MES) %>% tally()
filtro <- filtro[,-c(2)]
View(rmsp_mun)
rmsp_mun <- rmsp %>% group_by(cidade) %>% tally()
rmsp_mun <- rmsp_mun[,-c(2)]