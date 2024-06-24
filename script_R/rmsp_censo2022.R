pacman::p_load(
  readxl,
  dplyr,
  tidyverse, 
  reshape2,
  janitor,              
  epitools)  

###Limpando o banco de dados 
rmsp_2022 <- read.table("./pop_rmsp/rmsp_censo_2022.csv", 
                   header=T,sep=';',dec = ',', na.strings=c("-"))

####################Organizando o banco de dados censo 2022 por faixa etaria e mes
setwd("/Volumes/DiscoD/fapesp/dados")
rmsp <- read.table("./pop_rmsp/SP_censo2022_FE_Sx.csv", 
                   header=T,sep=';',dec = ',', na.strings=c("-"))
rmsp<-mutate_at(rmsp, c('M_100.anos.ou.mais','F_100.anos.ou.mais'),
                ~replace(.,is.na(.),0))
rmsp <- mutate(rmsp, M_0a9 = M_0.a.4.anos + M_5.a.9.anos,
               M_10a19 = M_10.a.14.anos + M_15.a.19.anos,
               M_20a29 = M_20.a.24.anos + M_25.a.29.anos,
               M_30a39 = M_30.a.34.anos + M_35.a.39.anos,
               M_40a49 = M_40.a.44.anos + M_45.a.49.anos,
               M_50a59 = M_50.a.54.anos + M_55.a.59.anos,
               M_maior60 = M_60.a.64.anos + M_65.a.69.anos+
                 M_70.a.74.anos + M_75.a.79.anos+
                 M_80.a.84.anos + M_85.a.89.anos+
                 M_90.a.94.anos + M_95.a.99.anos+
                 M_100.anos.ou.mais,
               F_0a9 = F_0.a.4.anos + F_5.a.9.anos,
               F_10a19 = F_10.a.14.anos + F_15.a.19.anos,
               F_20a29 = F_20.a.24.anos + F_25.a.29.anos,
               F_30a39 = F_30.a.34.anos + F_35.a.39.anos,
               F_40a49 = F_40.a.44.anos + F_45.a.49.anos,
               F_50a59 = F_50.a.54.anos + F_55.a.59.anos,
               F_maior60 = M_60.a.64.anos + F_65.a.69.anos+
                 F_70.a.74.anos + F_75.a.79.anos+
                 F_80.a.84.anos + F_85.a.89.anos+
                 F_90.a.94.anos + F_95.a.99.anos+
                 F_100.anos.ou.mais) 

rmsp<-rmsp[,-c(3:44)] 
rmsp <- rmsp %>% pivot_longer(col = M_0a9:F_maior60, names_to = 'faixa',values_to = 'populacao')
rmsp <- data.frame(rmsp, colsplit(rmsp$nome,pattern = '\\(|\\)', names=paste('sep',1:2)))
rmsp <- data.frame(rmsp,colsplit(rmsp$faixa, pattern ='_', names=paste('sep',1:2)))

rmsp <- rmsp[,-c(2,3,6)]
colnames (rmsp) <- c ("cidade", "populacao","nome","sexo","faixa") 
rmsp <- rmsp %>% mutate(sexo = recode(sexo, "M" = "MASCULINO","F" = "FEMININO"))
rmsp$cidade <- str_sub(rmsp$cidade, start = 1, end = 6) ###Identificar municip por meio do setor cens
write.table(rmsp,'rmsp_censo_2022.csv', row.names=FALSE, sep=';')
mes_rmsp<-read.csv("/Volumes/DiscoD/fapesp/dados/rmsp_mes.csv", header = T,sep=';')
mes_rmsp$cidade <- as.character(mes_rmsp$cidade)
rmsp_mes_fe_pop<-left_join(mes_rmsp,rmsp, by='cidade')
write.table(rmsp_mes_fe_pop,'rmsp_mes_fe_pop_2022.csv', row.names=FALSE, sep=';')

###Organizar os dados da CDESP
cdesp <-read.table("./raiz/CDESP_PEDIDO28_2020_20230430_SIVEP_GDESP_GERADOEM_20230505.csv",
                   header=T,sep =';')
cdesp$faixa_etaria <- with(cdesp, 
                           cut(x=NU_IDADE_N, breaks = c (0,9, 19,29,39,49,59, Inf),
                               labels=c('0a9','10a19','20a29','30a39',
                                        '40a49','50a59','maior60'),
                               ordered_result = TRUE, right = TRUE)) ###Criar faixa etaria

############INTERNACAO
cdesp <- filter(cdesp, CLASSI_FIN ==5 & EVOLUCAO == 'OBITO') ###Separar interncacoes de covid
cdesp$mes <- str_sub(cdesp$DT_EVOLUCA, start = 1, end = 7) ###criando variavel mes
cdesp <- cdesp[,-c(2:8,10:20,22:26)]
cdesp <- cdesp %>% group_by(faixa_etaria, CS_SEXO, CO_MUN_RES,mes)  %>% tally()  ####Agrupar dados CDESP
colnames (cdesp) <- c ("faixa","sexo", "cidade", "mes","obito") ####renomeando colunas
cdesp$cidade <- as.character(cdesp$cidade)
cdesp$faixa <- as.character(cdesp$faixa)
View(cdesp)

####Juntando tabelas
rmsp <- read.table("./pop_rmsp/rmsp_mes_fe_pop_2022.csv", 
                   header=T,sep=';',dec = ',', na.strings=c("-"))
rmsp$cidade <- as.character(rmsp$cidade)
rmsp$faixa <- as.character(rmsp$faixa)
rmsp_cdesp <- left_join(rmsp,cdesp, by = c("faixa","sexo", "cidade","mes")) 
rmsp_cdesp <- mutate_at(rmsp_cdesp, c("obito"), ~replace(., is.na(.), 0))
padrao <- rmsp %>% group_by(sexo,faixa)  %>% summarise(pop = sum(populacao))
rmsp_cdesp_padrao <- left_join(rmsp_cdesp,padrao, by = c("faixa","sexo"))
View(rmsp_cdesp_padrao)

#################Taxas padronizadas temporal
rmsp_pd_temp <- rmsp_cdesp_padrao %>% group_by(nome,mes) %>%
  summarise(age_adjust = list(ageadjust.direct(count = obito,#count of events
                                               pop = populacao, #person years of DFpop
                                               rate = NULL,                
                                               stdpop = pop,            #standard population (European standard population per age_cat)
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))
rmsp_pd_temp <- mutate (rmsp_pd_temp, txObi_br = crude.rate * 10000,
                        txObi_pd = adj.rate * 10000)
#Exportar tabelas
write.table(rmsp_pd_temp, "rmsp_pd_obi_temp.csv", sep=";",
            row.names = FALSE, dec = ".")

####Grafico de linha
rmsp_temp_graf <- ggplot(data = subset(rmsp_pd_temp, !is.na(mes)), 
                         aes(x = mes, y = tx_pd, group = nome,color = nome)) +  
  geom_line(na.rm = TRUE) +
  labs(x = '', y = 'Data de internação', 
       title = 'Incidencia mensal por 10.000 habitantes') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

rmsp_temp_graf

#################Tabela para datawrapper
rmsp_pd_temp <- rmsp_pd_temp[,-c(3:7)]
rmsp_pd_temp <- rmsp_pd_temp %>% pivot_wider(names_from = mes,
                                             values_from = txObi_pd)
write.table(rmsp_pd_temp , "rmsp_pd_obi_tempwr22.csv", sep=";",
            row.names = FALSE, dec = ".")

#######################################################Analise espacial
rmsp <-read.csv("./pop_rmsp/rmsp_censo_2022.csv", header = T,sep=';')
rmsp$cidade <- as.character(rmsp$cidade)
rmsp$faixa <- as.character(rmsp$faixa)

###Organizar os dados da CDESP
cdesp <-read.table("./raiz/CDESP_PEDIDO28_2020_20230430_SIVEP_GDESP_GERADOEM_20230505.csv",
                   header=T,sep =';')
cdesp$faixa_etaria <- with(cdesp, 
                           cut(x=NU_IDADE_N, breaks = c (0,9, 19,29,39,49,59, Inf),
                               labels=c('0a9','10a19','20a29','30a39',
                                        '40a49','50a59','maior60'),
                               ordered_result = TRUE, right = TRUE)) ###Criar faixa etaria

############INTERNACAO/OBITO
cdesp <- filter(cdesp, CLASSI_FIN ==5) ###Separar interncacoes de covid
cdesp$mes <- str_sub(cdesp$DT_INTERNA, start = 1, end = 7) ###criando variavel mes
#######Filtrar ano
filtro <-
  data.frame(Date = seq(as.Date("2021-03-01"), as.Date("2022-02-28"), by="month"))
filtro$mes <- format(filtro$Date, '%Y-%m')
cdesp <- cdesp %>% filter(mes %in% filtro$mes)
cdesp <- cdesp %>% group_by(faixa_etaria, CS_SEXO, CO_MUN_RES)  %>% tally()  ####Agrupar dados CDESP
colnames (cdesp) <- c ("faixa","sexo", "cidade", "internacao") ####renomeando colunas
cdesp$cidade <- as.character(cdesp$cidade)
cdesp$faixa <- as.character(cdesp$faixa)

###############Juntando bancos de dados
rmsp_cdesp <- left_join(rmsp, cdesp, by=c('cidade','faixa','sexo'))
padrao <- rmsp %>% group_by(sexo,faixa)  %>% summarise(pop = sum(populacao))
rmsp_cdesp <- left_join(rmsp_cdesp, padrao,
                        by=c('faixa','sexo'))
rmsp_cdesp <- mutate_at(rmsp_cdesp, c("internacao"),
                        ~replace(., is.na(.), 0))

####################Taxa padronizada por ubs espacial
rmsp_cdesp_pd <- rmsp_cdesp %>% group_by(cidade) %>%
  summarise(age_adjust = list(ageadjust.direct(count = internacao,  
                                               pop = populacao,          
                                               rate = NULL,                
                                               stdpop = pop,            
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))
rmsp_cdesp_pd <- mutate (rmsp_cdesp_pd, txbr_int21 = crude.rate * 10000,
                         txpd_int21 = adj.rate * 10000)
rmsp_cdesp_pd <- rmsp_cdesp_pd[,-c(2:5)]
write.table(rmsp_cdesp_pd, 'rmsp_int_censo2022_pd21.csv',sep=';',row.names=FALSE, dec = '.')

##Juntando tabela de taxas
rmsp_obito_pd20 <- read.table("/Volumes/DiscoD/fapesp/dados/pop_rmsp/rmsp_obito_espacial_censo2022_pd20.csv",
                              header=T,sep =';')
rmsp_obito_pd21 <- read.table("/Volumes/DiscoD/fapesp/dados/pop_rmsp/rmsp_obito_espacial_censo2022_pd21.csv",
                              header=T,sep =';')
rmsp_ob_cov_espacial_padro_censo2022 <- left_join(rmsp_obito_pd20,rmsp_obito_pd21, by = c("cidade"))
view(rmsp_ob_cov_espacial_padro_censo2022)

write.table(rmsp_ob_cov_espacial_padro_censo2022, 'rmsp_ob_cov_espacial_padro_censo2022.csv',
            sep=';', row.names=FALSE, dec = '.')

#####Grafico de barras
rmsp_bar <- ggplot(rmsp_cdesp_pd, 
                   aes( x = reorder(nome,+txpd_int21),y = txpd_int21, fill=nome)) +
  labs(x = '', y = 'Taxa de internação padronizada (2021)') +
  geom_bar(stat = "identity") + guides(fill=FALSE) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
rmsp_bar

####################################Razao de taxas
##########Internação
TxInt_20 <- read.csv('./pop_rmsp/rmsp_int_espacial_censo2022_pd20.csv',sep = ';')
TxInt_21 <- read.csv('./pop_rmsp/rmsp_int_espacial_censo2022_pd21.csv',sep = ';')

TxOb_20$RtxInt20_21 <- TxInt_21$txpd_int21/ TxInt_20$txpd_int20

#########obito
TxOb_20 <- read.csv('./pop_rmsp/rmsp_obito_espacial_censo2022_pd20.csv',sep = ';')
TxOb_21 <- read.csv('./pop_rmsp/rmsp_obito_espacial_censo2022_pd21.csv',sep = ';')

TxOb_20$RtxObi_20_21 <- TxOb_21$txpd_obi21/ TxOb_20$txpd_obi20

write.table(TxOb_20, 'R_TxOb_20_21.csv', sep=';', row.names = FALSE, dec='.')






