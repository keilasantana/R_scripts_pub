##########Taxas baysianas padronizadas por sexo e idade
#####Carregando pacotes
library(maptools)
library (spdep)
library(sf)

#####################Taxa padronizada

rmsp_tx1 <- read.table("./pop_rmsp/rmsp_int_espacial_censo2022_pd20.csv",
                              header=T,sep =';')
rmsp_tx2 <- read.table("./pop_rmsp/rmsp_int_espacial_censo2022_pd21.csv",
                      header=T,sep =';')
rmsp_tx3 <- read.table("./pop_rmsp/rmsp_ob_cov_espacial_padro_censo2022.csv",
                      header=T,sep =';')


# taxa bruta
mama$tx_br <- (mama$obt/mama$p_tot)*100000
head(mama)

### 4 - Taxa bayesiana empirica 
## 4.1 Global
mama <- st_read("./bd_aula5/munic_esp_ob_pop_2010.shp") # lendo mapa
plot(st_geometry(mama))





## 1 - Matriz de vizinhanca de um shape de poligonos
rmsp.map <- st_read("./Shape/rmsp.shp")
plot(st_geometry(rmsp.map))

###Matriz de contiguidade
pol.gal.q <- poly2nb(as(rmsp.map, "Spatial"),queen=TRUE)

# obtendo o mapa de centroides
pol.centr <- st_geometry(st_centroid(rmsp.map))
plot(st_geometry(pol.centr),pch=3, col='red', add=TRUE)

# obtendo a matriz com as coord dos centroides
pol.coord <- st_coordinates(pol.centr)
class(pol.coord)

# plotando matriz de contig queen
plot(pol.gal.q, pol.coord, add=TRUE)

# matriz de pesos
pol.gal.q.pes <- nb2listw(pol.gal.q)
print(pol.gal.q.pes)
str(pol.gal.q.pes)

# Comandos do pacote 'spdep'
#localmoran(x, listw, zero.policy=NULL, na.action=na.fail,
#           alternative = "greater", p.adjust.method="none", mlvar=TRUE,
#           spChk=NULL)
??localmoran
rmsp.moran.local <- localmoran(police.map$CRIME,
                              listw = pol.gal.q.pes,alternative="two.sided")
class(rmsp.moran.local)
rmsp.moran.local
head(rmsp.moran.local)

# juntando o objeto 'sf' com a matriz

police.moran.local <- cbind(police.map,pol.moran.local)
class(police.moran.local)
str(police.moran.local)

# criando a  coluna 'moran.local' e dizendo q seus valores sao nulos


police.moran.local$moran.local <- 0

# classificando o moran local segundo as categorias 1 (alto-alto); 2 (baixo-baixo); 4 (baixo-alto) e 3 (alto-baixo)

str(police.moran.local)

# m?dia da vari?vel CRIME

mean(police.moran.local$CRIME) # 194.5

### poder?amos usar tb os valores da coluna Z.Ii q rerpresenta a m?dia dos vizinhos

# atribuindo os valores corretos

head(police.moran.local) ## para copiar nome da coluna 'Pr.z....E.Ii..'
police.moran.local$moran.local <- ifelse(police.moran.local$Pr.z....E.Ii..>=0.05 , 0,
                                         ifelse(police.moran.local$CRIME>194.5 & police.moran.local$Ii > 0, 1,
                                                ifelse (police.moran.local$CRIME<194.5 & police.moran.local$Ii>0,2,
                                                        ifelse(police.moran.local$CRIME<194.5 & police.moran.local$Ii<0,4,3))))

# mostrando o resultado
str(police.moran.local)
police.moran.local$moran.local

print(police.moran.local[c(16,22,25,26,28)],n=82)

# mapeando

plot(police.moran.local["moran.local"])

# exportando o shape

st_write(police.moran.local,"./bd_aula5/police.moran.local.shp")

### Moran plot

# moran.plot(x, listw, zero.policy=NULL, spChk=NULL, labels=NULL,
#           xlab=NULL, ylab=NULL, quiet=NULL, ...)

moran.plot(police.map$CRIME,listw = pol.gal.q.pes)
































