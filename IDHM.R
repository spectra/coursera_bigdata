# Código adaptado da aula 2.4, que por sua vez foi adaptado
# de https://sociaisemetodos.wordpress.com/2013/09/15/mapas-no-r-parte-2-utilizando-shapes/

library(XML)
library(RCurl)
library(maptools)
library(RColorBrewer)
library("plyr")

unzip("estados_2010.zip")

mapaUF <- readShapePoly("estados_2010.shp")

# Dados do IDHM e da Renda per Capita dos Estados
# dos censos de 1991, 2000 e 2010
estados <- read.csv("AtlasBrasil_Consulta2.csv", sep=";")

# Transforma os dados do IDHM em variáveis categóricas
estados$IDHM_cat_1991 <- cut(estados$IDHM..1991., breaks=c(0,0.65,0.67,0.7,0.75,1), labels=c(
  '0.600~0.650', '0.660~0.670', '0.680~0.700', '0.710~0.750', '> 0.750'))
estados$IDHM_cat_2000 <- cut(estados$IDHM..2000., breaks=c(0,0.65,0.67,0.7,0.75,1), labels=c(
  '0.600~0.650', '0.660~0.670', '0.680~0.700', '0.710~0.750', '> 0.750'))
estados$IDHM_cat_2010 <- cut(estados$IDHM..2010., breaks=c(0,0.65,0.67,0.7,0.75,1), labels=c(
  '0.600~0.650', '0.660~0.670', '0.680~0.700', '0.710~0.750', '> 0.750'))

# Paleta de cores para as 5 categorias
paletaDeCores <- brewer.pal(5, 'OrRd')
paletaDeCores <- rev(paletaDeCores)

# 1991
coresDasCategorias1991 <- data.frame(IDHM_cat_1991=levels(estados$IDHM_cat_1991), Cores1991=paletaDeCores)
estados <- merge(estados, coresDasCategorias1991)

# 2000
coresDasCategorias2000 <- data.frame(IDHM_cat_2000=levels(estados$IDHM_cat_2000), Cores2000=paletaDeCores)
estados <- merge(estados, coresDasCategorias2000)

# 2010
coresDasCategorias2010 <- data.frame(IDHM_cat_2010=levels(estados$IDHM_cat_2010), Cores2010=paletaDeCores)
estados <- merge(estados, coresDasCategorias2010)

# mapaData
mapaData <- attr(mapaUF, 'data')
mapaData$Index = row.names(mapaData)
names(mapaData)[3] = "Lugar"
mapaData <- merge(mapaData, estados, by="Lugar")
attr(mapaUF, 'data') = mapaData

# Configurando tela
parDefault = par(no.readonly = T)
layout(matrix(c(1,2),nrow=2),widths= c(1,1), heights=c(4,1))
par (mar=c(0,0,0,0))

# Plotando mapa 1991
plot(mapaUF, col=as.character(mapaData$Cores1991))
plot(1,1,pch=NA, axes=F)
legend(x='center', legend=rev(levels(estados$IDHM_cat_1991)),
       box.lty=0, fill=rev(paletaDeCores),cex=.8, ncol=2,
       title='IDHM (1991):')

# Plotando mapa 2000
plot(mapaUF, col=as.character(mapaData$Cores2000))
plot(1,1,pch=NA, axes=F)
legend(x='center', legend=rev(levels(estados$IDHM_cat_2000)),
       box.lty=0, fill=rev(paletaDeCores),cex=.8, ncol=2,
       title='IDHM (2000):')

# Plotando mapa 2010
plot(mapaUF, col=as.character(mapaData$Cores2010))
plot(1,1,pch=NA, axes=F)
legend(x='center', legend=rev(levels(estados$IDHM_cat_2010)),
       box.lty=0, fill=rev(paletaDeCores),cex=.8, ncol=2,
       title='IDHM (2010):')
