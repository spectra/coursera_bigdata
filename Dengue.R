# Código adaptado da aula 2.4, que por sua vez foi adaptado
# de https://sociaisemetodos.wordpress.com/2013/09/15/mapas-no-r-parte-2-utilizando-shapes/

library(XML)
library(RCurl)
library(maptools)
library(RColorBrewer)
library("plyr")

unzip("estados_2010.zip")

mapaUF <- readShapePoly("estados_2010.shp")

# Dados de casos de dengue
dengue <- read.csv("Dengue2.csv")
populacao <- read.csv("populacao_2010.csv")
dengue <- merge(dengue, populacao)

# Calcula a quantidade percentual de casos/100 mil habitantes
dengue_per_100k_2010 <- ddply(dengue, .(UF), summarise, c_per_100k_2010 = X2010 / Habitantes * 100000)
dengue_pct_2010 <- ddply(dengue_per_100k_2010, .(UF), summarise, pct_2010 = c_per_100k_2010 / sum(dengue_per_100k_2010$c_per_100k_2010) * 100)
dengue <- merge(dengue, dengue_pct_2010)

# Transforma os dados do percentual de dengue em categorias
dengue$pct_2010_cat <- cut(dengue$pct_2010, breaks=c(0,1,2,3,6,100), labels=c(
  '< 1%', '1-2%', '2-3%', '3-6%', '> 6%'))

# Paleta de cores para as 5 categorias
paletaDeCores <- brewer.pal(5, 'OrRd')

# Cores das Categorias
coresDasCategorias2010 <- data.frame(pct_2010_cat=levels(dengue$pct_2010_cat), CoresDengue2010=paletaDeCores)
dengue <- merge(dengue, coresDasCategorias2010)

# mapaData
mapaData <- attr(mapaUF, 'data')
mapaData$Index = row.names(mapaData)
names(mapaData)[3] = "UF"
mapaData <- merge(mapaData, dengue, by="UF")
attr(mapaUF, 'data') = mapaData

# Configurando tela
parDefault = par(no.readonly = T)
layout(matrix(c(1,2),nrow=2),widths= c(1,1), heights=c(4,1))
par (mar=c(0,0,0,0))

# Plotando mapa 2010
plot(mapaUF, col=as.character(mapaData$CoresDengue2010))
plot(1,1,pch=NA, axes=F)
legend(x='center', legend=rev(levels(dengue$pct_2010_cat)),
       box.lty=0, fill=rev(paletaDeCores),cex=.8, ncol=2,
       title='Percentual de Casos de Dengue / 100 mil habitantes (2010):')
