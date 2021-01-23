#   librerias
library(dplyr)

#   paso 1
#   Leer datos generados en el postwork 2

data1720 <-read.csv('GolesAwayHome1720.csv')

goles_casa      <- data1720$FTHG
goles_visitante <- data1720$FTAG

table(goles_casa)
table(goles_visitante)
table(goles_casa,goles_visitante)

#   Obtener porbabilidades de goles casa y visitante
home_margin <- table(goles_casa)/nrow(data1720)
away_margin <- table(goles_visitante)/nrow(data1720)
matriz_conj <- table(goles_casa,goles_visitante)/nrow(data1720)

#   Paso 2
#   Obtener gráficos de las probabilidades

#   Creación de dataFrames para su uso con ggplot
dfHOME <- data.frame(home_margin)
dfAway <- data.frame(away_margin)
dfConjunta <- data.frame(matriz_conj)

library(ggplot2)

#   Plot probabilidades marginales goles casa
pHome<-ggplot(data=dfHOME, aes(x=goles_casa, y=Freq)) +
  geom_bar(stat="identity")
pHome
#   Plot probabilidades marginales goles visitante
pAway <-ggplot(data=dfAway, aes(x=goles_visitante, y=Freq)) +
  geom_bar(stat="identity")
pAway

#   Plot probabilidades conjuntas goles casa y visitante
pConjunta <- ggplot(dfConjunta, aes(x = goles_casa, y = goles_visitante, fill = Freq)) + 
    geom_tile()
pConjunta

