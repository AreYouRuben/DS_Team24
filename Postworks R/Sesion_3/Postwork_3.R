#librerias
library(dplyr)

#paso 1
data2017 <-read.csv('https://www.football-data.co.uk/mmz4281/1718/SP1.csv')
data2018 <-read.csv('https://www.football-data.co.uk/mmz4281/1819/SP1.csv')
data2019 <-read.csv('https://www.football-data.co.uk/mmz4281/1920/SP1.csv')

lista_temporadas <- list(temporada1=data2017,temporada2=data2018,temporada3=data2019)

lista_temporadas <-lapply(lista_temporadas, select,Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

#paso 4       
lista_temporadas <-lapply(lista_temporadas,mutate, Date = as.Date(Date, "%d/%m/%Y"))
# nos aseguramos que Date sea de tipo Date al verificar una de las listas
class(lista_temporadas[[1]]$Date)

#las siguientes son dos maneras diferentes de utilizar rbind
# una listando cada elemento
paseFinal <-rbind(lista_temporadas[[1]],lista_temporadas[[2]],lista_temporadas[[3]])
View(paseFinal)       
#la segunda utilizando la funcion do.call
paseFinal2 <- do.call(rbind, lista_temporadas)


####################################################################

goles_casa      <- paseFinal2$FTHG
goles_visitante <- paseFinal2$FTAG

table(goles_casa)
table(goles_visitante)
x<-table(goles_casa,goles_visitante)
#Funci?n que cuenta la cantidad de veces que un valor se encuentra en un arreglo

home_margin <- table(goles_casa)/nrow(paseFinal2)
away_margin <- table(goles_visitante)/nrow(paseFinal2)
matriz_conj <- x/nrow(paseFinal2)

#################################################################

dfHOME <- data.frame(home_margin)
dfAway <- data.frame(away_margin)

library(ggplot2)
# Basic barplot
pHome<-ggplot(data=dfHOME, aes(x=goles_casa, y=Freq)) +
  geom_bar(stat="identity")
pHome

pAway <-ggplot(data=dfAway, aes(x=goles_visitante, y=Freq)) +
  geom_bar(stat="identity")
pAway

dfConjunta <- data.frame(matriz_conj)
ggplot(dfConjunta, aes(x = goles_casa, y = goles_visitante, fill = Freq)) + 
    geom_tile()


