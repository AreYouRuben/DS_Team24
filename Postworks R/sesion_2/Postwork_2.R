#librerias
library(dplyr)

#paso 1
data2017 <-read.csv('https://www.football-data.co.uk/mmz4281/1718/SP1.csv')
data2018 <-read.csv('https://www.football-data.co.uk/mmz4281/1819/SP1.csv')
data2019 <-read.csv('https://www.football-data.co.uk/mmz4281/1920/SP1.csv')

lista_temporadas <- list(temporada1=data2017,temporada2=data2018,temporada3=data2019)

#paso 2
lapply(lista_temporadas,View)
       
lapply(lista_temporadas[[1]],head)
       
       
lapply(lista_temporadas, str)
       
lapply(lista_temporadas, summary)
#paso 3
       
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

# Guardamos los datos para el postwork 3
write.csv(paseFinal2, 'GolesAwayHome1720.csv')
