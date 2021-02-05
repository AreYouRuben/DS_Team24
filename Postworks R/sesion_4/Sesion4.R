
####################################################################
###
###             Código sesión 2
###
####################################################################
#Librerias a utilizar
library(dplyr)

#Cargar datos
data2017 <-read.csv('https://www.football-data.co.uk/mmz4281/1718/SP1.csv')
data2018 <-read.csv('https://www.football-data.co.uk/mmz4281/1819/SP1.csv')
data2019 <-read.csv('https://www.football-data.co.uk/mmz4281/1920/SP1.csv')
lista_temporadas <- list(temporada1=data2017,temporada2=data2018,temporada3=data2019)
lista_temporadas <-lapply(lista_temporadas, select,Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

# Código generado desde la sesión 2       
lista_temporadas <-lapply(lista_temporadas,mutate, Date = as.Date(Date, "%d/%m/%Y"))
# nos aseguramos que Date sea de tipo Date al verificar una de las listas
class(lista_temporadas[[1]]$Date)

# Las siguientes son dos maneras diferentes de utilizar rbind:
# una listando cada elemento
paseFinal <-rbind(lista_temporadas[[1]],lista_temporadas[[2]],lista_temporadas[[3]])
View(paseFinal)       
#la segunda utilizando la funcion do.call
paseFinal2 <- do.call(rbind, lista_temporadas)

####################################################################
###
###             Código sesión 3
###
####################################################################

#Paso 1
goles_casa      <- paseFinal2$FTHG
goles_visitante <- paseFinal2$FTAG

table(goles_casa)
table(goles_visitante)
x<-table(goles_casa,goles_visitante)

#Funci?n que cuenta la cantidad de veces que un valor se encuentra en un arreglo
home_margin <- table(goles_casa)/nrow(paseFinal2)
away_margin <- table(goles_visitante)/nrow(paseFinal2)
matriz_conj <- x/nrow(paseFinal2)

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


####################################################################
###
###             Código sesión 4
###
####################################################################

# Paso 1. Obtener probabilidades conjuntas
matriz_dependencia <- matriz_conj/outer(home_margin, away_margin, "*")
matriz_dependencia

# Paso 2. Actividad de boostrap
#mean(sample(x=matriz_dependencia,size = 4,replace = TRUE))
# Intento 1
sampleMean  <- rep(0,40)
sampleVar   <- rep(0,40) 
for (i in 1:40) {
  aux <- sample(x=matriz_dependencia,size = 4,replace = TRUE)
  sampleMean[i] <- mean(aux)
  sampleVar[i]  <- var(aux)
}

sampleMean
sampleVar
var(sampleMean) 
mean(sampleVar)

# Intento 2
estimacionVar <-var(as.vector(matriz_dependencia))
a <- qnorm(mean = 1, sd = sqrt(estimacionVar), p = 0.475)
b <- qnorm(mean = 1, sd = sqrt(estimacionVar), p = 0.525)
regionRechazo <- c(a,b)
for(i in 1:9){
  for(j in 1:7){
    if(matriz_dependencia[i,j] > a & matriz_dependencia[i,j] < b){
      matriz_dependencia[i,j] <- "TRUE"
    }else{
      
      matriz_dependencia[i,j] <- "FALSE"
    }
  }
}
matriz_dependencia

# Intento 3
# Técnica de remuestreo
for(i in 1:100){
  
  # Primero seleccionamos de manera aleatoria algunas filas de nuestro data frame original (todos los datos)
  indices <- sample(dim(paseFinal)[1], size = 300, replace = TRUE)
  newdata <- data[indices, ]
  
  # Repetimos el paso de las estimaciones de probabilidades
  # Probabilidades marginales estimadas para los equipos que juegan en casa
  proba_casa <- round(table(newdata$FTHG)/dim(newdata)[1], 3)
  # Probabilidades marginales estimadas para los equipos que juegan como visitante
  proba_visita <- round(table(newdata$FTAG)/dim(newdata)[1], 3)
  # Probabilidades conjuntas estimadas
  pcta <- round(table(newdata$FTHG, newdata$FTAG)/dim(newdata)[1], 3)
  #Obtenemos nuevamente los cocientes de probabilidades conjuntas entre probabilidades marginales
  print(cocientes <- pcta/outer(proba_casa, proba_visita, "*")) 
}
# Con el intento 3, vemos que en muchos casos se obtienen valores iguales a cero a una mayora cantidad de goles. 
# También vemos que a una menor cantidad de goles (aproximadamente menor a 4), los valores están cercanos a 1, viendo que en realidad la tabla se llena más a favor del equipo local

#   Definimos la hipotesis nula como el escenario donde son dependientes 
#   La hipotesis alternativa es que son dependientes
#   El nivel de significancia fue del 95%
#   y la region de rechazo alrededor de la media

#   A mayor numero de goles de un equipo influye mas
#   en la cantidad de goles a anotar de el equipó rival (recordando hasta 4 goles)

# Esto se puede interpretar que el rival se ve motivado a anotar cuando el equipo local comienza a anotar goles


