#   Paso 1
#   Importa los datos de soccer de la temporada 2019/2020 de la primera división de la liga española
#   https://www.football-data.co.uk/mmz4281/1920/SP1.csv

#   Primer opción: descargar el archivo directamente
#Para importar los datos
#Obtenemos el working directory
getwd()
#Seleccionamos el directorio de trabajo
setwd("C:/Users/mgarc/OneDrive/Escritorio/9no Semestre/Bedu_Modulo_II/Sesion_1/")
misdatos <- read.csv("SP1.csv")

#   Segunda opción: Leer el archivo desde internet
misdatos <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

#   Paso 2
#   extrae las columnas que contienen los números de goles anotados por los 
#   equipos que jugaron en casa (FTHG) y los goles anotados por los equipos 
#   que jugaron como visitante (FTAG)

#Extraemos las columnas FTHG y FTAG
goles_casa      <- misdatos$FTHG
goles_visitante <- misdatos$FTAG

#   Paso 3
#   Se elabora un primer acercamiento para calcular las probabilidades
#   marginales y conjuntas. Ver más adelante otra manera de hacer este 
#   procedimiento de una manera más sencilla

#Procedimiento 1

#Elaboramos las tablas de frecuancia
table(goles_casa)
table(goles_visitante)
table(goles_casa,goles_visitante)
#Función que cuenta la cantidad de veces que un valor se encuentra en un arreglo

cuenta <- function(valor, vector){
  contador <- 0
  for(i in seq(from = 1, to = length(vector), by =1)){
    if(valor == vector[i]){
      contador <- contador + 1
    }
  }
  return(contador)
}
#Para estimar la priobabilidad de que un equipo anote x cantidad de goles
estima_proba <- function(goles, vector){
  total <- length(vector)
  goles <- cuenta(goles,vector)
  resp  <- goles/total
  return(resp)
}
#Llamamos a la funcion estima_proba con los valores de goles_casa

#Estimaci?n puntual de la proba de meter exactamente 3 goles jugando en casa
estima_proba(3,goles_casa)

#Estimaci?n puntial de la proba de meter exactamente 3 goles jugando de visitante
estima_proba(3,goles_visitante)

#Bajo el supuesto de que los goles de casa y los goles de visita son variables aleatorias independientes
#podemos calcular la probabilidad conjunta mediante la multiplicaci?n de las probabilidades marginales

#Si, por ejemplo, deseamos calcular la probabilidad de que el marcador sea 2-1 enonces hacemos el siguiente calculo
estima_proba(2,goles_casa)*estima_proba(1,goles_visitante)

#   Paso 3
#   Procedimiento 2

home_margin <- table(goles_casa)/nrow(misdatos)
away_margin <- table(goles_visitante)/nrow(misdatos)
prob_conj <- table(goles_casa,goles_visitante)/nrow(misdatos)

