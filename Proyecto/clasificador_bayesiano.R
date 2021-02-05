#DEFINIMOS EL DIRECTORIO DE TRABAJO
getwd()
setwd("C:/Users/mgarc/OneDrive/Escritorio/9no Semestre/Bedu_Modulo_II/Proyecto")
#DESCARGAMOS LAS LIBRERIAS NECESRIAS
library(dplyr)
library(scales)
library(ggplot2)
# DESCARGA DE LA BASE DDE DATOS
data     <- read.csv("archivo1.csv")
tabla_mujer    <- filter(data, SEXO == "FEMENINO")
tabla_hombre   <- filter(data, SEXO == "MASCULINO")
n_mujer  <- nrow(tabla_mujer)
n_hombre <- nrow(tabla_hombre)
#FUNCIÓN DE PROBABILIDAD CONDICIONAL
proba_condicional <- function(campo,valor){
  #PRoba condicional mujeres
  tabla_mujer_filtrado  <- filter(data, campo == valor & SEXO == "FEMENINO")
  tabla_hombre_filtrado <- filter(data, campo == valor & SEXO == "MASCULINO")
  resp1 <- nrow(tabla_mujer_filtrado)/n_mujer
  resp2 <- nrow(tabla_hombre_filtrado)/n_hombre
  return(c(resp1,resp2))
}

#FUNCION DE PROBABILIDAD CONJUNTA
proba_conjunta <- function(lista){
  iter <- length(lista)
  resp <- list()
  for(i in 1:iter){
    resp <- append(resp,list(proba_condicional(lista[[i]],names(lista[i]))))
  }
  mult_probas_mujeres <- resp[[1]][1]
  mult_probas_hombre  <- resp[[1]][2]
  for(i in 2:iter){
    mult_probas_mujeres <- resp[[i]][1]*mult_probas_mujeres
    mult_probas_hombre  <- resp[[i]][2]*mult_probas_hombre
  }
  
  if(mult_probas_mujeres>mult_probas_hombre){
    resp_final = "FEMENINO"
  }else{
    resp_final = "MASCULINO"
  }
  return(resp_final)
}

#APLICAMOS LA FUNCIÓN A CADA UNO DE LOS REGISTROS DE LA TABLA
data_test <- data[,-c(1,2,3,4,5,6,8,9,12,13,14,15,16,17,18,19,20,23,24,25,26,27,28)]
data_test <- data_test[complete.cases(data_test),]
data_test$PREDICCION_SEXO <- NA

for (i in data_test) {
  aux <-i[[1]]
  aux2<-i[[2]]
  lista <- list(aux=data$SEXO,aux2=data$OCUPACION)
 print( names(lista[1]))
}
View(lista)

###############################################################################
for (i in nrow(data_test)) {
  renglon  <- list(data_test[i,2] = data_test$OCUPACION,
                  data_test[i,3] = data_test$ESCOLARIDAD,
                  data_test[i,4] = data_test$SERVICIO,
                  data_test[i,5] = data_test$TEMATICA_1)
  data_test[i,6] <- proba_conjunta(lista = renglon)
}

