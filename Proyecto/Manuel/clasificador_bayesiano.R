#DEFINIMOS EL DIRECTORIO DE TRABAJO
getwd()
setwd("C:/Users/mgarc/OneDrive/Escritorio/9no Semestre/Bedu_Modulo_II/Proyecto")

#DESCARGAMOS LAS LIBRERIAS NECESRIAS
library(dplyr)
library(scales)
library(ggplot2)

################################################################################
#RESUMEN DEL CONTENIDO:
#A CONTINUACIÓN SE PRESENTA UN ALGORITMO QUE RECIBE COMO PARÁMETROS LA OCUPACIÓN
#DE LA PERSONA QUE ESTÁ REALIZANDO LA LLAMADA, ASÍ COMO EL MOTIVO DE LA LLAMADA
#Y DEVUELVE UNA PREDICCIÓN DEL SEXO DEL USUARIO EN CUESTIÓN.
#UNA VEZ IMPLEMENTADO EL ALGORITMO, TRATAMOS DE EVALUAR EL NIVEL DE PRECISIÓN DE
#NUESTRO CLASIFICADOR MEDIANTE UNA ESTIMACIÓN DE SU PRECISIÓN MEDIA A TRAVÉS DE
#UN INTERVALO DE CONFIANZA A NIVEL 90%.
################################################################################

# DESCARGA DE LA BASE DE DATOS
data           <- read.csv("archivo1.csv")
tabla_mujer    <- filter(data, SEXO == "FEMENINO")
tabla_hombre   <- filter(data, SEXO == "MASCULINO")
n_mujer        <- nrow(tabla_mujer)
n_hombre       <- nrow(tabla_hombre)
n_total        <- nrow(data)

#EN EL RESUMEN SE MENCIONÓ QUE LAS VARIABLES QUE IBAN A SER CONSIDERADAS PARA
#PREDECIR EL SEXO DEL USUARIO ERAN LA OCUPACIÓN Y LA TEMÁTICA. PARA JUSTIFICAR
#ESTA ELECCIÓN BASTA OBSERVAR QUE EXITE UNA GRAN DISPARIDAD DE GÉNERO SI HACERMOS
#REFERENCIA A LA OCUPACIÓN O A LA TEMÁTICA COMO SE MUESTRA A CONTINUACIÓN.
ggplot(data) +
  geom_bar(aes(x = OCUPACION, fill = SEXO), width = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.text=element_text(size=7)) +
  labs(
    title = "Frecuencia de llamadas por ocupación",
    subtitle = "Subdividido por sexo",
    x = "Ocupación",
    y = "n"
  )

ggplot(data) +
  geom_bar(aes(x = TEMATICA_1, color = SEXO), width = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), axis.text=element_text(size=7)) +
  labs(
    title = "Frecuencia de llamadas por temática",
    subtitle = "Subdividido por sexo",
    x = "Temática",
    y = "n"
  )

#SELECCIONAMOS ÚNICAMENTE LOS CAMPOS QUE SERÁN CONSIDERADOS EN EL MODELO PARA
#TRATAR DE PREDECIR EL SEXO DE LA PERSONA QUE REALIZA LA LLAMADA
data                  <- select(data, SEXO, OCUPACION, TEMATICA_1, TEMATICA_2, TEMATICA_3, TEMATICA_4)
data_test             <- data[complete.cases(data),]
data_test             <- data.frame("SEXO" = data_test$SEXO,
                                    "OCUPACION" = data_test$OCUPACION,
                                    "TEMATICA_1" = data_test$TEMATICA_1,
                                    "TEMATICA_2" = data_test$TEMATICA_2,
                                    "TEMATICA_3" = data_test$TEMATICA_3,
                                    "TEMATICA_4" = data_test$TEMATICA_4)
data_test             <- data_test[1:400,]
data_test$PREDICCION_SEXO <- NA

#FUNCIÓN DE PROBABILIDAD CONDICIONAL
#ENTRADAS:
#  campo ==> una columna de la base de datos
#  valor ==> el valor que va a buscar en la columna seleccionada
#SALIDA:
#  Un vector de dos entradas. La primera entrada contiene la proporción
#  de mujeres que en la columna "columna" tienen el valor "valor". La segunda
#  entrada del vector contiene lo mismo pero para hombres.

  proba_condicional <- function(campo,valor){
  tabla_mujer_filtrado  <- filter(data, campo == valor & SEXO == "FEMENINO")
  tabla_hombre_filtrado <- filter(data, campo == valor & SEXO == "MASCULINO")
  resp1 <- nrow(tabla_mujer_filtrado)/n_mujer
  resp2 <- nrow(tabla_hombre_filtrado)/n_hombre
  return(c(resp1,resp2))
}

#FUNCION DE PROBABILIDAD CONJUNTA
#ENTRADAS:
#  lista ==> cada entrada de la lista tiene un string y una columna del
#            dataframe. El string es el valor que va a ser buscado en la
#            columna proporcionada.
#SALIDA:
#  Devuelve el valor "FEMENINO" o el valor "MASCULINO" dependiendo de cuál de
#  ambos sexos es más probable dadas las características proporcionadas en "lista".
  
proba_conjunta <- function(lista){
  iter <- length(lista)
  resp <- list()
  for(i in 1:iter){
    resp <- append(resp,list(proba_condicional(lista[[i]][-1],lista[[i]][1])))
  }
  mult_probas_mujeres  <- n_mujer/n_total
  mult_probas_hombres  <- n_hombre/n_total
  for(i in 1:iter){
    mult_probas_mujeres  <- resp[[i]][1]*mult_probas_mujeres
    mult_probas_hombres  <- resp[[i]][2]*mult_probas_hombres
  }
  if(mult_probas_mujeres>mult_probas_hombres){
    resp_final = "FEMENINO"
  }else{
    resp_final = "MASCULINO"
  }
  return(resp_final)
}

#APLICAMOS LA FUNCIÓN proba_conjunta A CADA UNO DE LOS REGISTROS DE LA BASE DE
#DATOS LLAMADA data_test.
for (i in 1:nrow(data_test)) {
  renglon  <- list(c(data_test[i,2],data[,2]), c(data_test[i,3],data[,3]),
                   c(data_test[i,4],data[,4]), c(data_test[i,5],data[,5]),
                   c(data_test[i,6],data[,6]))
  data_test[i,7] <- proba_conjunta(renglon)
}

#AGREGAMOS LA COLUMNA EXITO A NUESTRA TABLA. VALE 1 SI EL ALGORITMO CLASIFICÓ
#CORRECTAMENTE Y 0 SI NO.
data_test$EXITO <- NA
for (i in 1:nrow(data_test)) {
  if(data_test$SEXO[i] == data_test$PREDICCION_SEXO[i]){
    data_test[i,8] <- 1
  }else{
    data_test[i,8] <- 0
  }
}

#TOMAMOS GRUPOS DE 10 REGISTORS DE LA TABLA data_test Y CALCULAMOS LA PROPORCION
#QUE FUE CLASIFICADA CORRECTAMENTE Y ALMACENAMOS EL VALOR OBTENIDO EN UN VECTOR. 
#REPETIMOS ESTO 40 VECES HASTA TERMINAR CON LOS 400
#REGISTROS PARA OBTENER 40 OBSERVACIONES DE NUESTRA VARIABLE ALEATORIA.

muestra <- rep(0,40) #vector que almacena las observaciones de la v.a
for (i in seq(1,410,10)) {
  contador <- 0
  for (j in i:(i+9)) {
    contador <- contador + data_test$EXITO[j]
  }
  muestra[i/10] <- contador/10
}

#YA QUE TENEMOS 40 OBSERVACIONES DE NUESTRA VARIABLE ALEATORIA, PODEMOS CREAR UN
#INTERVALO DE CONFIANZA A NIVEL 90% PARA LA MEDIA.

sigma_gorro <- sd(muestra)
mu_gorro    <- mean(muestra)
n           <- length(muestra)

#Dado que se utilizó una estimación de la varianza y no
#la varianza real, entonces nuestra variable pivotal tiene
#una distribución t-student con n-1 grados de libertad.

a           <- qt(.05,39)  
b           <- qt(.95,39)
conf_int    <- c(0,0)
conf_int[1] <- mu_gorro - (b*sigma_gorro/sqrt(n))
conf_int[2] <- mu_gorro - (a*sigma_gorro/sqrt(n))

#Entonces nuestro intervalo de confianza a nivel 90% para la verdadera precisión
#de nuestro clasificador bayesiano está dado por:

print(conf_int)
