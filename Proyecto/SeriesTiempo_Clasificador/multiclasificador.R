########################
###
###   Multiclasificador usando redes neuronales (deep learning)
###
###   Fuente de datos:
### https://datos.cdmx.gob.mx/dataset/servicios-para-la-poblacion-en-general/resource/59af003e-042e-4aeb-b4f0-8ca9a6600ec4
###
###   Fecha: 7/feb/2021
###   Se considera que la sesion ha sido establecida previamente, en donde encontraremos el archivo CSV necesario
########################

# Librerias del proyecto
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(neuralnet)
library(nnet)
library(rccmisc)

# Lectura de datos. Hacemos una exploracion rapida de los datos
datos <- read.csv("./servicios-para-la-poblacion-en-general.csv")
head(datos)
summary(datos)
# Transformamos las columnas del data frame a minusculas
datos <- lownames(datos)
names(datos)

# Limpieza de datos
# Valores "" a NA
datos <- datos %>% mutate_if(is.character, list(~na_if(.,""))) 
# Data set sin NA
datos_limpios <- datos[complete.cases(datos[, 4:22]), ]
# Comprobamos obteniendo niveles sin "", NA
# Estos servicios son los que queremos clasificar a partir de nuestra red
levels(factor(datos_limpios$servicio))

# Establecer datos de entrenamiento y prueba
# Nos enfocamos en los servicios registrado, los cuales son 3 categorias (Juridico, medico, psicologico)
# Al ser multiclasificacion, estableces un vectorde la forma [l1 l2 l3] para representar nuestras categorias
# Del vector anterior, solo una posicion puede tener valor 1, mientras que las otras 0
train <- cbind(datos_limpios, class.ind(as.factor(datos_limpios$servicio)))
# Establecemos etiquetas que representan las 3 posiciones de nuestro vector
# Por lo tanto:
# Juridico = [1 0 0], Medico = [0 1 0], Psicologico = [0 0 1]
names(train) <- c(names(datos_limpios),"l1","l2","l3")
head(train)

# Separamos los datos por tipo de etiqueta
datos_jur <- train[train$l1 == 1,]
datos_med <- train[train$l2 == 1,]
datos_psi <- train[train$l3 == 1,]
# Establecemos un minimo de datos para formar nuestros conjuntos. Esto para evitar sesgos ya que el servicio juridico tiene mayor datos en comparacion a los demas
minimo_datos <- 1200
indice <- ceiling(minimo_datos * .8)

# Establecemos vectores para escoger datos de manera aleatoria
rows_jur <- sample(nrow(datos_jur))
rows_med <- sample(nrow(datos_med))
rows_psi <- sample(nrow(datos_psi))

# Generamos los conjuntos de entrenamiento y prueba
datos_train <- bind_rows(datos_jur[rows_jur[1:indice-1],], datos_med[rows_med[1:indice-1],], datos_psi[rows_psi[1:indice-1],])
names(datos_train)
datos_test <- bind_rows(datos_jur[rows_jur[indice:minimo_datos],], datos_med[rows_med[indice:minimo_datos],], datos_psi[rows_psi[indice:minimo_datos],])
names(datos_test)

# Generamos factores para valores categoricos. Aplicamos un proceso para generar escalas
# Seleccionamos las columnas que seran utilizadas como datos de entrada en el modelo
columnas_a_factor <- c("hora_alta","sexo","estado_civil","ocupacion","escolaridad", "estado_usuaria", "tematica1", "estado_hechos")
head(datos_train)
head(datos_train[names(datos_train) == columnas_a_factor,])
datos_train <-lapply(datos_train[names(datos_train) == columnas_a_factor,], as.factor) %>% data.frame()
datos_train <-lapply(datos_train[names(datos_train) == columnas_a_factor,], as.numeric) %>% data.frame()
datos_train <-lapply(datos_train[names(datos_train) == columnas_a_factor,], scale) %>% data.frame()
head(datos_train)
names(datos_train)

# Comenzamos con el modelo de redes neuronales
set.seed(123)
# Agregar etiquetas l1,l2,l3
new_cols <- c(columnas_a_factor, c("l1","l2","l3"))
names(datos_train[new_cols])

# Establecemos la funcion que queremos modelar en nuestra red
nom <- names(train[new_cols])
f <- as.formula(paste("l1 + l2 + l3 ~", paste(nom[!nom %in% c("l1","l2","l3")], collapse = " + ")))
f

# Obtenemos un analisis previo de correlaciones para asegurarnos que son buenas elecciones
corr <- lm(f, data = datos_train)
corr

# Modelo de red neuronal
nn1 <- neuralnet(f,
                 data = datos_train,
                 hidden = c(9,6,4),
                 act.fct = "logistic",
                 linear.output = FALSE,
                 threshold = 0.3,
                 rep = 3,
                 lifesign = "minimal")

# Calculamos procentaje de error, mostramos la red
nn1$result.matrix[,3]["error"] <- nn1$result.matrix[,3]["error"] / 100
plot(nn1, rep = 'best')

# Analizamos los resultados/ Hacemos ejemplos de clasificacion
datos_test[columnas_a_factor] <-lapply(datos_test[columnas_a_factor], as.factor) %>% data.frame()
datos_test[columnas_a_factor] <-lapply(datos_test[columnas_a_factor], as.numeric) %>% data.frame()
datos_test[columnas_a_factor] <-lapply(datos_test[columnas_a_factor], scale) %>% data.frame()
head(datos_test)

# Realizamos la prediccion con datos de prueba
pr.nn <- compute(nn1, datos_test)
head(pr.nn$net.result)

# Asignamos a la etiqueta con mayor probabilidad el valor de 1
for (index in 1:length(datos_test)){
  main_ind <- which.max(pr.nn$net.result[index,])
  pr.nn$net.result[index,main_ind] <- 1
}
head(pr.nn$net.result)

# Generamos una matriz de confusion (aqui solo consideramos una categoria)
results <- data.frame(actual = datos_test$l3, prediction = pr.nn$net.result[,3])
row_test <- datos_test[1,]
as.numeric(factor(row_test$servicio))
head(results)

# Obtenemos, por ejemplo, la precision
roundedresults<-sapply(results,round,digits=0)
head(roundedresults)
roundedresultsdf=data.frame(roundedresults)
tabla_res <- table(actual=roundedresultsdf$actual,prediction=roundedresultsdf$prediction)
tabla_res
precision <- tabla_res[2,2] / (tabla_res[2,2] + tabla_res[1,2])

# Tambien lo podemos obtener de la siguiente forma
original_values <- max.col(datos_test[, c("l1","l2","l3")])
pr.nn_2 <- max.col(pr.nn$net.result)
mean(pr.nn_2 == original_values)

# Intervalo de confianza
#vector_40_pruebas <- sample(4:10, 40, replace=T)
vector_40_pruebas <- seq(1,40,by=1)
rows_mixed <- sample(nrow(datos_test))

for (i in 1:length(vector_40_pruebas)){
  count <- 0
  for (j in 1:10){
    row_test <- datos_test[rows_mixed[i],]
    pr.nn_aux <- compute(nn1, row_test)
    index_final <- which.max(pr.nn_aux$net.result)
    if (as.numeric(factor(row_test$servicio)) == as.numeric(index_final) ){
      count <- count + 1
    }
  }
  print(count)
  vector_40_pruebas[i] <- 1
}
vector_40_pruebas
#vector_40_pruebas <- sample(4:10, 40, replace=T)

# Intervalos de confianza
muestra <- c(5,4,5,9,10,7,9,5,9,5,5,10,8,7,7,7,4,5,5,10,6,7,8,8,8,7,8,10,4,6,6,7,10,4,8,9,10,6,9,10) / 10
head(muestra)
#YA QUE TENEMOS 40 OBSERVACIONES DE NUESTRA VARIABLE ALEATORIA, PODEMOS CREAR UN
#INTERVALO DE CONFIANZA A NIVEL 90% PARA LA MEDIA.

sigma_gorro <- sd(muestra)
mu_gorro    <- mean(muestra)
n           <- length(muestra)

#Dado que se utiliza una estimacion de la varianza y no
#la varianza real, entonces nuestra variable pivotal tiene
#una distribucion t-student con n-1 grados de libertad.

a           <- qt(.05,39)  
b           <- qt(.95,39)
conf_int    <- c(0,0)
conf_int[1] <- mu_gorro - (b*sigma_gorro/sqrt(n))
conf_int[2] <- mu_gorro - (a*sigma_gorro/sqrt(n))
paste("Nuestro modelo tiene un intervalo de confianza de: ", list(conf_int))
