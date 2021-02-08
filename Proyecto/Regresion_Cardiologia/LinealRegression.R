#DESCARGAMOS LAS LIBRERÍAS NECESARIAS
library(dplyr)

# Datos actualizados : https://datos.cdmx.gob.mx/dataset/servicios-para-la-poblacion-en-general/resource/59af003e-042e-4aeb-b4f0-8ca9a6600ec4
# Datos actualizados csv: https://datos.cdmx.gob.mx/dataset/40d58f40-39f9-45ee-a30d-72f674fc3bf9/resource/59af003e-042e-4aeb-b4f0-8ca9a6600ec4/download/base-integrales-0102.csv


# Descargamos los datos para poder analizarlos
datos <- read.csv("servicios-para-la-poblacion-en-general.csv", encoding = "UTF-8")
# Limpiamos los datos de todos los registros con CP igual a -1
dataF <- filter(datos,cp_usuaria != -1)

# En este análisis de regresión lineal se busca observar la relación de alguna
# variable numérica como edad o frecuencia de llamadas con la temática de cardiología
dataCardio <- filter(dataF,tematica_1=='CARDIOLOGIA' | tematica_2=='CARDIOLOGIA' | tematica_3=='CARDIOLOGIA')
attach(dataCardio)

# Hacemos una prueba con datos de CARDIOLOGÍA respecto a la edad
plot(table(dataCardio$edad))
table(tematica_1)
EdadFr <- table(edad)
EdadFr <- as.data.frame(EdadFr)
plot.default(EdadFr$edad,EdadFr$Freq)
abline(lsfit(EdadFr$edad, EdadFr$Freq))
# Encontramos que los datos basados en la edad forman un comportamiento
# de campana por lo que no es muy bueno para regresión lineal


# Buscamos una relación de la frecuencia de llamadas respecto la hora de alta
table(dataCardio$hora_alta)
horaFr <- table(dataCardio$hora_alta)
horaFr <- as.data.frame(horaFr)

# Transformamos los valores a numericos para mejor control en el modelo de regresión lineal
horaFr$Var1 <- as.numeric(as.character(horaFr$Var1))
horaFr$Freq <- as.numeric(as.character(horaFr$Freq))

attach(horaFr)
head(horaFr)

# La creación del modelo es la frecuencia de llamadas contra la edad
modelo <- lm(Freq~Var1)
summary(modelo)

# Se crea un gráfico de dispersión para observar el comportamiento
# Graficamos nuestros datos nuevamente, pero ahora con la recta de regresión
# ajustada

plot.default(Var1, Freq,xlab = "Hora de alta", 
             ylab = "Frecuencuia Cardiología", main = 'Frecuencia CARDIOLOGIA ~ Hora de alta')
abline(lsfit(horaFr$Var1, horaFr$Freq))


# Residuales
points(13, 373, pch=16, col = "red")             # Punto muestral
205.26333 + 11.10391 * 13                        # Valor y sobre la recta estimada
lines(c(13, 13), c(349.6142, 373), col = "red")

points(15, 356, pch=16, col = "red")             # Punto muestral
205.26333 + 11.10391 *                           # Valor y sobre la recta estimada
lines(c(15, 15), c(371.822, 356), col = "red")

# Acontinuación encontramos el cuantil de orden 0.975 de la distribución
# t de Student con 22 (n - 2) grados de libertad. En total tenemos n = 24 
# observaciones en nuestro conjunto de datos. Estamos encontrando el valor 
# que satisface P(T > tval) = 0.025

tval <- qt(1-0.05/2,22)
tval

# Comprobamos
pt(tval,df=22)

# Encontramos intervalos de confianza del 95% para el intercepto y la pendiente
# del modelo de regresión lineal simple

round(confint(modelo, level = 0.95), 3)

# Ahora encontramos intervalos de confianza del 95% para la recta de regresión
# poblacional en algunos valores de X (Var0) o Hora de alta

Var0 <- c(0,5,10,15,20,24)      # Algunos posibles valores de Hora de alta

(conf <- predict(modelo, newdata = 
                   data.frame(Var1 = Var0), 
                 interval = "confidence", level = 0.95))


# Podemos visualizar gráficamente estos intervalos de confianza
lines(Var0, conf[, 2], lty = 2, lwd = 2, col = "green") # límites inferiores
lines(Var0, conf[, 3], lty = 2, lwd = 2, col = "green") # límites superiores


# También podemos encontrar intervalos de predicción del 95% para el valor
# real de la variable de respuesta Y (Frecuencia) en algunos valores de X (Hora de alta)

(pred <- predict(modelo, newdata = 
                   data.frame(Var1 = Var0), 
                 interval = "prediction", level = 0.95))

# Podemos visualizar gráficamente estos intervalos de predicción
lines(Var0, pred[, 2], lty = 2, lwd = 2, col = "blue") # límites inferiores
lines(Var0, pred[, 3], lty = 2, lwd = 2, col = "blue") # límites superiores

# También es posible llevar a cabo un análisis de varianza para decidir si 
# existe asociación lineal entre Hora de alta y Frecuencia de llamadas
x<-anova(modelo)
write.csv(x, 'VarianceAnalysys.csv')

par(mfrow = c(2, 2))
plot(modelo)
dev.off()

