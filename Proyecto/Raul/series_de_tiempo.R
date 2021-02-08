########################
###
###   Analisis de series de tiempo
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
library(rccmisc)

# Lectura de datos. Hacemos una exploracion rapida de los datos
datos <- read.csv("./servicios-para-la-poblacion-en-general.csv")
head(datos)
summary(datos)
# Transformamos las columnas del data frame a minusculas
datos <- lownames(datos)
names(datos)

# Comenzamos a agrupar la informacion que necesitamos
# Checamos el formato de ciertos datos
class(datos$fecha_alta)
class(datos$hora_alta)

# Obtenemos la frecuencia de llamadas por dia/ hora
conta_registro_dia <- count(datos, fecha = datos$fecha_alta)
conta_registro_hora <- count(datos, hora = datos$hora_alta)
head(conta_registro_dia)
head(conta_registro_hora)

# Generamos un data frame por dia
df_registro_dia <- data.frame(eje_x=as.Date(conta_registro_dia$fecha), n = conta_registro_dia$n)
head(df_registro_dia)
# Por hora
df_registro_hora <- data.frame(eje_x=conta_registro_hora$hora, n = conta_registro_hora$n)
head(df_registro_hora)

# DF a graficar (se puede cambiar para que se grafique por dia o por hora)
df_registro <- df_registro_dia
dim(df_registro)
names(df_registro)

p <- ggplot(df_registro, aes(x=eje_x, y=n)) + 
  geom_line( color="blue") +
  labs(x = "Fecha", 
       y = "Cantidad de llamadas registradas",
       title = paste("Llamadas a la linea de emergencia acumuladas por dia")) +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1)) 

p <- p  + scale_x_date(date_labels="%b %y",date_breaks="1 month")

p <- p +
  theme(plot.margin=margin(10,10,20,10), plot.caption=element_text(hjust=1.05, size=10)) +
  annotate("text", x = df_registro$eje_x[round(dim(df_registro[1])*0.4)], y = max(df_registro$n), colour = "blue", size = 5, label = paste("Ultima cantidad registrada: ", df_registro$n[dim(df_registro)[1]]))
p
tail(df_registro)
class(df_registro$eje_x)

# Transformacion a series de tiempo por mes
ts_por_mes <- df_registro %>% group_by(fecha = format(eje_x, "%Y-%m")) %>% summarise(frecuencia_llamadas = sum(n))
head(ts_por_mes)
tail(ts_por_mes)
(datos_ts <- ts(ts_por_mes$frecuencia_llamadas, start = c(2016, 11), end = c(2021, 1), frequency = 12))
plot(datos_ts, 
     main = "Llamadas a la linea de emergencia acumuladas por mes", 
     xlab = "Tiempo",
     ylab = "Cantidad de llamadas registradas",
     sub = "Serie mensual: Noviembre de 2016 - Enero de 2021")

# Realizamos una descomposicion sobre nuestra serie de tiempo (modelo aditivo o modelo multiplicativo cuando sea razonable suponer la descomposicion)
decom_add <- decompose(datos_ts)
plot(decom_add, xlab = "Tiempo", 
     sub = "Descomposicion aditiva de los datos de llamadas mensuales")

decom_mul <- decompose(datos_ts, type = "mult")
plot(decom_mul, xlab = "Tiempo", 
     sub = "Descomposicion multiplicativa de los datos de llamadas mensuales")

Tendencia <- decom_add$trend
Estacionalidad <- decom_add$seasonal
Aleatorio <- decom_add$random

ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Tiempo", main = "Datos de Producción de Electricidad", 
        ylab = "Producción de electricidad", lty = 1:2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")

# Comprobamos los valores de una descomposicion
Tendencia[20] + Estacionalidad[20] + Aleatorio[20]
datos_ts[20]

# Procedemos a elaborar un primer modelo
Time <- 1:length(datos_ts)
Imth <- cycle(datos_ts)
primer_modelo <- lm(log(datos_ts) ~ Time + I(Time^2) + factor(Imth))
# Analizamos correlogramas y residuales
acf(resid(primer_modelo), main = "")
title(main = "Correlograma de la serie de residuales del modelo de regresion 1",
      sub = "Serie de llamadas registradas")

plot(resid(primer_modelo), type = "l", main = "", xlab = "", ylab = "")
title(main = "Serie de residuales del modelo de regresion ajustado",
      sub = "Serie de llamadas registradas",
      xlab = "Tiempo",
      ylab = "Residuales")

# Procedemos a tener un mejor modelo (modelo ARIMA)
best.order <- c(0, 0, 0)
best.aic <- Inf
for(i in 0:2)for(j in 0:2){
  model <- arima(resid(primer_modelo), order = c(i, 0, j))
  fit.aic <- AIC(model)
  if(fit.aic < best.aic){
    best.order <- c(i, 0, j)
    best.arma <- arima(resid(primer_modelo), order = best.order)
    best.aic <- fit.aic
  }
}
# Mostramos el mejor resultado obtenido
best.order
acf(resid(best.arma), main = "")
title(main = "Serie de residuales del modelo ARIMA(2,0,1) ajustado",
      sub = "Serie de residuales del modelo de regresion ajustado a los datos de llamadas")

# Generamos el modelo de prediccion
new.time <- seq(length(datos_ts)+1, length = 12)
new.data <- data.frame(Time = new.time, Imth = rep(1:12, 1))
predict.lm <- predict(primer_modelo, new.data)
predict.arma <- predict(best.arma, n.ahead = 12)
modelo.pred <- ts(exp(predict.lm + predict.arma$pred), start = 2021, freq = 12)

# Mostramos el resultado de la prediccion
ts.plot(cbind(datos_ts, modelo.pred), lty = 1:2, 
        col = c("blue", "red"), xlab = "Tiempo", 
        ylab = "Cantidad de llamadas",
        main = "Prediccion de la cantidad de llamadas que se esperan",
        sub = "Prediccion de 12 meses adicionales (linea punteada en rojo)")
