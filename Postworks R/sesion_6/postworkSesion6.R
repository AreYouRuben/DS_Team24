# Solución Postwork sesión 6
# Librerias de apoyo
library(dplyr)

#Importa el conjunto de datos match.data.csv a R y realiza lo siguiente:
datos <- read.csv("./match.data.csv", header = TRUE)
# Analizamos un poco los datos
class(datos)
names(datos)
head(datos)
  
# Paso 1. Agrega una nueva columna sumagoles que contenga la suma de goles por partido.
datos <- mutate(datos, sumagoles = home.score + away.score)

# Paso 2. Obtén el promedio por mes de la suma de goles.
# Cambiamos a tipo Date
datos <- datos %>% mutate(date = as.Date(date, format = "%Y-%m-%d")) 
class(datos$date)
head(datos)
# Obtenemos el promedio por mes usando sumagoles
promedio_por_mes <- datos %>% group_by(fecha = format(date, "%Y-%m")) %>% summarise(promedio_goles = mean(sumagoles))
head(promedio_por_mes)

# Paso 3. Crea la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.
Global.ts <- ts(promedio_por_mes$promedio_goles, st = c(2010, 8), end = c(2019, 12), fr = 12)

# Paso 4. Grafica la serie de tiempo.
plot(Global.ts, xlab = "Tiempo", ylab = "Promedio de la suma de goles", main = "Serie del promedio por mes de la suma de goles de partidos disputados",
     sub = "Serie mensual: Agosto de 2010 a Diciembre de 2019")
