#Nos aseguramos de estar trabajando en el directorio correcto
setwd("C:/Users/mgarc/OneDrive/Escritorio/9no Semestre/Bedu_Modulo_II/Sesion_2/")
#Para manejar las medidas de tendencia central será útil descargar el siguiente paquete
install.packages("DescTools")
library(DescTools)
#Para la media (promedio)
x <- c(4000, 9000, 9000, 10000)
mean(x)
#Para la mediana (cuantil 0.5)
median(x)
#Para la moda (valor más repetido)
Mode(x)
#Para obtener lo cuantiles de un arreglo de números
x <- c(29,13,62,4,63,96,1,90,50,46)
quantile(x, 0.1)
quantile(x, c(0.25,0.5,0.75))
quantile(x, seq(0.1,0.9, by = 0.1))
#El IQR y la varianza como medidas de dispersión
#El IQR puede calcularse con una función o bien aplicando la definición
IQR(x)
quantile(x,0.75) - quantile(x,0.25)
#La varianza y la desviación estándar
var(x)
sd(x)
#Funciónes que nos permiten obtener información sobre un objeto en R
str(iris)
summary(iris)
head(iris)
tail(iris)
View(iris)

#PAQUETE dplyr Y SUS APLICACIONES
library(dplyr)
#Para descargar y leer archivos csv
url1 <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv"
url2 <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv"
download.file(url = url1, destfile = "st19ncov-confirmados.csv", mode = "wb")
download.file(url = url2, destfile = "st19ncov-muertes.csv", mode = "wb")
confirmados <- read.csv("st19ncov-confirmados.csv")
muertes     <- read.csv("st19ncov-muertes.csv")
#Utilizando las funciones anteriores para obtener información de estos data frames
str(muertes)
View(muertes)
#Quitamos el primer renglón del dataframe en ambos
Sconf <- confirmados[-1,]
Smu   <- muertes[-1,]
#Utilizamos la función select para extraer únicamente las columnas de interés
Sconf <- select(Sconf, Country.Region, Date, Value)
Sconf <- rename(Sconf, Country = Country.Region, Fecha = Date, Infectados = Value)
#Utilizando la funció mutate para mutar las columnas del dataframe
Sconf <- mutate(Sconf, Fecha = as.Date(Fecha, "%Y-%m-%d"), Infectados = as.numeric(Infectados))
#Hacemos algo similar con el dataframe correspondiemte al número acumulado de muertos
Smu   <- select(Smu, Country.Region, Date, Value)
Smu   <- rename(Smu, Country = Country.Region, Fecha = Date, Muertos = Value)
Smu   <- mutate(Smu, Country = as.character(Country), Fecha = as.Date(Fecha, "%Y-%m-%d"), Muertos = as.numeric(Muertos))
#Unimos las tablas Smu y Sconf
Scm <- merge(Sconf, Smu)
#Para aplicar un filtro a la nueva base de datos
mex <- filter(Scm, Country == "Mexico")
mex <- filter(mex, Infectados != 0)
#Podemos agregar otras columnas con la ayuda de la función mutate
mex <- mutate(mex, NI = c(1,diff(Infectados)))
mex <- mutate(mex, NM = c(0, diff(Muertos)))
mex <- mutate(mex, Letalidad = Muertos/Infectados*100)
mex <- mutate(mex, IDA = lag(Infectados), MDA = lag(Muertos))
mex <- mutate(mex, FCI = Infectados/IDA, FCM = Muertos/MDA)
mex <- mutate(mex, Dia = 1:dim(mex)[1])

#Para las funciones cbind y rbind
cbind(1:5,6:10,11:15,16:20,21:25)
cbind(1:10, matrix(11:30, ncol = 2))
cbind(data.frame(x = 1:10, y = 11:20), z = 21:30)
test <- data.frame(Nombre = c("Manuel","Patricia","Hector"),
                   Edades = c(24,57,17))
test <- cbind(test, aux = c(1,1,1))

#Recordando cómo funciona la función apply
x <- matrix(1:49, ncol = 7)
apply(x, 1, mean)

#La función lapply
u1011 <- "https://www.football-data.co.uk/mmz4281/1011/SP1.csv"
u1112 <- "https://www.football-data.co.uk/mmz4281/1112/SP1.csv"
u1213 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"
u1314 <- "https://www.football-data.co.uk/mmz4281/1314/SP1.csv"

download.file(url = u1011, destfile = "SP1-1011.csv", mode = "wb")
download.file(url = u1112, destfile = "SP1-1112.csv", mode = "wb")
download.file(url = u1213, destfile = "SP1-1213.csv", mode = "wb")
download.file(url = u1314, destfile = "SP1-1314.csv", mode = "wb")

#Creamos una lista en donde cada elemento es un archivo csv
lista <- lapply(dir(), read.csv)

#Seleccionamos solo algunas columnas de cada data frame
lista <- lapply(lista, select, Fecha:FTR)
View(lista[[2]])

#Para la lectura de archivos JSON y XML
library(rjson)
library(XML)
URL1 <- "https://tools.learningcontainer.com/sample-json-file.json"
JsonData <- fromJSON(file = URL1)
class(JsonData)
length(JsonData)
str(JsonData)

URL2 <- "http://www-db.deis.unibo.it/courses/TW/DOCS/w3schools/xml/cd_catalog.xml"
data_df <- xmlToDataFrame(URL2)
head(data_df)
head(airquality)
str(airquality)
dim(airquality)
