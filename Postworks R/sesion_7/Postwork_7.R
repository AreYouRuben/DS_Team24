#install.packages("mongolite")

library(mongolite)
library(dplyr)

#   Conectar con la base de datos de MONGODB en localhost

#mongodb://localhost:27017/?readPreference=primary&appname=MongoDB%20Compass&ssl=false
conn <- mongo(
  collection = "match_games",
  db = "match_games",
  url = "mongodb://localhost:27017",
  verbose = FALSE,
  options = ssl_options()
)

#   Leer CSV del reto desde el repositorio
data <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-07/Postwork/data.csv")

#   Transformar la fecha a tipo Date por buena práctica
str(data)
class(data$Date)

data <- data %>%
  mutate(
    Date = as.Date(Date,"%Y-%m-%d"))

str(data)
class(data$Date)

#   insertar los datos a la base de datos
#conn$drop()              # Eliminar datos para evitar escribir los mismos datos
#conn$insert(data)        # Insertar los datos del dataframe

#   Contar el número de registros en nuesta base de datos
data.count <- conn$count("{}")
data.count

#   Obtener los partidos del Real Madrid jugados en diciembre
#   El Postwork pide 20 de diciembre de 2015, pero esta fecha no se encuentra
RealMadrid.Dec <- conn$find('{
            "Date": { "$regex" : "-12-", "$options" : "i" }, 
            "$or": [
                {
                    "HomeTeam": "Real Madrid"
                }, {
                    "AwayTeam": "Real Madrid"
                }
            ]
        }')

#   Añadir una columna que especifique si el Real Madrid ganó o perdió el partido

RealMadrid.Dec$Winner <- ifelse(
  RealMadrid.Dec$HomeTeam == 'Real Madrid' & RealMadrid.Dec$FTR == 'H','Gana',
  ifelse(RealMadrid.Dec$AwayTeam == 'Real Madrid' & RealMadrid.Dec$FTR == 'A','Gana',
         ifelse(RealMadrid.Dec$FTR == 'D', 'empate', 'Pierde')))


