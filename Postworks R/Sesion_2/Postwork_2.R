getwd()
#Paso 1
#Para importar los datos
url1  <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
url2  <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
url3  <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"

temp1 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")
temp2 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
temp3 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")

#Paso 2
#Almacenamos las data frames en una lista
lista_temps <- list(Temporada1 = temp1, Temporada2 = temp2, Temporada3 = temp3)

#Aplicamos las funciones str, head, View y summary a todos los elementos de la lista
lapply(lista_temps, str)
lapply(lista_temps, head)
lapply(lista_temps, View)
lapply(lista_temps, summary)

#Paso 3
#Seleccionamos únicamemnte los campos Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR
lista_temps <- lapply(lista_temps, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
lapply(lista_temps, View)

#Paso 4
#Para asignar el correcto tipo de dato
lista_temps <- lapply(lista_temps, mutate, Date = as.Date(Date, "%Y-%m-%d"), FTHG = as.integer(FTHG), FTAG = as.integer(FTAG))

lista_temps <- rbind(lista_temps[[1]],lista_temps[[2]],lista_temps[[3]])
