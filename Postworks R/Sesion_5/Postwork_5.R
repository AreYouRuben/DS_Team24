setwd("C:/Users/mgarc/OneDrive/Escritorio/9no Semestre/Bedu_Modulo_II/Sesion_5/")
# Paso 1. A partir del conjunto de datos de soccer de la liga española de las temporadas 2017/2018, 2018/2019 y 2019/2020, crea el data frame SmallData
temporadas<- lapply(dir(),read.csv)
temporadas<- lapply(temporadas,select,"Date","HomeTeam","FTHG","AwayTeam","FTAG")
temporadas<- lapply(temporadas,rename,date = Date,home.team = HomeTeam, 
home.score = FTHG, away.team = AwayTeam, away.score = FTAG)
temporadas[[1]]<-mutate(temporadas[[1]], date = as.Date(date,"%d/%m/%y"))
temporadas[[2]]<-mutate(temporadas[[2]], date = as.Date(date,"%d/%m/%Y"))
temporadas[[3]]<-mutate(temporadas[[3]], date = as.Date(date,"%d/%m/%Y"))
SmallData<- do.call(rbind,temporadas)
write.csv(SmallData,"D:\\ACS\\BEDU\\Modulo 2\\Postwork_5\\soccer.csv",row.names = F)

# Paso 2. Con la función create.fbRanks.dataframes del paquete fbRanks importe el archivo soccer.csv a R y al mismo tiempo asignelo a una variable llamada listasoccer. 
# Crear data frames a variables llamadas anotaciones y equipos
install.packages("fbRanks")
library("fbRanks")
listasoccer<-create.fbRanks.dataframes("soccer.csv")
anotaciones <- listasoccer$scores
equipos <- listasoccer$teams

# Paso 3. Con ayuda de la función unique crea un vector de fechas (fecha) que no se repitan y que correspondan a las fechas en las que se jugaron partidos.
# Guarda los resultados con el nombre ranking
fechas <- unique(SmallData$date)
n <- length(fechas)
ranking <- rank.teams(anotaciones, equipos, min.date = fechas[1], max.date = fechas[n-1])

# Paso 4. Finalmente estima las probabilidades de los eventos, el equipo de casa gana, el equipo visitante gana o el resultado es un empate para los partidos que se jugaron en la última fecha del vector de fechas fecha
predict(ranking, date = fechas[n])
