setwd("C:/Users/mgarc/OneDrive/Escritorio/9no Semestre/Bedu_Modulo_II/Sesion_5/")
temporadas<- lapply(dir(),read.csv)
temporadas<- lapply(temporadas,select,"Date","HomeTeam","FTHG","AwayTeam","FTAG")
temporadas<- lapply(temporadas,rename,date = Date,home.team = HomeTeam, 
home.score = FTHG, away.team = AwayTeam, away.score = FTAG)
temporadas[[1]]<-mutate(temporadas[[1]], date = as.Date(date,"%d/%m/%y"))
temporadas[[2]]<-mutate(temporadas[[2]], date = as.Date(date,"%d/%m/%Y"))
temporadas[[3]]<-mutate(temporadas[[3]], date = as.Date(date,"%d/%m/%Y"))
SmallData<- do.call(rbind,temporadas)
write.csv(SmallData,"D:\\ACS\\BEDU\\Modulo 2\\Postwork_5\\soccer.csv",row.names = F)
install.packages("fbRanks")
library("fbRanks")
listasoccer<-create.fbRanks.dataframes("soccer.csv")
anotaciones <- listasoccer$scores
equipos <- listasoccer$teams
fechas <- unique(SmallData$date)
n <- length(fechas)
ranking <- rank.teams(anotaciones, equipos, min.date = fechas[1], max.date = fechas[n-1])
predict(ranking, date = fechas[n])
