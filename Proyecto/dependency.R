library(ggplot2)
library(dplyr)
library(scales)
library(vcd)
suppressMessages(suppressWarnings(library(e1071)))
suppressMessages(suppressWarnings(library(ISLR)))

datos <- read.csv("servicios-para-la-poblacion-en-general.csv", encoding = "UTF-8")
sDatos <- datos %>% select(fecha_alta:tematica_7)

fecha <- gsub("T.*","", sDatos$fecha_alta)
head(fecha)
fecha <- as.Date(fecha, "%Y-%m-%d")
class(fecha)
sDatos$fecha_alta <- fecha
class(sDatos$fecha_alta)
sDatos$sexo <- ifelse(sDatos$sexo == "FEMENINO",1,0)
class(sDatos$sexo)

a <- lm(sexo~ tematica_1, data = sDatos)
summary(a)

b <- lm(sexo~ edad+hora_alta+dia_alta+mes_alta, data = sDatos)
summary(b)
anova(b)

sDatos$servicio <- factor(sDatos$servicio)
sDatos$estado_civil <- factor(sDatos$estado_civil)

c <- lm(sexo~ servicio+estado_civil+escolaridad, data = sDatos)
summary(c)
prop.table(table(sDatos$escolaridad))

d <- lm(sexo~ servicio+estado_civil+escolaridad+ocupacion, data = sDatos)
summary(d)
anova(d)
pd <- predict(d,sDatos)
head(pd)
pd1<-ifelse(pd > 0.6, 1, 0)
tableD<-table(Prediction = pd1,  
            Actual = sDatos$sexo) 
tableD
1 - sum(diag(tableD)) / sum(tableD)
round(sum(diag(tableD))/sum(colSums(tableD)), 5)

e <- lm(sexo~ servicio+ocupacion, data = sDatos)
summary(e)

class(sDatos$cp_usuaria)

f <- lm(cp_usuaria ~ tematica_1, data = sDatos)
summary(f)


set.seed(2020)
train = sample(nrow(sDatos), 
               round(nrow(sDatos)/100))
tail(sDatos[train, ])

best <- svm(sexo~servicio+ocupacion,  data = sDatos[train,],
            kernel = "radial",
            cost = 100,
            gamma = 1.51
)

mc <- table(true = sDatos[-train, "sexo"], 
            pred = predict(best, 
                           newdata = sDatos[-train,]))
mc
prsvm <- predict(best,newdata = sDatos[-train,])
head(prsvm)
pd2<-ifelse(prsvm > 0.6, 1, 0)
tablesvm<-table(Prediction = pd2,  
              Actual = sDatos$sexo)

round(sum(diag(mc))/sum(colSums(mc)), 5)