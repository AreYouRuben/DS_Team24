#Instalar paquetes
library("ggplot2")
library("dplyr")
#Para aprender a crear vectores
a <- c(4,6,8,10,12)
b <- c(3,5,7,9)
#Para encontrar la longitud de un vector
length(a)
length(b)
#Para acceder a cada una de las entradas del arreglo
a[1]
b[3]
#Para concatenar dos vectores
unidos <- c(a,b)
#Para ordenar las entradas de un vector
a.sorted <- sort(a, decreasing = T)
#Otra forma de generar un vector si es una secuencia
secuencia1 <- 1:10
secuencia2 <- c(1,2,3,4,5,6,7,8,9,10)
suma.secuancias <- secuencia1 + secuencia2
#Para asegurarme de que ambas secuencias son el mismo tipo de dato
class(secuencia1)
class(secuencia2)
#Para generar secuencias que no sean sucesivas pero que sigan un patrón
seq(from = 1, to = 10, by = 2)
seq(1,10,2)
#Para crear una secuencia repetida
rep(5,10)
rep(secuencia1, 2)
#EN RESUMEN
#Existen tres formas de crear arreglos
#1) x <- c(1,2,3,4,5,6,7,8,9,10)
#2) x <- 1:10
#3) x <- seq(from = 1, to = 10, by = 1)
#4) x <- rep(1,10)

#Para entender las operaciones entre vectores
a+b
a*b
a/b
b/a
a**2

#Para aprender a crear matrices
m <- matrix(1:25, nrow = 5, ncol = 5)
#Si quiero crear una matriz de puros ceros
mm <- matrix(rep(0,25), nrow = 5, ncol = 5)
#Para extraer una entrada de la matriz
m[1,3]
#Para extraer el primer renglón
m[1,]
#Para extraer la primer columna
m[,1]
#Qué sucede si se suma un vector y una matriz
m1 <- matrix(1:9, nrow = 3, ncol = 3)
v1 <- c(1,2,3)
m1
v1
m1 + v1
#Creamos otra matriz y obtenemos su dimensión
n <- matrix(1:15, 5,3)
dim(n)
#Para extraer un subconjunto de una matriz
n[n > 4]
n[n > 4 & n < 10]
#Para obtener los indicadores
which(n > 4)
# EN RESUMEN
#Para seleccionar un subconjunto de entradas de una matriz
# m[i,j] para el i-esimo renglon y la j-esima columna
# m[i,] para el i_esimo renglón
# m[,j] para la j-esima columna
# m[m > 3] regresa las entradas de m que cumplen la condición
# which(m > 3) regresa los indicadores de m que cumplen la condicion
# Usar la función apply

#Para crear listas
milista <- list(nombre = "Manuel", no.hijos = 5, edades.hijos = c(18,16,12,8,6))
#Para acceder a cada elemento de la lista
milista$nombre
milista$no.hijos
milista$edades.hijos

#EN RESUMEN
#Para manipular listas
#1) Son creadas con la función list(elemento1, elemento2,...)
#2) Cada entrada puede ser etiquetada con un nombre
#3) milista$etiqueta regresa la entrada de la ista nombrada etiqueta
#4) Las listas almacenan datos de distinto tipo y pueden agregarse entradas

#Para crear dataframes
nombres <- c("Laura","Rafael","Ana Paula","Karla","Oscar")
edades  <- c(20,34,56,71,12)
mifile  <- data.frame( nombre = nombres, edad = edades)
str(mifile)
#Extraer infomración de un dataframe se hace igual que en las matrices
mifile[,1]
mifile[,2]
mifile[1,]
mifile$nombre
mifile[1,]$nombre
#Para hacer operaciones con un datframe
mean(mifile$edad)
str(mifile)
summary(mifile)
#Para agregar una columna
mifile$ocupacion <- c("Estudiante", "Abogado", "Contador", "Estudiante", "Profesor")
mifile$ocupacion <- NULL

#En cuanto a la lectura y la descarga de data sets
getwd()
setwd("C:/Users/mgarc/OneDrive/Escritorio/9no Semestre/Bedu_Modulo_II/")
data.url <- read.csv("https://www.football-data.co.uk/mmz4281/2021/SP1.csv")

#Loops y pseudocódigo: for, while, id, else
#Crearemos un arreglo de 20 entradas y elevamos al cuadrado las primeras 10
w <- rnorm(20)
for(i in 1:10){
  w[i] <- w[i]**2
  print(w[i])
}
#Para el while, crear una funcion que sume las entradas
#del arreglo hasta que la suma exceda el valor de 5
w <- c(2,1.2,0.88,3,1)
suma <- 0
i <- 1
while(suma < 5){
  suma <- suma + w[i]
  i    <- i +1
}
print(suma)
