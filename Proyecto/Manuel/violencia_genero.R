getwd()
setwd("C:/Users/mgarc/OneDrive/Escritorio/9no Semestre/Bedu_Modulo_II/Proyecto/")

#DESCARGAMOS LAS LIBRERÍAS NECESARIAS
library(tidyverse)
library(kableExtra)
library(lubridate)
library(dplyr)
library(ggplot2)
library(rgdal)



#DESCARGAMOS LA BASE DE DATOS PARA PODER REALIZAR EL ANÁLISIS EXPLORATORIO.
#NOS VAMOS A ENFOCAR EN LAS LLAMADAS QUE REALIZAN MUJERES POR TEMAS RELACIONADOS
#CON VIOLNECIA DE GÉNERO.
data            <- read.csv("archivo1.csv")
linea.mujeres   <- data %>% filter(SEXO == "FEMENINO",
                                   TEMATICA_1 == "VIOLENCIA" | TEMATICA_2 == "VIOLENCIA",
                                   AÑO_ALTA >= 2017)
linea.mujeres   <- linea.mujeres %>% mutate(fecha = date(FECHA_ALTA))

#¿HA EXISTIDO UN AUMENTO DE LA VIOLENCIA DE GÉNERO DESDE QUE INICIÓ EL CONFINAMIENTO?
#PARA DAR RESPUESTA A ESTA PREGUNTA VEAMOS CÓMO HA CAMBIADO EL NÚMERO DE LLAMDAS RECIBIDAS
#ANTES Y DESPUÉS DEL CONFINAMIENTO.
conteo_llamadas <-linea.mujeres %>% group_by(fecha) %>% tally()
ggplot(conteo_llamadas) +
  geom_line(aes(x = fecha, y = n), colour = "steelblue") +
  theme_bw() +
  labs(
    x = "Fecha de Llamada",
    y = "Reportes de Violencia hechos por Mujeres"
  ) +
  geom_point(aes(x = dmy("23/03/2020"), y = 66), color = "red", size = 2) +
  geom_text(aes(x = dmy("23/03/2020"), y = 66), label = "Inicio Cuarentena", size= 3.5, nudge_y= -10)

#¿EXISTE ALGUNA EDAD EN LA CUAL LAS MUJERES SON MÁS PROPENSAS A EXPERIMENTAR
#ALGÚN TIPO DE VIOLNECIA DE GÉNERO?
llamadas.tabla.edad <- linea.mujeres %>%
  summarise(MAD = mad(EDAD),
            Promedio = mean(EDAD),
            Mediana = median(EDAD),
            IQR = IQR(EDAD),
            Varianza = var(EDAD))
kable(llamadas.tabla.edad, booktabs = T) %>% kable_styling(latex_options = "striped")


llamadas.edad <- linea.mujeres %>% group_by(EDAD) %>% count()
ggplot(llamadas.edad)+
  geom_col(aes(x = EDAD, y = n, fill = EDAD)) +
  ggtitle("Edades de las mujeres que llaman por motivos de violencia") +
  theme_minimal() +
  labs(
    x = "Edades",
    y = "Total de llamadas"
  )

#¿QUÉ RELACÓN EXISTE ENTRE EL ESTADO CIVIL, LA OCUPACIÓN Y LA PROBABILIDAD DE SER
#VÍCTIMA DE VIOLENCIA DE GÉNERO
ggplot(linea.mujeres)+
  geom_count(aes(x =ESTADO_CIVIL , y = OCUPACION), color = "darkblue", show.legend=TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(subtitle="Estado Civil vs Ocupación",
       y="Ocupación",
       x="Estado Civil",
       title="Gráfica de Conteo") +
  theme(axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"))


#¿QUÉ DELEGACIONES DE LA CDMX TIENEN EL MAYOR NÚMERO DE INCIDENTES DE VIOLENCIA
# DE GÉNERO?
recuento_delegaciones <- linea.mujeres %>% filter(ESTADO_HECHOS == "CIUDAD DE MÉXICO") %>%
  group_by(MUNICIPIO_HECHOS) %>%
  tally()
kable(recuento_delegaciones, booktabs = T) %>% kable_styling(latex_options = "striped")


shape_cdmx <- readOGR(dsn = "alcaldias.shp", layer = "alcaldias")
shp_df <- broom::tidy(shape_cdmx)
id=as.character(c(9,7,1,3,13,14,2,8,15,5,4,0,6,12,11,10))
recuento_delegaciones=cbind(recuento_delegaciones, id)
shp_df <- left_join(shp_df, recuento_delegaciones, by="id")
names(shp_df)[names(shp_df) == "n"] <- "Denuncias"
map <- ggplot() + 
  geom_polygon(data = shp_df, 
               aes(x = long, y = lat, group = group, fill=Denuncias), 
               colour = "black")+
  labs(title="Reportes de violencia hechos por mujeres en la CDMX",
       subtitle = "A partir de 2017",
       caption = "Fuente: Gobierno de la CDMX")+
  theme(legend.title = "Llamadas recibidas")
map + theme_void()
