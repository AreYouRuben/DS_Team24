#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)
library(shiny)


library(dplyr)
library(ggplot2)

dataServer<-read.csv('DataLM1.csv')
data <- read.csv("DataSample.csv")
variance <- read.csv("VarianceAnalysis.csv")


linea.mujeres   <- data %>% filter(SEXO == "FEMENINO",
                                   TEMATICA_1 == "VIOLENCIA" | TEMATICA_2 == "VIOLENCIA")

server <- function(input, output,session) {
    library(shinydashboard)
    observeEvent(input$switchtab, {
        newtab <- switch(input$tabs, "one" = "two","two" = "one")
        updateTabItems(session, "tabs", newtab)
    })
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]

        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    output$output_plot <- renderPlot({ 

        datafiltered <- filter(dataServer,tematica_1==input$f)
        
        horaFr <- table(datafiltered$hora_alta)
        horaFr <- as.data.frame(horaFr)
        
        horaFr$Var1 <- as.numeric(as.character(horaFr$Var1))
        horaFr$Freq <- as.numeric(as.character(horaFr$Freq))
        
        ggplot(horaFr, aes(x =Var1, y =Freq)) +
            geom_point() +
            ggtitle("Análisis de regresión lineal por temática") +
            ylab('No. casos') +
            xlab('Hora de alta')
            
        
    }) 
    output$output_plot1 <- renderPlot({ 
        aux <-input$g
        llamadas <- as.data.frame(table(linea.mujeres[,input$g]))

        ggplot(llamadas) +
            geom_col(aes(x = Var1, y = Freq, fill = Var1)) +
            ggtitle("Edades de las mujeres que llaman por motivos de violencia") +
            theme_minimal() +
            labs(
                x = input$g,
                y = "Total de llamadas"
            )
    
        
    }) 
    output$output_plot2 <- renderPlot({
        ggplot(linea.mujeres)+
            geom_count(aes(x =linea.mujeres[,input$h] , y = linea.mujeres[,input$i]), color = "darkblue", show.legend=TRUE) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
            labs(
                 y=input$i,
                 x=input$h,
                 title="Gráfica de Conteo") +
            theme(axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"))
    })
    
    output$data_table <- renderDataTable( {variance}, 
                                          options = list(aLengthMenu = c(5,25,50),
                                                         iDisplayLength = 5)
    )
    
}


ui = 
    dashboardPage(
        
        dashboardHeader(title = "Equipo 24"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Análisis Exploratorio ", tabName = "analisisD",icon = icon("th"),badgeLabel = "Dinamico", badgeColor = "green"),
                menuItem("Análisis Exploratorio ", tabName = "analisisE",icon = icon("th"),badgeLabel = "Estatico", badgeColor = "green"),
                menuItem("Hipótesis Bayesiano", icon = icon("th"), tabName = "bayesiano",
                         badgeLabel = "Intervalos de confianza", badgeColor = "green"),
                menuItem("Hipótesis Redes Neuronales", icon = icon("th"), tabName = "redes",
                         badgeLabel = "Intervalos de confianza", badgeColor = "green"),
                menuItem("Regresión Lineal", icon = icon("th"), tabName = "regresion"),
                menuItem("Series de tiempo", icon = icon("th"), tabName = "series"),
                menuItem("Conclusiones", icon = icon("th"), tabName = "Conclusiones")
            )
            
        ),
        dashboardBody(
            tags$b(h1("Analisis de lineas de soporte en Mexico"))
            ,

            tags$div(class="container float-none",
                     tabItems(
                         tabItem(tabName = "analisisD",h2("Análisis Exploratorio Dinámico"),
                                 tags$div(class="col-sm-1 col-md-1 col-lg-1",
                                          fluidRow(
                                              tags$div(class="container",p('
                                     A continuación se observan diversos gráficos dinámicos que pueden ayudar a comprender
                                                                           de mejor manera los datos de la base de
                                                                           de datos obtenida en https://datos.cdmx.gob.mx/dataset/40d58f40-39f9-45ee-a30d-72f674fc3bf9/resource/59af003e-042e-4aeb-b4f0-8ca9a6600ec4/download/base-integrales-0702.csv
                                                                           ')),
                                             tags$div(class="container",titlePanel(h3("Gráficos de dispersión")),
                                                      selectInput("f", "Selecciona la temática a evaluar",
                                                                  choices = names(table(dataServer$tematica_1)))),
                                             tags$div(class="container",
                                              plotOutput("output_plot", width = "85%") ),
                                             
                                             tags$div(class="container",
                                                      selectInput("g", "Selecciona el valor de x",
                                                                  choices = c("EDAD","OCUPACION","ESCOLARIDAD")  ),
                                              plotOutput("output_plot1", width = "85%") ),
                                             tags$div(class="container",
                                                      selectInput("h", "Selecciona el valor de x",
                                                                  choices = c("EDAD","ESTADO_CIVIL","OCUPACION","ESCOLARIDAD","SERVICIO")  ),
                                                      selectInput("i", "Selecciona el valor de y",
                                                                  choices = c("EDAD","ESTADO_CIVIL","OCUPACION","ESCOLARIDAD","SERVICIO")  ),
                                              plotOutput("output_plot2", width = "85%") )
                                              
                                          ))
                                 ),
                         
                         tabItem(tabName = "analisisE",
                                 tags$div(class="col-sm-1 col-md-1 col-lg-1",tags$div(class="container",h1("Analisis Exploratorio Estatico")),
                                 tags$div(class="container mx-auto margin-bottom",img( src = "EdadMujeresReport.png", width = "85%"),br()),
                                 tags$div(class="container mx-auto margin-bottom",img( src = "MunicipioHechos.png", width = "85%"),br()),
                                 tags$div(class="container mx-auto margin-bottom",img( src = "ReportesCDMXGeo.png", width ="85%"),br()),
                                 tags$div(class="container mx-auto margin-bottom",img( src = "scatterCivilOcupacion.png", width = "85%"),br()),
                                 tags$div(class="container mx-autor margin-bottom",img( src = "ViolenciaMDateCuarentena.png", width = "85%"))

                                 )),
                         
                         tabItem(tabName = "bayesiano",h2("Contraste de Hipotesis Bayesiano"),
                                 tags$div(class="col-sm-1 col-md-1 col-lg-1",style="width=800",
                                          tags$div(class="container",style="width=800",h3("Justificación de variables tomadas para el clasificador Bayesiano")),
                                          tags$div(class="container",p('
                                     A CONTINUACIÓN SE PRESENTA UN ALGORITMO QUE RECIBE COMO PARÁMETROS LA OCUPACIÓN
                                     DE LA PERSONA QUE ESTÁ REALIZANDO LA LLAMADA, ASÍ COMO EL MOTIVO DE LA LLAMADA
                                     Y DEVUELVE UNA PREDICCIÓN DEL SEXO DEL USUARIO EN CUESTIÓN.
                                     UNA VEZ IMPLEMENTADO EL ALGORITMO, TRATAMOS DE EVALUAR EL NIVEL DE PRECISIÓN DE
                                     NUESTRO CLASIFICADOR MEDIANTE UNA ESTIMACIÓN DE SU PRECISIÓN MEDIA A TRAVÉS DE
                                     UN INTERVALO DE CONFIANZA A NIVEL 90%.')),
                                          tags$div(class="container",br(offset = 0.1,img( src = "LlamadasOcupacionSexo.png", width = "85%"))),
                                          tags$div(class="container",h3("  Frecuencia de llamadas por temática")),
                                          tags$div(class="container",br(offset = 0.1,img( src = "FrecuanciaTematicaSexo.png", width = "85%")))
                                          ),
                                 # fluidRow(
                                 #     titlePanel(h3("Justificación de variables tomadas para el clasificador Bayesiano")),
                                 # 
                                 #     h3('
                                 #     A CONTINUACIÓN SE PRESENTA UN ALGORITMO QUE RECIBE COMO PARÁMETROS LA OCUPACIÓN
                                 #     DE LA PERSONA QUE ESTÁ REALIZANDO LA LLAMADA, ASÍ COMO EL MOTIVO DE LA LLAMADA
                                 #     Y DEVUELVE UNA PREDICCIÓN DEL SEXO DEL USUARIO EN CUESTIÓN.
                                 #     UNA VEZ IMPLEMENTADO EL ALGORITMO, TRATAMOS DE EVALUAR EL NIVEL DE PRECISIÓN DE
                                 #     NUESTRO CLASIFICADOR MEDIANTE UNA ESTIMACIÓN DE SU PRECISIÓN MEDIA A TRAVÉS DE
                                 #     UN INTERVALO DE CONFIANZA A NIVEL 90%.'),
                                 #     br(offset = 0.1,img( src = "LlamadasOcupacionSexo.png", width = 800)),
                                 # 
                                 #     h3("  Frecuencia de llamadas por temática"),
                                 #     br(offset = 0.1,img( src = "FrecuanciaTematicaSexo.png",
                                 #                          height = 550, width = 1100))
                                 # )
                                 ),
                         
                         tabItem(tabName = "redes",h2("Contraste de Hipotesis Redes Neuronales"),
                                 tags$div(class="col-sm-1 col-md-1 col-lg-1",
                                          tags$div(class="container mx-autor margin-bottom",img( src = "red_neuronal.png",width = "85%"))
                                          )
                                 
                                 
                                 ),
                         
                         
                         tabItem(tabName = "regresion",h2("Regresion Lineal Cardiología"),
                                 tags$div(class="container",p('
                                 A continucación se muestran los resultados obtenidos a partir de la cantidad de
                                 llamadas hechas respecto a la temática de cardiología y su hora de alta. Como se puede
                                 observar en el análisis exploratorio dinámico, se pueden escoger diversas temáticas para
                                 observar su comportamiento en un diagrama de dispersión.
                                     
                                                                           ')),
                                 tags$div(class="col-sm-1 col-md-1 col-lg-1",
                                          tags$div(class="container mx-auto margin-bottom",img( src = "Scatter.png",width = "85%"),br()),
                                          tags$div(class="container mx-auto margin-bottom", h2("Análisis de varianza"),br()),
                                          tags$div(class="container mx-auto margin-bottom",dataTableOutput ("data_table"),br()),
                                          tags$div(class="container mx-auto margin-bottom",img( src = "residuals.png",width = "85%"))
                                          )
                                 


                                
                                 ),
                         
                         tabItem(tabName = "series",h2("Series de Tiempo"),
                                 tags$div(class="col-sm-1 col-md-1 col-lg-1",
                                          tags$div(class="container mx-auto margin-bottom",img( src = "llamadas_por_dia.png",width = "85%"),br()),
                                          tags$div(class="container mx-auto margin-bottom",img( src = "llamadas_por_mes.png",width = "85%"),br()),
                                          tags$div(class="container mx-auto margin-bottom",img( src = "prediccion_llamadas.png",width = "85%"),br()),
                                          )
                                 

                                 
                                 ),
                         
                         tabItem(tabName = "Conclusiones",h2("Imagenes de resultados y conclusiones"))
                     ))
                )
        ) 
        
    

# Run the application 
shinyApp(ui = ui, server = server)
