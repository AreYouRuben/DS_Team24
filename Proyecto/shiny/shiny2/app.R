#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
dataServer<-read.csv('www/DataLM1.csv')

# Define server logic required to draw a histogram
server <- function(input, output,session) {
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
            ylab('No. casos') +
            xlab('Hora de alta')
        
    }) 
}


ui = 
    dashboardPage(
        dashboardHeader(title = "Equipo 24"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Analisis Exploratorio ", tabName = "analisisD",icon = icon("th"),badgeLabel = "Dinamico", badgeColor = "green"),
                menuItem("Analisis Exploratorio ", tabName = "analisisE",icon = icon("th"),badgeLabel = "Estatico", badgeColor = "green"),
                menuItem("Hipotesis Bayesiano", icon = icon("th"), tabName = "bayesiano",
                         badgeLabel = "Intervalos de confianza", badgeColor = "green"),
                menuItem("Hipotesis Redes Neuronales", icon = icon("th"), tabName = "redes",
                         badgeLabel = "Intervalos de confianza", badgeColor = "green"),
                menuItem("Prediccion RN", icon = icon("th"), tabName = "rn"),
                menuItem("Regresion Lineal", icon = icon("th"), tabName = "regresion"),
                menuItem("Series de tiempo", icon = icon("th"), tabName = "series"),
                menuItem("Conclusiones", icon = icon("th"), tabName = "Conclusiones")
            )
            
        ),
        dashboardBody(
            tags$b(h1("Analisis de lineas de soporte en Mexico"))
            ,
            tabItems(
                tabItem(tabName = "analisisD",h2("Analisis Exploratorio Dinamico"),
                        fluidRow(
                            titlePanel(h3("Gráficos de dispersión")),
                            selectInput("f", "Selecciona el valor de x",
                                        choices = names(table(dataServer$tematica_1))),
                            
                            box(plotOutput("output_plot", height = 300, width = 460) )
                            
                        )
                        ),
                
                tabItem(tabName = "analisisE",h2("Analisis Exploratorio Estatico")),
                
                tabItem(tabName = "bayesiano",h2("Contraste de Hipotesis Bayesiano"),
                        fluidRow(
                            titlePanel(h3("Justificación de variables tomadas para el clasificador Bayesiano")),
                            
                            p(column(4, offset = 0.1,
                            'En el resumen se mencionó que las variables que iban a ser consideradas para
                            predecir el sexo del usuario eran la ocupación y la temática. para justificar
                            esta elección basta observar que existe una gran disparidad de género si hacemos
                            referencia a la ocupación o a la temática como se muestra a continuación
                            ')),
                            br(offset = 0.1,img( src = "LlamadasOcupacionSexo.png", 
                                                 height = 550, width = 1100)),
                            
                            h3("  Frecuencia de llamadas por temática"),
                            br(offset = 0.1,img( src = "FrecuanciaTematicaSexo.png", 
                                                 height = 550, width = 1100))
                            )
                        ),
                
                tabItem(tabName = "redes",h2("Contraste de Hipotesis Redes Neuronales")),
                
                tabItem(tabName = "rn",h2("Redes Neuronales")),
                
                tabItem(tabName = "regresion",h2("Regresion Lineal")),
                
                tabItem(tabName = "series",h2("Series de Tiempo")),
                
                tabItem(tabName = "Conclusiones",h2("Imagenes de resultados y conclusiones"))
            )
            
        ) ,
        
    )

# Run the application 
shinyApp(ui = ui, server = server)
