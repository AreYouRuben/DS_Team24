#Ejemplo 2. Normal con Labels

library(class)
library(dplyr)
library(stringr)

library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)

shinyUI(
    pageWithSidebar(
        headerPanel("Aplicacion básica con Shiny"),
        sidebarPanel(
            p("Crear plots con el DF 'auto'"), 
            selectInput("x", "Seleccione el valor de X",
                        choices = c('away.score','home.score'))
        ),
        mainPanel(
            
          
    #Agregando pestaÃ±as
    tabsetPanel(
        tabPanel("Plots",   #Pestaña de Plots
                 h3(textOutput("output_text")), 
                 plotOutput("output_plot",height = 700, width = 700) 
                 
        ),
        
        tabPanel("Momios",  #Pestaña de imágenes
                 img( src = "plot1.png", 
                      height = 450, width = 450),
                 img( src = "plot2.png", 
                      height = 450, width = 450)
        ), 
        tabPanel("imagenes Postwork 3",  #Pestaña de imágenes
                 img( src = "marginAwayScore.png", 
                      height = 450, width = 450),
                 img( src = "marginHomeScore.png", 
                      height = 450, width = 450),
                 img( src = "HeatmapScores.png", 
                      height = 450, width = 450),
        ), 
        
        


        tabPanel("Data Table", dataTableOutput("data_table"))
    )
)
)

)

