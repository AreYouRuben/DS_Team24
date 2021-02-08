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
            tags$div(class="container float-none",
                     tabItems(
                         tabItem(tabName = "analisisD",h2("Analisis Exploratorio Dinamico")),
                         
                         tabItem(tabName = "analisisE",h2("Analisis Exploratorio Estatico"),br(),
                                 img( src = "EdadMujeresReport.png"),br(),
                                 img( src = "MunicipioHechos.png"),br(),
                                 img( src = "ReportesCDMXGeo.png"),br(),
                                 img( src = "scatterCivilOcupacion.png"),br(),
                                 img( src = "ViolenciaMDateCuarentena.png")),
                         
                         tabItem(tabName = "bayesiano",h2("Contraste de Hipotesis Bayesiano")),
                         
                         tabItem(tabName = "redes",h2("Contraste de Hipotesis Redes Neuronales")),
                         
                         tabItem(tabName = "rn",h2("Redes Neuronales")),
                         
                         tabItem(tabName = "regresion",h2("Regresion Lineal")),
                         
                         tabItem(tabName = "series",h2("Series de Tiempo")),
                         
                         tabItem(tabName = "Conclusiones",h2("Imagenes de resultados y conclusiones"))
                     )),
            
            
        ) ,
        
    )

# Run the application 
shinyApp(ui = ui, server = server)
