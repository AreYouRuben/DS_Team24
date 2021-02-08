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
                menuItem("Analisis Exploratorio", tabName = "analisis",icon = icon("th")),
                menuItem("Contraste de Hipotesis", icon = icon("th"), tabName = "hipotesis",
                         badgeLabel = "Intervalos de confianza", badgeColor = "green"),
                menuItem("Conclusiones", icon = icon("th"), tabName = "Conclusiones",
                         badgeLabel = "e Imagenes de resultados", badgeColor = "green")
            )
            
        ),
        dashboardBody(
            tags$b(h1("Analisis de lineas de soporte en Mexico")),
            tabItems(
                tabItem(tabName = "analisis",div(h2("Dashboard tab content"))),
                tabItem(tabName = "hipotesis",h2("Contraste de Hipotesis")),
                tabItem(tabName = "Conclusiones",h2("Imagenes de resultados y conclusiones"))
            )
            
        ) ,
        
    )

# Run the application 
shinyApp(ui = ui, server = server)
