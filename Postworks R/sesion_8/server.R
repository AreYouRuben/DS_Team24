#Ejemplo 2. Normal con Labels  

library(shiny)
library(dplyr)
library(stringr)



dataServer<-read.csv('www/match.data.csv')
shinyServer(function(input, output) {
  
  

 output$output_text <- renderText(paste( input$x))
 
 #Gráficas  
 output$output_plot <- renderPlot({ ggplot(data = dataServer,aes(dataServer[,input$x]))+geom_bar()+facet_wrap('away.team') })


 
 #Agregando el data table
 output$data_table <- renderDataTable({dataServer}, 
                                      options = list(aLengthMenu = c(5,25,50),
                                                     iDisplayLength = 5))
                                    
       
})
