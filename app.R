#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:  
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("WholeSale Customers Prediction"),
  tabsetPanel(
    tabPanel("Home",img(src='undraw_code_thinking_re_gka2.svg')
  ),
  tabPanel("data",dataTableOutput('data')
           )
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  install.packages("ggplot2")
  #Chargement du jeu de donnees WholeSale Customers et Affichage
  
  data = read.csv('./data/Wholesale customers data (1).csv')
  output$data = renderDataTable(data,options=list(pageLength=5))
  
  ##
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
