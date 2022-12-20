#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:  
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny.tailwind)
options(shiny.launch.browser = .rs.invokeShinyWindowExternal)

# Define UI for application that draws a histogram
ui <- fluidPage(
  use_tailwind(),
  includeCSS('./style.css'),tags$header(
    tags$h2("WholeSale Customers Prediction",class='text-[20px] text-[arial] font-[bold] ml-[20px] mb-[7px] mt-[10px]',style="font-family:'arial';"),
    tags$div(class='btn-l',
      tags$button('Sign In',class='btn text-[arial]',style="color:#ab53dd; font-family:'arial';"),
      tags$button('Log In',style="font-family:'arial';",class='text-[whitesmoke] bg-[#8c07da] h-[40px] w-[100px] mr-[50px] border-[1px] p-[10px] text-[15px] rounded-[4px] mt-[10px] text-[arial]',style="font-family:'arial';")))
  ,tabsetPanel(
    tabPanel("Home",tags$div(img(src='undraw_code_thinking_re_gka2.svg',style="height:400px;width:400px;position:relative;"),class='image',tags$p('Welcome to Data World with R-Shiny',class='p-text'))
  ),
  tabPanel("data",dataTableOutput('data')
           )
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  library(ggplot2)
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
