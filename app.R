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
library(shinydashboard)
library(shinyBS)
options(shiny.launch.browser = .rs.invokeShinyWindowExternal)

# Define UI for application that draws a histogram
ui <- fluidPage(
  use_tailwind(),
  includeCSS('./style.css'),tags$header(
    tags$h2("WholeSale Customers Prediction",class='text-[20px] text-[arial] font-[bold] ml-[20px] mb-[7px] mt-[10px]',style="font-family:'arial';"),
    tags$div(class='btn-l',
      tags$button('Sign In',id='modal',class='btn text-[arial]',style="color:#ab53dd; font-family:'arial';"),
      tags$button('Log In',id='logmod',style="font-family:'arial';",class='text-[whitesmoke] bg-[#8c07da] h-[40px] w-[100px] mr-[50px] border-[1px] p-[10px] text-[15px] rounded-[4px] mt-[10px] text-[arial]',style="font-family:'arial';")
      ),bsModal(id = 'signIn' , trigger = "modal",size = "large" ,
                tags$div(
        tags$h2(icon('lock',class = 'mr-[5px]'),' Admin Sign In', class="relative font-[arial] w-[full] h-[40px] text-[#8c07da] font-[bold] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
        class='w-[40vw] h-[50vh] border-[1px] relative left-[13%] top-[15px] border-[#8c07da] text-[#333] font-[bold] text-[20px] font-[arial] rounded-[10px]')
        ),bsModal(id = 'Login' , trigger = "logmod",size = "large" ,
                  tags$div(
                    tags$h2(icon('user',class = 'mr-[5px]'),' Welcome Back : Log In', class="relative font-[arial] w-[full] text-[#8c07da] font-[bold] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                    class='w-[40vw] h-[50vh] border-[1px] relative left-[13%] top-[15px] border-[#8c07da] text-[#333] font-[bold] text-[20px] font-[arial] rounded-[10px]')
        )
    )
  ,
tabsetPanel(
    tabPanel("Home",
             sidebarLayout(
      conditionalPanel(condition = "input.toggleSidebarPanel%2==0",sidebarPanel(tags$h3('Predictive Value Form', class="relative w-[full] h-[40px] bottom-[20px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]") ,class="w-[30vw] h-[70vh] mt-[12px]"))
      ,mainPanel(class='relative right-[4vw]',actionButton("toggleSidebarPanel", "", icon = icon("bars")),
                 tags$div(class='flex gap-[30px] relative left-[70px] bottom-[40px]' ,
                   tags$div(class='flex flex-col ',
                     tags$h3('Model Plot', class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),class='w-[35vw]   h-[83vh] border-[1px] rounded-[10px] pr-[10px] pb-[10px] pl-[10px]',
                     tags$div(class='w-[90%] h-[50%] border-[1px] rounded-[10px] border-[] relative top-[5%] left-[1.5vw] ',
                             # tags$h3('', class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]")
                     ),                     
                     tags$div(class='w-[90%] h-[30%] border-[1px] rounded-[10px] border-[#8c07da] relative top-[10%] left-[1.5vw] ',
                              tags$h3('Predictive Result', class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]")
                              )
                     ),
                  tags$div(
                    tags$h3( class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]", 'Accuracy Calcul'),class='w-[30vw] h-[70vh] border-[1px] rounded-[10px]'))
                 )
      )
      #,
     # tags$div(class="relative left-[40vw]",
       #        img(src='undraw_code_thinking_re_gka2.svg',style="height:400px;width:400px;position:relative;"),class='image',
        #       tags$p('Welcome to Data World with R-Shiny',class='p-text'))
  ),
  tabPanel(class="text-[arial] ml-[5vw]" ,"Data",
           tags$div(class='w-full h-[30%]',
             tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'Original Data'),
            dataTableOutput("data")
            )
    ,tags$div(class='w-full h-[30%]',
            tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'Clean Data'),
            dataTableOutput("data1")
            ),
    tags$div(class='w-full h-[30%]',
             tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'TrainSet'),
             dataTableOutput("data2")
             ),
    tags$div(class='w-full h-[30%]',
             tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'TestSet'),
             dataTableOutput("data3")
             )
    )
  ,
  tabPanel("Handle Missing data",tags$div(class="",))
  ,
  tabPanel("Data Vizualization",
           sidebarLayout(
             conditionalPanel(condition = "input.toggleSidebarPanel%2==0",sidebarPanel("Select Vizualizing attributes",sidebarMenu()))
             ,mainPanel(actionButton("toggleSidebarPanel", "", icon = icon("bars")),
                        plotOutput("distPlot")
             )
           )
           ),
  tabPanel("Relevant Attributes")
  ,
  tabPanel("About",
           sidebarPanel('Why is this WholeSale Customers Data has been collected?' , class='mt-[10px]'),
           tags$div(class='flex position relative right-[400px] gap-[30px]', tags$div(class='w-[40vw] h-[55vh] border-[1px] rounded-[20px] justify-center items-center relative left-[9%] top-[70px]'),
                    tags$div(class='w-[40vw] h-[55vh] border-[1px] rounded-[20px] justify-center items-center relative left-[9%] top-[70px]')),
  ),tags$footer(class='w-[98vw] p-[100] h-[70px] bg-[#8c07da] relative top-[155px] text-[whitesmoke]',tags$div(class='flex  space-around flex-row', tags$p('Copyright 2022'),tags$p('DataMining'),tags$p('Classification Model')
                                                                                                               )
                )
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(ggplot2)
  #Chargement du jeu de donnees WholeSale Customers et Affichage
  
  data = read.csv('./data/Wholesale customers data (1).csv')
  output$data = renderDataTable(data,options=list(pageLength=5))
  output$data1 = renderDataTable(data,options=list(pageLength=5))
  output$data2 = renderDataTable(data,options=list(pageLength=5))
  output$data3 = renderDataTable(data,options=list(pageLength=5))
  
  
  ##
    output$distPlot <- renderPlot({
       plot(iris$Sepal.Length,iris$Petal.Width)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
