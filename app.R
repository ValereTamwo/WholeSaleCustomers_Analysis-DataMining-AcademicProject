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
        wellPanel(
                  textInput('email','Enter your Email',value = "dataMining@gmail.com"),textInput('pss','Enter Your PassWord',value = "2303"),
                  submitButton(text = 'Send',width = '200px',icon('th'))
        ),
        class='w-[40vw] h-[50vh] border-[1px] relative left-[13%] top-[15px] border-[#8c07da] text-[#333] font-[bold] text-[20px] font-[arial] rounded-[10px]'
        )
        ),bsModal(id = 'Login' , trigger = "logmod",size = "large" ,
                  tags$div(
                    tags$h2(icon('user',class = 'mr-[5px]'),' Welcome Back : Log In', class="relative font-[arial] w-[full] text-[#8c07da] font-[bold] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                    wellPanel(
                      textInput('email','Enter your Email',value = "dataMining@gmail.com"),textInput('pss','Enter Your PassWord',value = "2303"),
                      submitButton(text = 'Send',width = '200px',icon('th'))
                    ),
                    class='w-[40vw] h-[50vh] border-[1px] relative left-[13%] top-[15px] border-[#8c07da] text-[#333] font-[bold] text-[20px] font-[arial] rounded-[10px]')
        )
    )
  ,
tabsetPanel(
    tabPanel("Home",
             sidebarLayout(
      conditionalPanel(condition = "input.toggleSidebarPanel%2==0",
                       sidebarPanel(tags$h3('Predictive Value Form', 
                                            class="relative w-[full] h-[40px] bottom-[20px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]")
                                    ,wellPanel(selectInput('Model','Choose a predictive Model',choices = c("decision Tree"="Decision Tree","Neural Network"="Neural Network","KNN"="KNN","Bayes"="Bayes")),
                                               numericInput('Fresh','Fresh Depense',value = 2303),numericInput('Milk','Milk Depense',value = 2303),
                                               numericInput('grocery','Grocery Depense',value = 203),numericInput('Frozen','Frozen Depense',value = 2303),
                                               numericInput('detergent','Detergent Paper Depense',value = 2303),numericInput('Delis','Delicassen Depense',value = 2303),
                                              submitButton(text = 'Predict',width = '200px',icon('th'))
                                                       ),
                                    class="w-[30vw] h-[90vh] mt-[12px]")
                       )
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
           tabsetPanel(tabPanel("Original Data",
                                tags$div(class='w-full h-[30%]',
                                         tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'Original Data'),
                                         dataTableOutput("data")
                                )),
                              
                       tabPanel("Clean Data",tags$div(class='w-full h-[30%]',
                                                      tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'Clean Data'),
                                                      dataTableOutput("data1")
                       )),
                       tabPanel("TrainSet",tags$div(class='w-full h-[30%]',
                                                    tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'TrainSet'),
                                                    tags$div(class='relative left-[7vw] w-[100%] h-[100%] p-[10px] mt-[20px]',sliderInput('trainValue','Choose Train Data Size(%)',min = 24,value = 70,max = 100)),
                                                    dataTableOutput("data2")
                       )),
                       tabPanel("TestSet",  tags$div(class='w-full h-[30%]',
                                                     tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'TestSet'),
                                                     dataTableOutput("data3")
                       ))
                       )
                       )
  ,
  tabPanel("Handle Missing data",tags$div(class="",))
  ,
  tabPanel("Data Vizualization",
           sidebarLayout(
             conditionalPanel(condition = "input.toggleSidebarPanel%2==0",sidebarPanel("Select Vizualizing attributes",class="relative top-[20px] bottom-[30px]",
                                                                                       wellPanel(
                                                                                         selectInput('vizA','Select the attribute to vizualize',choices = c("Fresh"="Fresh","Milk"="Milk","Grocery"="Grocery","Frozen"="Frozen","Detergent_Paper"="Detergent_Paper","Delicassen"="Delicassen")),
                                                                                         selectInput('vizA2','Select the Y attributes for line plot',choices = c("Fresh"="Fresh","Milk"="Milk","Grocery"="Grocery","Frozen"="Frozen","Detergent_Paper"="Detergent_Paper","Delicassen"="Delicassen")),
                                                                                           #      submitButton(text = 'Predict',width = '200px',icon('th'))
                                                                                       ),
                                                                                       tags$img(src='undraw_visualization_re_1kag.svg',alt='Vizualise Data',class="w-[50vw] h-[50vh]")
                                                                                       )
                              )
             ,mainPanel(actionButton("toggleSidebarPanel", "", icon = icon("bars")),
                        tags$div(class="grid grid-cols-2 gap-4",
                                tags$div(class="w-[400px] h-[500px] border-[1px] mr-[10vw] p-[10px] rounded-[10px]",
                                         tags$h3('Histogram', 
                                                 class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                                         tags$div(class='w-full h-[50%]',
                                                  #plotOutput("distPlot")
                                                  ),
                                         tags$div(class="w-full h-[40%]",tags$h3('Analysis', 
                                                                                 class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]")
                                         )
                                         ),
                                 tags$div(class="w-[400px] h-[500px] flex flex-col gap-[4px] border-[1px] rounded-[10px]",
                                          tags$h3('Pie Chart', 
                                                  class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                                          tags$div(class='w-full h-[50%]'),
                                          tags$div(class="w-full h-[40%]",tags$h3('Analysis', 
                                                           class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]")
                                                   )
                                          ),
                                 tags$div(class="w-[400px] h-[500px] border-[1px] rounded-[10px]",
                                          tags$h3('Scatter plot', 
                                                  class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                                          tags$div(class='w-full h-[50%]'),
                                          tags$div(class="w-full h-[40%]",tags$h3('Analysis', 
                                                                                  class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]")
                                          )
                                          ),
                                 tags$div(class="w-[400px] h-[500px] border-[1px] rounded-[10px]",
                                          tags$h3('Line Plot', 
                                                  class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]"),
                                          tags$div(class='w-full h-[50%]'),
                                          tags$div(class="w-full h-[40%]",tags$h3('Analysis', 
                                                                                  class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]")
                                          )
                                          
                                          )
                                )
                                 #  tags$div(class="w-[200px] h-[400px] border-[1px] rounded-[10px]"),tags$div(class="w-200px] h-[400px] border-[1px] rounded-[10px]"))
                        
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
