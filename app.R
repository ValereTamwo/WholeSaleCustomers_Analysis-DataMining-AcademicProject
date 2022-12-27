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
library(corrplot)

options(shiny.launch.browser = .rs.invokeShinyWindowExternal)

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
      ),tags$footer(class='w-[98vw] p-[100] h-[70px] bg-[#8c07da] relative top-[155px] text-[whitesmoke]',tags$div(class='flex  space-around flex-row', tags$p('Copyright 2022'),tags$p('DataMining'),tags$p('Classification Model')
      )
      #,
     # tags$div(class="relative left-[40vw]",
       #        img(src='undraw_code_thinking_re_gka2.svg',style="height:400px;width:400px;position:relative;"),class='image',
        #       tags$p('Welcome to Data World with R-Shiny',class='p-text'))
  )),
  tabPanel(class="text-[arial] ml-[5vw]" ,"Data",
           tabsetPanel(tabPanel("Original Data",
                                tags$div(class='w-full h-[30%]',
                                         tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'Original Data'),
                                         dataTableOutput("data")
                                )),
                              
                       tabPanel("Clean Data",tags$div(class='w-full h-[30%]',
                                                      tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'Clean Data'),
                                                      dataTableOutput("clean")
                       )),
                       tabPanel("TrainSet",tags$div(class='w-full h-[30%]',
                                                    tags$h2( class="relative w-[80vw] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'TrainSet'),
                                                    tags$div(class='relative left-[7vw] w-[100%] h-[100%] p-[10px] mt-[20px]',
                                                             sliderInput('traine','Choose Train Data Size(%)',min = 24,value = 70,max = 100)),
                                                    dataTableOutput("train")
                       )),
                       tabPanel("TestSet",  tags$div(class='w-full h-[30%]',
                                                     tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'TestSet'),
                                                     dataTableOutput("test")
                       ))
                       ),tags$footer(class='w-[98vw] p-[100] h-[70px] right-[5vw] bg-[#8c07da] relative top-[155px] text-[whitesmoke]',tags$div(class='flex  space-around flex-row', tags$p('Copyright 2022'),tags$p('DataMining'),tags$p('Classification Model')
                       ))
                       )
  ,
  tabPanel("Handle Missing data",sidebarLayout(sidebarPanel (class="mt-[30px]",
    wellPanel("Warning",icon('warning'),class=" text-[20px] text-[red]",
              tags$p(class="w-[full] text-[14px] text-[#333] h-[70px] justify-center items-center p-[10px]","Initially Our DataSet Has no Missing Data"),
              tags$div(class="w-[80%] left-[1.5vw] p-[10px]  relative h-[40vh] border-[1px] rounded-[10px] text-[17px] text-[#333]",tags$p("Deleting Outliers Value Will bring us missing data for Each attributes : "),
                       tags$button(id="delete",icon("trash"),"Delete Outlier",class="bg-[red] text-[white] relative left-[20px] top-[23vh] h-[40px] w-[90%] text-[center] rounded-[10px]"),
                       bsModal(id = 'outdata',title = 'Warning',trigger = 'delete',
                               wellPanel(
                                 tags$div(
                                   tags$h2(icon('warning',class = 'mr-[5px]'),'Delete Outliers', class="relative font-[arial] w-[full] text-[#8c07da] font-[bold] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                                   wellPanel(tags$p("This action will remove Outdata Value and And replace them by the mean of each attribute accordind to it region"),
                                             actionButton("conf","Validate") ),
                                   class='w-[25vw] h-[50vh] border-[1px] relative left-[13%] top-[15px] border-[#8c07da] text-[#333] font-[bold] text-[20px] font-[arial] rounded-[10px]')
                               ),textOutput("conf")
                       
                               ))
              )),mainPanel(
                tags$div(class="grid grid-cols-2 gap-4 mt-[40px]",
                         tags$div(class="w-[400px] h-[500px] border-[1px] mr-[10vw] p-[10px] rounded-[10px] ",
                                  tags$h3('Fresh BoxPlot', 
                                          class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                                  tags$div(class='w-full h-[50%]',
                                           plotOutput("boxF")
                                  ),
                                  tags$div(class="w-full h-[40%]",tags$h3('Analysis', 
                                                                          class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]"),
                                           tags$p("Outliers Value as we see start from ",class="relative top-[5px] left-[6vw] text-[#8c07da]",tags$em("35000",class="text-[red]"))
                                  )
                         ),
                         tags$div(class="w-[400px] h-[500px] flex flex-col gap-[4px] border-[1px] rounded-[10px]",
                                  tags$h3('Milk BoxPlot', 
                                          class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                                  tags$div(class='w-full h-[50%]',
                                           plotOutput("boxM")),
                                  tags$div(class="w-full h-[40%]",tags$h3('Analysis', 
                                                                          class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]"),
                                           tags$p("Outliers Value as we see start from ",class="relative top-[5px] left-[6vw] text-[#8c07da]",tags$em("15000",class="text-[red]"))
                                  )
                         ),
                         tags$div(class="w-[400px] h-[500px] border-[1px] rounded-[10px]",
                                  tags$h3('Grocery Boxplot', 
                                          class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                                  tags$div(class='w-full h-[50%]',
                                           plotOutput("boxG")),
                                  tags$div(class="w-full h-[40%]",tags$h3('Analysis', 
                                                                          class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]"),
                                           tags$p("Outliers Value as we see start from ",class="relative top-[5px] left-[6vw] text-[#8c07da]",tags$em("24000",class="text-[red]"))
                                  )
                         ),
                         tags$div(class="w-[400px] h-[500px] border-[1px] rounded-[10px]",
                                  tags$h3('Detergent_Paper BoxPlot', 
                                          class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]"),
                                  tags$div(class='w-full h-[50%]',
                                           plotOutput("boxD")),
                                  tags$div(class="w-full h-[40%]",tags$h3('Analysis', 
                                                                          class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]"),
                                           tags$p("Outliers Value as we see start from ",class="relative top-[5px] left-[6vw] text-[#8c07da]",tags$em("9500",class="text-[red]"))
                                  )
                                  
                         ),
                         tags$div(class="w-[400px] h-[500px] border-[1px] rounded-[10px]",
                                  tags$h3('Frozen BoxPlot', 
                                          class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]"),
                                  tags$div(class='w-full h-[50%]',
                                           plotOutput("boxFr")),
                                  tags$div(class="w-full h-[40%]",tags$h3('Analysis', 
                                                                          class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]"),
                                           tags$p("Outliers Value as we see start from ",class="relative top-[5px] left-[6vw] text-[#8c07da]",tags$em("24000",class="text-[red]"))
                                  )
                                  
                         ),
                         tags$div(class="w-[400px] h-[500px] border-[1px] rounded-[10px]",
                                  tags$h3('Delicassen BoxPlot', 
                                          class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]"),
                                  tags$div(class='w-full h-[50%]',
                                           plotOutput("boxDe")),
                                  tags$div(class="w-full h-[40%]",tags$h3('Analysis', 
                                                                          class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]"),
                                           tags$p("Outliers Value as we see start from ",class="relative top-[5px] left-[6vw] text-[#8c07da]",tags$em("24000",class="text-[red]"))
                                  )
                                  
                         )
                )
              )
    ),tags$footer(class='w-[98vw] p-[100] h-[70px] bg-[#8c07da] relative top-[155px] text-[whitesmoke]',tags$div(class='flex  space-around flex-row', tags$p('Copyright 2022'),tags$p('DataMining'),tags$p('Classification Model')
    )
    )
    )
  
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
           ),tags$footer(class='w-[98vw] p-[100] h-[70px] bg-[#8c07da] relative top-[155px] text-[whitesmoke]',tags$div(class='flex  space-around flex-row', tags$p('Copyright 2022'),tags$p('DataMining'),tags$p('Classification Model')
           )
           )
           ),
  tabPanel("Relevant Attributes",
           tabsetPanel(
    tabPanel("Statistical Analysis",
      sidebarLayout(
        sidebarPanel(wellPanel(tags$h2(class="text-[#8c07da] text-[17px]", "HeatMap Correlation plot"),
                               tags$div(class="w-full p-[10px] h-[70%] rounded-[15%] ",plotOutput("heat"),tags$div(class="w-[90%] mt-[30px] h-[30vh] rounded-[10px] border-[blue] border-[1px] p-[10px]","Analisis")
                                        )
                               )),mainPanel(
                                 tags$div(class='w-[100%] h-[30%]',
                                          tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'Position Statistics'),
                                          dataTableOutput("sum")
                                 ),
                                 tags$div(class='w-[100%] h-[30%]',
                                          tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'Dispersion Statistics'),
                                          dataTableOutput("sum2")
                                 )
                                 ),position = c("left")
                                                             ),
           tags$footer(class='w-[98vw] p-[100] h-[70px] bg-[#8c07da] relative  text-[whitesmoke]',tags$div(class='flex  space-around flex-row', tags$p('Copyright 2022'),tags$p('DataMining'),tags$p('Classification Model') ))
           ),
    tabPanel("Correlation matrix",
             tags$div(class='w-[90vw] h-[30%]',
                                           tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'Correlation matrix'),
                                           dataTableOutput("data5")
    ),
    tags$div(class='w-[90vw] h-[30%]',
             tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'Correlation matrix with Value >= 0.5'),
             dataTableOutput("data6")
    )
    )
    ))
  ,
  tabPanel("About",
           sidebarLayout(sidebarPanel('Why is this WholeSale Customers Data has been collected?' , class='relative top-[5vh] text-[#333]',wellPanel(class="p-[10px] relative top-[30px]",
                                                                                                                              # tags$div(class="w-[400px] h-[500px] border-[1px] mr-[10vw] p-[10px] rounded-[10px]",
                                                                                                                                        tags$h3('Our Goal', icon("arrow-right"),
                                                                                                                                                class="relative w-[full] text-[#8c07da] text-[20px] h-[40px]   flex items-center justify-center pt-[5px] border-y-[1px]"),
                                                                                                                                        tags$div(class='w-full h-[50%] text-[#8c07da]  text-[15px]',
                                                                                                                                                 #plotOutput("distPlot")
                                                                                                                                                 "We intern to predict the possible Region of a client 
                                                                                                                                                 according to all his depenses Either his is from ",tags$em("Lisnon . Porto or Other",class="text-[blue]")
                                                                                                                                        ),
                                                                                                                              tags$div(class='w-full h-[50%] p-[10px] border-[1px] text-[#333] text-[15px]',
p("Attribute Information",class='[mb-[20px]') ,

p(" FRESH: annual spending (m.u.) on fresh products (Continuous)"),
p("MILK: annual spending (m.u.) on milk products (Continuous)"),
 p("GROCERY: annual spending (m.u.)on grocery products (Continuous)"),
 p("FROZEN: annual spending (m.u.)on frozen products (Continuous)"),
 p("DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous)"),
 p("DELICATESSEN: annual spending (m.u.)on and delicatessen products (Continuous)"),
 p("CHANNEL: customersâ€™ Channel - Horeca (Hotel/Restaurant/CafÃ©) or Retail channel (Nominal)"),
 p("REGION: customersâ€™ Region â€“ Lisnon, Oporto or Other (Nominal)" )                                                                                                                                      #plotOutput("distPlot")
                                                                                                                                       
                                                                                                                              )
                                                                                                                              
                                                                                                                          
                                                                                                                         #      ) 
                                                                                                                               )
                                      ),mainPanel(
             tags$div(class='flex position relative gap-[30px]',
                      tags$div(class='w-[40vw] h-[55vh] border-[1px] rounded-[20px] justify-center items-center relative  top-[70px]',
                               tags$p("Who are we ?",class="relative w-[full] text-[blue] text-[15px] h-[40px]   flex items-center justify-center pt-[5px] border-y-[1px]"),
                               tags$p(" We are the Group 1 of DataMing project for INF3115 courses" ,class="relative top-[40px] left-[50px]"),
                               tags$img(src="arrow.png",class="w-[20vw] relative top-[40px] h-[40vh] transform rotate-[150xdeg]" )),
                      tags$div(class='w-[40vw] h-[55vh] border-[1px] rounded-[20px] justify-center items-center relative  top-[70px]',
                               tags$p("Contributors",class="relative w-[full] text-[blue] text-[15px] h-[40px]   flex items-center justify-center pt-[5px] border-y-[1px]")
                               ,tags$div(class="p-[20px] flex justify-center items-center flex-col gap-[50px] pt-[20%]",
                                         tags$div(class="w-[20vw] rounded-[5px] h-[30%] border-[1px] border-[#8c07da] flex items-center justify-center text-[blue]","TAMWO FEUWO FRANCK VALERE 20U2837"),
                                         tags$div(class="w-[20vw] rounded-[5px] h-[30%] border-[1px] border-[#8c07da] flex items-center justify-center text-[blue]","NGASSEU NDIFO LYSE PRISCILLE 20U2626"),
                                         tags$div(class="w-[20vw] rounded-[5px] h-[30%] border-[1px] border-[#8c07da] flex items-center justify-center text-[blue]","BAHAOUDDYN 19T...."))
                               ))
           )
                         )
           ,tags$footer(class='w-[98vw] p-[100] h-[70px] bg-[#8c07da] relative top-[155px] text-[whitesmoke]',tags$div(class='flex  space-around flex-row', tags$p('Copyright 2022'),tags$p('DataMining'),tags$p('Classification Model'),
                                                                                                                       
           )
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
  
  #+++++++++++++++++Handling Missing Data Vizualising BoxPlot++++++++++++++++++++++++++++++++++# 
  
  output$boxF= renderPlot({
    data = read.csv('./data/Wholesale customers data (1).csv')
    options(repr.plot.width=9,repr.plot.height=9)
  box1= ggplot(data,aes(x=Fresh,color=Fresh))+geom_boxplot(outlier.colour = "#8c07da",outlier.shape = 16,outlier.size = 2,fill="white")
  box=box1+scale_color_brewer(palette = "Dark2")
  plot(box)
  })
  
  output$boxM= renderPlot({
    data = read.csv('./data/Wholesale customers data (1).csv')
  
    options(repr.plot.width=9,repr.plot.height=9)
    box2= ggplot(data,aes(x=Milk,color=Milk))+geom_boxplot(outlier.colour = "#8c07da",outlier.shape = 16,outlier.size = 2,fill="white")
    boxx=box2+scale_color_brewer(palette = "Dark2")
    plot(boxx)
  })
  
  output$boxG= renderPlot({
    data = read.csv('./data/Wholesale customers data (1).csv')
    
    options(repr.plot.width=9,repr.plot.height=9)
    box3= ggplot(data,aes(x=Grocery,color=Grocery))+geom_boxplot(outlier.colour = "#8c07da",outlier.shape = 16,outlier.size = 2,fill="white")
    boxxx=box3+scale_color_brewer(palette = "Dark2")
    plot(boxxx)
  })
  
  output$boxD= renderPlot({
    data = read.csv('./data/Wholesale customers data (1).csv')
    
    options(repr.plot.width=9,repr.plot.height=9)
    box4= ggplot(data,aes(x=Detergents_Paper,color=Detergents_Paper))+geom_boxplot(outlier.colour = "#8c07da",outlier.shape = 16,outlier.size = 2,fill="white")
    boxxxx=box4+scale_color_brewer(palette = "Dark2")
    plot(boxxxx)
  })
  
  output$boxFr= renderPlot({
    data = read.csv('./data/Wholesale customers data (1).csv')
    
    options(repr.plot.width=9,repr.plot.height=9)

        box5= ggplot(data,aes(x=Frozen,color=Frozen))+geom_boxplot(outlier.colour = "#8c07da",outlier.shape = 16,outlier.size = 2,fill="white")
    boxxxxx=box5+scale_color_brewer(palette = "Dark2")
    plot(boxxxxx)
  })
  
  output$boxDe= renderPlot({
    data = read.csv('./data/Wholesale customers data (1).csv')
    
    options(repr.plot.width=9,repr.plot.height=9)
    box6= ggplot(data,aes(x=Delicassen,color=Delicassen))+geom_boxplot(outlier.colour = "#8c07da",outlier.shape = 16,outlier.size = 2,fill="white")
    boxxxxxxx=box6+scale_color_brewer(palette = "Dark2")
    plot(boxxxxxxx)
  })
  #mean(iris["Sepal.Length"][iris$Sepal.Length>6,])
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
 
  
  #++++++++++++++++++++++++++++++++++++HANDLING OUTLIERS +++++++++++++++++++++++++++++++++++++++++++++++++#

cleaning = function(){
  clean = read.csv('data/Wholesale customers data (1).csv')
  #Fresh Outlier
  
  clean$Fresh[clean$Fresh>35000&clean$Region==3]=mean(clean$Fresh[clean$Region==3&clean$Fresh<35000])
  clean$Fresh[clean$Fresh>35000&clean$Region==2]=mean(clean$Fresh[clean$Region==2&clean$Fresh<35000])
  clean$Fresh[clean$Fresh>35000&clean$Region==1]=mean(clean$Fresh[clean$Region==1&clean$Fresh<35000])
  
  #MilK Outlier
  
  clean$Milk[clean$Milk>15000&clean$Region==3]=mean(clean$Milk[clean$Region==3&clean$Milk<15000])
  clean$Milk[clean$Milk>15000&clean$Region==2]=mean(clean$Milk[clean$Region==2&clean$Milk<15000])
  clean$Milk[clean$Milk>15000&clean$Region==1]=mean(clean$Milk[clean$Region==1&clean$Milk<15000])
  
  #Grocery Outlier
  
  clean$Grocery[clean$Grocery>24000&clean$Region==3]=mean(clean$Grocery[clean$Region==3&clean$Grocery<24000])
  clean$Grocery[clean$Grocery>24000&clean$Region==2]=mean(clean$Grocery[clean$Region==2&clean$Grocery<24000])
  clean$Grocery[clean$Grocery>24000&clean$Region==1]=mean(clean$Grocery[clean$Region==1&clean$Grocery<24000])
  
  #Detergent_Paper
  
  clean$Detergents_Paper[clean$Detergents_Paper>9500&clean$Region==3]=mean(clean$Detergents_Paper[clean$Region==3&clean$Detergents_Paper<9500])
  clean$Detergents_Paper[clean$Detergents_Paper>9500&clean$Region==2]=mean(clean$Detergents_Paper[clean$Region==2&clean$Detergents_Paper<9500])
  clean$Detergents_Paper[clean$Detergents_Paper>9500&clean$Region==1]=mean(clean$Detergents_Paper[clean$Region==1&clean$Detergents_Paper<9500])
  
  #Frozen
  
  clean$Frozen[clean$Frozen>7000&clean$Region==3]=mean(clean$Frozen[clean$Region==3&clean$Frozen<7000])
  clean$Frozen[clean$Frozen>7000&clean$Region==2]=mean(clean$Frozen[clean$Region==2&clean$Frozen<7000])
  clean$Frozen[clean$Frozen>7000&clean$Region==1]=mean(clean$Frozen[clean$Region==1&clean$Frozen<7000])
  #Delicassen
  clean$Delicassen[clean$Delicassen>3500&clean$Region==3]=mean(clean$Delicassen[clean$Region==3&clean$Delicassen<3500])
  clean$Delicassen[clean$Delicassen>3500&clean$Region==2]=mean(clean$Delicassen[clean$Region==2&clean$Delicassen<3500])
  clean$Delicassen[clean$Delicassen>3500&clean$Region==1]=mean(clean$Delicassen[clean$Region==1&clean$Delicassen<3500])
  
  return(clean)
}
        output$conf=renderText({
        input$conf
      paste("Operation Performed Sucessfully") 
  })
  # OutPut Clean Data 
  output$clean = renderDataTable({
    cleanData = cleaning()
    
  },options =list(pageLength=5))
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  
  # Setting Train and Test Data 
  split_data=reactive({
     cleanS = cleaning()
     percent = input$traine*0.01
     nt = sample(1:nrow(cleanS),percent*nrow(cleanS))
     return(nt)
  })
  output$train = renderDataTable({
    clean = cleaning()
    nt=split_data()
    train = clean[nt,]
    
  },options =list(pageLength=5))
  
  output$test = renderDataTable({
    clean = cleaning()
    nt=split_data()
    test = clean[-nt,]    
  },options =list(pageLength=5))
    output$distPlot <- renderPlot({
       plot(iris$Sepal.Length,iris$Petal.Width)
    })
    # ---------------Matrice de correlation pour les attributs pertinents
    library(reshape2)
    dataC = read.csv('./data/Wholesale customers data (1).csv')
    cormat=round(cor(dataC),2)
    melted_cormat = melt(cormat)
    corrA = melted_cormat[melted_cormat$value>=0.5,]
    output$data5 = renderDataTable(melted_cormat, options = list(pageLength = 5))
    output$data6 = renderDataTable(corrA, options = list(pageLength = 5))
    
    # HeapMap de la Matrice des Correlations
    heat=ggplot(data = melted_cormat,aes(x = Var1,y=Var2,fill=value))+geom_tile(colour="whitesmoke")+ scale_fill_gradient2(low = "violet", high = "blue", mid = "whitesmoke", 
                                                                                                                           midpoint = 0, limit = c(-1,1), space = "Lab",
                                                                                                                           name="Pearson\nCorrelation")
    uiPlot=plot(heat)
   output$heat<-renderPlot(uiPlot)
   
   #--------------------------Resumer des Donnees ---------------
   
     attrib = c("Channel","Region","Fresh","Milk","Grocery","Frozen","Detergents_Paper","Delicassen")
     
     Channel=c(paste("variance: ",round(var(dataC$Channel)),2),paste("Ecart-Type: ",round(sd(dataC$Channel),2)),paste("Mean :",round(mean(dataC$Channel))))
     Region=c(paste("variance: ",var(dataC$Region)),paste("Ecart-Type: ",sd(dataC$Region)),paste("Mean :",mean(dataC$Region)))
     Fresh=c(paste("variance: ",var(dataC$Fresh)),paste("Ecart-Type: ",sd(dataC$Fresh)),paste("Mean :",mean(dataC$Fresh)))
     Milk=c(paste("variance: ",var(dataC$Milk)),paste("Ecart-Type: ",sd(dataC$ Milk)),paste("Mean :",mean(dataC$Milk)))
     Grocery=c(paste("variance: ",var(dataC$Grocery)),paste("Ecart-Type: ",sd(dataC$Grocery)),paste("Mean :",mean(dataC$Grocery)))
     Frozen=c(paste("variance: ",var(dataC$Frozen)),paste("Ecart-Type: ",sd(dataC$Frozen)),paste("Mean :",mean(dataC$Frozen)))
     Detergents_Paper=c(paste("variance: ",var(dataC$Detergents_Paper)),paste("Ecart-Type: ",sd(dataC$Detergents_Paper)),paste("Mean :",mean(dataC$Detergents_Paper)))
     Delicassen=c(paste("variance: ",var(dataC$Delicassen)),paste("Ecart-Type: ",sd(dataC$Delicassen)),paste("Mean :",mean(dataC$Delicassen)))
     
     df = data.frame(Channel,Region,Fresh,Milk,Grocery,Frozen,Detergents_Paper,Delicassen)

      output$sum = renderDataTable({
       summary(dataC)
     })
     output$sum2=renderDataTable(df,options = list(pageLength=5))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
