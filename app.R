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
#library(shinydashboard)
library(shinyBS)
library(corrplot)
#library(tidyverse)
library(plotly)
library(class)
library(rpart)
library(nnet)
library(neuralnet)
library(caret)
library(e1071)
library(DT)
library(arules)
library(ggplot2)
#library(arulesViz)
library("gmodels")

# charger le jeu de donnee + Preprocessing data
data = read.csv('./data/Wholesale customers data (1).csv')
cleaning = function(data){
  #Fresh Outlier
  clean = data
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
  
  clean$Region[clean$Region==1]="Lisnon"
  clean$Region[clean$Region==2]="Oporto"
  clean$Region[clean$Region==3]="Autres"
  
  return(clean)
}

options(shiny.launch.browser = .rs.invokeShinyWindowExternal)
ui <- fluidPage(  
  use_tailwind(),
  tags$style(type = "text/tailwindcss","
  .irs-single {@apply bg-pink-500 !important;}
  .irs-bar {@apply bg-gradient-to-r from-indigo-500 via-purple-500 to-pink-500 border-none !important;}
  "),
  includeCSS('./style.css'),tags$header(
    tags$h2("WholeSale Customers Prediction",class='text-[20px] text-[arial] font-[bold] ml-[20px] mb-[7px] mt-[10px]',style="font-family:'arial';"),
    tags$div(class='btn-l',
            # tags$div(class="relative right-[20vw]",submitButton(text = 'update View',width = '200px',icon('th'))),
      tags$button('Sign In',id='modal',class='btn text-[arial]',style="color:#ab53dd; font-family:'arial';"),
      
      tags$button('Log In',id='logmod',style="font-family:'arial';",class='text-[whitesmoke] bg-[#8c07da] h-[40px] w-[100px] mr-[50px] border-[1px] p-[10px] text-[15px] rounded-[4px] mt-[10px] text-[arial]',style="font-family:'arial';")
      
      ),bsModal(id = 'signIn' , trigger = "modal",size = "large" ,
                tags$div(
        tags$h2(icon('lock',class = 'mr-[5px]'),' Admin Sign In', class="relative font-[arial] w-[full] h-[40px] text-[#8c07da] font-[bold] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
        wellPanel(
                  textInput('email','Enter your Email',value = "dataMining@gmail.com"),textInput('pss','Enter Your PassWord',value = "2303"),
                  #submitButton(text = 'Send',width = '200px',icon('th'))
        ),
        class='w-[40vw] h-[50vh] border-[1px] relative left-[13%] top-[15px] border-[#8c07da] text-[#333] font-[bold] text-[20px] font-[arial] rounded-[10px]'
        )
        ),bsModal(id = 'Login' , trigger = "logmod",size = "large" ,
                  tags$div(
                    tags$h2(icon('user',class = 'mr-[5px]'),' Welcome Back : Log In', class="relative font-[arial] w-[full] text-[#8c07da] font-[bold] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                    wellPanel(
                      textInput('email','Enter your Email',value = "dataMining@gmail.com"),textInput('pss','Enter Your PassWord',value = "2303"),
                    #  submitButton(text = 'Send',width = '200px',icon('th'))
                    ),
                    class='w-[40vw] h-[50vh] border-[1px] relative left-[13%] top-[15px] border-[#8c07da] text-[#333] font-[bold] text-[20px] font-[arial] rounded-[10px]')
        )
    )
  ,
tabsetPanel(
    tabPanel("Home",
             sidebarLayout(
                       sidebarPanel(tags$h3('Predictive Value Form', 
                                            class="relative w-[full] text-[17px] text-[#8c07da] h-[40px] bottom-[20px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]")
                                    ,wellPanel(selectInput('model','Choose a predictive Model',choices = c("Decision Tree","Neural Network","KNN","SVM")),
                                              numericInput('fresh','Fresh Depense',value = 2303),
                                              numericInput('milk','Milk Depense',value = 2303),
                                               numericInput('grocery','Grocery Depense',value = 203),
                                              numericInput('frozen','Frozen Depense',value = 2303),
                                               numericInput('detergent','Detergent Paper Depense',value = 2303),
                                              numericInput('delis','Delicassen Depense',value = 2303),
                                              tags$div(class="relative right-[]",submitButton(text = 'update View',width = '200px',icon('th'))),                                                       ),
                                    class="w-[30vw] h-[90vh] mt-[12px]")
                       
      ,mainPanel(class='relative right-[4vw]',actionButton("toggleSidebarPanel", "", icon = icon("bars")),
                 tags$div(class='flex gap-[30px] relative left-[70px] bottom-[40px]' ,
                   tags$div(class='flex flex-col ',
                     tags$h3('Model Plot', class="text-[#8c07da] relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),class='w-[35vw]   h-[83vh] border-[1px] rounded-[10px] pr-[10px] pb-[10px] pl-[10px]',
                     tags$div(class='w-[90%] h-[50%] border-[1px] rounded-[10px] border-[] relative top-[5%] left-[1.5vw] ',
                           plotOutput('plotM',width = '445px',height = '300px')
                     ),                     
                     tags$div(class='w-[90%] h-[30%] border-[1px] rounded-[10px] border-[#8c07da] relative top-[10%] left-[1.5vw] ',
                              tags$h3('Predictive Result', class="relative text-[17px] text-[#8c07da] w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                             tags$div(class="flex justify-center items-center",tableOutput("restable"))
                              )
                     ),
                  tags$div(
                    tags$h3( class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px] text-[#8c07da]", 'Accuracy Calcul'),class='w-[30vw] h-[88vh] p-[10px] border-[1px] rounded-[10px]',
                    tags$div(class = " w-[27vw] h-[50%] ",
                             #tags$p(class="relative text-[#8c07da] right-[80px] mb-[10px] mt-[10px]",'Confusion Matrice'),
                   # tableOutput('matC')
                   verbatimTextOutput("matC")
                    ),
                #    tags$div(class = "items-center justify-center flex flex-col relative right-[5vw]",tags$p(class="relative text-[#8c07da] right-[80px] mb-[10px] mt-[10px]",'Accuracy'),
                 #            tags$div("Autres Accuracy :",textOutput('au')),
                  #           tags$div("Lisnon Accuracy :",textOutput('li')),
                   #          tags$div("Oporto Accuracy :",textOutput('op'))
                             
                             
                    #         ),
                   # tags$p('Final Accuracy :',class="relative left-[40px] text-[red] text-[17px]",textOutput('ACC'))
                    ))
                 )
      ),textOutput('TEST')
      ,tags$footer(class='w-[98vw] p-[100] h-[70px] bg-[#8c07da] relative top-[155px] text-[whitesmoke]',tags$div(class='flex  space-around flex-row', tags$p('Copyright 2022'),tags$p('DataMining'),tags$p('Classification Model')
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
                                        dataTableOutput('data'),downloadButton('save','Download')
                                         
                                )),
                              
                       tabPanel("Clean Data",tags$div(class='w-full h-[30%]',
                                                      tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'Clean Data'),
                                                      dataTableOutput("clean"),downloadButton('save1','Download')
                       )),
                       tabPanel("TrainSet",tags$div(class='w-full h-[30%]',
                                                    tags$h2( class="relative w-[80vw] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'TrainSet'),
                                                    tags$div(class='relative left-[7vw] w-[100%] h-[100%] p-[10px] mt-[20px]',
                                                             twSliderInput(inputId = 'traine',label = 'Choose Train Data Size(%)',min = 24,value =70,max = 100)),textOutput("value"),
                                                    tags$div(class="relative left-[20vw]",submitButton(text = 'update View',width = '150px',icon('th'))),
                                                    dataTableOutput("train")
                       )),
                       tabPanel('Normalized Data',
                                tags$div(class='w-full h-[30%]',
                                         tags$h2( class="relative w-[full] h-[70px] p-[4px] flex items-right left-[5vw] font-[arial] mt-[30px] pt-[5px] border-y-[1px] text-[#8c07da] font-[bold] text-[20px]", 'Normalize data'),
                                         dataTableOutput('dat2'),downloadButton('save2','Download')
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
                       tags$button(id="delete",icon("trash"),"View Confirmation",class="bg-[red] text-[white] relative left-[20px] top-[23vh] h-[40px] w-[90%] text-[center] rounded-[10px]"),
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
                                                                                         twSelectInput(inputId = 'attr1',label = 'Select the attribute to vizualize',choices = c("Fresh","Milk","Grocery","Frozen","Detergents_Paper","Delicassen")),
                                                                                         twSelectInput(inputId = 'attr2',label = 'Select the Y attributes for line plot',choices = c("Fresh","Milk","Grocery","Frozen","Detergents_Paper","Delicassen")),
                                                                                         tags$div(class="relative right-[]",submitButton(text = 'update View',width = '200px',icon('th'))),
                                                                                           #      submitButton(text = 'Predict',width = '200px',icon('th'))
                                                                                       ),
                                                                                       tags$img(src='undraw_visualization_re_1kag.svg',alt='Vizualise Data',class="w-[50vw] h-[50vh]")
                                                                                       )
                              )
             ,mainPanel(actionButton("toggleSidebarPanel", "", icon = icon("bars")),
                        tags$div(class="grid grid-cols-2 gap-4",
                                tags$div(class="w-[400px] h-[500px] border-[1px] mr-[10vw] p-[10px] rounded-[10px]",
                                         tags$h3('pie Chart', 
                                                 class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                                         tags$div(class='w-full h-[50%]',
                                                 plotOutput("pie")
                                              
                                                  ),
                                         tags$div(class="w-full h-[40%]",tags$h3('Analysis', 
                                                                                 class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]")
                                         )
                                         ),
                                 tags$div(class="w-[400px] h-[500px] flex flex-col gap-[4px] border-[1px] rounded-[10px]",
                                          tags$h3('Histogram', 
                                                  class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                                          tags$div(class='w-full h-[50%]',
                                                   plotOutput("my_plot")),
                                          tags$div(class="w-full h-[40%]",tags$h3('Analysis', 
                                                           class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]")
                                                   )
                                          ),
                                 tags$div(class="w-[400px] h-[500px] border-[1px] rounded-[10px]",
                                          tags$h3('Scatter plot', 
                                                  class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),
                                          tags$div(class='w-full h-[50%]',plotOutput('scat_plot')),
                                          tags$div(class="w-full h-[40%]",tags$h3('Analysis', 
                                                                                  class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]")
                                          )
                                          ),
                                 tags$div(class="w-[400px] h-[500px] border-[1px] rounded-[10px]",
                                          tags$h3('Line Plot', 
                                                  class="relative w-[full] h-[40px]  p-[4px] flex items-center justify-center pt-[15px] border-y-[1px]"),
                                          tags$div(class='w-full h-[50%]',plotOutput('line_plot')),
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
                               tags$div(class="w-full p-[10px] h-[70%] rounded-[15%] ",plotOutput("heat"),tags$div(class="w-[90%] mt-[30px] h-[40vh] rounded-[10px] border-[blue] border-[1px] p-[10px]",tags$h3(class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] text-[#8c07da] text-[17px] border-y-[1px]", "Analisis"),
                                                                                                                   tags$p("By observing the position statistics, we notice the variance of the data is very high compared to the average. The standard deviation is very far from the mean."),tags$p("On the other hand, the attribute data : ") ,tags$p("Channel and Region ",class="text-[red]"),tags$p("Vary very little. We are therefore going to Delete the Channel attribute which does not seem interesting for the Problem to be solved and Region will be used as the Class attribute") )
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
  tabPanel('Unsupervized learning',
           tabsetPanel(
             tabPanel('kmeans',
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          numericInput('clust','Select the number of clusters',min=2,value=2,step=1),
                          # h3('visualization'),
                          #numericInput('clust1','Select the number of clusters for the visualization',min=2,value=2,step=1),
                          selectInput('X','Select the X variable',choices = c("Fresh","Milk","Grocery","Frozen","Detergents_Paper","Delicassen") ),
                          selectInput('Y','Select the Y variable',choices = c("Fresh","Milk","Grocery","Frozen","Detergents_Paper","Delicassen") ),
                          submitButton(text = 'Apply',width = '200px',icon('th'))
                          
                        ),
                        mainPanel(
                          
                          tags$div(class='div',
                                   tags$div(class=' div1 ',verbatimTextOutput('print')),
                                   tags$div(class='div3',h3('clusters visualization'),plotOutput('plot1')),
                                   tags$div(class='div2',tableOutput('kmean'))
                                   
                          )
                          
                        )
                        
                      )
                      
                      
             ),
           

           tabPanel('hierachic',
                    #on identifie d'abord si nos donnees sont numeriques si non on les discretise avec as.numeric()
                    #ensuite on centre et on reduit nos donnees avec la normalisation z-score ou avec scale puis on tramnsforme en matrice
                    #on centre et on reduit pourenlever le probleme des differentes unites p e
                    #on calcul la matrice de distance entre les individus avec ?dist
                    #classification ascendante hierachique 'ward'ou 'ward.D' ou 'ward.D2'
                    #affichage du dendogramme avec materialisation des classes, border=1 a 5 pour le calcul des cardes
                    #decoupage en k groupes
                    tabsetPanel(
                     # tabPanel('Numerisation',dataTableOutput('numdata')),
                      tabPanel('Centralisation et Reduction',dataTableOutput('hier1')),
                      tabPanel('matrice de distance',verbatimTextOutput('hier2')),
                     # tabPanel('CAH',dataTableOutput('hier3')),
                     tabPanel('Dendogramme',
                              sidebarLayout(
                                sidebarPanel( numericInput('den','Select the number of clusters',min=2,value=2,step=1 ),
                                              submitButton(text = 'View',width = '200px',icon('th'))
                                              
                                ),
                                mainPanel(
                                  tags$div(class='div8',
                                           tags$div(class='div9',plotOutput('hier4')),
                                           tags$div(class='div10',tableOutput('hier5'))
                                  ),
                                  tags$div(class='div11',tableOutput('hier6'))
                                )
                              )
                              
                              
                     )
             
           ),
           tags$footer(class='w-[98vw] p-[100] h-[70px] bg-[#8c07da] relative top-[155px] text-[whitesmoke]',tags$div(class='flex  space-around flex-row', tags$p('Copyright 2022'),tags$p('DataMining'),tags$p('Classification Model')
           )
           )
    
    
  ))),
  
  tabPanel("Rules Extraction", sidebarLayout(
    sidebarPanel(tags$h3('Extract rules concluding with class Region', 
                         class="relative w-[full] h-[40px] bottom-[20px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]")
                 ,wellPanel(twSliderInput("sup","Choose support (%)",label_class = "font-bold", input_class = "rounded-md border border-2 border-stone-200",min = 20,value = 60,max = 100),
                            twSliderInput("conf","choose Conf (%)",min = 20,value = 80,max = 100),
                            tags$div(class="relative right-[]",submitButton(text = 'update View',width = '200px',icon('th'))),                                                       ),
                 class="w-[30vw] h-[60vh] mt-[12px]")
    
    ,mainPanel(class='relative right-[4vw]',actionButton("toggleSidebarPanel", "", icon = icon("bars")),
               tags$div(class='flex gap-[30px] relative left-[70px] bottom-[40px]' ,
                        tags$div(class='flex flex-col ',
                                 tags$h3('Rules', class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]"),class='w-[35vw]   h-[77vh] border-[1px] rounded-[10px] pr-[10px] pb-[10px] pl-[10px]',
                                 tags$div(class='w-[90%] h-[50%] border-[1px] rounded-[10px] border-[] relative top-[5%] left-[1.5vw] p-[10px] overflow-scroll ',
                                          dataTableOutput('rule_f')
                                 ), tags$div(class='w-[85%] h-[33%]  border-[1px] rounded-[10px] border-[] relative top-[10%] left-[1.5vw] p-[10px] ',
                                             tags$h3(class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] text-[#8c07da] text-[17px] border-y-[1px]", "Analisis (for support : 60% , confidence :80%)"),
                                          tags$p("
                                          if a customer's expenses are categorized as: "),
                                          tags$p("{Grocery=un,Frozen=un,Detergents_Paper=un}"),
                                          tags$p("{Milk=un,Grocery=un,Frozen=un,Detergents_Paper=un}"),
                                          tags$p("Milk=un,Grocery=un,Frozen=un,Detergents_Paper=un,Delicassen=un}, "),
                                          tags$p("{Grocery=un,Detergents_Paper=un} "),
                                          tags$p("{Grocery=un,Frozen=un,Detergents_Paper=un,Delicassen=un}"),
                                          tags$p( class="text-[#8c07da]", "then their region is equivalent to categorized region 3"),
                                 )
                        ),
                        tags$div(
                          tags$h3( class="relative w-[full] h-[40px] p-[4px] flex items-center justify-center pt-[5px] border-y-[1px]", 'Visualising Rules'),class='w-[30vw] h-[60vh] p-[10px] border-[1px] rounded-[10px]',
                          tags$div(class = " w-[27vw] h-[50%] justify-center items-center flex ",tags$p(class=" text-[20px] text-[#8c07da] mt-[40%] ", "The package needed for rules Visualisation can not be install (arulesViz) ")
                                   #tags$p(class="relative text-[#8c07da] right-[80px] mb-[10px] mt-[10px]",'Confusion Matrice'),
                                   # tableOutput('matC')
                               #   plotOutput("plotrule")
                          ),)
    )
    )),
    tags$footer(class='w-[98vw] p-[100] h-[70px] bg-[#8c07da] relative  text-[whitesmoke]',tags$div(class='flex  space-around flex-row', tags$p('Copyright 2022'),tags$p('DataMining'),tags$p('Classification Model'),
                                                                                                               
    )
    )
    ),
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
 p("REGION: customersâ€™ Region â€“ Lisnon, Oporto or Other (Nominal)   " )                                                                                                                                      #plotOutput("distPlot")
                                                                                                                                       
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
                                         tags$div(class="w-[20vw] rounded-[5px] h-[30%] border-[1px] border-[#8c07da] flex items-center justify-center text-[blue]","BAHAOUDDYN 19M2565"))
                               ))
           )
                         )
           ,tags$footer(class='w-[98vw] p-[100] h-[70px] bg-[#8c07da] relative top-[155px] text-[whitesmoke]',tags$div(class='flex  space-around flex-row', tags$p('Copyright 2022'),tags$p('DataMining'),tags$p('Classification Model'),
                                                                                                                       
           )
           )
  )
  ))

server <- function(input, output,session) {
  
  
  
  
  #classifiaction ascendante hierachique
  
  
  ##numerisation des donnees et suppression de l'attribut region car nominale
  #data = read.csv('./data/Wholesale customers data (1).csv')
  #hierac=data
 # hierac[,3:8]=as.numeric(c(hierac$Fresh,hierac$Milk,hierac$Grocery,hierac$Frozen,hierac$Detergents_Paper,hierac$Delicassen))
 # output$numdata=renderDataTable({hierac})
  
  #centrer et reduire les donnees
  data = read.csv('./data/Wholesale customers data (1).csv')
  hierac=data
  hierac$Channel = NULL
  hier=hierac
   #hier$Region=NULL
  hier$Channel=NULL
  hier$Fresh=((hier$Fresh-mean(hier$Fresh))/sd(hier$Fresh))
  hier$Milk=((hier$Milk-mean(hier$Milk))/sd(hier$Milk))
  hier$Grocery=((hier$Grocery-mean(hier$Grocery))/sd(hier$Grocery))
  hier$Frozen=((hier$Frozen-mean(hier$Frozen))/sd(hier$Frozen))
  hier$Detergents_Paper=((hier$Detergents_Paper-mean(hier$Detergents_Paper))/sd(hier$Detergents_Paper))
  hier$Delicassen=((hier$Delicassen-mean(hier$Delicassen))/sd(hier$Delicassen))

   
  
  #ou avec scale
  #hier1=scale(as.matrix(hier[,c(-1,-2)],center=TRUE,scale=TRUE))
  # rownames(hier1)=hier[,c(1,2)] puis on rajoute les colonnes numeriques
  output$hier1=renderDataTable({
    cendata=hier
    cendata$Region[cendata$Region==1]="Lisnon"
    cendata$Region[cendata$Region==2]="Oporto"
    cendata$Region[cendata$Region==3]="Autres"
    cendata
    },options =list(pageLength=5))
  
  #calcul des distances(euclidean,maximum,manhattan,canberra,binary,minkowski) pour la matrice de distance entre les individu
  cendata=as.matrix(hier) 
  distdata=dist(cendata,method='euclidean')
  output$hier2=renderPrint({distdata})
  
  #classification ascendante hierarchique 'single','complete','average','ward','ward.D','ward.D2'
   cahdata=hclust(distdata,method = 'ward')
  output$hier3=renderPrint({cahdata})
  
  #affichage du dendogramme avec materialisation des classes border=1 a 5 pour la couleur du cardre
  output$hier4=renderPlot({
    plot(cahdata)
    rect.hclust(cahdata,k=input$den,border=2)
    })
  
  #decoupage en k groupes ou classes
  output$hier5=renderTable({
    hier$Channel=NULL
    kcahdata=cutree(cahdata,k=input$den)
    table(kcahdata)
    })
  
 # #ajout au fichier d'origine la nouvelle variable kcahdata
  
  output$hier6=renderTable({
    kcahdata=cutree(cahdata,k=input$den)
    ajout=cbind(hierac,kcahdata)
    head(ajout)
    
  })

  
  
  ##----autre methode kmeans----##
  data = read.csv('./data/Wholesale customers data (1).csv')
  
  output$print=renderPrint({kmoy=kmeans(distdata,input$clust)
  kmoy$cluster}) 
  
  output$kmean=renderTable({ kmoy=kmeans(distdata,input$clust)
  attributes(kmoy)
  kmoy$cluster
  table(kmoy$cluster)})
  
  data_norm2 = read.csv('./data/Wholesale customers data (1).csv')
  data_norm2$Channel=NULL
  data_norm2$Region=NULL
  data_norm2$Fresh=(data_norm2$Fresh-min(data_norm2$Fresh))/(max(data_norm2$Fresh)-min(data_norm2$Fresh))
  data_norm2$Milk=(data_norm2$Milk-min(data_norm2$Milk))/(max(data_norm2$Milk)-min(data_norm2$Milk))
  data_norm2$Grocery=(data_norm2$Grocery-min(data_norm2$Grocery))/(max(data_norm2$Grocery)-min(data_norm2$Grocery))
  data_norm2$Frozen=(data_norm2$Frozen-min(data_norm2$Frozen))/(max(data_norm2$Frozen)-min(data_norm2$Frozen))
  data_norm2$Detergents_Paper=(data_norm2$Detergents_Paper-min(data_norm2$Detergents_Paper))/(max(data_norm2$Detergents_Paper)-min(data$Detergents_Paper))
  data_norm2$Delicassen=(data_norm2$Delicassen-min(data_norm2$Delicassen))/(max(data_norm2$Delicassen)-min(data_norm2$Delicassen))
  
  
  ##clusters
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    data_norm2[, c(input$X, input$Y)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clust)
  })
  
  output$plot1 <- renderPlot({
    
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    
    # par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  
  
  
  
  
  df=data[,-c(1,2)]
  #output$dataclean=renderDT({df})
  
  ##sauvegarde du jeu 
  output$save=downloadHandler(filename = function(){
    paste('data_',Sys.Date(),'.csv',sep = '') #Sys.Date() met la date du jour
  },
  content=function(file){
    write.csv(df,file)#enregistre sous forme de csv
  }
  )
  
  output$save1=downloadHandler(filename = function(){
    paste('Clean_data_',Sys.Date(),'.csv',sep = '') #Sys.Date() met la date du jour
  },
  content=function(file){
    write.csv(df,file)#enregistre sous forme de csv
  }
  )
  output$save2=downloadHandler(filename = function(){
    paste('Normalized_data_',Sys.Date(),'.csv',sep = '') #Sys.Date() met la date du jour
  },
  content=function(file){
    write.csv(df,file)#enregistre sous forme de csv
  }
  )
  
  
  ##normalisation
  data_norm =  read.csv('./data/Wholesale customers data (1).csv')
  data_norm$Channel=NULL
  data_norm$Region=NULL
  data_norm$Fresh=(data_norm$Fresh-min(data_norm$Fresh))/(max(data_norm$Fresh)-min(data_norm$Fresh))
  data_norm$Milk=(data_norm$Milk-min(data_norm$Milk))/(max(data_norm$Milk)-min(data_norm$Milk))
  data_norm$Grocery=(data_norm$Grocery-min(data_norm$Grocery))/(max(data_norm$Grocery)-min(data_norm$Grocery))
  data_norm$Frozen=(data_norm$Frozen-min(data_norm$Frozen))/(max(data_norm$Frozen)-min(data_norm$Frozen))
  data_norm$Detergents_Paper=(data_norm$Detergents_Paper-min(data_norm$Detergents_Paper))/(max(data_norm$Detergents_Paper)-min(data$Detergents_Paper))
  data_norm$Delicassen=(data_norm$Delicassen-min(data_norm$Delicassen))/(max(data_norm$Delicassen)-min(data_norm$Delicassen))
  output$dat2=renderDataTable({data_norm})


 # output$TEST=renderText(input$milk)
  
  
  # Extraction des regles 
  
  discretise = function(data){
    data = data
    data$Region=cut(data$Region,breaks=3,labels=c("un","deux","trois"))
    data$Fresh=cut(data$Fresh,breaks=3,labels=c("un","deux","trois"))
    data$Milk=cut(data$Milk,breaks=3,labels=c("un","deux","trois"))
    data$Grocery=cut(data$Grocery,breaks=3,labels=c("un","deux","trois"))
    data$Frozen=cut(data$Frozen,breaks=3,labels=c("un","deux","trois"))
    data$Detergents_Paper=cut(data$Detergents_Paper,breaks=3,labels=c("un","deux","trois"))
    data$Delicassen=cut(data$Delicassen,breaks=3,labels=c("un","deux","trois"))
    
    trans = as(data[,-1],"transactions")
    return(trans)
  }
  appriori = reactive({
    trans = discretise(data = data)
    rules = apriori(trans,parameter=list(supp=as.numeric(input$sup)*0.01,conf = as.numeric(input$conf)*0.01,target="rules"))
   # rules = apriori(trans,parameter=list(supp=0.6,conf = 0.7,target="rules"))
    ruleselect <- subset(rules, subset = rhs %pin% "Region=")
    subrules2 <- head(sort(ruleselect, by="lift"), 5)
  })
  
  output$plotrule = renderPlot({
    regle = appriori()
    plot(regle)
  })
  
  output$rule_f = renderDataTable({
    regle = appriori()
    rdf  = data.frame(
      lhs = labels(lhs(regle)),	
      rhs = labels(rhs(regle))
      )
    })
  
  
  # Remplacement des Niveau de l'attribut Region par les valeurs categorielles
  
  predValues = reactive({
    Fresh = as.numeric(input$fresh)
    Milk = as.numeric(input$milk )
    Grocery =as.numeric(input$grocery) 
    Frozen = as.numeric(input$frozen)
    Detergents_Paper = as.numeric(input$detergent)
    Delicassen = as.numeric(input$delis)
    pred = data.frame(Fresh,Milk,Grocery,Frozen,Detergents_Paper,Delicassen) 
    return(pred)
  })
  
  output$restable = renderTable({
    
    clean = cleaning(data=data)
    clean[['Region']]=factor(clean[['Region']])
    pred = predValues()
    nt=sample(1:nrow(clean),0.7*nrow(clean))
    trains=clean[nt,-1]
    tests = clean[-nt,-1]  

    if(input$model=="Decision Tree"){ 
      
    ad = rpart(Region~Fresh+Milk+Grocery+Frozen+Detergents_Paper+Delicassen,data = trains)
    predict_ = predict(ad,pred,type=c("class"))
    
    }else if(input$model=="KNN"){
      
      classe = clean[nt,2]
      predict_=knn(train = trains[,-1],test = pred,cl = classe,k=5)
      
    }else if(input$model=="Neural Network"){
      nn = neuralnet(Region~Fresh+Milk+Grocery+Frozen+Detergents_Paper+Delicassen,data=trains,hidden=3,act.fct = "logistic",
                     linear.output = FALSE)
      #model = nnet(Region~Fresh+Milk+Grocery+Frozen+Detergents_Paper+Delicassen,trains,size=3)
      predict= predict(nn,pred)
      
    }else if(input$model=="SVM"){
  #    trctrl=trainControl(method = "repeatedcv",number = 10,repeats = 3)
  #    svmLinear = train(Region~.,data = trains,method ="svmLinear",trControl=trctrl,
   #                     preProcess = c("center", "scale"),
    #                    tuneLength = 10)
     # pred_S = predict(svmLinear,newdata = pred)
      svm_= svm(Region~Fresh+Milk+Grocery+Frozen+Detergents_Paper+Delicassen,data = trains)
      pred_S = predict(svm_,newdata =pred)
    }
    
   # predict
  })
  
  
  output$confNN = renderTable({
    clean = cleaning(data=data)
    #clean[['Region']]=factor(clean[['Region']])
    nt=sample(1:nrow(clean),0.7*nrow(clean))
    trains=clean[nt,-1]
    tests = clean[-nt,-1]  
    
    if(input$model=="Neural Network"){
  
          nn = neuralnet(Region~Fresh+Milk+Grocery+Frozen+Detergents_Paper+Delicassen,data=trains,hidden=3,act.fct = "logistic",
                     linear.output = TRUE)
      # predictNN = compute(nn,tests[,-1])
      # prob = predictNN$net.result
      #pred <- ifelse(prob>0.5, 0,1)
      predd = compute(nn,tests[,-1])
      table(apply(predd, 1, which.max),tests[,1])
      #predict_N= predict(model,tests[,-1])
      
       CrossTable(predd,tests[,1])
    }
  })
  output$matC = renderPrint({
    clean = cleaning(data=data)
    clean[['Region']]=factor(clean[['Region']])
    nt=sample(1:nrow(clean),0.7*nrow(clean))
    trains=clean[nt,-1]
    tests = clean[-nt,-1]  

        if(input$model=="Decision Tree"){ 
          
      ad = rpart(Region~Fresh+Milk+Grocery+Frozen+Detergents_Paper+Delicassen,data = trains)
      predict_Acc = predict(ad,tests[,-1],type=c("class"))
      matc = table(predict_Acc,tests[,1])
      confusionMatrix(matc)
    }else if(input$model=="KNN"){
      
      classe = as.factor(clean[nt,2])
      predict_=knn(trains[,-1],tests[,-1],classe,k=5)
      matcc = table(tests[,1],predict_)
      confusionMatrix(table(predict_,tests[,1]))
    }else if(input$model=="Neural Network"){
      
    
      
    }else if(input$model=="SVM"){
      #trctrl=trainControl(method = "repeatedcv",number = 10,repeats = 3)
        #svmLinear = train(Region~.,data = trains,method ="svmLinear",trControl=trctrl,
        #                preProcess = c("center", "scale"),
         #               tuneLength = 10)
      svm_= svm(Region~Fresh+Milk+Grocery+Frozen+Detergents_Paper+Delicassen,data = trains)
      pred_S = predict(svm_,newdata = tests[,-1])
      confusionMatrix(table(pred_S,tests[,1])) 
    }
    # predict
  })
  
  # Afficher les models
  output$plotM = renderPlot({
    clean = cleaning(data = data)
    clean[['Region']]=factor(clean[['Region']])
    nt=sample(1:nrow(clean),0.7*nrow(clean))
    trains=clean[nt,-1]
    tests = clean[-nt,-1]  
    
    if(input$model=="Decision Tree"){ 
      
      ad = rpart(Region~Fresh+Milk+Grocery+Frozen+Detergents_Paper+Delicassen,data = trains)
      plot(ad)

    }else if(input$model=="KNN"){
      
      classe = as.factor(clean[nt,2])
      predict_=knn(trains[,-1],tests[,-1],classe,k=5,prob=TRUE)
      plot(predict_)
      
    }else if(input$model=="Neural Network"){
      nn = neuralnet(Region~Fresh+Milk+Grocery+Frozen+Detergents_Paper+Delicassen,data=trains,hidden=3,act.fct = "logistic",
                     linear.output = FALSE)
      plot(nn)
    }else if(input$model=="SVM"){
      svm_= svm(Region~Fresh+Milk+Grocery+Frozen+Detergents_Paper+Delicassen,data = trains)
      plot(svm_,data=trains,Fresh~Milk,slice = list())
      }
    # predict
  })
  # calcul Precision 
  output$au = renderText({
    if(input$model=="Decision Tree"){
      as.character(0.66)
    }
    else if(input$model=="KNN"){
      as.character(0.64)
    }else if(input$model=="Neural Network"){
      
    }
    else if(input$model=="SVM"){
      
    }
  })
  output$li = renderText({
    if(input$model=="Decision Tree"){
      as.character(0)
    }else if(input$model=="KNN"){
      as.character(0)
    }else if(input$model=="Neural Network"){
      
    }else if(input$model=="SVM"){
      
    }
  })
  output$op = renderText({
    if(input$model=="Decision Tree"){
      as.character(0)
    }else if(input$model=="KNN"){
      as.character(0.4)
    }else if(input$model=="Neural Network"){
      
    }else if(input$model=="SVM"){
      
    }
  })
  output$ACC = renderText({
    if(input$model=="Decision Tree"){
      paste(as.character(38.9),"%")
    }else if(input$model=="KNN"){
      paste(as.character(34.84),"%")
    }else if(input$model=="Neural Network"){
      
    }else if(input$model=="SVM"){
      
    }
    
  })
  # ---------------
  
  
  #---------------------Lyse-------------------------------------------
  
  
  
  
  #----

  #+++++++++++++++++Handling Missing Data Vizualising BoxPlot++++++++++++++++++++++++++++++++++# 
  
  output$boxF= renderPlot({
    options(repr.plot.width=9,repr.plot.height=9)
  box1= ggplot(data,aes(x=Fresh,color=Fresh))+geom_boxplot(outlier.colour = "#8c07da",outlier.shape = 16,outlier.size = 2,fill="white")
  box=box1+scale_color_brewer(palette = "Dark2")
  plot(box)
  })
  
  output$boxM= renderPlot({

    options(repr.plot.width=9,repr.plot.height=9)
    box2= ggplot(data,aes(x=Milk,color=Milk))+geom_boxplot(outlier.colour = "#8c07da",outlier.shape = 16,outlier.size = 2,fill="white")
    boxx=box2+scale_color_brewer(palette = "Dark2")
    plot(boxx)
  })
  
  output$boxG= renderPlot({

    options(repr.plot.width=9,repr.plot.height=9)
    box3= ggplot(data,aes(x=Grocery,color=Grocery))+geom_boxplot(outlier.colour = "#8c07da",outlier.shape = 16,outlier.size = 2,fill="white")
    boxxx=box3+scale_color_brewer(palette = "Dark2")
    plot(boxxx)
  })
  
  output$boxD= renderPlot({

    options(repr.plot.width=9,repr.plot.height=9)
    box4= ggplot(data,aes(x=Detergents_Paper,color=Detergents_Paper))+geom_boxplot(outlier.colour = "#8c07da",outlier.shape = 16,outlier.size = 2,fill="white")
    boxxxx=box4+scale_color_brewer(palette = "Dark2")
    plot(boxxxx)
  })
  
  output$boxFr= renderPlot({

    options(repr.plot.width=9,repr.plot.height=9)

        box5= ggplot(data,aes(x=Frozen,color=Frozen))+geom_boxplot(outlier.colour = "#8c07da",outlier.shape = 16,outlier.size = 2,fill="white")
    boxxxxx=box5+scale_color_brewer(palette = "Dark2")
    plot(boxxxxx)
  })
  
  output$boxDe= renderPlot({

    options(repr.plot.width=9,repr.plot.height=9)
    box6= ggplot(data,aes(x=Delicassen,color=Delicassen))+geom_boxplot(outlier.colour = "#8c07da",outlier.shape = 16,outlier.size = 2,fill="white")
    boxxxxxxx=box6+scale_color_brewer(palette = "Dark2")
    plot(boxxxxxxx)
  })
  #mean(iris["Sepal.Length"][iris$Sepal.Length>6,])
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
 
  
  #++++++++++++++++++++++++++++++++++++HANDLING OUTLIERS +++++++++++++++++++++++++++++++++++++++++++++++++#

output$data= renderDataTable({data},options =list(pageLength=5))
        output$conf=renderText({
        input$conf
      paste("Operation Performed Sucessfully") 
  })
  # OutPut Clean Data 
  output$clean = renderDataTable({
    cleanData = cleaning(data = data)
    
  },options =list(pageLength=5))
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  
  # Setting Train and Test Data 
  split_data=reactive({
     cleanS = cleaning(data = data)
     percent = input$traine*0.01
     nt = sample(1:nrow(cleanS),percent*nrow(cleanS))
     return(nt)
  })
  output$train = renderDataTable({
    clean = cleaning(data=data)
    nt=split_data()
    train = clean[nt,]
    
  },options =list(pageLength=5))
  
  output$test = renderDataTable({
    clean = cleaning(data=data)
    nt=split_data()
    test = clean[-nt,]    
  },options =list(pageLength=5))
    output$distPlot <- renderPlot({
       plot(iris$Sepal.Length,iris$Petal.Width)
    })
    # ---------------Matrice de correlation pour les attributs pertinents
    library(reshape2)

    cormat=round(cor(data),2)
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
     
     Channel=c(paste("variance: ",round(var(data$Channel)),2),paste("Ecart-Type: ",round(sd(data$Channel),2)),paste("Mean :",round(mean(data$Channel))))
     Region=c(paste("variance: ",var(data$Region)),paste("Ecart-Type: ",sd(data$Region)),paste("Mean :",mean(data$Region)))
     Fresh=c(paste("variance: ",var(data$Fresh)),paste("Ecart-Type: ",sd(data$Fresh)),paste("Mean :",mean(data$Fresh)))
     Milk=c(paste("variance: ",var(data$Milk)),paste("Ecart-Type: ",sd(data$ Milk)),paste("Mean :",mean(data$Milk)))
     Grocery=c(paste("variance: ",var(data$Grocery)),paste("Ecart-Type: ",sd(data$Grocery)),paste("Mean :",mean(data$Grocery)))
     Frozen=c(paste("variance: ",var(data$Frozen)),paste("Ecart-Type: ",sd(data$Frozen)),paste("Mean :",mean(data$Frozen)))
     Detergents_Paper=c(paste("variance: ",var(data$Detergents_Paper)),paste("Ecart-Type: ",sd(data$Detergents_Paper)),paste("Mean :",mean(data$Detergents_Paper)))
     Delicassen=c(paste("variance: ",var(data$Delicassen)),paste("Ecart-Type: ",sd(data$Delicassen)),paste("Mean :",mean(data$Delicassen)))
     
     df = data.frame(Channel,Region,Fresh,Milk,Grocery,Frozen,Detergents_Paper,Delicassen)

      output$sum = renderDataTable({
       summary(data)
     },options = list(pageLength=5))
     output$sum2=renderDataTable(df,options = list(pageLength=5))
     
     #-----------------------------Visualizing data ----------------------
     
     viz_pie = reactive({
       # Create a basic bar
       dat = data 
       dat$Region[dat$Region==1]="Lisnon"
       dat$Region[dat$Region==2]="Oporto"
       dat$Region[dat$Region==3]="Autres"
       pie = ggplot(dat , aes(x="", y=input$attr1, fill=Region)) + geom_bar(stat="identity", width=1)
       
       # Convert to pie (polar coordinates) and add labels
       pie = pie + coord_polar("y", start=0) 
       
       # Add color scale (hex colors)
       pie = pie + scale_fill_manual(values=c("#0797da","#5807da","#8d07da")) 
       
       # Remove labels and add title
       pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "")
       
       # Tidy up the theme
       pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                           axis.text = element_blank(),
                                           axis.ticks = element_blank(),
                                           plot.title = element_text(hjust = 0.5, color = "#666666"))
       plot(pie)
     })
     
     
     viz_his = reactive({
       dt=data[,c(input$attr1)]
       hist(dt)
     })
     
     output$my_plot <- renderPlot({
       ggplot(data = data) +
         geom_histogram(aes_string(x = input$attr1))
     })
     
     output$scat_plot <- renderPlot({
       ggplot(data = data) +
         geom_point(aes_string(x = input$attr1,y=input$attr2))
     })
     
     output$line_plot <- renderPlot({
       ggplot(data = data) +
         geom_line(aes_string(x = input$attr1,y=input$attr2))
     })
     
    output$pie=renderPlot(viz_pie())
    
    output$his=renderPlot(
    #  ggplot(data = data) +
   #   geom_histogram(aes_string(x = input$attr1))
      viz_his()
      )
    
    
    
    
  # Decision Tree Modele Prediction
    predicte = isolate({
      Fresh = input$fresh
      Milk = input$milk
      Grocery =input$grocery
      Frozen = input$frozen
      Detergents_Paper = input$detergent
      Delicassen = input$delis
      pred = data.frame(Fresh,Milk,Grocery,Frozen,Detergents_Paper,Delicassen) 
      return(pred)
    })
    
    ##kmeans
   # data$=NULL
    #----------------------------------------
    df=data[,-c(1,2)]
    output$dataclean=renderDT({df})
    
    ##sauvegarde du jeu 
    output$save=downloadHandler(filename = function(){
      paste('clean_',Sys.Date(),'.csv',sep = '') #Sys.Date() met la date du jour
    },
    content=function(file){
      write.csv(df,file)#enregistre sous forme de csv
    }
    )
    
    
    ##normalisation
    data_norm =  read.csv('./data/Wholesale customers data (1).csv')
    data_norm$Channel=NULL
    data_norm$Region=NULL
    data_norm$Fresh=(data_norm$Fresh-min(data_norm$Fresh))/(max(data_norm$Fresh)-min(data_norm$Fresh))
    data_norm$Milk=(data_norm$Milk-min(data_norm$Milk))/(max(data_norm$Milk)-min(data_norm$Milk))
    data_norm$Grocery=(data_norm$Grocery-min(data_norm$Grocery))/(max(data_norm$Grocery)-min(data_norm$Grocery))
    data_norm$Frozen=(data_norm$Frozen-min(data_norm$Frozen))/(max(data_norm$Frozen)-min(data_norm$Frozen))
    data_norm$Detergents_Paper=(data_norm$Detergents_Paper-min(data_norm$Detergents_Paper))/(max(data_norm$Detergents_Paper)-min(data$Detergents_Paper))
    data_norm$Delicassen=(data_norm$Delicassen-min(data_norm$Delicassen))/(max(data_norm$Delicassen)-min(data_norm$Delicassen))
    output$dat2=renderDataTable({data_norm})
    
    #prediction(input$model,input$milk,input$fresh,input$grocery,input$frozen,input$detergent,input$delis)
    

    
}



# Run the application 
shinyApp(ui = ui, server = server)
