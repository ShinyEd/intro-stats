#ui.R

install.packages("ggplot2")
install.packages("ellipse")
require(shiny)
require(ggplot2)
require(ellipse)


shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    "))
  ),
  
  titlePanel("Correlation Game"),
  
  fluidRow(
    column(width=4,
           selectInput("difficulty",label="Difficulty",choices=list("Easy","Medium","Hard")),
           h5(textOutput("score")),
           br(),
           checkboxGroupInput(inputId="options", label="Show:",choices=list("Averages","Standard deviation line","Ellipse")),
           br(),
           sliderInput("slider",label="The correlation between X and Y is...",min=-1,max=1,value=0,step=0.01),
           br(),
           p(textOutput("status1"),style="font-weight=500; color: #000000;"),
           h5(textOutput("status2"),style="font-weight=500; color: #00CC00;"),
           h5(textOutput("status3"),style="font-weight=500; color: #FF0000;"),
           br(),
           actionButton("submit","Submit"),
           actionButton("newplot","New Plot")
           ),
    
    column(width=8,
           plotOutput("plot1",width="400px",height="400px"))
            
    )
  
  
))
