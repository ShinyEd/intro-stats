library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Central Limit Theorem for Proportions"),
  
  sidebarPanel(
    wellPanel( # add a different panel for the parameters and for the user feedback
    
    sliderInput("n", 
                "Sample size:", 
                value = 200,
                min = 2, 
                max = 1000),
    br(),
    
    sliderInput("p", 
                "Population Proportion:", 
                value = .5,
                step = .01,
                min = 0, 
                max = 1),
    br(),
    
    sliderInput("k", 
                "Number of samples:", 
                value = 1000,
                min = 10, 
                max = 1000)
  ),
  wellPanel(
    
    helpText(a(href="https://duke.qualtrics.com/SE/?SID=SV_3L8WjmwQo32cVk9", target="_blank", "Rate this app!")),
    helpText(a(href="https://github.com/ShinyEd/ShinyEd/tree/master/CLT_prop", target="_blank", "View code")),
    helpText(a(href="http://stat.duke.edu/~mc301/shiny/applets.html", target="_blank", "Check out other apps")),
    helpText(a(href="https://www.coursera.org/course/statistics", target="_blank", "Want to learn more for free?"))
  )),
  
  
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Population Distribution", br(), plotOutput("pop.dist", height = "450px")),
                tabPanel("Samples", div(h3(textOutput("num.samples")), align ="center"), br(), plotOutput("sample.dist")),
                tabPanel( "Sampling Distribution", fluidRow( column(8, br(), br(), br(), div(textOutput("CLT.descr"), align = "justify"), br()), 
                                                             column(4, br(), plotOutput("pop.dist1", height = "200px"))),
                          plotOutput("sampling.dist", height = "450px") , div(textOutput("plot.descr"), align = "center"), br())
    )
  )
))