library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Central Limit Theorem for Proportions"),
  
  sidebarPanel(
    
    sliderInput("n", 
                "Sample size:", 
                value = 20,
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
                max = 1000),
    br(),
    
    helpText(a(href="https://duke.qualtrics.com/SE/?SID=SV_3L8WjmwQo32cVk9", target="_blank", "Rate this app!")),
    helpText(a(href="https://github.com/ShinyEd/ShinyEd/tree/master/CLT_prop", target="_blank", "View code")),
    helpText(a(href="http://stat.duke.edu/~mc301/shiny/applets.html", target="_blank", "Check out other apps")),
    helpText(a(href="https://www.coursera.org/course/statistics", target="_blank", "Want to learn more for free?"))
  ),
  
  
  
  mainPanel(
    plotOutput("pop.dist"),
    br(),
    plotOutput("sample.dist"),
    div(h3(textOutput("num.samples")), align = "center"),
    br(),
    plotOutput("sampling.dist"),
    div(textOutput("sampling.descr"), align = "center"),
    br(),
    div(h5(textOutput("CLT.descr"), align = "center"))
  )
))