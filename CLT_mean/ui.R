library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Central Limit Theorem for Means"),
  
  sidebarPanel(
    radioButtons("dist", "Parent distribution (population):",
                 list("Normal" = "rnorm",
                      "Uniform" = "runif",
                      "Right skewed" = "rlnorm",
                      "Left skewed" = "rbeta")),
    br(),
    
    uiOutput("mu"),
    uiOutput("sd"),
    uiOutput("min"),
    uiOutput("max"),
    uiOutput("skew"),
    
    sliderInput("n", 
                "Sample size:", 
                value = 30,
                min = 2, 
                max = 500),
    br(),
    
    sliderInput("k", 
                "Number of samples:", 
                value = 200,
                min = 10, 
                max = 1000),
    br(),
    
    helpText(a(href="https://duke.qualtrics.com/SE/?SID=SV_3L8WjmwQo32cVk9", target="_blank", "Rate this app!")),
    helpText(a(href="https://github.com/ShinyEd/ShinyEd/tree/master/CLT_mean", target="_blank", "View code")),
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