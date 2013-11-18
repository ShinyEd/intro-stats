library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Central Limit Theorem"),
  
  sidebarPanel(
    radioButtons("dist", "Parent distribution (population):",
                 list("Normal" = "rnorm",
                      "Right skewed" = "rexp",
                      "Left skewed" = "rbeta",
                      "Uniform" = "runif")),
    br(),
    
    sliderInput("mu",
                "Mean (of population distribution):",
                value = 0,
                min = -50,
                max = 50),
    
    br(),
    
    sliderInput("sd",
                "Standard deviation (of population distribution):",
                value = 20,
                min = 1,
                max = 30),
    
    br(),
    
    sliderInput("n", 
                "Sample size:", 
                value = 50,
                min = 2, 
                max = 1000),
    br(),
    
    sliderInput("k", 
                "Number of samples:", 
                value = 10,
                min = 10, 
                max = 1000),
    br(),
    helpText(a(href="http://stat.duke.edu/~mc301/shiny/CLT_mean", target="_blank", "View code"))),
  
  mainPanel(
    plotOutput("pop.dist"),
    plotOutput("sample.dist"),
    div(h3(textOutput("num.samples")), align = "center"),
    br(),
    plotOutput("sampling.dist")
  )
))