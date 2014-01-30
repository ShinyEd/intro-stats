library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Distribution Calculator"),
  
  sidebarPanel(
    #radio button or dropdown?
    radioButtons("dist", "Distribution:",
                 list("Normal" = "rnorm",
                      "t" = "rt",
                      "Chi-Squared" = "rchisq",
                      "F" = "rf",
                      "Binomial" = "rbinom")),
    br(),
    
    uiOutput("slider.mean"),
    uiOutput("slider.sd"),
    uiOutput("x.value"),
    uiOutput("x.lower"),
    uiOutput("x.upper"),
    uiOutput("df"),
    uiOutput("df1"),
    uiOutput("df2"),
    uiOutput("f.value"),
    uiOutput("chisq.value"),
    uiOutput("n"),
    uiOutput("p"),
    uiOutput("k"),
    
    br(),
    
    uiOutput("tail")),
  
  
  
  mainPanel(
    plotOutput("plot.dist")
    #tableOutput("area")
  )
))