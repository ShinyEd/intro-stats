library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Distribution Calculator"),
  
  sidebarPanel(
    #radio button or dropdown?

    selectInput(inputId = "dist",
                label = "Distribution:",
                choices = c("Normal"      = "rnorm",
                            "t"           = "rt",
                            #"Chi-Squared" = "rchisq",
                            #"F"           = "rf",
                            "Binomial"    = "rbinom"),
                selected = "rnorm"),

    br(),

    uiOutput("mean"),
    uiOutput("sd"),
    uiOutput("df1"),
    uiOutput("df2"),
    uiOutput("n"),
    uiOutput("p"),

    br(),
    br(),

    helpText("Model:"),
    uiOutput("model"),

    uiOutput("tail"),
    uiOutput("lower_bound"),
    uiOutput("upper_bound"),
    

    uiOutput("a"),
    uiOutput("b")  
  ),
  
  
  
  mainPanel(
    plotOutput("plot"),
    uiOutput("area")
  )
))