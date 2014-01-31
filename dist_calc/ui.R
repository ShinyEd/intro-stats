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
    div(textOutput("model"),style="text-indent:20px;font-size:125%;"),
    br(),

    uiOutput("tail"),
    uiOutput("lower_bound"),
    uiOutput("upper_bound"),
    

    uiOutput("a"),
    uiOutput("b")  
  ),
  
  
  
  mainPanel(
    plotOutput("plot"),
    div(textOutput("area"), align = "center", style="font-size:150%;")
  )
))