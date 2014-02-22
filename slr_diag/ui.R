library(shiny)

# Define UI for OLS demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Diagnostics for simple linear regression"),
  
  sidebarPanel(
    
    radioButtons("type", "Select a trend:",
                 list("Linear up" = "linear.up",
                      "Linear down" = "linear.down",
                      "Curved up" = "curved.up",
                      "Curved down" = "curved.down",
                      "Fan-shaped" = "fan.shaped")),
    br(),
    
    checkboxInput("show.resid", "Show residuals", FALSE),
    
    br(),
    
    helpText("This applet uses ordinary least squares (OLS) to fit a regression line to the data with the selected trend. The applet is designed to help you practice evaluating whether or not the linear model is an appropriate fit to the data. The three diagnostic plots on the lower half of the page are provided to help you identify undesirable patterns in the residuals that may arise from non-linear trends in the data."),
    br(),
    
    helpText(a(href="https://duke.qualtrics.com/SE/?SID=SV_3L8WjmwQo32cVk9", target="_blank", "Rate this app!")),
    helpText(a(href="https://github.com/ShinyEd/ShinyEd/tree/master/slr_diag", target="_blank", "View code")),
    helpText(a(href="http://stat.duke.edu/~mc301/shiny/applets.html", target="_blank", "Check out other apps")),
    helpText(a(href="https://www.coursera.org/course/statistics", target="_blank", "Want to learn more for free?"))),
  
  
  
  # Show the main display
  mainPanel(
    plotOutput("scatter"),
    br(),
    br(),
    plotOutput("residuals")
  )
))
