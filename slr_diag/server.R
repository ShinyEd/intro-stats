# Derived from http://econometricsbysimulation.shinyapps.io/OLS-App/

# set mirror
options(repos=structure(c(CRAN="http://cran.rstudio.com")))

if (!("shiny" %in% names(installed.packages()[,"Package"]))) {install.packages("shiny")}
suppressMessages(library(shiny, quietly = TRUE))

if (!("openintro" %in% names(installed.packages()[,"Package"]))) {install.packages("openintro")}
suppressMessages(library(openintro, quietly = TRUE))

if (!("plotrix" %in% names(installed.packages()[,"Package"]))) {install.packages("plotrix")}
suppressMessages(library(plotrix, quietly = TRUE))

input <- list(rseed=1)

seed = as.numeric(Sys.time())

# A function for generating the data.
draw.data <- function(type){

  n <- 250
  if(type=="linear.up"){
    x <- c(runif(n-2, 0, 4), 2, 2.1)
    y <- 2*x + rnorm(n, sd=2)
  }
  if(type=="linear.down"){
    x <- c(runif(n-2, 0, 4), 2, 2.1)
    y <- -2*x + rnorm(n, sd=2)
  }
  if(type=="curved.up"){
    x <- c(runif(n-2, 0, 4), 2, 2.1)
    y <- 2*x^4 + rnorm(n, sd=16)
  }
  if(type=="curved.down"){
    x <- c(runif(n-2, 0, 4), 2, 2.1)
    y <- -2*x^3 + rnorm(n, sd=9)
  }
  if(type=="fan.shaped"){
    x = seq(0,3.99,4/n)
    y = c(rnorm(n/8,3,1),rnorm(n/8,3.5,2),rnorm(n/8,4,2.5),rnorm(n/8,4.5,3),rnorm(n/4,5,4),rnorm((n/4)+2,6,5))
  }
  
  data.frame(x=x,y=y)
}


shinyServer(function(input, output) {
  
  mydata <- reactive({
    draw.data(input$type)
  })
  
  lmResults <- reactive({
    regress.exp <- "y~x"
    lm(regress.exp, data=mydata())
  })
  
  
  
  # Show plot of points, regression line, residuals
  output$scatter <- renderPlot({
    data1 <- mydata()
    x <- data1$x
    y <- data1$y
    
    #used for confidence interval
    xcon <- seq(min(x)-.1, max(x)+.1, .025)
    
    predictor <- data.frame(x=xcon)
    
    yhat <- predict(lmResults())    
    yline <- predict(lmResults(), predictor)
    
    par(cex.main=1.5, cex.lab=1.5, cex.axis=1.5, mar = c(4,4,4,1))
 
    r.squared = round(summary(lmResults())$r.squared, 4)
    corr.coef = round(sqrt(r.squared), 4)
    
    plot(c(min(x),max(x)) 
         ,c(min(y,yline),max(y,yline)), 
         type="n",
         xlab="x",
         ylab="y",
         main=paste0("Regression Model\n","(R = ", corr.coef,", ", "R-squared = ", r.squared,")"))
         

    newx <- seq(min(data1$x), max(data1$x), length.out=400)
    confs <- predict(lmResults(), newdata = data.frame(x=newx), 
                     interval = 'confidence')
    preds <- predict(lmResults(), newdata = data.frame(x=newx), 
                     interval = 'predict')

    polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = grey(.95), border = NA)
    polygon(c(rev(newx), newx), c(rev(confs[ ,3]), confs[ ,2]), col = grey(.75), border = NA)
    
    points(x,y,pch=19, col=COL[1,2])
    lines(xcon, yline, lwd=2, col=COL[1])
    
    if (input$show.resid) for (j in 1:length(x)) 
      lines(rep(x[j],2), c(yhat[j],y[j]), col=COL[4])
    
    legend_pos = ifelse(lmResults()$coefficients[1] < 1, "topleft", "topright")
    if(input$type == "linear.down") legend_pos = "topright"
    if(input$type == "fan.shaped") legend_pos = "topleft"   
    legend(legend_pos, inset=.05,
           legend=c("Regression Line", "Confidence Interval", "Prediction Interval"), 
           fill=c(COL[1],grey(.75),grey(.95)))
    box()
  })
  
  output$residuals <- renderPlot({
    par(mfrow=c(1,3), cex.main=2, cex.lab=2, cex.axis=2, mar=c(4,5,2,2))
    residuals = summary(lmResults())$residuals
    predicted = predict(lmResults(), newdata = data.frame(x=mydata()$x))
    plot(residuals ~ predicted, 
         main="Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals", 
         pch=19, col = COL[1,2])
    abline(h = 0, lty = 2)
    d = density(residuals)$y
    h = hist(residuals, plot = FALSE)
    hist(residuals, main="Histogram of Residuals", xlab="Residuals", 
         col=COL[1,2], prob = TRUE, ylim = c(0,max(max(d), max(h$density))))
    lines(density(residuals), col = COL[1], lwd = 2)
    qqnorm(residuals, pch=19, col = COL[1,2], main = "Normal Q-Q Plot of Residuals")
    qqline(residuals, col = COL[1], lwd = 2)
  }, height=280 )
})
