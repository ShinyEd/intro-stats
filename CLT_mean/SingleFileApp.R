# Load packages -----------------------------------------------------
library(shiny)
library(openintro)
library(gridExtra)
library(BHH2)

# Define UI ---------------------------------------------------------
ui <- pageWithSidebar(
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
)

# Define server function --------------------------------------------

seed = as.numeric(Sys.time())

server <- function(input, output) {
  
  output$mu = renderUI(
    {
      req(input$dist)
      if (input$dist == "rnorm")
      {
        sliderInput("mu",
                    "Mean",
                    value = 0,
                    min = -40,
                    max = 50)
      }
    })
  
  output$sd = renderUI(
    {
      req(input$dist)
      if (input$dist == "rnorm")
      {
        sliderInput("sd",
                    "Standard deviation",
                    value = 20,
                    min = 1,
                    max = 30)
      }
    })
  
  output$min = renderUI(
    {
      req(input$dist)
      #print("min")
      if (input$dist == "runif")
      {
        sliderInput("min",
                    "Lower Bound",
                    value = 0,
                    min = 0,
                    max = 20)
      }
    })
  
  output$max = renderUI(
    {
      req(input$dist)
      #print("max")
      if (input$dist == "runif")
      {
        sliderInput("max",
                    "Upper Bound",
                    value = 1,
                    min = 1,
                    max = 20)
      }
    })
  
  output$skew = renderUI(
    {
      req(input$dist)
      #print("skew options")
      if (input$dist == "rlnorm" | input$dist == "rbeta"){
        selectInput(inputId = "skew",
                    label = "Skew",
                    choices = c("Low skew" = "low",
                                "Medium skew" = "med",
                                "High skew" = "high"),
                    selected = "low")
      }
    })
  
  rand_draw = function(dist, n, mu, sd, min, max, skew) 
  {
    vals = NULL
    if (dist == "rbeta") {
      if (skew == "low"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=2))
      }
      else if (skew == "med"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=1.5))
      }
      else if (skew == "high"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=1)) 
      }
    }     
    else if (dist == "rnorm"){
      mean = input$mu ; sd = input$sd 
      vals = do.call(dist, list(n=n, mean=mu, sd=sd))
    }    
    else if (dist == "rlnorm"){
      if (skew == "low"){
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=.25))
      }
      else if (skew == "med"){
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=.5))
      }
      else if (skew == "high"){
        vals = do.call(dist, list(n=n, meanlog=0, sdlog=1))
      }
    }
    else if (dist == "runif"){
      vals = do.call(dist, list(n=n, min=min, max=max))
    }    
    return(vals)
  }
  
  rep_rand_draw = repeatable(rand_draw)  
  
  parent = reactive({
    req(input$dist, input$mu, input$sd)
    n = 1e5
    return(rep_rand_draw(input$dist, n, input$mu, input$sd, input$min, input$max, input$skew))
  })
  
  samples = reactive({
    req(parent())
    pop = parent()
    n = input$n
    k = input$k
    return(replicate(k, sample(pop, n, replace=TRUE)))
  })
  
  # plot 1   
  output$pop.dist = renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "Population distribution: Normal",
                      rlnorm = "Population distribution: Right skewed",
                      rbeta = "Population distribution: Left skewed",
                      runif = "Population distribution: Uniform")   
    
    pop = parent()
    m_pop =  round(mean(pop),2)
    sd_pop = round(sd(pop),2)
    mu = input$mu
    
    L = NULL
    U = NULL
    
    error = FALSE
    
    if (input$dist == "runif"){
      L = input$min
      U = input$max
      if (L > U){
        error = TRUE
      }
    }
    
    if (error)
    {
      plot(0,0,type='n',axes=FALSE,xlab="",ylab="",mar=c(1,1,1,1))
      text(0,0,"Error: Lower bound greater than upper bound.",col="red",cex=2)
    }
    else{
      
      pdens=density(pop)
      phist=hist(pop, plot=FALSE)
      if (input$dist == "rnorm"){
        hist(pop, main=distname, xlab="", freq=FALSE, xlim = c(min(-100,pop),max(100,pop)),
             ylim=c(0, max(pdens$y, phist$density)), col=COL[1,2], border = "white",
             cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
        legend_pos = ifelse(mu > 0, "topleft", "topright")
        legend(legend_pos, inset = 0.025,
               legend=bquote(atop(mu==.(round(m_pop)),sigma==.(round(sd_pop)))),
               bty = "n", cex = 1.5, text.col = COL[1], text.font = 2)
      }
      if (input$dist == "runif"){
        hist(pop, main=distname, xlab="", freq=FALSE,
             ylim=c(0, max(pdens$y, phist$density)+.5), col=COL[1,2], border = "white",
             cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
        legend_pos = ifelse(mu > 0, "topleft", "topright")
        legend(legend_pos, inset = 0.025,
               legend=bquote(atop(mu==.(round(m_pop)),sigma==.(round(sd_pop)))),
               bty = "n", cex = 1.5, text.col = COL[1], text.font = 2)
      }
      if (input$dist == "rlnorm") {
        hist(pop, main=distname,
             xlab="", freq=FALSE, ylim=c(0, max(pdens$y, phist$density)),
             col=COL[1,2], border = "white",
             cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
        legend("topright", inset = 0.025,
               legend=bquote(atop(mu==.(round(m_pop)),sigma==.(round(sd_pop)))),
               bty = "n", cex = 1.5, text.col = COL[1], text.font = 2)
      }
      if (input$dist == "rbeta"){
        hist(pop, main=distname, xlab="", freq=FALSE,
             ylim=c(0, max(pdens$y, phist$density)+.5), col=COL[1,2], border = "white",
             cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
        legend("topleft", inset = 0.025,
               legend=bquote(atop(mu==.(round(m_pop)),sigma==.(round(sd_pop)))),
               bty = "n", cex = 1.5, text.col = COL[1], text.font = 2)
      }
      lines(pdens, col=COL[1], lwd=3)
      box()
    }
  })
  
  # plot 2
  output$sample.dist = renderPlot({
    
    L = NULL ; U = NULL ; error = FALSE
    
    if (input$dist == "runif"){
      L = input$min
      U = input$max
      if (L > U){
        error = TRUE
      }
    }
    
    if (error)
      return
    
    else{
      
      par(mfrow=c(3,3))
      x = samples()
      
      par(mfrow=c(2,4))
      for(i in 1:8){
        BHH2::dotPlot(x[,i], col = COL[2,3],
                      main = paste("Sample",i),
                      xlab = "", pch=19,
                      ylim = c(0,2), xlim = c(min(-100,x),max(100,x)),
                      cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
        box()
        mean_samp = round(mean(x[,i]),2)
        sd_samp = round(sd(x[,i]),2)
        legend("topright",
               legend=bquote(atop(bar(x)[.(i)]==.(mean_samp),
                                  s[.(i)]==.(sd_samp))),
               bty = "n", cex = 1.5, text.font = 2)
        abline(v=mean_samp, col=COL[2],lwd=2)
      }
    }
  })
  
  # text
  output$num.samples = renderText({
    L = NULL ; U = NULL ; error = FALSE
    
    if (input$dist == "runif"){
      L = input$min ; U = input$max
      if (L > U){
        error = TRUE
      }
    }
    
    if (error)
      paste0()
    
    else{
      
      k = input$k
      paste0("... continuing to Sample ",k,".")
    }
  })
  
  # plot 3
  output$sampling.dist = renderPlot({
    
    L = NULL ; U = NULL ; error = FALSE
    
    if (input$dist == "runif"){
      L = input$min ; U = input$max
      if (L > U){
        error = TRUE
      }
    }
    
    if (error)
      return
    
    else{
      
      distname = switch(input$dist,
                        rnorm = "normal population",
                        rlnorm  = "right skewed population",
                        rbeta = "left skewed population",
                        runif = "uniform population")   
      
      
      n = input$n
      k = input$k
      
      pop = parent()
      
      m_pop =  round(mean(pop),2)
      sd_pop = round(sd(pop),2)
      
      ndist = colMeans(samples())
      
      m_samp =  round(mean(ndist),2)
      sd_samp = round(sd(ndist),2)
      
      ndens=density(ndist)
      nhist=hist(ndist, plot=FALSE)
      
      if (input$dist == "rnorm"){
        hist(ndist, main = paste("Sampling distribution:\nDistribution of means of ", k, 
                                 " random samples, each\nconsisting of ", n, 
                                 " observations from a ", distname, sep=""),              
             xlab="Sample means", freq=FALSE,
             xlim=c(min(-100,pop),max(100,pop)),
             ylim=c(0, max(ndens$y, nhist$density)),
             col=COL[2,2], border = "white", 
             cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
        legend_pos = ifelse(m_samp > 40, "topleft", "topright")
        legend(legend_pos, inset = 0.025, 
               legend=bquote(atop("mean of " ~ bar(x)==.(m_samp),"sd of " ~ bar(x) ~ "(SE)" ==.(sd_samp))), 
               bty = "n", cex = 1.5, text.col = COL[2,2], text.font = 2)
      }
      else{
        hist(ndist, main=paste("Distribution of means of ", k, 
                               " random samples, each\nconsisting of ", n, 
                               " observations from a ", distname, sep=""), 
             xlab="Sample means", freq=FALSE, ylim=c(0, max(ndens$y, nhist$density)),
             col=COL[2,3], border = "white", 
             cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
        legend_pos = ifelse(m_samp > 40, "topleft", "topright")
        legend(legend_pos, inset = 0.025, 
               legend=bquote(atop("mean of " ~ bar(x)==.(m_samp),"sd of " ~ bar(x) ~ "(SE)" ==.(sd_samp))), 
               bty = "n", cex = 1.5, text.col = COL[2], text.font = 2)
      }
      lines(ndens, col=COL[2], lwd=3)
      box()
    }
  })
  
  # text
  output$sampling.descr = renderText({
    
    distname = switch(input$dist,
                      rnorm = "normal population",
                      rlnorm  = "right skewed population",
                      rbeta = "left skewed population",
                      runif = "uniform population")  
    
    L = NULL ; U = NULL ; error = FALSE
    
    if (input$dist == "runif"){
      L = input$min ; U = input$max
      if (L > U){
        error = TRUE
      }
    }
    
    if (error)
      paste0()
    
    else{
      
      k = input$k
      n = input$n
      paste("Distribution of means of", k, "random samples,\n
            each consisting of", n, " observations\n
            from a", distname)
    }
    })
  
  # text
  output$CLT.descr = renderText({
    
    L = NULL ; U = NULL ; error = FALSE
    
    if (input$dist == "runif"){
      L = input$min ; U = input$max
      if (L > U){
        error = TRUE
      }
    }
    
    if (error)
      paste0()
    
    else{
      pop = parent()
      m_pop =  round(mean(pop),2)
      s_pop = round(sd(pop),2)
      
      n = input$n
      se=round(s_pop/sqrt(n),2)
      paste("According to the Central Limit Theorem (CLT), the distribution of sample means 
            (the sampling distribution) should be nearly normal. The mean of 
            the sampling distribution should be approximately equal to the population mean (", m_pop, ") 
            and the standard error (the standard deviation of
            sample means) should be approximately equal to the SD of the population divided by square root of
            sample size (", s_pop,
            "/sqrt(",n, ") =", se,").")
    }
  })
}
# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
