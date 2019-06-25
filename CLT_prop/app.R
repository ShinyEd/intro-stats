# Load packages -----------------------------------------------------
library(shiny)
library(gridExtra)
library(tidyverse)

# Define UI ---------------------------------------------------------

ui <-shinyUI(fluidPage(
  
  titlePanel("Central Limit Theorem for Proportions", windowTitle = "CLT for Proportions"),
  
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
    
    helpText(a(href="https://github.com/ShinyEd/ShinyEd/tree/master/CLT_prop", target="_blank", "View code")),
    helpText(a(href="http://shinyed.github.io/intro-stats", target="_blank", "Check out other apps")),
    helpText(a(href="https://openintro.org", target="_blank", "Learn more for free!"))
   
  ),
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Population Distribution", br(), plotOutput("pop.dist", height = "450px")),
                tabPanel("Samples", br(), br(), plotOutput("sample.dist"), br(), div(h3(textOutput("num.samples")), align ="center")),
                tabPanel( "Sampling Distribution", 
                          fluidRow( column(8, br(), br(), div(textOutput("CLT.descr"), align = "justify"), br()), 
                          column(4, br(), plotOutput("pop.dist1", height = "200px"))),
                          plotOutput("sampling.dist") , div(textOutput("plot.descr"), align = "center"), br())
    )
  )
))

# Define server function --------------------------------------------
seed = as.numeric(Sys.time())

server <- shinyServer(function(input, output) {
  
  rand_draw = function(n, p) 
  {
    vals = NULL
    vals = do.call(rbinom, list(n=n, size=1, prob=p))      
    return(vals)
  }
  
  rep_rand_draw = repeatable(rand_draw)  
  
  parent = reactive({
    n = 1e5
    return(rep_rand_draw(input$n, input$p))
  })
  
  samples = reactive({
    pop = parent()
    n = input$n
    k = input$k
    return(replicate(k, sample(pop, n, replace=TRUE)))
  })
  
  # plot 1   
  
  output$pop.dist = renderPlot({
    popsize = 1000
    counts = data.frame(number = c("0","1"), freq= c(popsize*(1-input$p), popsize*input$p)/popsize)
    
    
    ggplot (counts, aes(x = number, y = freq)) + geom_bar(stat = "identity", color ="#263056", fill="#007CB7") + labs( x="",  y = "Relative Frequency", 
                                                                                                                       title = paste0("Population distribution: \n p = ", input$p), size=14, face="bold") + scale_y_continuous(limits= c(0, 1)) + theme_light(base_size = 19) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) 
  })
  
  # plot 2
  
  output$sample.dist = renderPlot({ 
    
    x = samples()
    
    plot <- list()
    
    for(i in 1:8){
      df <- tibble(obs = x[,i])
      counts <- df %>% count(obs)
      
      
      plot[[i]] <- ggplot(counts, aes(x = obs, y = n)) + geom_bar(stat = "identity", color ="#263056", fill="#007CB7") +
        scale_y_continuous(limits= c(0,1.2*max(counts$n))) +
        scale_x_discrete(limits= c(0, 1)) + theme_light(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +  
              labs( x="",  y = "Counts",
              title = paste("Sample",i), size=14, face="bold")
      
      mean_samp = round(mean(x[,i]),2)
      
      sd_samp = round(sd(x[,i]),2)
      
      y_pos = max(counts$n) + 0.07*max(counts$n)
      
      
      # #added if statement to check if count 1 or count 2 are NA. this check
      # #eliminated the error messages in the app
      
      if(!is.na(counts$n[1]) & !is.na(counts$n[2])) {
        if(counts$n[1] > counts$n[2]) {
          
          plot[[i]] <- plot[[i]] +  annotate("text", x = 1, y = y_pos,
                                             label = paste(expression(hat(p)),  "=" , bquote(.(mean_samp))),
                                             color = "black", size = 3) 
        }
        else {
          
          plot[[i]] <- plot[[i]] +  annotate("text", x = 0, y = y_pos,
                                             label = paste(expression(hat(p)),  "=" , bquote(.(mean_samp))),
                                             color = "black", size = 3) 
        }}
      else {
        plot[[i]] <- plot[[i]] +  annotate("text", x = 0.5, y = y_pos,
                                           label = paste(expression(hat(p)),  "=" , bquote(.(mean_samp))),
                                           color = "black", size = 3) 
      }
    }
    grid.arrange(plot[[1]], plot[[2]],plot[[3]],plot[[4]],plot[[5]],plot[[6]],plot[[7]],plot[[8]], ncol=4)
  })
  
  # text
  output$num.samples = renderText({
    k = input$k
    paste0("... continuing to Sample ",k,".")
  })
  
  # plot 3
  
  output$pop.dist1 = renderPlot({
    popsize = 1000
    counts = data.frame(number = c("0","1"), freq= c(popsize*(1-input$p), popsize*input$p)/popsize)
    
    
    ggplot (counts, aes(x = number, y = freq)) + geom_bar(stat = "identity", color ="#263056", fill="#007CB7") + labs( x="",  y = "Relative Frequency", 
                                                                                                                       title = paste0("Population distribution: \n p = ", input$p), size=8, face="bold") + scale_y_continuous(limits= c(0, 1)) + theme_light(base_size = 8) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  
  output$sampling.dist = renderPlot({
    n = input$n
    p = input$p
    k = input$k
    pop = parent()
    ndist = tibble(means = colMeans(samples()))
    
    ndens=density(ndist$means)
    nhist=hist(ndist$means, plot=FALSE)
    
    m_samp =  round(mean(ndist$means),2)
    sd_samp = round(sd(ndist$means),2)
    
    x_range = max(ndist$means) - min(ndist$means)
    y_pos = max(ndens$y) - 0.1*max(ndens$y)
    x_pos = if_else(m_samp > 0, min(ndist$means) + 0.1*x_range, max(ndist$means) - 0.1*x_range)
    
    # minor change in the way the title is displayed
    
    ggplot(ndist, aes(x=ndist$means, y = ..density..)) +
      geom_histogram(bins = 20, color ="#263056", fill="#98DDDE") +
      stat_density(geom = "line", color = "#263056", size = 2) +
      labs(title = paste("Sampling distribution*:"), x = "Sample means", y = "") +
      annotate("text", x = x_pos, y = y_pos,
               label = paste("mean of p_hat","=", bquote(.(m_samp)),"\n", "SD of p_hat ", "=", bquote(.(sd_samp))),
               color = "black", size = 5) +
      theme_light(base_size = 17) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) 
  })
  
  # text
  output$plot.descr = renderText({
    n = input$n
    p = input$p
    k = input$k
    
    paste("*Distribution of means of ", k, 
          " random samples, each\nconsisting of ", n, 
          " observations from the population", sep="")
    
  })
  
  # text
  output$CLT.descr = renderText({
    
    n = input$n ; p = input$p ; q = 1-p
    
    pop = parent()
    m_pop =  p
    
    n = input$n
    se=round(sqrt(p*(1-p)/n),4)
    
    paste("According to the Central Limit Theorem (CLT), the distribution of sample proportions 
        (the sampling distribution) should be nearly normal. The mean of 
        the sampling distribution is approximately equal to p (", m_pop, ") 
        and the standard error (the standard deviation of
        sample proportions) should be approximately equal to the square root of probability of
        success (p) times the probability of failure (1-p) divided by the sample size (sqrt(", p, "*", q,
          "/",n, ") =", se,").")
    
  })
  
  
})

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)