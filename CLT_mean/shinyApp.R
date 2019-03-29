# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(openintro)
library(gridExtra)
library(BHH2)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  # Title ----
  titlePanel("Central Limit Theorem for Means", windowTitle = "CLT for means"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        # Select distribution ----
        radioButtons("dist", "Parent distribution (population):",
                     c("Normal" = "rnorm",
                          "Uniform" = "runif",
                          "Right skewed" = "rlnorm",
                          "Left skewed" = "rbeta"),
                     selected = "rnorm"),
        hr(),
        
        # Distribution parameters / features ----
        uiOutput("mu"),
        uiOutput("sd"),
        uiOutput("minmax"),
        uiOutput("skew"),
        
        # Select sample size ----
        sliderInput("n",
                    "Sample size:", 
                    value = 30,
                    min = 2,
                    max = 500),
        br(),
        
        # Number of samples ----
        sliderInput("k",
                    "Number of samples:",
                    value = 200,
                    min = 10,
                    max = 1000)
      ),
      
      # Informational text ---- 
      wellPanel(
        helpText(a(href="https://duke.qualtrics.com/SE/?SID=SV_3L8WjmwQo32cVk9",
                   target="_blank", "Rate this app!")),
        helpText(a(href="https://github.com/ShinyEd/ShinyEd/tree/master/CLT_mean",
                   target="_blank", "View code")),
        helpText(a(href="http://stat.duke.edu/~mc301/shiny/applets.html",
                   target="_blank", "Check out other apps")),
        helpText(a(href="https://www.coursera.org/course/statistics", 
                   target="_blank", "Want to learn more for free?"))
      )
    ),
    
    mainPanel(
      # Population plot ----
      plotOutput("pop.dist"),
      br(),
      # Sample plots ----
      plotOutput("sample.dist"),
      #  Number of samples ----
      div(h4(textOutput("num.samples")), align = "center"),
      br(),
      # Sampling plot ----
      plotOutput("sampling.dist"),
      # Sampling description ----
      div(textOutput("sampling.descr"), align = "center"),
      br(),
      # CLT description ----
      div(h5(textOutput("CLT.descr"), align = "center"))
    )
  )
)

# Define server function --------------------------------------------

seed <- as.numeric(Sys.time())


server <- function(input, output, session) {
  
  # Mean slider for Normal distribution ----
  output$mu = renderUI(
    {
      # req(input$dist)
      if (input$dist == "rnorm")
      {
        sliderInput("mu",
                    "Mean",
                    value = 0,
                    min = -40,
                    max = 50)
      }
    })
  
  # SD slider for Normal distribution ----
  output$sd = renderUI(
    {
      # req(input$dist) - write-up
      if (input$dist == "rnorm")
      {
        sliderInput("sd",
                    "Standard deviation",
                    value = 20,
                    min = 1,
                    max = 30)
      }
    })
  
  # Minmax slider for Uniform distribution ----
  output$minmax = renderUI(
    {

      if (input$dist == "runif")
      {
        sliderInput("minmax",
                    "Lower and Upper Bounds",
                    value = c(5, 15),
                    min = 0,
                    max = 20)
      }
    })
  
  # skew slider for rlnorm and rbeta ----
  output$skew = renderUI(
    {

      if (input$dist == "rlnorm" | input$dist == "rbeta"){
        selectInput(inputId = "skew",
                    label = "Skew:",
                    choices = c("Low skew" = "low",
                                "Medium skew" = "med",
                                "High skew" = "high"),
                    selected = "low")
      }
    })

  # generating random samples ----
  rand_draw <- function(dist, n, mu, sd, min, max, skew){
    
    vals = NULL
    
    if (dist == "rbeta"){
      req(skew)
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
      req(mu, sd)
      vals = do.call(dist, list(n=n, mean=mu, sd=sd))
    }
    
    else if (dist == "rlnorm"){
      req(skew)
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
      req(min, max)
      vals = do.call(dist, list(n=n, min=min, max=max)) # new
    }
    return(vals)
  }
  
  rep_rand_draw = repeatable(rand_draw)
  
  parent = reactive({
    
    n_sample = 1e5
    
    return(rep_rand_draw(input$dist, n_sample, input$mu, input$sd,
                         input$minmax[1], input$minmax[2], input$skew))
  })
  
  samples = reactive({
    
    pop = parent()
    
    n = input$n
    k = input$k
    
    return(replicate(k, sample(pop, n, replace=TRUE)))
  })
  
  # plot 1 ----
  output$pop.dist = renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "Population distribution: Normal",
                      rlnorm = "Population distribution: Right skewed",
                      rbeta = "Population distribution: Left skewed",
                      runif = "Population distribution: Uniform")
    
    pop = parent()
    
    # m_pop =  round(mean(pop), 2) ????
    # sd_pop = round(sd(pop), 2) ????
    
    m_pop =  round(mean(pop))
    sd_pop = round(sd(pop))
    
    pop = tibble(samples = pop)
    pdens = density(pop$samples)

    x_range = max(pop$samples) - min(pop$samples)
    y_pos = max(pdens$y) - 0.2*max(pdens$y)
    
    if (input$dist == "rnorm"){
      
      req(input$mu)
      mu = input$mu
      
      x_pos = ifelse(mu > 0, min(-100, min(pop$samples)) + 20,
                     max(100, max(pop$samples)) - 20)
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white", fill = "deepskyblue3") +
        # geom_density() + draws a weird baseline. using stat_density() instead.
        stat_density(geom="line", color = "deepskyblue3", size = 1) +
        scale_x_continuous(limits = c(min(-100, pop$samples), max(100, pop$samples))) +
        labs(title = distname, x = "") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("mean of x", "=", bquote(.(m_pop)),
                               "\n", "sd of x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_gray(base_size = 19) + # better than doing title sizes inside theme().
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    } else if (input$dist == "runif"){
      
      x_pos = max(pop$samples) - 0.1*x_range

      ggplot(data = pop, aes(x = samples, y = ..density..)) +
        geom_histogram(bins = 45, color = "white", fill = "deepskyblue3") +
        stat_density(geom = "line", color = "deepskyblue3", size = 1) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .3))) +
        labs(title = distname, x = "") +
        annotate("text", x = x_pos, y = y_pos + 0.5*max(pdens$y),
                 label = paste("mean of x", "=", bquote(.(m_pop)),
                               "\n", "sd of x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_gray(base_size = 19) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    
    } else if (input$dist == "rlnorm"){
      
      x_pos = max(pop$samples) - 0.1*x_range
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white", fill = "deepskyblue3") +
        stat_density(geom = "line", color = "deepskyblue3", size = 1) +
        labs(title = distname, x = "") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("mean of x", "=", bquote(.(m_pop)), 
                               "\n", "sd of x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_gray(base_size = 19) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    
    } else if (input$dist == "rbeta"){
      
      x_pos = min(pop$samples) + 0.1*x_range
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white", fill = "deepskyblue3") +
        stat_density(geom = "line", color = "deepskyblue3", size = 1) +
        labs(title = distname, x = "") +
        annotate("text", x = x_pos, y = y_pos, 
                 label = paste("mean of x", "=", bquote(.(m_pop)), 
                               "\n", "sd of x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_gray(base_size = 19) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
      }
  })
  
  # plot 2 ----
  output$sample.dist = renderPlot({

    # par(mfrow=c(3,3)) ?????
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
    
  })
  
  
  # text for sample plots ----
  output$num.samples = renderText({

    k = input$k
    paste0("... continuing to Sample ",k,".")

  })

  # plot 3 ----
  output$sampling.dist = renderPlot({

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

    ndist = tibble(means = colMeans(samples()))

    m_samp =  round(mean(ndist$means),2)
    sd_samp = round(sd(ndist$means),2)
    
    ndens = density(ndist$means)
    nhist = hist(ndist$means, plot=FALSE)
    
    x_range = max(ndist$means) - min(ndist$means)
    
    y_pos = max(ndens$y) - 0.1*max(ndens$y)
    x_pos = ifelse(m_samp > 0, min(ndist$means) + 0.1*x_range, 
                   max(ndist$means) - 0.1*x_range)
      
    ggplot(data = ndist, aes(x = means, y = ..density..)) +
      geom_histogram(bins = 20, color = "white", fill = "chartreuse4") +
      stat_density(geom = "line", color = "chartreuse4", size = 1) +
      labs(title = paste("Sampling Distribution*"),
           x = "Sample means") +
      annotate("text", x = x_pos, y = y_pos,
               label = paste("mean", "=", bquote(.(m_pop)),
                             "\n", "sd", "=", bquote(.(sd_pop))),
               color = "black", size = 5) +
      theme_gray(base_size = 19) +
      theme(plot.title = element_text(hjust = 0.45),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

  })

  # description for sampling distribution plot ----
  output$sampling.descr = renderText({

    distname = switch(input$dist,
                      rnorm = "normal population",
                      rlnorm  = "right skewed population",
                      rbeta = "left skewed population",
                      runif = "uniform population")

    k = input$k
    n = input$n
    paste("*Distribution of means of", k, "random samples,\n
          each consisting of", n, " observations\n
          from a", distname)
    })

  # description for CLT ----
  output$CLT.descr = renderText({

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
  })
}
# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
