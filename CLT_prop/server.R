# Derived from https://github.com/tgouhier/climit

library(shiny)
library(openintro)
library(gridExtra)
library(BHH2)

seed = as.numeric(Sys.time())

shinyServer(function(input, output) {
  
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
    counts = c(popsize*(1-input$p), popsize*input$p)/popsize
    barplot(counts, 
            main= paste0("Population distribution: \n p = ", input$p),
            #main = "Population distribution:",
            names.arg=c(0,1),
            ylab="Relative Frequency" ,ylim=c(0,1),
            col=COL[1,2],
            cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex.names = 1.5)
    
  })
  
  # plot 2
  output$sample.dist = renderPlot({ 
    
    par(mfrow=c(3,3))
    x = samples()
    
    par(mfrow=c(2,4))
    for(i in 1:8){
      counts <- table(x[,i])
      barplot(counts, col = COL[7,2], 
                    main = paste("Sample",i),
                    xlab = "", pch=19,
                    cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex.names = 1.5,
                    ylim = c(0,1.2*max(counts)))
      
      box()
      mean_samp = round(mean(x[,i]),2)
      sd_samp = round(sd(x[,i]),2)
      if(counts[1] > counts[2]){
        legend("topright", 
        legend=bquote(atop(hat(p)[.(i)]==.(mean_samp))), 
        bty = "n", cex = 1.5, text.font = 3)        
      }
      if(counts[1] <= counts[2]){
        legend("topleft", 
               legend=bquote(atop(hat(p)[.(i)]==.(mean_samp))), 
               bty = "n", cex = 1.5, text.font = 3)        
      }

    }  
    
  })
  
  # text
  output$num.samples = renderText({
    
    k = input$k
    paste0("... continuing to Sample ",k,".")
    
  })
  
  # plot 3
  output$sampling.dist = renderPlot({
    
    n = input$n
    p = input$p
    k = input$k
    
    pop = parent()
    
    ndist = colMeans(samples())
    
    m_samp =  round(mean(ndist),2)
    sd_samp = round(sd(ndist),2)
    
    ndens=density(ndist)
    nhist=hist(ndist, plot=FALSE)
    
    
    hist(ndist, main=paste("Sampling distribution:\n",
                          "Distribution of means of ", k, 
                           " random samples, each\nconsisting of ", n, 
                           " observations from the population", sep=""), 
         xlab="Sample means", freq=FALSE, ylim=c(0, max(ndens$y, nhist$density)),
         col=COL[2,3], border = "white", 
         cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
    legend_pos = ifelse(m_samp > 40, "topleft", "topright")
    legend(legend_pos, inset = 0.025, 
           legend=bquote(atop("mean of " ~ hat(p)==.(m_samp),"sd of " ~ hat(p) ~ "(SE)" ==.(sd_samp))), 
           bty = "n", cex = 1.5, text.col = COL[2], text.font = 2)
    
    lines(ndens, col=COL[2], lwd=3)
    box()
    
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