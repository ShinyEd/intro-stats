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
    
    pop = parent()
    hist(pop, plot=FALSE)
    
  })
  
  # plot 2
  output$sample.dist = renderPlot({ 
    
    par(mfrow=c(3,3))
    x = samples()
    
    par(mfrow=c(2,4))
    for(i in 1:8){
      BHH2::dotPlot(x[,i], col = COL[2,3], 
                    main = paste("Sample",i), cex.main = 1.5,
                    xlab = "", pch=19, cex.axis = 1.5,
                    ylim = c(0,2), xlim = c(min(0,x),max(1,x)))
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
    
    
    hist(ndist, main=paste("Distribution of means of ", k, 
                           " random samples, each\nconsisting of ", n, 
                           " observations from a Binomial", sep=""), 
         xlab="Sample means", freq=FALSE, ylim=c(0, max(ndens$y, nhist$density)),
         col=COL[2,3], border = "white", cex.main = 1.5)
    legend_pos = ifelse(m_samp > 40, "topleft", "topright")
    legend(legend_pos, inset = 0.025, 
           legend=bquote(atop("mean of " ~ bar(x)==.(m_samp),"sd of " ~ bar(x) ~ "(SE)" ==.(sd_samp))), 
           bty = "n", cex = 1.5, text.col = COL[2], text.font = 2)
    
    lines(ndens, col=COL[2], lwd=3)
    box()
    
  })
  
  # text
  output$sampling.descr = renderText({
    
    k = input$k
    n = input$n
    paste("Distribution of means of", k, "random samples,\n
          each consisting of", n, " observations\n
          from a Binomial distribution.")
  })
  
  # text
  output$CLT.descr = renderText({
    
    n = input$n ; p = input$p ; q = 1-p
    
    pop = parent()
    m_pop =  p
    sd_pop = round(sqrt((p*q)/n),2)
    
    n = input$n
    se=round((p*(1-p))/n,2)
    paste("According to the Central Limit Theorem (CLT), the distribution of sample means 
        (the sampling distribution) should be nearly normally distributed. The mean of 
        the sampling distribution should be approximately equal to p (", m_pop, ") 
        and the standard error (the standard deviation of
        sample means) should be approximately equal to the sd of the population divided by square root of
        sample size (", sd_pop,
          "/sqrt(",n, ") =", se,").")
    
  })
  
  
  })