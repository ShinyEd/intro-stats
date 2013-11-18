
# Derived from https://github.com/tgouhier/climit

library(shiny)
library(openintro)
library(ggplot2)
library(gridExtra)
library(BHH2)

seed = as.numeric(Sys.time())

shinyServer(function(input, output) {
  
    rand_draw = function(dist, n, mu=0, sd=1) 
    {
        vals = NULL
        if (dist == "rbeta") {
            vals = do.call(dist, list(n=n, shape1=5, shape2=1))
        }     
        else if(dist == "rnorm"){
            vals = do.call(dist, list(n=n, mean=mu, sd=sd))
        }    
        else if (dist == "rexp") {
            vals = do.call(dist, list(n=n))
        }
        else if (dist == "runif"){
            vals = do.call(dist, list(n=n))
        }
        
        return(vals)
    }
    rep_rand_draw = repeatable(rand_draw)
    
    parent = reactive({
                n = 1e5
                return(rep_rand_draw(input$dist,n, input$mu, input$sd))
             })
    
    samples = reactive({
                pop = parent()
                n = input$n
                k = input$k
                
                return(replicate(k, sample(pop, n, replace=TRUE)))
              })
  

  
    output$pop.dist = renderPlot({
        distname = switch(input$dist,
                          rnorm = "Normal population",
                          rexp = "Right skewed population",
                          rbeta = "Left skewed population",
                          runif = "Uniform population")   

        pop = parent()
        m_pop =  round(mean(pop),2)
        sd_pop = round(sd(pop),2)
       
        #plot 1   
        pdens=density(pop)
        phist=hist(pop, plot=FALSE)
        if (input$dist == "rnorm"){
          hist(pop, main=paste(distname,sep=""), 
               xlab="", freq=FALSE, xlim = c(-3,3)*input$sd, ylim=c(0, max(pdens$y, phist$density)),
               col=COL[1])
        }
        else{
          hist(pop, main=paste(distname,sep=""), 
               xlab="", freq=FALSE, ylim=c(0, max(pdens$y, phist$density)+5),
               col=COL[1])
        }
        lines(pdens, col=COL[2,2], lwd=3)
        legend("topright",legend=paste("Mean =", m_pop, "\nSD =", sd_pop), bg=COL[2,2])
        box()
    })

    #n = sample size
    output$sample.dist = renderPlot({    
        par(mfrow=c(3,3))
        x = samples()

        for(i in 1:9){
            BHH2::dotPlot(x[,i], col = COL[2,3], 
                    main = paste("Sample",i),  xlab = "", pch=19)
            box()
            legend("topright","x_bar")
        }   
    })

  
    output$sampling.dist = renderPlot({
      
        distname = switch(input$dist,
                          rnorm = "Normal population",
                          rexp  = "Right skewed population",
                          rbeta = "Left skewed population",
                          runif = "Uniform population")   
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
          hist(ndist, main=paste("Distribution of means of ", k, 
                                 " random samples, each\nconsisting of ", n, 
                                 " observations from a ", distname, sep=""), 
               xlab="Sample means", freq=FALSE, xlim = c(-100,100), ylim=c(0, max(ndens$y, nhist$density)),
               col="darkorchid1")
        }
        else{
          hist(ndist, main=paste("Distribution of means of ", k, 
                                 " random samples, each\nconsisting of ", n, 
                                 " observations from a ", distname, sep=""), 
               xlab="Sample means", freq=FALSE, ylim=c(0, max(ndens$y, nhist$density)),
               col="darkorchid1")
        }
        lines(ndens, col=COL[2,2], lwd=3)
        legend("topright",legend=paste("Mean =", m_samp, "\nSD =", sd_samp), bg=COL[2,2])
        box()
    })
})