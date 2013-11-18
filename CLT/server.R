
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
  
    # plot 1   
    output$pop.dist = renderPlot({
        distname = switch(input$dist,
                          rnorm = "Population distribution: Normal",
                          rexp = "Population distribution: Right skewed",
                          rbeta = "Population distribution: Left skewed",
                          runif = "Population distribution: Uniform")   

        pop = parent()
        m_pop =  round(mean(pop),2)
        sd_pop = round(sd(pop),2)
        mu = input$mu

        pdens=density(pop)
        phist=hist(pop, plot=FALSE)
        if (input$dist == "rnorm"){
          hist(pop, main=distname, xlab="", freq=FALSE, xlim = c(min(-100,pop),max(100,pop)), 
            ylim=c(0, max(pdens$y, phist$density)), col=COL[1,2], border = "white", cex.main = 1.5)
          legend_pos = ifelse(mu > 0, "topleft", "topright")
          legend(legend_pos, inset = 0.025, 
            legend=bquote(atop(mu==.(m_pop),sigma==.(sd_pop))), 
            bty = "n", cex = 1.5, text.col = COL[1], text.font = 2)
        }
        else {
          hist(pop, main=distname, 
               xlab="", freq=FALSE, ylim=c(0, max(pdens$y, phist$density)+5),
               col=COL[1,2], border = "white", cex.main = 1.5)
        }
        lines(pdens, col=COL[1], lwd=3)
        box()
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
                     ylim = c(0,2), xlim = c(min(-100,x),max(100,x)))
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
      
        distname = switch(input$dist,
                          rnorm = "normal population",
                          rexp  = "right skewed population",
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
          hist(ndist, main="Sampling distribution", 
               xlab="Sample means", freq=FALSE, xlim = c(-100,100), 
               ylim=c(0, max(ndens$y, nhist$density)),
               col=COL[2,2], border = "white", cex.main = 1.5)
        }
        else{
          hist(ndist, main=paste("Distribution of means of ", k, 
                                 " random samples, each\nconsisting of ", n, 
                                 " observations from a ", distname, sep=""), 
               xlab="Sample means", freq=FALSE, ylim=c(0, max(ndens$y, nhist$density)),
               col="darkorchid1", border = "white", cex.main = 1.5)
        }
        lines(ndens, col=COL[2,2], lwd=3)
        legend("topright",legend=paste("Mean =", m_samp, "\nSD =", sd_samp), bg=COL[2,2])
        box()
    })

    # text
    output$sampling.descr = renderText({

      distname = switch(input$dist,
                        rnorm = "normal population",
                        rexp  = "right skewed population",
                        rbeta = "left skewed population",
                        runif = "uniform population")   

      k = input$k
      n = input$n
      paste("Distribution of means of", k, "random samples,\n
             each consisting of", n, " observations\n
             from a", distname)
      })
})