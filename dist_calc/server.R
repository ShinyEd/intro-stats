source('./helper/chiTail.R')
source('./helper/FTail.R')
source('./helper/normTail.R')

library(shiny)
library(openintro)

seed = as.numeric(Sys.time())

shinyServer(function(input, output)
{  
  output$tail = renderUI(
  {
    if (input$dist == "rnorm" | input$dist == "rt")
    {
      selectInput(inputId = "tail",
                  label = "Find Area:",
                  choices = c("Lower", "Between", "Upper", "Tails"),
                  selected = "Lower")
    }
    else if (input$dist == "rchisq" | input$dist == "rf")
    {
      selectInput(inputId = "tail",
                  label = "Find Area:",
                  choices = c("Lower", "Upper"),
                  selected = "Upper")
    }
    else if (input$dist == "rbinom")
    {
      selectInput(inputId = "tail",
                  label = "Find Area:",
                  choices = c("<", "\u2264", "=",
                              "\u2265", ">"),
                  selected = "=")
    }
  })

  #######################
  # Normal distribution #
  #######################

  output$slider.mean = renderUI(
  {
    if (input$dist == "rnorm")
    {
      sliderInput("mu",
                  "Mean",
                  value = 0,
                  min = -50,
                  max = 50)
    }
  })
    
  output$slider.sd = renderUI(
  {
    if (input$dist == "rnorm")
    {
      sliderInput("sd",
                  "Standard deviation",
                  value = 1,
                  min = 0.1,
                  max = 30,
                  step=0.1)
    }
  })
  
  find_normal_step = function(sd)
  {
    10^round(log(7*sd/100,10))
  }

  output$x.value = renderUI(
  {
    if (  (input$dist == "rnorm" | input$dist == "rt") 
        & (input$tail == "Lower" | input$tail == "Upper"))
    {
      sliderInput("x.value",
                  ifelse(input$dist == "rnorm","X","t"),
                  value = input$mu,
                  min   = input$mu - 4 * input$sd,
                  max   = input$mu + 4 * input$sd,
                  step  = find_normal_step(input$sd))
    }
  })

  output$x.lower = renderUI(
  {
    if (  (input$dist == "rnorm" | input$dist == "rt") 
        & (input$tail == "Between" | input$tail == "Tails"))
    {
      sliderInput("x.lower",
                  ifelse(input$dist == "rnorm","X lower","t lower"),
                  value = input$mu - 1.96 * input$sd,
                  min   = input$mu - 4 * input$sd,
                  max   = input$mu + 4 * input$sd,
                  step  = find_normal_step(input$sd))
    }
  })
  
  output$x.upper = renderUI(
  {
    if (  (input$dist == "rnorm" | input$dist == "rt") 
        & (input$tail == "Between" | input$tail == "Tails"))
    {
      sliderInput("x.upper",
                  ifelse(input$dist == "rnorm","X upper","t upper"),
                  value = input$mu + 1.96 * input$sd,
                  min   = input$mu - 4 * input$sd,
                  max   = input$mu + 4 * input$sd,
                  step  = find_normal_step(input$sd))
    }
  })

  
  ##################
  # F distribution #
  ##################

  output$df1 = renderUI(
  {
    if (input$dist == "rf")
    {
      sliderInput("df1",
                  "Degrees of freedom",
                  value = 10,
                  min = 1,
                  max = 50)
    }
  })
  
  output$df2 = renderUI(
  {
    if (input$dist == "rf")
    {
      sliderInput("df2",
                  "Degrees of freedom (2)",
                  value = 10,
                  min = 1,
                  max = 50)
    }
  })
  
  output$f.value = renderUI(
  {
    if (input$dist == "rf")
    {
      sliderInput("f.value",
                  "F",
                  value = 1,
                  min = 0,
                  max = 4,
                  step = .01)
    }    
  })


  ##################
  # t distribution #
  ##################

  output$df = renderUI(
  {
    if (input$dist == "rt" | input$dist == "rchisq")
    {
      sliderInput("df",
                  "Degrees of freedom",
                  value = 10,
                  min = 1,
                  max = 50)
    }
  })
  

  ######################
  # chi^2 distribution #
  ######################
  
  output$chisq.value = renderUI(
  {
    if (input$dist == "rchisq")
    {
      sliderInput("chisq.value",
                  "X^2",
                  value= 15,
                  min=0,
                  max=30)
    }    
  })
  

  #########################
  # Binomial distribution #
  #########################

  output$n = renderUI(
  {
    if (input$dist == "rbinom")
    {
      sliderInput("n",
                  "n",
                  value = 10,
                  min = 1,
                  max = 250,
                  step = 1)
    }
  })

  output$p = renderUI(
  {
    if (input$dist == "rbinom")
    {
      sliderInput("p",
                  "p",
                  value = 0.5,
                  min = 0,
                  max = 1,
                  step = .01)
    }
  })
  
  output$k = renderUI(
  {
    if (input$dist == "rbinom")
    {
      sliderInput("k",
                  "k",
                  value = 0,
                  min = 0,
                  max = input$n,
                  step = 1)
    }
  })


  ############
  # Plotting #
  ############
  
  output$plot.dist = renderPlot(
  { 
    if (input$dist == "rnorm" | input$dist == "rt") 
    {
      L = NULL
      M = NULL
      U = NULL
      DF = NULL

      error = FALSE

      if (input$tail == "Lower")
      {
        L = input$x.value 
      }
      else if (input$tail == "Between")
      {
        M = c(input$x.lower, input$x.upper)
        
        if (input$x.lower > input$x.upper)
        {
          error = TRUE
        }
      }
      else if (input$tail == "Upper") 
      {
        U = input$x.value
      }
      else if (input$tail == "Tails")
      {
        L = input$x.lower
        U = input$x.upper

        if (L > U)
        {
          error = TRUE
        }
      }

      if (!error)
      {
        if(input$dist == "rnorm")
          normTail(m=input$mu, s=input$sd, L=L, U=U, M=M, axes=3)
        else if (input$dist == "rt")
          normTail(m=input$mu, s=input$sd, df=input$df, L=L, U=U, M=M, axes=3)
        
        title(main=ifelse(input$dist == "rnorm","Normal Distribution","t Distribution"))
      }
      else
      {
        plot(0,0,type='n',axes=FALSE,xlab="",ylab="",mar=c(1,1,1,1))
        text(0,0,"Error: Lower bound greater than upper bound.",col="red",cex=2)
      }
    }
    else if (input$dist == "rchisq")
    {
      chiTail(U=input$chisq.value, df=input$df)
      title(main="Chi^2 Distribution")
    }
    else if (input$dist == "rf")
    {
      FTail(U=input$f.value, df_n=input$df1, df_d=input$df2)
      title(main="F Distribution")
    }
    else if (input$dist == "rbinom")
    {
      
      test = function(val)
      {
        if (input$tail == "<")
        {
          return(val <  input$k)
        }
        else if (input$tail == "\u2264")
        {
          return(val <= input$k)
        }
        else if (input$tail == "=")
        {
          return(val == input$k)
        }
        else if (input$tail == "\u2265")
        {
          return(val >= input$k)
        }
        else if (input$tail == ">")
        {
          return(val >  input$k)
        }
      }

      d = dbinom(0:input$n,input$n,input$p)

      plot(0,0,type='n',xlim=c(-0.5,input$n+0.5),ylim=c(0,max(d)),
           xlab="",ylab="",cex.axis=2,
           axes=FALSE)
      axis(1)
      axis(2)
      title(main=paste("Binomial Distribution",input$k,test(input$k)))

      

      for (k in 1+(0:input$n)) 
      {
          p = matrix(c(-1.5+k,0, -0.5+k,0, -0.5+k,d[k], -1.5+k,d[k], -1.5+k,0),ncol=2,byrow=TRUE)
          
          if (test(k-1))
            polygon(p, col='#569BBD')
          else
            polygon(p)
      }
    }
  })
})