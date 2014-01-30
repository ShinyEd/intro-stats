source('./helper/chiTail.R')
source('./helper/FTail.R')
source('./helper/normTail.R')

library(shiny)
library(openintro)

seed = as.numeric(Sys.time())

shinyServer(function(input, output)
{ 
  print(input)

  output$tail = renderUI(
  {
    print("tail")
    if (input$dist == "rbinom")
    {
      selectInput(inputId = "tail",
                  label = "Find Area:",
                  choices = c("Lower Tail"="lower", 
                              "Upper Tail"="upper", 
                              "Both Tails"="both",
                              "Middle"="middle",
                              "Equality"="equal"),
                  selected = "lower")
    }
    else
    {
      selectInput(inputId = "tail",
                  label = "Find Area:",
                  choices = c("Lower Tail"="lower", 
                              "Upper Tail"="upper", 
                              "Both Tails"="both",
                              "Middle"="middle"),
                  selected = "lower")
    }
  })

  output$lower_bound = renderUI(
  {
    print("lower bound")
    if (input$dist == "rbinom")
    {
      if (input$tail %in% c("both","middle"))
      {
        selectInput(inputId = "lower_bound",
                    label = "Lower bound:",
                    choices = c("<" = "open", 
                                "\u2264" = "closed"),
                    selected = "open")
      }
      else if (input$tail == "lower")
      {
        selectInput(inputId = "lower_bound",
                    label = "Bound:",
                    choices = c("<" = "open", 
                                "\u2264" = "closed"),
                    selected = "open")
      }
      else if (input$tail == "upper")
      {
        selectInput(inputId = "lower_bound",
                    label = "Bound:",
                    choices = c(">" = "open", 
                                "\u2265" = "closed"),
                    selected = "open")
      }
    }
  })

  output$upper_bound = renderUI(
  {
    print("upper bound")
    if (input$dist == "rbinom")
    {
      if (input$tail == "middle")
      {
        selectInput(inputId = "upper_bound",
                    label = "Upper bound:",
                    choices = c("<" = "open", 
                                "\u2264" = "closed"),
                    selected = "open")
      }
      else if (input$tail == "both")
      {
        selectInput(inputId = "upper_bound",
                    label = "Upper bound:",
                    choices = c(">" = "open", 
                                "\u2265" = "closed"),
                    selected = "open")
      }
    }
  })

  output$model = renderUI(
  {
    print("model")
    low_less = "<"
    low_greater = ">"

    up_less = "<"
    up_greater = ">"

    if (input$dist == "rbinom")
    {
      if (input$tail != "equal")
      {
        if (input$lower_bound == "closed")
        {
          low_less = "\u2264"
          low_greater = "\u2265"
        }

        if (length(input$upper_bound) != 0)
        {    
          if (input$upper_bound == "closed")
          {
            up_less = "\u2264"
            up_greater = "\u2265"
          }
        }
      }
    }

    if (input$tail == "lower")
    {
      # P(X < a)
      text = paste0("P(X ", low_less, " a)")
    }
    else if (input$tail == "upper")
    {
      # P(X > a)
      text = paste0("P(X ", low_greater, " a)")
    }
    else if (input$tail == "middle")
    {
      # P(a < X < b)
      text = paste0("P(a ", low_less, " X ", up_less, " b)")
    }
    else if (input$tail == "both")
    {
      # P(X < a or X > b)
      text = paste0("P(X ", low_less, " a or X ", up_greater, " b)")
    }
    else if (input$tail == "equal")
    {
      # P(X = a)
      text = paste0("P(X = a)")
    }
    helpText(div(text,style="text-indent:20px;font-size:125%;"))
  })

  #######################
  # Normal distribution #
  #######################

  output$mean = renderUI(
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
    
  output$sd = renderUI(
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
  
  ##########################
  # t, F, X^2 distribution #
  ##########################

  output$df1 = renderUI(
  {

    

    if (input$dist %in% c("rt","rchisq","rf"))
    {
      sliderInput(ifelse(input$dist %in% c("rt","rchisq"), "df","df1"),
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
  



  output$a = renderUI(
  {
    if (input$dist == "rnorm")
    {
      find_normal_step = function(sd)
      {
        10^round(log(7*sd/100,10))
      }

      value = input$mu - 1.96 * input$sd
      min   = input$mu - 4 * input$sd
      max   = input$mu + 4 * input$sd
      step  = find_normal_step(input$sd)
    }
    else if (input$dist == "rt")
    {
      value = -1.96 
      min   = -6
      max   = 6
      step  = 0.1
    }
    else if (input$dist == "rf")
    {
      value = 1
      min   = 0
      max   = 4
      step  = 0.01
    }
    else if (input$dist == "rchisq")
    {
      value = 10
      min   = 0
      max   = 30
      step  = 0.1
    }
    else if (input$dist == "rbinom")
    {
      value = round(input$n/4)
      min = 0
      max = input$n
      step = 1
    }

    sliderInput("a", "a",
                value = value,
                min   = min,
                max   = max,
                step  = step)
  })

  output$b = renderUI(
  {
    if (input$tail %in% c("middle","both"))
    {
      if (input$dist == "rnorm")
      {
        find_normal_step = function(sd)
        {
          10^round(log(7*sd/100,10))
        }

        value = input$mu + 1.96 * input$sd
        min   = input$mu - 4 * input$sd
        max   = input$mu + 4 * input$sd
        step  = find_normal_step(input$sd)
      }
      else if (input$dist == "rt")
      {
        value = 1.96 
        min   = -6
        max   = 6
        step  = 0.1
      }
      else if (input$dist == "rf")
      {
        value = 2
        min   = 0
        max   = 4
        step  = 0.01
      }
      else if (input$dist == "rchisq")
      {
        value = 20
        min   = 0
        max   = 30
        step  = 0.1
      }
      else if (input$dist == "rbinom")
      {
        value = round(input$n*3/4)
        min = 0
        max = input$n
        step = 1
      }

      sliderInput("b", "b",
                  value = value,
                  min   = min,
                  max   = max,
                  step  = step)
    }
  })  


  ############
  # Plotting #
  ############
  
  output$plot = renderPlot(
  { 
    L = NULL
    U = NULL

    error = FALSE

    if (input$tail == "lower" | input$tail == "equal")
    {
      L = input$a 
    }
    else if (input$tail == "upper")
    {
      U = input$a 
    }
    else if (input$tail %in% c("both","middle"))
    {
      L = input$a
      U = input$b
    
      if (L > U)
        error = TRUE
    }

    if (error)
    {
      plot(0,0,type='n',axes=FALSE,xlab="",ylab="",mar=c(1,1,1,1))
      text(0,0,"Error: Lower bound greater than upper bound.",col="red",cex=2)
    }
    else
    {
      if (input$dist == "rnorm" | input$dist == "rt") 
      {
        M = NULL
        if (input$tail == "middle")
        {
          M = c(L,U)
          L = NULL
          U = NULL
        }

        if(input$dist == "rnorm")
        {
          normTail(m=input$mu, s=input$sd, L=L, U=U, M=M, axes=3)
          title(main="Normal Distribution")
        }
        else if (input$dist == "rt")
        {
          normTail(m=0, s=1, df=input$df, L=L, U=U, M=M, axes=3)
          title(main="t Distribution")
        }
      }
      # else if (input$dist == "rchisq")
      # {
      #   chiTail(U=input$chisq.value, df=input$df)
      #   title(main="Chi^2 Distribution")
      # }
      # else if (input$dist == "rf")
      # {
      #   FTail(U=input$f.value, df_n=input$df1, df_d=input$df2)
      #   title(main="F Distribution")
      # }
      else if (input$dist == "rbinom")
      {
        d = dbinom(0:input$n,input$n,input$p)

        plot(0,0,type='n',xlim=c(-0.5,input$n+0.5),ylim=c(0,max(d)),
             xlab="",ylab="",cex.axis=2,
             axes=FALSE)
        axis(1)
        axis(2)
        title(main=paste("Binomial Distribution"))

        for (k in 1+(0:input$n)) 
        {
            col = NA

            if (input$tail == "lower")
            {
              if (input$lower_bound == "open"   & k-1 <  L) col = "#569BBD"
              if (input$lower_bound == "closed" & k-1 <= L) col = "#569BBD"
            }
            else if (input$tail == "upper")
            {
              if (input$lower_bound == "open"   & k-1 >  U) col = "#569BBD"
              if (input$lower_bound == "closed" & k-1 >= U) col = "#569BBD"
            }
            else if (input$tail == "equal")
            {
              if (k-1 == L) col = "#569BBD"
            }
            if (input$tail == "both")
            {
              if (input$lower_bound == "open"   & input$upper_bound == "open"   & k-1 <  L & k-1 >  U) col = "#569BBD"
              if (input$lower_bound == "open"   & input$upper_bound == "closed" & k-1 <  L & k-1 >= U) col = "#569BBD"
              if (input$lower_bound == "closed" & input$upper_bound == "open"   & k-1 <= L & k-1 >  U) col = "#569BBD"
              if (input$lower_bound == "closed" & input$upper_bound == "closed" & k-1 <= L & k-1 >= U) col = "#569BBD"
            }
            else if (input$tail == "middle")
            {
              if (input$lower_bound == "open"   & input$upper_bound == "open"   & k-1 >  L & k-1 <  U) col = "#569BBD"
              if (input$lower_bound == "open"   & input$upper_bound == "closed" & k-1 >  L & k-1 <= U) col = "#569BBD"
              if (input$lower_bound == "closed" & input$upper_bound == "open"   & k-1 >= L & k-1 <  U) col = "#569BBD"
              if (input$lower_bound == "closed" & input$upper_bound == "closed" & k-1 >= L & k-1 <= U) col = "#569BBD"
            }

            p = matrix(c(-1.5+k,0, -0.5+k,0, -0.5+k,d[k], -1.5+k,d[k], -1.5+k,0),ncol=2,byrow=TRUE)
          
            polygon(p, col=col)
        }
      }
    }
  })
})