source('./helper/chiTail.R')
source('./helper/FTail.R')
source('./helper/normTail.R')

# set mirror
options(repos=structure(c(CRAN="http://cran.rstudio.com")))

if (!("shiny" %in% names(installed.packages()[,"Package"]))) {install.packages("shiny")}
suppressMessages(library(shiny, quietly = TRUE))

if (!("openintro" %in% names(installed.packages()[,"Package"]))) {install.packages("openintro")}
suppressMessages(library(openintro, quietly = TRUE))

defaults = list("tail" = "lower",
                "lower_bound" = "open",
                "upper_bound" = "open")

shinyServer(function(input, output)
{ 
  output$tail = renderUI(
  {
    #print("tail")
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
    else if (input$dist == "rf" | input$dist == "rchisq"){
      selectInput(inputId = "tail",
                  label = "Find Area:",
                  choices = c("Upper Tail"="upper"),
                  selected = "upper")
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
    #print("lower bound")

    if (input$dist == "rbinom")
    {
      if (is.null(input$tail))
      {
        shiny:::flushReact()
        return()
      }

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
    #print("upper bound")

    if (input$dist == "rbinom")
    {
      if (is.null(input$tail))
      {
        shiny:::flushReact()
        return()
      }

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

  get_model_text = reactive(
  {
    if (is.null(input$tail)){
      shiny:::flushReact()
      return()
    }

    low_less = "<"
    low_greater = ">"

    up_less = "<"
    up_greater = ">"

    if (input$dist == "rbinom" & input$tail != "equal")
    {
      if (is.null(input$lower_bound))
      {
        shiny:::flushReact()
        return()
      }

      if (input$lower_bound == "closed")
      {
        low_less = "\u2264"
        low_greater = "\u2265"
      }

      if (input$tail %in% c("middle","both"))
      { 
        if (is.null(input$upper_bound)){
          shiny:::flushReact()
          return()
        }

        if (input$upper_bound == "closed")
        {
          up_less = "\u2264"
          up_greater = "\u2265"
        }
      }
    }

    text = ""
    if (length(input$tail) != 0)
    {
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
    }

    return(text)
  })

  output$model = renderText(
  {
    #print("model")

    get_model_text()
  })

  #######################
  # Normal distribution #
  #######################

  output$mean = renderUI(
  {
    #print("mean")
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
    #print("sd")
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
    #print("df1")
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
    #print("df2")
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
    #print("n")
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
    #print("p")
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
    #print("a")

    value = 1
    min = 0
    max = 1
    step = 1

    if (input$dist == "rnorm")
    {
      find_normal_step = function(sd)
      {
        10^round(log(7*sd/100,10))
      }

      if (is.null(input$mu) | is.null(input$sd)){
        shiny:::flushReact()
        return()
      }

      mu = input$mu
      sd = input$sd
      if (is.null(mu)) mu = 0
      if (is.null(sd)) sd = 1

      value = mu - 1.96 * sd
      min   = mu - 4 * sd
      max   = mu + 4 * sd
      step  = find_normal_step(sd)
      if (mu == 0 & sd == 1) {step = .01}
    }
    else if (input$dist == "rt")
    {
      value = -1.96 
      min   = -6
      max   = 6
      step  = 0.01
    }
    else if (input$dist == "rf")
    {
      value = round(qf(.95,as.numeric(input$df1),as.numeric(input$df2)),digits=2)
      min   = 0
      max   = round(qf(.995,as.numeric(input$df1),as.numeric(input$df2))*1.05,digits=2)
      step  = 0.01
    }
    else if (input$dist == "rchisq")
    {
      value = round(qchisq(.95,as.numeric(input$df)),digits=2)
      min   = 0
      max   = round(qchisq(.995,as.numeric(input$df)),digits=2)
      step  = 0.01
    }
    else if (input$dist == "rbinom")
    {
      if (is.null(input$n)){
        shiny:::flushReact()
        return()
      }

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
    #print("b")
     
    if (is.null(input$tail))
    {
      shiny:::flushReact()
      return()
    }
    
    if (input$tail %in% c("middle","both"))
    {
      value = 1
      min = 0
      max = 1
      step = 1

      if (input$dist == "rnorm")
      {
        find_normal_step = function(sd)
        {
          10^round(log(7*sd/100,10))
        }

        if (is.null(input$mu) | is.null(input$sd)){
          shiny:::flushReact()
          return()
        }

        mu = input$mu
        sd = input$sd
        if (is.null(mu)) mu = 0
        if (is.null(sd)) sd = 1

        value = mu + 1.96 * sd
        min   = mu - 4 * sd
        max   = mu + 4 * sd
        step  = find_normal_step(sd)
      }
      else if (input$dist == "rt")
      {
        value = 1.96 
        min   = -6
        max   = 6
        step  = 0.01
      }
      else if (input$dist == "rbinom")
      {
        if (is.null(input$n)){
          shiny:::flushReact()
          return()
        }

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
    #print("plot")

    if (is.null(input$tail) | is.null(input$a))
    {
      shiny:::flushReact()
      return()
    }

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
      if (is.null(input$b)){
        shiny:::flushReact()
        return()
      }
      
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
          if(is.null(input$mu) | is.null(input$sd))
          {
            shiny:::flushReact()
            return()
          }

          normTail(m=input$mu, s=input$sd, L=L, U=U, M=M, axes=3, cex.axis=1.5)
          title(main="Normal Distribution")
        }
        else if (input$dist == "rt")
        {
          if(is.null(input$df))
          {
            shiny:::flushReact()
            return()
          }

          normTail(m=0, s=1, df=input$df, L=L, U=U, M=M, axes=3, cex.axis=1.5)
          title(main="t Distribution")
        }
      }
        else if (input$dist == "rchisq")
        {
          if(is.null(input$df))
          {
            shiny:::flushReact()
            return()
          }
          M = NULL
          if (input$tail == "middle")
          {
            M = c(L,U)
            L = NULL
            U = NULL
          }
          
          chiTail(U=U, df=input$df, xlim = c(0,round(qchisq(.995,input$df),digits=2)+1))
          title(main="Chi^2 Distribution")
        }
        else if (input$dist == "rf")
        {        
          if(is.null(input$df1) | is.null(input$df2))
          {
            shiny:::flushReact()
            return()
          }
        
          M = NULL
          if (input$tail == "middle")
          {
            M = c(L,U)
            L = NULL
            U = NULL
          }
                   
          FTail(U=U,df_n=input$df1, df_d=input$df2)
          title(main="F Distribution")
        }
      else if (input$dist == "rbinom")
      {
        if(  is.null(input$n)
           | is.null(input$p)
           | is.null(input$lower_bound))
        {
          shiny:::flushReact()
          return()
        }

        if(input$tail %in% c("both","middle") & is.null(input$upper_bound))
        {
          shiny:::flushReact()
          return()
        }

        d = dbinom(0:input$n,input$n,input$p)

        plot(0,0,type='n',xlim=c(-0.5,input$n+0.5),ylim=c(0,max(d)),
             xlab="",ylab="", axes=FALSE)
        axis(1, cex.axis=1.5)
        axis(2, cex.axis=1.5)
        title(main=paste("Binomial Distribution"))



        for (k in 1:length(d)) 
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
            else if (input$tail == "both")
            {
              if (input$lower_bound == "open"   & input$upper_bound == "open"   & (k-1 <  L | k-1 >  U)) col = "#569BBD"
              if (input$lower_bound == "open"   & input$upper_bound == "closed" & (k-1 <  L | k-1 >= U)) col = "#569BBD"
              if (input$lower_bound == "closed" & input$upper_bound == "open"   & (k-1 <= L | k-1 >  U)) col = "#569BBD"
              if (input$lower_bound == "closed" & input$upper_bound == "closed" & (k-1 <= L | k-1 >= U)) col = "#569BBD"
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

  ################
  # Calculations #
  ################

  output$area = renderText(
  {
    if (is.null(input$tail) | is.null(input$a))
    {
      shiny:::flushReact()
      return()
    }

    L = input$a
    U = NULL

    if (input$tail %in% c("both","middle")) 
    {
      if (is.null(input$b))
      {
        shiny:::flushReact()
        return()
      }

      U = input$b
      
      error = FALSE
      if (L>U) error = TRUE
      if (error){
        return()
      }
    }
    


    f = function() NULL

    if (input$dist == "rnorm")
    {
      if (is.null(input$mu) | is.null(input$sd))
      {
        shiny:::flushReact()
        return()
      }

      f = function(x) pnorm(x,input$mu,input$sd)
    }  
    else if (input$dist == "rt")
    {
      if (is.null(input$df))
      {
        shiny:::flushReact()
        return()
      }
      
      f = function(x) pt(x,input$df)
    }
    else if (input$dist == "rchisq"){
      if (is.null(input$df))
      {
        shiny:::flushReact()
        return()
      }
      
      f = function(x) pchisq(x,input$df)
    }
    else if (input$dist == "rf"){
      if (is.null(input$df1) | is.null(input$df2))
      {
        shiny:::flushReact()
        return()
      }
      
      f = function(x) pf(x,input$df1,input$df2)
    }    
    else if (input$dist == "rbinom")
    {
      if (is.null(input$n) | is.null(input$p) | is.null(input$lower_bound))
      {
        shiny:::flushReact()
        return()
      }

      if (input$tail == "equal")
      {
        f = function(x) dbinom(x,input$n,input$p)
      }
      else
      {
        f = function(x) pbinom(x,input$n,input$p)
      
        if (input$tail %in% c("lower","both") & input$lower_bound == "open") L = L-1
        if (input$tail %in% c("upper")        & input$lower_bound == "closed") L = L-1
        if (input$tail %in% c("middle")       & input$lower_bound == "closed") L = L-1

        if (input$tail %in% c("both","middle")) 
        {
          if (is.null(input$upper_bound))
          {
            shiny:::flushReact()
            return()
          }

          if (input$tail == "both"   & input$upper_bound == "closed") U = U-1
          if (input$tail == "middle" & input$upper_bound == "open") U = U-1
        } 
      }
    }

    val = NA
    if (input$tail == "lower")
      val = f(L)
    else if (input$tail == "upper")
      val = 1-f(L)
    else if (input$tail == "equal")
      val = f(L)
    else if (input$tail == "both")
      val = f(L) + (1-f(U))
    else if (input$tail == "middle")
      val = f(U) - f(L)
    
    text = paste(get_model_text(),"=",signif(val,3))
  
    
    text = sub("a",input$a,text)
    if (input$tail %in% c("both","middle")) 
      text = sub("b",input$b,text)
    
    text
  })
})