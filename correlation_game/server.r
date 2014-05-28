#server.R

install.packages("ggplot2")
install.packages("ellipse")
require(shiny)
require(ggplot2)
require(ellipse)

shinyServer(
  
  func=function(input, output, clientData, session) {

    #session-specific variables
    correlation <- -1 #current correlation
    score <- 0 #user's score
    answered <- FALSE # an indicator for whether question has been answered

    observe({
      #this observer monitors when input$submit is invalidated
      #and displays the answer
      input$submit
      
      isolate({
        #by isolating input$answer from the observer,
        #we wait until the submit button is pressed before displaying answer
        answer = as.numeric(input$slider)  
      })
      
      if(abs(answer-correlation)<0.1){
        output$status1 <- renderText({""})
        output$status2 <- renderText({paste(generateResponse(1),sprintf("(True correlation: %f)",round(correlation,2)))})
        output$status3 <- renderText({""})
        if(!answered){
          score <<- score+10
          output$score <- renderText({sprintf("Your score: %d",score)})
          answered <<- TRUE
        }
      }
      else if(abs(answer-correlation)<0.2){
        output$status1 <- renderText({""})
        output$status2 <- renderText({""})
        output$status3 <- renderText({generateResponse(2)})
        if(!answered){
          score <<- score+5
          output$score <- renderText({sprintf("Your score: %d",score)})
          answered <<- TRUE
        }
      }
      else if(abs(answer-correlation)<0.3){
        output$status1 <- renderText({""})
        output$status2 <- renderText({""})
        output$status3 <- renderText({generateResponse(3)})
        if(!answered){
          score <<- score+2
          output$score <- renderText({sprintf("Your score: %d",score)})
          answered <<- TRUE
        }
      }
      else{
        output$status1 <- renderText({""})
        output$status2 <- renderText({""})
        output$status3 <- renderText({generateResponse(4)})
        answered <<- TRUE
      }
      
    })
    
    observe({
      #this observer monitors when input$newplot is invalidated
      #or when input$difficulty is invalidated
      #and generates a new plot
      
      #update plot, calculate correlation
      if(input$difficulty=="Easy"){
        difficulty <- 3
        numPoints <- 10
        updateCheckboxGroupInput(session,inputId="options",choices=list("Averages","Standard deviation line","Ellipse"))
      }
      else if(input$difficulty=="Medium"){
        difficulty <- 2
        numPoints <- 25 
        updateCheckboxGroupInput(session,inputId="options",choices=list("Averages","Standard deviation line"))
      }
      else{
        difficulty <- 1
        numPoints <- 100
        updateCheckboxGroupInput(session,inputId="options",choices=list("Standard deviation line"))
      }
      
      input$newplot
      
      data = generateData(difficulty, numPoints)
      
      #VERY IMPORTANT <<- "double arrow" can assign values outside of the local envir!
      #i.e. outside of this observer!
      correlation<<-round(cor(data[,1],data[,2]),2)
      
      #descriptive statistics
      center <- apply(data,MARGIN=2,mean)
      corrmatrix <- cor(data)
      standevs=apply(data,MARGIN=2,sd)
      slope = sign(correlation)*standevs[2]/standevs[1]
      intercept = center[2]-center[1]*slope
      
      #plot data
      data_ellipse=as.data.frame(ellipse(corrmatrix,centre=center,scale=standevs))
      
      isolate({
        observe({
            
          options=is.na(pmatch(c("Averages", "Standard deviation line","Ellipse"),input$options))
            output$plot1 <- renderPlot({
              p <- ggplot(data,aes(X,Y))+
                geom_point(size=4,alpha=1/2)+
                theme(text=element_text(size=20))+
                coord_cartesian(xlim=c(min(data$X)-sd(data$X),max(data$X)+sd(data$X)),ylim=c(min(data$Y)-sd(data$Y),max(data$Y)+sd(data$Y)))
              if (!options[1]){
                p<-p+geom_vline(xintercept=mean(data$X),color="#569BBD")+
                  geom_hline(yintercept=mean(data$Y),color="#569BBD")+
                  geom_text(label="bar(X)",x=mean(data$X)+0.1*sd(data$X),y=mean(data$Y)+sd(data$Y),parse=TRUE)+
                  geom_text(label="bar(Y)",x=mean(data$X)+sd(data$X),y=mean(data$Y)+0.1*sd(data$Y),parse=TRUE)
              }
              if (!options[2]){
                p<-p+geom_abline(intercept=intercept,slope=slope,color="#569BBD")
              }
              if (!options[3]){
                p<-p+geom_path(data=data_ellipse,aes(x=X,y=Y),size=1,linetype=2,color="#569BBD")
              }
              print(p)
            })
         })
      })
      
      #update radio buttons
      answer_options <- list(correlation,generateAnswer(correlation,difficulty),
                             generateAnswer(correlation,difficulty),
                             generateAnswer(correlation,difficulty),
                             generateAnswer(correlation,difficulty),
                             generateAnswer(correlation,difficulty))
      answer_display = answer_options[sample(5,5,replace=FALSE)]
      updateRadioButtons(session,"answer",choices=answer_display)
      
      #display text
      output$status1 <- renderText({"Mark your answer and click 'Submit!'"})
      output$status2 <- renderText({""})
      output$status3 <- renderText({""})
      
      #reset answered status
      answered<<-FALSE
      
      
    })
   
    
  }
  
)

generateData = function(difficulty,numPoints){
  x_center = rnorm(1,0,10)
  x_scale = rgamma(1,4,1)
  if (difficulty ==3){
    choice = sample(2,1)
    if (choice ==2){
      X = rnorm(numPoints,x_center,x_scale)
      Y = rnorm(numPoints,X,rgamma(1,1)*x_scale)
      return(data.frame(X,Y))
    }
    else{
      X = rnorm(numPoints,x_center,x_scale)
      Y = rnorm(numPoints,-X,rgamma(1,1)*x_scale)
      return(data.frame(X,Y))      
    }
  }
  else if (difficulty == 2){
      X = rnorm(numPoints,x_center,x_scale)
      Y = rnorm(numPoints,rnorm(1)*X,rgamma(1,1)*x_scale)
      return(data.frame(X,Y))
  }
  else{
      X = rnorm(numPoints,x_center,x_scale)
      Y = rnorm(numPoints,rnorm(1)*X,rgamma(1,1)*x_scale)
      return(data.frame(X,Y))
  }
}

generateAnswer = function(correlation,difficulty){
  
  #generate answer
  answer = runif(1,-1,1)
  
  #base case
  if (abs(correlation-answer) > 0.05 * difficulty ){
    return(round(answer,2))
  }
  else {
    generateAnswer(correlation,difficulty)
  }
  
}

generateResponse = function(response){
  if (response==1){
    print(sample(list("Correct!","Spot on!","Got it!"),1)[[1]])
  }
  else if (response ==2){
    print(sample(list("Almost.","Close.","Just a bit off.."),1)[[1]])
  }
  else if (response == 3){
    print(sample(list("Warmer...","Getting there..."),1)[[1]])
  }
  else if (response ==4){
    print(sample(list("Try again.","Nope!"),1)[[1]])
  }
}
