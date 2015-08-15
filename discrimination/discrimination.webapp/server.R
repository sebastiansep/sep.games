
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(expm)
library(ggthemes)
library(markovchain)
library(dplyr)
library(reshape2)
library(scales)

shinyServer(function(input, output, session) {
 
   observeEvent(input$no_racism, {
     updateSliderInput(session, "racism", value = 1)
  })
  observeEvent(input$aust_racism, {
    updateSliderInput(session, "racism", value = 5)
    updateSliderInput(session, "pop", value = .72)
  })
 
  output$text = renderText({
    if (input$racism > 1)
    {
      return(paste("A red person is", input$racism, "more likely to get a job than a blue person"))
    }else
    {
      return(paste("No racism: A blue person is equally likely to get a job than a red person"))
    }
  })
    
  output$summaryText <- renderText({
    
    races = factor(c("red", "blue"), levels = c("red", "blue"), ordered=T)
    population = 1000
    proportion.red = input$pop
    proportion.blue= 1 - proportion.red 
    classes = factor(c("upper", "lower"), levels = c("upper", "lower"), ordered=T)
    wages = c(1100, 150) #c(1097, 1096/2, 576/2, 271/2) #http://profile.id.com.au/australia/individual-income-quartiles
    status.quo = 0.9
    racism = input$racism#http://www.smh.com.au/national/australian-bosses-are-racist-when-its-time-to-hire-20090617-chvu.html
    mobility = (1-status.quo)
    equalityMatrix = diag(x=status.quo, ncol = length(classes), nrow = length(classes))
    delta = row(equalityMatrix) - col(equalityMatrix)
    equalityMatrix[delta == 1] = mobility
    equalityMatrix[delta == -1] = mobility 
    equalityMatrix[1,1] = 1 - mobility 
    equalityMatrix[length(classes),length(classes)] = 1 - mobility
    equalityMatrix = rbind(cbind(equalityMatrix, matrix(0, ncol(equalityMatrix), nrow(equalityMatrix))),
                           cbind(matrix(0, ncol(equalityMatrix), nrow(equalityMatrix)), equalityMatrix))
    ###Initialise red Markov Chain setup
    
    red.upward.mobility = mobility*racism/(racism+1)
    red.downward.mobility = mobility-red.upward.mobility
    red.mobility = diag(x=status.quo, ncol = length(classes), nrow = length(classes))
    delta = row(red.mobility) - col(red.mobility)
    red.mobility[delta == 1] = red.upward.mobility
    red.mobility[delta == -1] = red.downward.mobility 
    red.mobility[1,1] = 1 - red.downward.mobility 
    red.mobility[length(classes),length(classes)] = 1 - red.upward.mobility
    ###Initialise blue Markov Chain setup
    blue.upward.mobility = red.downward.mobility
    blue.downward.mobility = red.upward.mobility
    blue.mobility = red.mobility
    blue.mobility[delta == 1] = blue.upward.mobility
    blue.mobility[delta == -1] = blue.downward.mobility 
    blue.mobility[1,1] = 1 - blue.downward.mobility 
    blue.mobility[length(classes),length(classes)] = 1 - blue.upward.mobility
    
    statesNames = expand.grid(race = races, class = classes)
    statesNames = arrange(statesNames, race, class )
    statesNames$names = paste(statesNames[,1], statesNames[,2], sep = "-")
    racistMatrix = cbind(red.mobility, matrix(0, ncol(red.mobility), nrow(red.mobility)))
    racistMatrix = rbind(racistMatrix, cbind(matrix(0, ncol(red.mobility), nrow(red.mobility)), blue.mobility))
    markovB<-new("markovchain", states=statesNames$names, transitionMatrix=racistMatrix)
    #plot(markovB, package="diagram", box.size=0.04)
    steadyStates(markovB)
    init.distribution = population*as.numeric(c(proportion.red, proportion.red, proportion.blue, proportion.blue))/2
    statesNames$init =  init.distribution
    statesNames$final = NA
    statesNames$wages = rep(wages,2)
    percapita = NULL
    for (i in seq(0,100, by = 1))
    {
      if (input$positive.discrimination)
      {
        if (i < 50)
        {
          statesNames$final = t(init.distribution %*% (racistMatrix %^% i))
          wealth.split = statesNames %>% group_by(race) %>% summarise(share = sum(wages*as.numeric(final)))
          wealth.split$pc.share = wealth.split$share/sum(wealth.split$share)
          wealth.split$pop = population*c(proportion.red, proportion.blue)
          wealth.split$percapita = with(wealth.split, share/pop)
          percapita = rbind(percapita, cbind(time = i, wealth.split))
          last.step = as.numeric(statesNames$final)
          summary = ceiling(wealth.split$percapita[1]/wealth.split$percapita[2])
          summary = paste("A red person on average earns", summary, "times more than a blue person")

        }else
        {
          statesNames$final = t(last.step %*% (equalityMatrix %^% (i-50)))
          
          wealth.split = statesNames %>% group_by(race) %>% summarise(share = sum(wages*as.numeric(final)))
          wealth.split$pc.share = wealth.split$share/sum(wealth.split$share)
          wealth.split$pop = population*c(proportion.red, proportion.blue)
          wealth.split$percapita = with(wealth.split, share/pop)
          percapita = rbind(percapita, cbind(time = i, wealth.split))
        }
        
      }else
      {
        statesNames$final = t(init.distribution %*% (racistMatrix %^% i))
        wealth.split = statesNames %>% group_by(race) %>% summarise(share = sum(wages*as.numeric(final)))
        wealth.split$pc.share = wealth.split$share/sum(wealth.split$share)
        wealth.split$pop = population*c(proportion.red, proportion.blue)
        wealth.split$percapita = with(wealth.split, share/pop)
        percapita <- rbind(percapita, cbind(time = i, wealth.split))
        summary = ceiling(wealth.split$percapita[1]/wealth.split$percapita[2])
        summary = paste("A red person on average earns", summary, "times more than a blue person")
      }
      
    }
    statesNames$final = t(init.distribution %*% (racistMatrix %^% 1000000))
    statesNames$final = round(statesNames$final,2)
    statesNames$final[c(1,3)] = statesNames$final[c(1,3)]/sum(statesNames$final[c(1,3)])
    statesNames$final[c(2,4)] = statesNames$final[c(2,4)]/sum(statesNames$final[c(2,4)])
    statesNames = melt(statesNames, c("race", "class", "names", "wages"))
    statesNames = subset(statesNames, variable == "final")
    summary = paste(summary, "and is over-represented in the upper class by a factor of", ceiling(statesNames$value[1]/proportion.red))
    if (input$racism==1)
    {
      summary = "There is equal distribution of resources between red and blue races"
    }
    return(summary)
  })
  output$percapitaPlot <- renderPlot({

    races = factor(c("red", "blue"), levels = c("red", "blue"), ordered=T)
    population = 1000
    proportion.red = input$pop
    proportion.blue= 1 - proportion.red 
    classes = factor(c("upper", "lower"), levels = c("upper", "lower"), ordered=T)
    wages = c(1100, 150) #c(1097, 1096/2, 576/2, 271/2) #http://profile.id.com.au/australia/individual-income-quartiles
    status.quo = 0.9
    racism = input$racism#http://www.smh.com.au/national/australian-bosses-are-racist-when-its-time-to-hire-20090617-chvu.html
    mobility = (1-status.quo)
    equalityMatrix = diag(x=status.quo, ncol = length(classes), nrow = length(classes))
    delta = row(equalityMatrix) - col(equalityMatrix)
    equalityMatrix[delta == 1] = mobility
    equalityMatrix[delta == -1] = mobility 
    equalityMatrix[1,1] = 1 - mobility 
    equalityMatrix[length(classes),length(classes)] = 1 - mobility
    equalityMatrix = rbind(cbind(equalityMatrix, matrix(0, ncol(equalityMatrix), nrow(equalityMatrix))),
                           cbind(matrix(0, ncol(equalityMatrix), nrow(equalityMatrix)), equalityMatrix))
    ###Initialise red Markov Chain setup
    
    red.upward.mobility = mobility*racism/(racism+1)
    red.downward.mobility = mobility-red.upward.mobility
    red.mobility = diag(x=status.quo, ncol = length(classes), nrow = length(classes))
    delta = row(red.mobility) - col(red.mobility)
    red.mobility[delta == 1] = red.upward.mobility
    red.mobility[delta == -1] = red.downward.mobility 
    red.mobility[1,1] = 1 - red.downward.mobility 
    red.mobility[length(classes),length(classes)] = 1 - red.upward.mobility
    ###Initialise blue Markov Chain setup
    blue.upward.mobility = red.downward.mobility
    blue.downward.mobility = red.upward.mobility
    blue.mobility = red.mobility
    blue.mobility[delta == 1] = blue.upward.mobility
    blue.mobility[delta == -1] = blue.downward.mobility 
    blue.mobility[1,1] = 1 - blue.downward.mobility 
    blue.mobility[length(classes),length(classes)] = 1 - blue.upward.mobility
    
    statesNames = expand.grid(race = races, class = classes)
    statesNames = arrange(statesNames, race, class )
    statesNames$names = paste(statesNames[,1], statesNames[,2], sep = "-")
    racistMatrix = cbind(red.mobility, matrix(0, ncol(red.mobility), nrow(red.mobility)))
    racistMatrix = rbind(racistMatrix, cbind(matrix(0, ncol(red.mobility), nrow(red.mobility)), blue.mobility))
    markovB<-new("markovchain", states=statesNames$names, transitionMatrix=racistMatrix)
    #plot(markovB, package="diagram", box.size=0.04)
    steadyStates(markovB)
    init.distribution = population*as.numeric(c(proportion.red, proportion.red, proportion.blue, proportion.blue))/2
    statesNames$init =  init.distribution
    statesNames$final = NA
    statesNames$wages = rep(wages,2)
    percapita = NULL
    for (i in seq(0,100, by = 1))
    {
      if (input$positive.discrimination)
      {
        if (i < 50)
        {
          statesNames$final = t(init.distribution %*% (racistMatrix %^% i))
          wealth.split = statesNames %>% group_by(race) %>% summarise(share = sum(wages*as.numeric(final)))
          wealth.split$pc.share = wealth.split$share/sum(wealth.split$share)
          wealth.split$pop = population*c(proportion.red, proportion.blue)
          wealth.split$percapita = with(wealth.split, share/pop)
          percapita = rbind(percapita, cbind(time = i, wealth.split))
          last.step = as.numeric(statesNames$final)
          
        }else
        {
          statesNames$final = t(last.step %*% (equalityMatrix %^% (i-50)))
          
          wealth.split = statesNames %>% group_by(race) %>% summarise(share = sum(wages*as.numeric(final)))
          wealth.split$pc.share = wealth.split$share/sum(wealth.split$share)
          wealth.split$pop = population*c(proportion.red, proportion.blue)
          wealth.split$percapita = with(wealth.split, share/pop)
          percapita = rbind(percapita, cbind(time = i, wealth.split))
        }
        
      }else
      {
        statesNames$final = t(init.distribution %*% (racistMatrix %^% i))
        wealth.split = statesNames %>% group_by(race) %>% summarise(share = sum(wages*as.numeric(final)))
        wealth.split$pc.share = wealth.split$share/sum(wealth.split$share)
        wealth.split$pop = population*c(proportion.red, proportion.blue)
        wealth.split$percapita = with(wealth.split, share/pop)
        percapita <- rbind(percapita, cbind(time = i, wealth.split))
      }
      
    }
    statesNames$final = t(init.distribution %*% (racistMatrix %^% 1000000))
    statesNames$final = round(statesNames$final,2)


    p = ggplot(percapita, aes(x=time, y=percapita, colour=race)) + geom_line(size = 2) + ylim(c(0,1200))
    p = p + theme_few()
    p = p + ggtitle("Income per Capita of Different Races")
    p = p + ylab("Average income per week per capita ($)")
    p = p + theme(axis.text.x = element_blank(), legend.position = "top")
    if (input$positive.discrimination)
    {
      p = p + geom_vline(xintercept = 50, colour = "red")
      p = p + annotate("text", x=48, y = 600, label = "Positive Discrimination Implemented", colour = "red", angle = 90)
    }

    print(p)
    
  })
  output$populationPlot <- renderPlot({
    
    races = factor(c("red", "blue"), levels = c("red", "blue"), ordered=T)
    population = 1000
    proportion.red = input$pop
    proportion.blue= 1 - proportion.red 
    classes = factor(c("upper", "lower"), levels = c("upper", "lower"), ordered=T)
    wages = c(1100, 150) #c(1097, 1096/2, 576/2, 271/2) #http://profile.id.com.au/australia/individual-income-quartiles
    status.quo = 0.9
    racism = input$racism#http://www.smh.com.au/national/australian-bosses-are-racist-when-its-time-to-hire-20090617-chvu.html
    mobility = (1-status.quo)
    equalityMatrix = diag(x=status.quo, ncol = length(classes), nrow = length(classes))
    delta = row(equalityMatrix) - col(equalityMatrix)
    equalityMatrix[delta == 1] = mobility
    equalityMatrix[delta == -1] = mobility 
    equalityMatrix[1,1] = 1 - mobility 
    equalityMatrix[length(classes),length(classes)] = 1 - mobility
    equalityMatrix = rbind(cbind(equalityMatrix, matrix(0, ncol(equalityMatrix), nrow(equalityMatrix))),
                           cbind(matrix(0, ncol(equalityMatrix), nrow(equalityMatrix)), equalityMatrix))
    ###Initialise red Markov Chain setup
    
    red.upward.mobility = mobility*racism/(racism+1)
    red.downward.mobility = mobility-red.upward.mobility
    red.mobility = diag(x=status.quo, ncol = length(classes), nrow = length(classes))
    delta = row(red.mobility) - col(red.mobility)
    red.mobility[delta == 1] = red.upward.mobility
    red.mobility[delta == -1] = red.downward.mobility 
    red.mobility[1,1] = 1 - red.downward.mobility 
    red.mobility[length(classes),length(classes)] = 1 - red.upward.mobility
    ###Initialise blue Markov Chain setup
    blue.upward.mobility = red.downward.mobility
    blue.downward.mobility = red.upward.mobility
    blue.mobility = red.mobility
    blue.mobility[delta == 1] = blue.upward.mobility
    blue.mobility[delta == -1] = blue.downward.mobility 
    blue.mobility[1,1] = 1 - blue.downward.mobility 
    blue.mobility[length(classes),length(classes)] = 1 - blue.upward.mobility
    
    statesNames = expand.grid(race = races, class = classes)
    statesNames = arrange(statesNames, race, class )
    statesNames$names = paste(statesNames[,1], statesNames[,2], sep = "-")
    racistMatrix = cbind(red.mobility, matrix(0, ncol(red.mobility), nrow(red.mobility)))
    racistMatrix = rbind(racistMatrix, cbind(matrix(0, ncol(red.mobility), nrow(red.mobility)), blue.mobility))
    markovB<-new("markovchain", states=statesNames$names, transitionMatrix=racistMatrix)
    #plot(markovB, package="diagram", box.size=0.04)
    steadyStates(markovB)
    init.distribution = population*as.numeric(c(proportion.red, proportion.red, proportion.blue, proportion.blue))/2
    statesNames$init =  init.distribution
    statesNames$final = NA
    statesNames$wages = rep(wages,2)
    percapita = NULL
    for (i in seq(0,100, by = 1))
    {
      if (input$positive.discrimination)
      {
        if (i < 50)
        {
          statesNames$final = t(init.distribution %*% (racistMatrix %^% i))
          wealth.split = statesNames %>% group_by(race) %>% summarise(share = sum(wages*as.numeric(final)))
          wealth.split$pc.share = wealth.split$share/sum(wealth.split$share)
          wealth.split$pop = population*c(proportion.red, proportion.blue)
          wealth.split$percapita = with(wealth.split, share/pop)
          percapita = rbind(percapita, cbind(time = i, wealth.split))
          last.step = as.numeric(statesNames$final)
          
        }else
        {
          statesNames$final = t(last.step %*% (equalityMatrix %^% (i-50)))
          
          wealth.split = statesNames %>% group_by(race) %>% summarise(share = sum(wages*as.numeric(final)))
          wealth.split$pc.share = wealth.split$share/sum(wealth.split$share)
          wealth.split$pop = population*c(proportion.red, proportion.blue)
          wealth.split$percapita = with(wealth.split, share/pop)
          percapita = rbind(percapita, cbind(time = i, wealth.split))
        }
        
      }else
      {
        statesNames$final = t(init.distribution %*% (racistMatrix %^% i))
        wealth.split = statesNames %>% group_by(race) %>% summarise(share = sum(wages*as.numeric(final)))
        wealth.split$pc.share = wealth.split$share/sum(wealth.split$share)
        wealth.split$pop = population*c(proportion.red, proportion.blue)
        wealth.split$percapita = with(wealth.split, share/pop)
        percapita <- rbind(percapita, cbind(time = i, wealth.split))
      }
      
    }
    statesNames$final = t(init.distribution %*% (racistMatrix %^% 1000000))
    statesNames$final = round(statesNames$final,2)
    statesNames$final[c(1,3)] = statesNames$final[c(1,3)]/sum(statesNames$final[c(1,3)])
    statesNames$final[c(2,4)] = statesNames$final[c(2,4)]/sum(statesNames$final[c(2,4)])
    statesNames = melt(statesNames, c("race", "class", "names", "wages"))
    statesNames = subset(statesNames, variable == "final")

    p = ggplot(statesNames, aes(x=race, y=value, fill=race)) + geom_bar(stat="identity") + facet_wrap(variable~class) + ylim(c(0,1))
    p = p + theme_few()
    p = p + ggtitle("Distribution of population in upper and lower classes at its most discriminatory")
    p = p + scale_y_continuous(labels = percent)
    p = p + theme(legend.position = "none")
    
    print(p)
    
  })
  
    

    

    
    
    



})
