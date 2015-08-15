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
    percapita <<- rbind(percapita, cbind(time = i, wealth.split))
  }
  
}
statesNames$final = t(init.distribution %*% (racistMatrix %^% 1000000))
statesNames$final = round(statesNames$final,2)
statesNames <<- statesNames