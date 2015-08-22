socialmarkov = reactive(
  {
    require(riverplot)
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
    markovB<-new("markovchain", states=statesNames$names, transitionMatrix=round(racistMatrix,2))
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
          summary = paste("In the ficticious country of Islandia, without inclusive policies a red person on average earns", summary, "times more than a blue person")
          
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
        summary = paste("In the ficticious country of Islandia, without inclusive policies a red person on average earns", summary, "times more than a blue person")
      }
      
    }
    statesNames$final = t(init.distribution %*% (racistMatrix %^% 1000000))
    statesNames$final = round(statesNames$final,2)
    #riverplot
    river = statesNames
    #nodes <- c( rev(as.character(races)), rev(as.character(classes)))
    edges = river[, c("race", "class", "final")]
    names(edges) = c("N1", "N2", "Value")
    edges$col = as.character(edges$N1)
    edges$edgecol <- "col"
    edges = rbind(edges[3:4,], edges[1:2,])
    nodes = c( rev(as.character(races)), rev(as.character(classes)))
    r <- makeRiver( nodes, edges, node_xpos= c( 1,1,2,2))
    r[["nodes"]]$labels = as.character(r[["nodes"]]$ID)
    pos = r[["nodes"]]$ID %in% classes
    r[["nodes"]]$labels[pos] = paste(r[["nodes"]]$labels[pos], "class")
    temp = merge(river[,1:5], paste(as.character(classes)))
    if(input$racism==1)
    {
      temp$fix = temp$final
      temp = subset(temp, as.character(temp$class) == as.character(temp$y))
    }else
    {
      temp$fix = temp$final-temp$init
      pos = as.character(temp$class) == as.character(temp$y) & temp$final > temp$init
      temp$fix[pos] = temp$init[pos]
      pos = as.character(temp$class) == as.character(temp$y) & temp$final < temp$init
      temp$fix[pos] = temp$final[pos]
      pos = temp$fix < 0
      temp$fix[pos] =  0
    }
      
    

    temp$col = temp$race
    temp2 = select(temp, race, names, final)
    temp2$col = temp$race
    temp2 = temp2[!duplicated(temp2),]
    temp = select(temp, names, y, fix, col)
    temp$y = paste(temp$y, "after policy")
    names(temp2) = names(temp)
    temp = temp[!duplicated(temp2),]
    temp = rbind(temp, temp2)
    
  
    edges = temp
    names(edges) = c("N1", "N2", "Value", "col")
    edges$N2 = gsub(" after policy", "", edges$N2)
    edges$col = as.character(edges$col)
    edges$edgecol <- "col"
    nodes <- c(rev(as.character(races)), rev(edges$N2))
    r2 <- makeRiver( nodes, edges, node_xpos= c( 1,1,2, 2,2,2, 3,3))
    r2[["nodes"]]$labels = as.character(r2[["nodes"]]$ID)
    r2[["nodes"]]$labels[3:6] = ""
    pos = r2[["nodes"]]$ID %in% classes
    r2[["nodes"]]$labels[pos] = paste(r2[["nodes"]]$labels[pos], "class")
    if(!input$racism==1)
      r2[["nodes"]][4:5,] = r2[["nodes"]][5:4,] 

    #####
    
    statesNames$final[c(1,3)] = statesNames$final[c(1,3)]/sum(statesNames$final[c(1,3)])
    statesNames$final[c(2,4)] = statesNames$final[c(2,4)]/sum(statesNames$final[c(2,4)])
    statesNames = melt(statesNames, c("race", "class", "names", "wages"))
    statesNames = subset(statesNames, variable == "final")
    summary = paste(summary, "and accounts for", percent(statesNames$value[1]), "of the upper class")
    if (input$racism==1)
    {
      summary = "There is equal distribution of resources between red and blue races"
    }
    
    data = list()
    data[["statesNames"]] = statesNames
    data[["river"]] = river
    data[["percapita"]] = percapita
    data[["summary"]] = summary
    data[["river"]] = r
    data[["river2"]] = r2
    data[["markov"]] = markovB
    return(data)
  }

)
