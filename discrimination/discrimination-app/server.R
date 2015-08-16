
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
library(riverplot)
library(diagram)

shinyServer(function(input, output, session) {
 
  source("socialmobilitycalcs.R", local = TRUE)

   observeEvent(input$no_racism, {
     updateSliderInput(session, "racism", value = 1)
  })
  observeEvent(input$aust_racism, {
    updateSliderInput(session, "racism", value = 5)
    updateSliderInput(session, "pop", value = .72)
  })

    
  output$summaryText <- renderText({
    data <- socialmarkov()
    return(data[["summary"]])
  })
  output$percapitaPlot <- renderPlot({
    data <- socialmarkov()
    p = ggplot(data[["percapita"]], aes(x=time, y=percapita, colour=race)) + geom_line(size = 2) + ylim(c(0,1200))
    p = p + theme_few()
    p = p + ggtitle("Income per Capita of Different Races")
    p = p + ylab("Average income per week per capita ($)")
    p = p + theme(axis.text.x = element_blank(), legend.position = "top")
    if (input$positive.discrimination)
    {
      p = p + geom_vline(xintercept = 50, colour = "red")
      p = p + annotate("text", x=48, y = 600, label = "Anti-Discrimination Implemented", colour = "red", angle = 90)
    }
    print(p)

  },
  height = 400, 
  width = 600 )
  
  output$populationPlot <- renderPlot({
    
    data <- socialmarkov()
  if(!input$positive.discrimination)
  {
    plot( data[["river"]])
    text(1.5, 129, "Distribution of Red and Blue in the\nUpper and Lower Classes of Islandia")
  }else
  {
    plot( data[["river2"]])
    text(2, 129, "Distribution of Red and Blue in the\nUpper and Lower Classes in Islandia")
    text(2.3, -1500, "Distribution of Red and Blue in the upper and lower\nclasses evens out after anti-discrimination policies")
  }
  
    

    
  },
  height = 400, 
  width = 600 )
  output$markovPlot <- renderPlot({
    
    data <- socialmarkov()
    par(mar=c(5,3,2,2)+0.1)
  plot(data[["markov"]], package="diagram", box.size=0.04)
  },
  height = 600, 
  width = 600)

    

    
    
    



})
