
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("How Unconscious Racism Affects Income Distribution"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      p("The country of Islandia has two races, one red, one blue. The red race is the predominant race of the country.
        While Islandians are good, inclusive people, studies have shown that when recruiting for jobs, 
        you are more likely to be successful if you have a 'red' sounding name. This has a knock on effect 
        to the earning capacities of the red and blue races, with the best jobs inadvertantly going to 'red' people."),
      p("Use the slider bars to alter the demography and prevalence of unconscious racism in Islandia. Click to see what happens when 'positive discrimination' policies are introduced."),
      sliderInput("pop",
                  "Proportion of Red Race:",
                  min = 0.01,
                  max = 0.99,
                  value = 0.5),
      sliderInput("racism",
                  "Level of Unconscious Racism:",
                  min = 1,
                  max = 100,
                  value = 5),
      p(textOutput("racismText")),
      actionButton("no_racism", "Eliminate Racism"),
      actionButton("aust_racism", "Set to Australian Levels of Unconscious Racism"),
      checkboxInput("positive.discrimination", label = "Implement Positive Discrimination Policy", value = FALSE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      p(textOutput("summaryText")),
      plotOutput("percapitaPlot"),
      plotOutput("populationPlot")
      
    )
  )
))
