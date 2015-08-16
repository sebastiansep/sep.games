
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
      p("The country of Islandia has two races, one red, one blue. The red race is the predominant race of the country."),
      p(" While Islandians are good, inclusive people, studies have shown that when recruiting for jobs, 
        you are more likely to be successful if you have a 'red' sounding name."), 
      p("This has a knock on effect to the earning capacities of the red and blue races, with the best jobs inadvertantly going to 'red' people."),
      p("Use the slider bars to alter the demography and prevalence of unconscious racism in Islandia. Click to see what happens when inclusive policies are introduced."),
      sliderInput("pop",
                  "Proportion of Red Race in Islandia:",
                  min = 0.01,
                  max = 0.99,
                  value = 0.5),
      sliderInput("racism",
                  "Level of Unconscious Racism in Islandia:",
                  min = 1,
                  max = 100,
                  value = 5),
      p(textOutput("racismText")),
      actionButton("no_racism", "Eliminate Racism"),
      actionButton("aust_racism", "Australian Unconscious Racism", size = "extra-small"),
      checkboxInput("positive.discrimination", label = "Anti-Discrimination Policy", value = FALSE),
      p("This simulation was made by Sebastian Sep 16 August 2015")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Results",
      p(textOutput("summaryText")),
      plotOutput("populationPlot"),
      plotOutput("percapitaPlot")),
      tabPanel("Method",
                p("According to a recent study, people with foreign sounding names were up to five times less likely to be selected for a job interview in Australia."),
               helpText(   a("Click Here to Read Article",     href="http://www.smh.com.au/national/australian-bosses-are-racist-when-its-time-to-hire-20090617-chvu.html")
               ),
                 p("Therefore, out of 100 job interviews, Australian sounding names account for 100*5/(1+5) = 83 of them, while foreign sounding names are interviewed for the remaining 17."),
                p("This is disproportionate at 28% of all residents in Australia were born overseas. It also means that certain people have to apply for up to 68% more jobs in order to get an interview."),
               helpText(   a("Click Here to read about Australian Immigration",     href="http://www.sebastiansep.org/rally-against-reclaim-australia-part-6-immigration/")
               ),

               p("This simulation has been constructed using Markov Chains. Mobility between classes is assumed to be 10% in any given time step and the Markov chain is run until it reaches equilibrium.
                  Unconscious racism is modelled by favouring the red race by a factor of racism/(1+racism)."),
               p("To estimate income disparity I have used Australian median incomes of the highest (~$1100) and lowest (~$150) quartiles."),
               helpText(   a("Click Here to see Australian Income Levels",     href="http://profile.id.com.au/australia/individual-income-quartiles")
               ),
               helpText(   a("Click Here to read more about Markov Chains",     href="https://en.wikipedia.org/wiki/Markov_chain")
               ),
               p("The current settings currently selected on this webpage produces the following markov mobility proportions 
                 (note the differences in probabilities of staying in one class: when racism is present red are always more likely to remain in the upper class and less likely to stay in the lower class):"),
               plotOutput("markovPlot")
      
      )

      
    )
  )
)))
