#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("ABM of Reservoir Outflow"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      p("Press here to generate random inflow to the reservoir"),
      
      actionButton("q", "Random Inflow", icon = NULL),
       
      sliderInput("int",
                   "Initial Reservoir Storage (%):",
                   min = 10,
                   max = 100,
                   value = 40),
       
       h4("Reservoir Manager Types"),
       p("Red: Manager that is risk averse always aims to have levels some percentage below the rule curve."),
      sliderInput("R1p",
                  "Manager 1 percentage above the rule curve:",
                  min = 1,
                  max = 20,
                  value = 5),
       p("Blue: The less risk averse manager always aims to have levels 5% above the rule curve."),
      sliderInput("R2p",
                  "Manager 2 percentage below the rule curve:",
                  min = 1,
                  max = 20,
                  value = 5),
       p("Green: This manager changes his/her management based on the month of the year."),
       p("Light Blue: This manager doesn't know what they are doing.")
       
    ), 
    
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("qplot"),
       plotOutput("disPlot")
       #add either table or bar chart of total precip here
    )
  )
))
