
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("SearchTweets","Search New Tweets"),
      selectInput("selectColour","Select Colour var",cols_$colour,cols_$colour[1]),
      selectInput("selectSize","Select Size var",cols_$size,cols_$size[1]),
      sliderInput("selectSizeR","change Size",0,10,1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("mapa_leaflet", height = 500)
      )
  )
))
