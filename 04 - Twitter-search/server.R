
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ROAuth)
library(data.table)
library(streamR)

shinyServer(function(input, output, session) {
  
  output$mapa_leaflet<-updateMap(tweets.dt,cols_$size[1],1)

  observeEvent(input$SearchTweets, 
               {
                 filterStream("tweets.json",
                              timeout = 10,
                              locations=c(0.12,40.38,3.47,42.82),
                              oauth = twitCred
                              )
                 tweets.dt <- super_parseTweets()
                 cols_ <- cols_for_shaping(tweets.dt)
                 updateSelectInput(session,"selectColour",cols_$colour)
                 updateSelectInput(session,"selectSize",cols_$size)

                 tweets.dt <- get_colours(tweets.dt,input$selectColour)
                 output$mapa_leaflet<-updateMap(tweets.dt,input$selectSize,input$selectSizeR)
                                                                                                                print(cols_)
                 }
               )
  
  observeEvent(input$selectColour, 
               {
                 print(tweets.dt)
                 print(input$selectColour)
                 tweets.dt <- get_colours(tweets.dt,input$selectColour)
                 print(tweets.dt)
                 print(paste(input$selectSize,input$selectSizeR))
                 output$mapa_leaflet<-updateMap(tweets.dt,input$selectSize,input$selectSizeR)
                 }
               )
  
  observeEvent(input$selectSize, 
               {
                 tweets.dt <- get_colours(tweets.dt,input$selectColour)
                 output$mapa_leaflet<-updateMap(tweets.dt,input$selectSize,input$selectSizeR)
               }
               )
  observeEvent(input$selectSizeR, 
               {
                 tweets.dt <- get_colours(tweets.dt,input$selectColour)
                 output$mapa_leaflet<-updateMap(tweets.dt,input$selectSize,input$selectSizeR)
               }
  )

})
