#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -118.2000, lat = 33.8000, zoom = 7) %>%
      addTiles() %>% 
      addCircleMarkers(data=latlng_df,radius=1,group="All", color = "Blue") %>%
      addCircleMarkers(data=asian,radius=1,group="Asian", color = "Blue") %>%
      addCircleMarkers(data=blck,radius=1,group="Black or African-American", color = "Blue") %>%
      addCircleMarkers(data=hispanic,radius=1,group="Hispanic", color = "Blue") %>%
      addCircleMarkers(data=white,radius=1,group="White", color = "Blue") %>%
      addCircleMarkers(data=two,radius=1,group="Two or more races", color = "Blue") %>%
      addCircleMarkers(data=notid,radius=1,group="Choose not to identify", color = "Blue") %>%
      addLayersControl(
        baseGroups = c("All", "Asian", "Black or African-American", "Hispanic", "White", "Two or more races","Choose not to identify"),
        options = layersControlOptions(collapsed=FALSE)
      )  
  })
  
})