library(shiny)
library(leaflet)

latlng_df <- read.csv("/srv/shiny-server/maintmap/latlng.csv")
ed_hs <- read.csv("/srv/shiny-server/maintmap/ed_hs.csv")
ed_ad <- read.csv("/srv/shiny-server/maintmap/ed_ad.csv")
ed_bd <- read.csv("/srv/shiny-server/maintmap/ed_bd.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   output$mymap <- renderLeaflet({
     leaflet() %>%
       addTiles() %>% 
       addCircleMarkers(data=latlng_df,radius=1,group="All", color = "Blue") %>%
       addCircleMarkers(data=ed_hs,radius=1,group="High School or GED", color = "Blue") %>%
       addCircleMarkers(data=ed_ad,radius=1,group="Associate's Degree", color = "Blue") %>%
       addCircleMarkers(data=ed_bd,radius=1,group="Bachelor's Degree", color = "Blue") %>%
       addLayersControl(
         baseGroups = c("High School or GED", "Associate's Degree", "Bachelor's Degree", "All"),
         options = layersControlOptions(collapsed=FALSE)
       )  
  })
  
})
