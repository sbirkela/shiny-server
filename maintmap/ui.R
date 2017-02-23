
library(shiny)
library(leaflet)

shinyUI(fluidPage(
  leafletOutput("mymap", height = "800")
  ))  
