library(shiny)
library(shinydashboard)
library(shinyBS)
library(dplyr)
library(ggplot2)
#library(rgdal)
library(RColorBrewer)
library(googleVis)
library(leaflet)


header <- dashboardHeader(title = "Hiring Cycle Analytics")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Applicant Flow", tabName = "dashboard", 
             icon = icon("dashboard")),
    
    menuItem("Map", icon = icon("globe"), tabName = "map"),
    
    menuItem("Summary Text", icon = icon("globe"), tabName = "text"),
    
    selectInput("job_input", label = "Select a job",
                choices= c("Instrumentation"),
                selected = "Instrumentation")
  )
)

body <- dashboardBody(
  bsModal(id = "clientData", title = "Client Data", 
          trigger = "showData", 
          verbatimTextOutput("clientdataText")),
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              infoBoxOutput(width = 3, "cycle_days"),
              infoBoxOutput(width = 3, "applicants"),
              infoBoxOutput(width = 3, "pending_candidates"),
              infoBoxOutput(width = 3, "baseline_score"),
            fluidRow(
              box(width = 6, plotOutput("how_hear")),
              #box(width = 2, htmlOutput("app_sourc")),
              box(width = 6, plotOutput("app_vol"))
            )
    )
  ),
  tabItem(tabName = "map",
          box(width = 12, leafletOutput("app_map")))
)
)
#shinyUI(dashboardPage(header, sidebar, body))

ui <- dashboardPage(header, sidebar, body)
