library(shiny)
library(shinydashboard)
library(shinyBS)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(googleVis)
library(leaflet)

header <- dashboardHeader(title = "Hiring Cycle Analytics")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("2019 Machinist Hiring Process", icon = icon("dashboard"), tabName = "activity"),
    #menuItem("Candidate Background", icon = icon("bar-chart"), tabName = "background"),
    #menuItem("Applicant Location Map", icon = icon("globe"), tabName = "map"),
    
    
    selectInput("job_input", label = "Select a hiring cycle",
                choices= c("Machinist - March 2019"),
                selected = "Machinist - March 2019")
  )
)

body <- dashboardBody(
  bsModal(id = "clientData", title = "Client Data", 
          trigger = "showData", 
          verbatimTextOutput("clientdataText")),
  tabItems(
    tabItem(tabName = "activity",
            fluidRow(
              box(width = 12, plotOutput("app_funnel"))),
            fluidRow(
              box(width = 12, leafletOutput("app_map"))),
            fluidRow(
              box(width = 10, plotOutput("app_vol"))),
            fluidRow(
              box(width = 10, plotOutput("how_hear")))
            #fluidRow(
             # box(width = 9, plotOutput("education"))),
           # fluidRow(
            #  box(width = 9, plotOutput("experience"))),
            #fluidRow(
             # box(width = 9, plotOutput("military")))
    )

  )
)

dashboardPage(header, sidebar, body)
