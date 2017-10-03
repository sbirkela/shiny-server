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
    menuItem("Application Activity", icon = icon("dashboard"), tabName = "activity"),
    menuItem("Candidate Background", icon = icon("bar-chart"), tabName = "background"),
    menuItem("Applicant Location Map", icon = icon("globe"), tabName = "map"),
    
    
    selectInput("job_input", label = "Select a hiring cycle",
                choices= c("Operator - October 2017"),
                selected = "Operator - October 2017")
  )
)

body <- dashboardBody(
  bsModal(id = "clientData", title = "Client Data", 
          trigger = "showData", 
          verbatimTextOutput("clientdataText")),
  tabItems(
    tabItem(tabName = "activity",
            fluidRow(
              infoBoxOutput(width = 3, "time_update"),
              infoBoxOutput(width = 3, "app_responses"),
              infoBoxOutput(width = 3, "init_applicants"),
              infoBoxOutput(width = 3, "pend_applicants")),
            fluidRow(
              box(width = 12, plotOutput("app_vol"))),
            fluidRow(
              box(width = 12, plotOutput("how_hear")))
            #fluidRow(
            # box(width = 6, plotOutput("app_vol")),
            #box(width = 6, plotOutput("how_hear"))
            #)
    ),
    tabItem(tabName = "background",
            fluidRow(
              box(width = 12, plotOutput("education"))),
            fluidRow(
              box(width = 12, plotOutput("experience"))),
            fluidRow(
              box(width = 12, plotOutput("military")))
            #fluidRow(
            #box(width = 6, plotOutput("education")),
            #box(width = 6, plotOutput("experience"))
            
    ),
    tabItem(tabName = "map",
            box(width = 12, leafletOutput("app_map")))
  )
)

dashboardPage(header, sidebar, body)
