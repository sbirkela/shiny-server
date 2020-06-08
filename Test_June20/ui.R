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
    menuItem("2018 Ops Hiring Process", icon = icon("dashboard"), tabName = "activity"),
    #menuItem("Candidate Background", icon = icon("bar-chart"), tabName = "background"),
    #menuItem("Applicant Location Map", icon = icon("globe"), tabName = "map"),
    
    
    selectInput("job_input", label = "Select a hiring cycle",
                choices= c("Operator - December 2018"),
                selected = "Operator - December 2018")
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
              box(width = 12, leafletOutput("app_map"))),
            fluidRow(
              box(width = 10, plotOutput("app_vol"))),
            fluidRow(
              box(width = 10, plotOutput("how_hear"))),
            fluidRow(
              box(width = 9, plotOutput("education"))),
            fluidRow(
              box(width = 9, plotOutput("experience"))),
            fluidRow(
              box(width = 9, plotOutput("military")))
    )

  )
)

dashboardPage(header, sidebar, body)
