library(shiny)
library(shinydashboard)
library(shinyBS)
library(dplyr)
library(DT)

header <- dashboardHeader(title = "HireScore Analytics")

sidebar <- dashboardSidebar(
  sidebarMenu(
   
    menuItem("User Input", tabName = "activity"),
    uiOutput("status", tabname = "activity"),
    uiOutput("selection_value", tabname = "activity"),
    uiOutput("band_size", tabname = "activity"),
    uiOutput("race_band", tabname = "activity"),
    uiOutput("gender_band", tabname = "activity"),
    uiOutput("race_majority", tabname = "activity"),
    uiOutput("gender_majority", tabname = "activity"),
    fileInput("upload", "Upload portal file", accept = c(".xlsx", ".xls"))
    #actionButton("Instructions", label = "Click for instructions", tabName = "instructions")

  )
)

body <- dashboardBody(
#  bsModal(id = "clientData", title = "Client Data", 
 #         trigger = "showData", 
#          verbatimTextOutput("clientdataText")),
  tabItems(
    tabItem(tabName = "activity",
            strong("Pending Applicant Table"),
            br(),
            fluidRow(
              column(12, DT::dataTableOutput("x1"))),
            br(),
            strong("Applicant Analysis by Race"),
            br(),
            fluidRow(
              column(7, DT::dataTableOutput("x2"))),
            br(),
            strong("Applicant Analysis by Gender"),
            br(),
            fluidRow(
              column(7, DT::dataTableOutput("x3"))),
            br(),
            br(),
            fluidRow(
              column(5, downloadButton("Candidate_download", "Download Selected Candidates"))),
    )
    
  )
)

dashboardPage(header, sidebar, body)
