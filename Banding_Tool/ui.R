library(shiny)
library(shinydashboard)
library(shinyBS)
library(dplyr)
library(DT)

header <- dashboardHeader(title = "HireScore Analytics")

sidebar <- dashboardSidebar(
  sidebarMenu(
   
    menuItem("User Input", tabName = "activity"),
    uiOutput("status"),
    uiOutput("selection_value"),
    uiOutput("band_size"),
    uiOutput("race_band"),
    uiOutput("gender_band"),
    uiOutput("race_majority"),
    uiOutput("gender_majority"),
    fileInput("upload", "Upload portal file", accept = c(".xlsx", ".xls"))

  )
)

body <- dashboardBody(
  bsModal(id = "clientData", title = "Client Data", 
          trigger = "showData", 
          verbatimTextOutput("clientdataText")),
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
