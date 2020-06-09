library(shiny)
library(shinydashboard)
library(shinyBS)
library(dplyr)
library(DT)

header <- dashboardHeader(title = "Hiring Cycle Analytics")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("File Upload", tabName = "activity"),
    fileInput("upload", "Upload portal file", accept = c(".xlsx", ".xls")),
    
    menuItem("Race Analysis", tabName = "activity"),
    selectInput("race_majority", label = "Select race comparison group",
                choices= c("White", "Black or African-American", "Hispanic or Latino", "Asian", "American Indian or Alaska Native", "Two or More Races"),
                selected = "White"),
    
    menuItem("Gender Analysis", tabName = "activity"),
    selectInput("gender_majority", label = "Select gender comparison group",
                choices= c("Male", "Female"),
                selected = "Male")

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
              column(10, DT::dataTableOutput("x1"))),
            br(),
            strong("Applicant Analysis by Race"),
            br(),
            fluidRow(
              column(6, DT::dataTableOutput("x2"))),
            br(),
            strong("Applicant Analysis by Gender"),
            br(),
            fluidRow(
              column(6, DT::dataTableOutput("x3"))),
            br(),
            br(),
            fluidRow(
              column(5, downloadButton("Candidate_download", "Download Selected Candidates"))),
    )
    
  )
)

dashboardPage(header, sidebar, body)
