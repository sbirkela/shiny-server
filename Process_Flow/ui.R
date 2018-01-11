library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(ztable)
library(rlang)

#USER ENTRY SECTION____________________________________________________________________

df_background <- data.frame(read.csv("/srv/shiny-server/Process_Flow/df_background.csv", header = TRUE, sep = ","))

#This should be done by hand to ensure order is correct
component_name <- c("Pre-screening", "JST Invite", "JST Completion", "JSTs", 
                    "In-Person Invite", "Work Demo Completion", "Work Demo",
                    "Interview", "Post Interview Offer Extended", "Offer Accepted")
applicant_characteristic <- c("Background", "Degree", "Referral", "Military", "Race", "Gender")

#Location
location <- "Martinez"



#df_background <- raw_df %>% select(Status, Status_code, Background, Degree, Referral, Military, Race, Gender)

# Define UI for dataset viewer application
ui <- dashboardPage(
  dashboardHeader(title = "Hire Score Analytics"),
  dashboardSidebar(
    selectInput("dataset", "Choose a hiring step to analyze:", 
                choices = component_name),
    selectInput("characteristic", "Choose an applicant characteristic to analyze", 
                choices = applicant_characteristic)
  ),

  dashboardBody(
    fluidPage(
    fluidRow(htmlOutput(outputId = "top_title")),
    fluidRow(htmlOutput(outputId = "sub_title")),
    fluidRow(tableOutput(outputId = "view"))
    )
    ) 
)
