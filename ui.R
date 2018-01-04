
library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(exact2x2)
library(janitor)
library(ztable)

#USER ENTRY SECTION____________________________________________________________________

df_background <- data.frame(read.csv("df_background.csv", header = TRUE, sep = ","))

#This should be done by hand to ensure order is correct
component_name <- c("Pre-screening", "JST Invite", "JST Completion", "JSTs", 
                    "In-Person Invite", "Work Demo Competion", "Work Demo",
                    "Interview", "Post Interview Offer Extended", "Offer Accepted")
#Location
location <- "Martinez"
#Create list of races
races <- df_background %>% 
  tabyl(Race) %>% select(Race)
races <- races[["Race"]]

#Create list of genders
genders <- df_background %>% 
  tabyl(Gender) %>% select(Gender)
genders <- genders[["Gender"]]


# Define UI for dataset viewer application
ui <- fluidPage(

  
  #headerPanel("Adverse Impact Analysis"),
  sidebarLayout(
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    selectInput("dataset", "Choose a hiring step to analyze:", 
                choices = component_name),
    selectInput("majority_race", "Choose the race category to compare against:", 
                choices = races, selected = "White"),
    selectInput("majority_gender", "Choose the gender category to compare against:", 
                choices = genders, selected = "Male")
  ),

  mainPanel(
    headerPanel(paste(location, " - Adverse Impact Analysis", sep = "")),
    tableOutput("view")
  )
))
