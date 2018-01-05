library(shiny)
library(shinydashboard)
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
ui <- dashboardPage(
  dashboardHeader(title = "Hire Score Analytics"),
  dashboardSidebar(
    selectInput("dataset", "Choose a hiring step to analyze:", 
                choices = component_name),
    selectInput("majority_race", "Choose the race category to compare against:", 
                choices = races, selected = "White"),
    selectInput("majority_gender", "Choose the gender category to compare against:", 
                choices = genders, selected = "Male")
  ),

  dashboardBody(
    fluidPage(
    fluidRow(htmlOutput(outputId = "top_title")),
    fluidRow(htmlOutput(outputId = "sub_title")),
    fluidRow(tableOutput(outputId = "view")),
    fluidRow(h5("(1) The 80% test compares the passing rate percentage from the majority group to the passing rate percentage from minority group. The rule of thumb for
       interpreting the 80% test is that a number below .80 indicates initial evidence of adverse impact. Note that this test is much less meaningful with a small sample size.")),
    fluidRow(h5("(2) The Stat test is a statistical test that looks at whether or not the differences in pass rates between a majority group and a minority group are statistically significant.
        A Prob Value of .05 or below indicates the difference in pass rates between the groups is statistically significant (at the 95% level). So, if the minority group has a lower 
       pass rate than the majority group AND the Prob Value is .05 or below, it is evidence for adverse impact. Note that with a large sample size, even a small difference 
       in pass rates may be statistically significant. This test uses Fisher's Exact test with the Lancaster Mid-P correction.")),
    fluidRow(h5("Caution: Even though the testing above might show evidence of adverse impact, it is important to note that these are only initial tests of adverse impact. More 
       detailed analyses are often conducted (e.g. logistical regression to control for applicant characteristics) to determine the full extent of adverse impact. Finally, it is important 
       to note that evidence of adverse impact does not automatically make a selection process 'illegal.' It does, however, mean that the hiring process should have strong validation 
       evidence that the selection procedure are job related for the position in question and consistent with business necessity."))
    )
    ) 
)
