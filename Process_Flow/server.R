library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(ztable)
library(rlang)

location <- "Martinez"
component_name <- c("Pre-screening", "JST Invite", "JST Completion", "JSTs", 
                    "In-Person Invite", "Work Demo Completion", "Work Demo",
                    "Interview", "Post Interview Offer Extended", "Offer Accepted")
applicant_characteristic <- c("Background", "Degree", "Referral", "Military", "Race", "Gender")

df_background <- data.frame(read.csv("/srv/shiny-server/Process_Flow/df_background.csv", header = TRUE, sep = ",")) 

# Define server logic required to view the selected dataset
server <- function(input, output) {
    # Return the requested dataset
    datasetInput <- reactive({
      code_check <- which(component_name == input$dataset)
      df_background$Outcome <- NA
      df_background$Outcome[df_background$Status_code==code_check] <- "Fail"
      df_background$Outcome[df_background$Status_code > code_check] <- "Pass"
      df_background$Outcome[df_background$Status_code < code_check] <- NA
      df_background <- df_background %>% filter(!is.na(Outcome))
      
#Function for creating table 
      table_construct <- function(col1) {  
        col1_q <- rlang::enexpr(col1)     
        tab1 <- df_background %>% 
          tabyl(!!col1_q, Outcome)
        tab1 <- tab1 %>%
          adorn_totals("row") %>%
          adorn_percentages("row") %>%
          adorn_pct_formatting(digits = 0) %>% 
          adorn_ns(position = "front") 
        nrow_tab1 <- nrow(tab1)
        options(ztable.colnames.bold=TRUE, ztable.caption.bold = TRUE)
        options(ztable.zebra=NULL)
        options(ztable.type="html")
        
        tab1 <- tab1 %>% ztable(digits = 2, align="llcc", position = "l",
                                include.rownames=FALSE, hline.after=c(nrow_tab1 - 1))
        return(tab1)
      }
      x <- sym(input$characteristic)
      table_construct(!!x) #input variable
      
    })
    output$top_title <- renderUI(h3(strong(paste(location, " - Applicant Flow Analysis"))))
    output$sub_title <- renderUI(h4(em(strong(paste("Hiring Step - ", input$dataset)))))
    output$view <- renderPrint({
      datasetInput()
    })
}
