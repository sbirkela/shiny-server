
library(shiny)
#library(dplyr)
library(DT)

overall_earnings <- read.csv("/srv/shiny-server/Golfstandings/overall_earnings.csv")
roster <- read.csv("/srv/shiny-server/Golfstandings/Roster.csv") 
tourney_ID_list <- read.csv("/srv/shiny-server/Golfstandings/tourney_ID_list.csv")

#______________________________________________________________________________
# Define UI for application that draws a histogram

#UI
ui <- (fluidPage(
  titlePanel("Fantasy Golf Standings"),
        tabPanel("Table", DT::dataTableOutput("overall_earnings"),
         value = "table")
)
)

server <- function(input, output) {
  output$overall_earnings <- DT::renderDataTable(
    datatable(overall_earnings, options = list(pageLength = 52), rownames = FALSE, caption = paste("Results updated through ", colnames(roster)[length(tourney_ID_list$x) + 2], sep = "") ) %>% formatCurrency('Total_Earnings')
  ) 
}
  
# Run the application 
shinyApp(ui = ui, server = server)

