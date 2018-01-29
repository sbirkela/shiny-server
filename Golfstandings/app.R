
library(shiny)
library(DT)

#overall_earnings <- read.csv("/srv/shiny-server/Golfstandings/overall_earnings.csv")
#golfer_detail <- read.csv("/srv/shiny-server/Golfstandings/golfer_detail2.csv")
#roster <- read.csv("/srv/shiny-server/Golfstandings/roster.csv") 
#tourney_ID_list <- read.csv("/srv/shiny-server/Golfstandings/tourney_ID_list.csv")

overall_earnings <- read.csv("overall_earnings.csv")
golfer_detail <- read.csv("golfer_detail2.csv")
roster <- read.csv("roster.csv") 
tourney_ID_list <- read.csv("tourney_ID_list.csv")

#______________________________________________________________________________
# Define UI for application that draws a histogram

#UI
ui <- fluidPage(
  titlePanel("Fantasy Golf Standings"),
    sidebarLayout(     
      sidebarPanel(
        selectInput("report_input", label = "Select a Report",
                    choices = c("Overall_Standings", "Team_Detail"),
                    selected = "Overall_Standings"),
                    width = 2),
      mainPanel(DT::dataTableOutput("standings_table")
)
)
)
server <- function(input, output) 
  output$standings_table <- DT::renderDataTable( {
    if (input$report_input == "Overall_Standings") {
      df <- overall_earnings
      currency_col <- 'Total_Earnings'
      page_length <- 42
    }
    if (input$report_input == "Team_Detail") {
      df <- golfer_detail
      currency_col <- c('Jan.25.28', 'Feb.1.4', 'Feb.8.11', 'Feb.15.18', 'Feb.22.25', 'Mar.1.4', 'Mar.8.11', 'Mar.15.18', 'Mar.21.25', 'Mar.29.Apr.1', 'Apr.5.8.Masters', 'Apr.12.15', 'Apr.19.22', 'May.3.6', 'May.10.13', 'May.17.20', 'May.24.27', 'May.31.Jun.3', 'Jun.7.10', 'Jun.14.17.U.S.Open', 'Jun.21.24', 'Jun.28.Jul.1', 'Jul.5.8', 'Jul.12.15', 'Jul.19.22.British.Open', 'Jul.26.29', 'Aug.2.5Barracuda', 'Aug.2.5WGC', 'Aug.9.12.PGA', 'Aug.16.19', 'Aug.23.26', 'Aug.31.Sep.3', 'Sep.6.9', 'Sep.20.23')
      page_length <- 294
      }

    datatable(df, options = list(pageLength = page_length), rownames = FALSE, caption = paste("Results updated through ", colnames(roster)[length(tourney_ID_list$x) + 2], sep = "") ) %>% formatCurrency(currency_col)
      })

  
# Run the application 
shinyApp(ui = ui, server = server)

