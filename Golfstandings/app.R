
library(shiny)
library(DT)

overall_earnings <- read.csv("overall_earnings.csv")
majors_earnings <- read.csv("majors_earnings.csv")
golfer_detail <- read.csv("~golfer_detail2.csv")
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
                    choices = c("Overall_Standings", "Majors_Standings", "Team_Detail"),
                    selected = "Overall_Standings"),
                    width = 3),
      mainPanel(DT::dataTableOutput("standings_table")
)
)
)
server <- function(input, output) 
  output$standings_table <- DT::renderDataTable( {
    if (input$report_input == "Overall_Standings") {
      df <- overall_earnings
      currency_col <- 'Total_Earnings'
      page_length <- 52
    }
    if (input$report_input == "Majors_Standings") {
      df <- majors_earnings
      currency_col <- 'Majors_Total'
      page_length <- 52
    }
    if (input$report_input == "Team_Detail") {
      df <- golfer_detail
      currency_col <- c('Jan.26.29', 'Feb.2.5', 'Feb.9.12', 'Feb.16.19', 'Feb.23.26', 'Mar.2.5', 'Mar.9.12', 'Mar.16.19', 'Mar.22.26', 'Mar.30.Apr.2', 'Apr.6.9.Masters', 'Apr.13.16', 'Apr.20.23', 'Apr.27.30', 'May.4.7', 'May.11.14', 'May.18.21', 'May.25.28', 'Jun.1.4', 'Jun.8.11', 'Jun.15.18U.S.Open', 'Jun.22.25', 'Jun.29.Jul.2', 'Jul.6.9', 'Jul.13.16', 'Jul.20.23.British.Open', 'Jul.20.23', 'Jul.27.30', 'Aug.3.6', 'Aug.10.13.PGA', 'Aug.17.20', 'Aug.24.27', 'Sep.1.4', 'Sep.14.17', 'Sep.21.24', 'Total')
      page_length <- 312
      }

    datatable(df, options = list(pageLength = page_length), rownames = FALSE, caption = paste("Results updated through ", colnames(roster)[length(tourney_ID_list$x) + 11], sep = "") ) %>% formatCurrency(currency_col)
      })

  
# Run the application 
shinyApp(ui = ui, server = server)

