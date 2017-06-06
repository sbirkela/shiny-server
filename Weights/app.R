
library(shiny)
library(plotly)
library(corrr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   plotlyOutput("plot"),
   verbatimTextOutput("hover"),
   verbatimTextOutput("click"),
   verbatimTextOutput("brush"),
   verbatimTextOutput("weights")
)

server <- function(input, output, session) {
  df <- read.csv("/srv/shiny-server/Weights/df.csv")
   output$plot <- renderPlotly({
         plot_ly(df, x = ~r, y = ~ai, key = ~key) %>% 
           layout(dragmode = "select")
   })
   
   output$weights <- renderPrint({
     d1 <- event_data("plotly_click")
     d2 <- d1$key
     d <- df %>% filter(key == d2) %>% select(Cognitive:r) %>% fashion(decimals = 3)
     if (is.null(d)) "Click events appear here (double click to clear)" else d
   })
   
   }



shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))



