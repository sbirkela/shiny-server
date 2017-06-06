
library(shiny)
library(plotly)
library(corrr)


# Define UI for application that draws a histogram
ui <- fluidPage(
   plotlyOutput("plot"),
   verbatimTextOutput("weights")
)

server <- function(input, output, session) {
  df <- read.csv("df.csv")
   output$plot <- renderPlotly({
         plot_ly(df, x = ~r, y = ~ai, key = ~key) %>% 
           layout(dragmode = "select")
   })

   output$weights <- renderPrint({
     d1 <- event_data("plotly_click")
     if (is.null(d1)) {
      "Click graph to obtain weights" 
     } else {
     d2 <- d1$key
     d <- df %>% filter(key == d2) %>% select(Cognitive:r) %>% fashion(decimals = 3)
     print(d)
     }
   })
   }

shinyApp(ui = ui, server = server)



