
library(shiny)
library(ggplot2)

# Load data. Will need to add /srv/shiny-server before putting on digital ocean server
inst_df <- data.frame(read.csv("inst_df.csv", header = TRUE, sep = ","))
elect_df <- data.frame(read.csv("elect_df.csv", header = TRUE, sep = ","))
mech_df <- data.frame(read.csv("mech_df.csv", header = TRUE, sep = ","))
pipe_df <- data.frame(read.csv("pipe_df.csv", header = TRUE, sep = ","))
rig_df <- data.frame(read.csv("rig_df.csv", header = TRUE, sep = ","))

inst_df2 <- data.frame(read.csv("inst_df2.csv", header = TRUE, sep = ","))
elect_df2 <- data.frame(read.csv("elect_df2.csv", header = TRUE, sep = ","))
mech_df2 <- data.frame(read.csv("mech_df2.csv", header = TRUE, sep = ","))
pipe_df2 <- data.frame(read.csv("pipe_df2.csv", header = TRUE, sep = ","))
rig_df2 <- data.frame(read.csv("rig_df2.csv", header = TRUE, sep = ","))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("2017 LAR Maintenance Hiring Process Data"),
   
   # Generate a row with a sidebar
   sidebarLayout(      
     
     # Define the sidebar with one input
     sidebarPanel(
                    selectInput("job_input", label = "Select a job",
                   choices= c("Electrician", "Instrumentation", "Machinist/Millwright/Mechanic", "Pipefitter", "Rigger"),
                   selected = "Electrician"),
                   width = 3),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("recruitPlot")
         #plotOutput("minqualPlot")
    )
   )
)

server <- function(input, output) {
 
   output$recruitPlot <- renderPlot({
     if (input$job_input == "Instrumentation") {
       df <- inst_df
     }
     if (input$job_input == "Electrician") {
       df <- elect_df
     }
     if (input$job_input == "Machinist/Millwright/Mechanic") {
       df <- mech_df
     }
     if (input$job_input == "Rigger") {
       df <- rig_df
     }
     if (input$job_input == "Pipefitter") {
       df <- pipe_df
     } 
     ggplot(data = df) +
       geom_bar(mapping = aes(x = reorder(How_hear, n),
                              y = n), stat = "identity", 
                fill = "Blue") +
       labs(
         title = paste("Tesoro LAR ", input$job_input, " Recruiting Data", sep = ""), 
         x = "Recruiting Source", 
         y = "# of Applicants"
       ) + coord_flip() +
       theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text = element_text(size=12), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

