library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(googleVis)
library(leaflet)

time_update <- data.frame(read.csv("time_update.csv", header = TRUE, sep = ","))
n_jobseekers <- data.frame(read.csv("job_seekers.csv", header = TRUE, sep = ","))
n_applicants <- data.frame(read.csv("n_applicants.csv", header = TRUE, sep = ","))
n_qualified_apps <- data.frame(read.csv("n_qualified_apps.csv", header = TRUE, sep = ","))
df_time <- data.frame(read.csv("df_time.csv", header = TRUE, sep = ","))
hear_sorted <- data.frame(read.csv("hear_sorted.csv", header = TRUE, sep = ","))
educ_sorted <- data.frame(read.csv("educ_sorted.csv", header = TRUE, sep = ","))
mil_sorted <- data.frame(read.csv("mil_sorted.csv", header = TRUE, sep = ","))
latlng_apps <- data.frame(read.csv("latlng_apps.csv", header = TRUE, sep = ","))


shinyServer(function(input, output) {
  
  # icons
  
  output$time_update <- renderInfoBox({
    infoBox(
      "Last Update", time_update,   
      icon = icon("calendar", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$app_responses <- renderInfoBox({
    infoBox(
      "Application Responses", n_jobseekers$x, icon = icon("user"),
      color = "purple"
    )
  })
  
  output$init_applicants <- renderInfoBox({
    infoBox(
      "Initial Applicants", n_applicants$n_apps, #Need to specify the pending applicants change "10" 
      icon = icon("user"),
      color = "yellow"
    )
  })
  
  output$pend_applicants <- renderInfoBox({
    infoBox(
      "Pending Applicants", n_qualified_apps$n, #Need to specify the pending applicants change "10" 
      icon = icon("thumbs-up"),
      color = "green"
    )
  })
  
  output$app_vol <- renderPlot({
    
    ggplot(data = df_time) +
      geom_smooth(mapping = aes(x = app_day, y = Respondent)) +
      labs(
        title = "Application Activity", 
        x = "Days Job Has Been Posted", 
        y = "Application Responses"
      ) +
      theme(plot.title = element_text(hjust = 0.5, size = 18), axis.text=element_text(size=12), axis.title=element_text(size=16))
  })
  
  output$how_hear <- renderPlot({
    ggplot(data = hear_sorted) +
      geom_bar(mapping = aes(x = reorder(how_hear, n),
                             y = n), width = .65, stat = "identity", 
               fill = "#3D65A0") +
      labs(
        title = "Top Recruiting Sources", 
        x = "", 
        y = "Number of Application Responses"
      ) + coord_flip() +
      theme(plot.title = element_text(hjust = 0.5, size = 18), axis.text=element_text(size=12), axis.title=element_text(size=16))
  })
  
  output$education <- renderPlot({
    ggplot(data = educ_sorted) +
      geom_bar(mapping = aes(x = reorder(education, n),
                             y = n), width = .55, stat = "identity", 
               fill = "#3D65A0") +
      labs(
        title = "Educational Background", 
        x = "", 
        y = "Number of Applicants"
      ) + coord_flip() +
      theme(plot.title = element_text(hjust = 0.5, size = 18), axis.text=element_text(size=12), axis.title=element_text(size=16))
  })
  
  output$experience <- renderPlot({
    ggplot(data = exp_sorted) +
      geom_bar(mapping = aes(x = reorder(experience, n),
                             y = n), width = .55, stat = "identity", 
               fill = "#3D65A0") +
      labs(
        title = "At Least Two Years Refinery or Chemical Plant Experience", 
        x = "", 
        y = "Number of Applicants"
      ) + coord_flip() +
      theme(plot.title = element_text(hjust = 0.5, size = 18), axis.text=element_text(size=12), axis.title=element_text(size=16))
  })
  
  output$military <- renderPlot({
    ggplot(data = mil_sorted) +
      geom_bar(mapping = aes(x = reorder(armed_forces, n),
                             y = n), width = .65, stat = "identity", 
               fill = "#3D65A0") +
      labs(
        title = "Armed Services Background", 
        x = "", 
        y = "Number of Applicants"
      ) + coord_flip() +
      theme(plot.title = element_text(hjust = 0.5, size = 18), axis.text=element_text(size=12), axis.title=element_text(size=16))
  })
  
  
  # add leaflet map
  output$app_map <- renderLeaflet({
    latlng_apps %>%
      leaflet() %>%
      setView(lng = -122.6127, lat = 48.5126, zoom = 7) %>%
      addTiles() %>% 
      addCircleMarkers(data=latlng_apps,radius=1, color = "Blue")  
    
  })
  
})

#shinyApp(ui = ui, server = server)