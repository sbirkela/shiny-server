library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(googleVis)
library(leaflet)

first_app_day <- data.frame(read.csv("/srv/shiny-server/Lion_packager/first_app_day.csv", header = TRUE, sep = ","))
n_jobseekers <- data.frame(read.csv("/srv/shiny-server/Lion_packager/job_seekers.csv", header = TRUE, sep = ","))
n_applicants <- data.frame(read.csv("/srv/shiny-server/Lion_packager/n_applicants.csv", header = TRUE, sep = ","))
n_qualified_apps <- data.frame(read.csv("/srv/shiny-server/Lion_packager/n_qualified_apps.csv", header = TRUE, sep = ","))
df_time <- data.frame(read.csv("/srv/shiny-server/Lion_packager/df_time.csv", header = TRUE, sep = ","))
hear_sorted <- data.frame(read.csv("/srv/shiny-server/Lion_packager/hear_sorted.csv", header = TRUE, sep = ","))
educ_sorted <- data.frame(read.csv("/srv/shiny-server/Lion_packager/educ_sorted.csv", header = TRUE, sep = ","))
ind_background_graph <- data.frame(read.csv("/srv/shiny-server/Lion_packager/ind_background_graph.csv", header = TRUE, sep = ","))
latlng_apps <- data.frame(read.csv("/srv/shiny-server/Lion_packager/latlng_apps.csv", header = TRUE, sep = ","))


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
      theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=11), axis.title=element_text(size=14))
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
      theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=11), axis.title=element_text(size=14))
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
      theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=11), axis.title=element_text(size=14))
  })
  
  output$experience <- renderPlot({
    ggplot(data = ind_background_graph) +
      geom_bar(mapping = aes(x = reorder(experience, n),
                             y = n), width = .65, stat = "identity", 
               fill = "#3D65A0") +
      labs(
        title = "Industry Background", 
        x = "", 
        y = "Number of Applicants",
        caption = "Note that each applicant may have multiple experience categories"
      ) + coord_flip() +
      theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=11), axis.title=element_text(size=14))
  })
  
  
  # add leaflet map
  output$app_map <- renderLeaflet({
    latlng_apps %>%
      leaflet() %>%
      setView(lng = -93.94769, lat = 29.98585, zoom = 7) %>%
      addTiles() %>% 
      addCircleMarkers(data=latlng_apps,radius=1, color = "Blue")  
    
  })
  
})

#shinyApp(ui = ui, server = server)
