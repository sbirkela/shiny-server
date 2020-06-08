library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(googleVis)
library(leaflet)
library(shadowtext) #might be new
library(ggrepel)

time_update <- data.frame(read.csv("time_update.csv", header = TRUE, sep = ","))
n_jobseekers <- data.frame(read.csv("job_seekers.csv", header = TRUE, sep = ","))
n_applicants <- data.frame(read.csv("n_applicants.csv", header = TRUE, sep = ","))
n_qualified_apps <- data.frame(read.csv("n_qualified_apps.csv", header = TRUE, sep = ","))
df_time <- data.frame(read.csv("df_time.csv", header = TRUE, sep = ","))
hear_sorted <- data.frame(read.csv("hear_sorted.csv", header = TRUE, sep = ","))
#educ_sorted <- data.frame(read.csv("educ_sorted.csv", header = TRUE, sep = ","))
#exp_sorted <- data.frame(read.csv("exp_sorted.csv", header = TRUE, sep = ","))
mil_sorted <- data.frame(read.csv("mil_sorted.csv", header = TRUE, sep = ","))
latlng_apps <- data.frame(read.csv("latlng_apps.csv", header = TRUE, sep = ","))
to_funnel <- read_csv("to_funnel.csv")
to_funnel$Step <- as.character(to_funnel$Step)
status_df <- read_csv("status_df.csv")
status_df$Step <- as.character(status_df$Step)
labels <- read_csv("labels.csv")
labels$Step <- as.character(labels$Step)


shinyServer(function(input, output) {
  
  output$app_funnel <- renderPlot({
    to_funnel %>%   
    ggplot() +
      geom_polygon(aes(x, y, group = group, fill = Step)) +
      geom_shadowtext(aes(x = .5, y = y, label = paste0(count)), data = labels, size = 3.5, color = "white") +
      geom_text_repel(aes(x = 0.46, y = y, label = paste0(Status)), 
                      data = labels, 
                      size = 3.5, 
                      nudge_x = -.8,
                      arrow = arrow(length = unit(0.01, "npc")),
                      segment.colour = "#595959",
                      segment.size = .25
      ) +
      
      theme_minimal() +
      
      theme(
        legend.position = "none", 
        axis.title = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), #leave out if have y labels
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),  #leave out if have y labels
        panel.grid.minor.y = element_blank()
      ) +
      scale_fill_manual(values = status_df$color) 
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
  
  #output$education <- renderPlot({
   # ggplot(data = educ_sorted) +
    #  geom_bar(mapping = aes(x = reorder(education, n),
     #                        y = n), width = .55, stat = "identity", 
      #         fill = "#3D65A0") +
      #labs(
       # title = "Educational Background", 
      #  x = "", 
       # y = "Number of Applicants"
    #  ) + coord_flip() +
     # theme(plot.title = element_text(hjust = 0.5, size = 18), axis.text=element_text(size=12), axis.title=element_text(size=16))
  #})
  
  #output$experience <- renderPlot({
   # ggplot(data = exp_sorted) +
  #    geom_bar(mapping = aes(x = reorder(Background, n),
   #                          y = n), width = .55, stat = "identity", 
    #           fill = "#3D65A0") +
     # labs(
      #  title = "Background - 2 Yrs of Refinery Experience AND/OR PTech Degree", 
      #  x = "", 
      #  y = "Number of Applicants"
    #  ) + coord_flip() +
    #  theme(plot.title = element_text(hjust = 0.5, size = 18), axis.text=element_text(size=12), axis.title=element_text(size=16))
#  })
  
  #output$military <- renderPlot({
   # ggplot(data = mil_sorted) +
    #  geom_bar(mapping = aes(x = reorder(armed_forces, n),
     #                        y = n), width = .65, stat = "identity", 
      #         fill = "#3D65A0") +
      #labs(
       # title = "Armed Services Background", 
      #  x = "", 
       # y = "Number of Applicants"
      #) + coord_flip() +
      #theme(plot.title = element_text(hjust = 0.5, size = 18), axis.text=element_text(size=12), axis.title=element_text(size=16))
  #})
  
  
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