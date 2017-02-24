library(shiny)
library(dplyr)
library(ggplot2)
#library(rgdal)
#library(RColorBrewer)
#library(googleVis)
library(leaflet)


df <- data.frame(read.csv("/srv/shiny-server/inst_df.csv", header = TRUE, sep = ","))
df <- df[1:5, ]
df2 <- data.frame(read.csv("/srv/shiny-server/inst_df3.csv", header = TRUE, sep = ","))
latlng_all <- data.frame(read.csv("/srv/shiny-server/ltlng_all.csv", header = TRUE, sep = ","))

shinyServer(function(input, output, session) {
  
  # icons
  
  output$cycle_days <- renderInfoBox({
    infoBox(
      " Cycle Days", 56,   #Need to specify the date data change "56"
      icon = icon("calendar", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$applicants <- renderInfoBox({
    infoBox(
      "Job Seekers", 234, icon = icon("user"), #Need to specify the applicant number data change "34"
      color = "purple"
    )
  })
  
  output$pending_candidates <- renderInfoBox({
    infoBox(
      "Official Appicants", 10, #Need to specify the pending applicants change "10" 
      icon = icon("thumbs-up"),
      color = "yellow"
    )
  })
  
  output$baseline_score <- renderInfoBox({
    infoBox(
      "Ave Baseline Score", 64, #Need to specify the pending applicants change "10" 
      icon = icon("desktop"),
      color = "green"
    )
  })
  
  output$how_hear <- renderPlot({
    
    ggplot(data = df) +
      geom_bar(mapping = aes(x = reorder(How_hear, n),
                             y = n), stat = "identity", 
               fill = "Blue") +
      labs(
        title = "How Heard About Job Opening", 
        x = "Recruiting Source", 
        y = "# of Applicants"
      ) + coord_flip() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$app_vol <- renderPlot({
    ggplot(data = df2) +
      geom_bar(mapping = aes(x = day_bin, fill = Applicant)) +
      labs(
        title = "Applicant Volume", 
        x = "# Days Posted", 
        y = "# Applications Submitted"
      ) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_discrete(name = "Qualified Applicant")
  })
  # add leaflet map
  output$app_map <- renderLeaflet({
    latlng_all %>%
      leaflet() %>%
      setView(lng = -118.2000, lat = 33.8000, zoom = 7) %>%
      addTiles() %>% 
      addCircleMarkers(data=latlng_all,radius=1,group="All", color = "Blue") %>%
      addCircleMarkers(data=latlng_elect,radius=1,group="Electricians", color = "Blue") %>%
      addCircleMarkers(data=latlng_inst,radius=1,group="Instrument Technicians", color = "Blue") %>%
      addCircleMarkers(data=latlng_mech,radius=1,group="Mach/Mill/Mech", color = "Blue") %>%
      addCircleMarkers(data=latlng_pipe,radius=1,group="Pipefitter", color = "Blue") %>%
      addCircleMarkers(data=latlng_rigger,radius=1,group="Rigger", color = "Blue") %>%
      addLayersControl(
        baseGroups = c("All", "Electricians", "Instrument Technicians", "Mach/Mill/Mech", "Pipefitter", "Rigger"),
        options = layersControlOptions(collapsed=FALSE)
      )  
    
  })
  
})
