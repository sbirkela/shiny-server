library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(stringr)
library(exact2x2)
library(janitor)
library(ztable)


component_name <- c("Pre-screening", "JST Invite", "JST Completion", "JSTs", 
                    "In-Person Invite", "Work Demo Competion", "Work Demo",
                    "Interview", "Post Interview Offer Extended", "Offer Accepted")
  df_background <- data.frame(read.csv("/srv/shiny-server/Adverse_Impact/df_background.csv", header = TRUE, sep = ",")) 

# Define server logic required to view the selected dataset
server <- function(input, output) {

  # Return the requested dataset
  datasetInput <- reactive({
    code_check <- which(component_name == input$dataset)
    df_background$Outcome <- NA
    df_background$Outcome[df_background$Status_code==code_check] <- "Fail"
    df_background$Outcome[df_background$Status_code > code_check] <- "Pass"
    df_background$Outcome[df_background$Status_code < code_check] <- NA
    df_background <- df_background %>% filter(!is.na(Outcome))
    
    
    #Create frequency count table with Tabyl
    Table_title <- paste("Hiring Step - ", component_name[[code_check]], sep = "")
    #Table_title <- component_name[[code_check]]
    race_tab_raw <- df_background %>% 
      tabyl(Race, Outcome)
    
    race_tab1 <- df_background %>% 
      tabyl(Race, Outcome) %>%
      #adorn_totals("row") %>%
      adorn_percentages("row") %>%
      adorn_pct_formatting(digits = 0) %>% 
      adorn_ns(position = "front") 
    
    #Create ztable
    race_tab1 <- race_tab1 %>% select(Race, Pass, Fail) %>% 
      rename(`Demographic Categories` = Race)
    
    #Gender AI table
    gender_tab_raw <- df_background %>% 
      tabyl(Gender, Outcome)
    
    gender_tab1 <- df_background %>% 
      tabyl(Gender, Outcome) %>%
      adorn_totals("row") %>%
      adorn_percentages("row") %>%
      adorn_pct_formatting(digits = 0) %>% 
      adorn_ns(position = "front") 
    
    gender_tab1 <- gender_tab1 %>% select(Gender, Pass, Fail) %>% 
      rename(`Demographic Categories` = Gender)
    
    
    #Create column for 80% rule in 
    race_tab_raw <- race_tab_raw %>% rename(`Demographic Categories` = Race) %>% mutate(Success_rate = Pass/(Pass + Fail))
    majority_race_success <- race_tab_raw %>% filter(`Demographic Categories`== input$majority_race)
    majority_race_success <- majority_race_success$Success_rate
    race_tab_raw <- race_tab_raw %>% mutate(Impact_ratio = Success_rate/majority_race_success)
    race_tab_raw <- race_tab_raw %>% select(`Demographic Categories`, Impact_ratio)
    
    gender_tab_raw <- gender_tab_raw %>% rename(`Demographic Categories` = Gender) %>% mutate(Success_rate = Pass/(Pass + Fail))
    majority_gender_success <- gender_tab_raw %>% filter(`Demographic Categories`== input$majority_gender)
    majority_gender_success <- majority_gender_success$Success_rate
    gender_tab_raw <- gender_tab_raw %>% mutate(Impact_ratio = Success_rate/majority_gender_success)
    gender_tab_raw <- gender_tab_raw %>% select(`Demographic Categories`, Impact_ratio)
    
    #Combine race and gender tables into one
    race_combined <- left_join(race_tab1, race_tab_raw, by = "Demographic Categories")
    gender_combined <- left_join(gender_tab1, gender_tab_raw, by = "Demographic Categories")
    
    #________________________________________

    #Start of Fisher's Exact Test
    #_______________________________________
    race_categories <- race_tab1$`Demographic Categories`
    gender_categories <- gender_tab1$`Demographic Categories`
    
    #Race fisher p calculations
    race_cat_loop <- race_categories[race_categories != input$majority_race]  #subtract out majority race from race categories
    a <- 0
    race_midpvalues <- vector("integer", length(race_cat_loop))
    for (i in race_cat_loop){
      a <- a + 1
      df <- df_background %>% 
        tabyl(Race, Outcome)
      df <- df %>% filter(Race == input$majority_race| Race == i)
      x <- df[ ,c(2,3)]
      y <- df[ ,1]
      ai_table <- exact2x2(x, y = y, midp = TRUE)
      midp_value <- ai_table$p.value
      race_midpvalues[a] <- midp_value 
    }
    race_midpvalues_df <- tibble(race_cat_loop, race_midpvalues)
    race_midpvalues_df <- race_midpvalues_df %>% rename(`Demographic Categories` = race_cat_loop)
    race_combined_df <- left_join(race_combined, race_midpvalues_df, by = "Demographic Categories")
    race_combined_df <- race_combined_df %>% rename(`Impact Ratio` = Impact_ratio, `Prob Value` = race_midpvalues)
    
    #Gender fisher p calculations
    gender_cat_loop <- gender_categories[gender_categories != input$majority_gender]  #subtract out majority race from gender categories
    gender_cat_loop <- gender_cat_loop[gender_cat_loop != "Total"]  #need to also take out "Total" column from Gender table
    a <- 0
    gender_midpvalues <- vector("integer", length(gender_cat_loop))
    for (i in gender_cat_loop){
      a <- a + 1
      df <- df_background %>% 
        tabyl(Gender, Outcome)
      df <- df %>% filter(Gender == input$majority_gender | Gender == i)
      x <- df[ ,c(2,3)]
      y <- df[ ,1]
      ai_table <- exact2x2(x, y = y, midp = TRUE)
      midp_value <- ai_table$p.value
      gender_midpvalues[a] <- midp_value 
    }
    gender_midpvalues_df <- tibble(gender_cat_loop, gender_midpvalues)
    gender_midpvalues_df <- gender_midpvalues_df %>% rename(`Demographic Categories` = gender_cat_loop)
    gender_combined_df <- left_join(gender_combined, gender_midpvalues_df, by = "Demographic Categories")
    gender_combined_df <- gender_combined_df %>% rename(`Impact Ratio` = Impact_ratio, `Prob Value` = gender_midpvalues)
    
    
    #_________________________________________________________
    combined_tab <- bind_rows(race_combined_df, gender_combined_df)
    
    #Count of rows
    nrow_gender <- nrow(gender_combined_df)
    nrow_race <- nrow(race_combined_df)
    
    #Make the ztable
    options(ztable.colnames.bold=TRUE, ztable.caption.bold = TRUE)
    options(ztable.zebra=NULL)
    combined_ztab <- combined_tab %>% ztable(digits = 2, align="llllcc", caption=Table_title,
                                             caption.position="left",
                                             caption.placement="top",
                                             include.rownames=FALSE)
    
    cgroup <- c(" ", "Applicant Status", "80% Test", "Stat Test")
    n.cgroup <- c(1,2,1,1)
    combined_ztab <- combined_ztab %>% addcgroup(cgroup=cgroup, n.cgroup=n.cgroup)
    rgroup <- c("Race", "Gender", "All Participants")
    n.rgroup <- c(nrow_race, nrow_gender-1, 1)
    combined_ztab <- combined_ztab %>% addrgroup(rgroup=rgroup, n.rgroup=n.rgroup, cspan.rgroup=1)

  })
  
  # Show the table
  output$view <- renderPrint({
    print_ztable(datasetInput())
  })
}
