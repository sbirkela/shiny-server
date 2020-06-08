library(shiny)
library(DT)
library(readxl)
library(dplyr)
library(janitor)
library(shinydashboard)
library(shinyBS)

shinyServer(function(input, output) {
  

  output$x1 <-  DT::renderDataTable({
    #req(input$upload)
    raw_df <- read_xlsx(input$upload$datapath)
    
    portal_df <- raw_df %>% select(`First Name`, `Last Name`, `Status`, `Perc BASELINE`, 
                                   `Gender`, 
                                   `EthnicityAre you of Hispanic or Latino origin?`,
                                   `What is your race? (Only if Not Hispanic or Latino)`) 
    
    portal_df <- portal_df %>% rename(Race = `What is your race? (Only if Not Hispanic or Latino)`) #rename Race variable
    portal_df <- portal_df %>% rename(Eth_hispanic = `EthnicityAre you of Hispanic or Latino origin?`) #rename Are you of Hispanic or Latino origin variable
    portal_df$Race <- ifelse(is.na(portal_df$Race), portal_df$Eth_hispanic, portal_df$Race) #if race is left blank fill it in with response from Eth_hispanic
    portal_df <- portal_df %>% mutate(Race = replace(Race, Race == "Yes, I am Hispanic or Latino", "Hispanic or Latino")) #if original race was NA and yes to Eth_hispanic then code as Hispanic or Latino
    portal_df <- portal_df %>% mutate(Race = replace(Race, Race == "No, not of Hispanic or Latino", "Choose not to identify")) #if original race was NA and no to Eth_hispanic then code as Choose not to identify
    portal_df <- portal_df %>% mutate(Race = replace(Race, is.na(Race), "Choose not to identify")) #if both Race and Ethnic_hisp are NA then code as Choose not to identify
    portal_df <- portal_df %>% mutate(Gender = replace(Gender, is.na(Gender), "Choose not to identify"))
    portal_df <- portal_df %>% select(-Eth_hispanic)
    
    #Only select with status of pending and sort by BASELINE
    
    portal_df <- portal_df %>% filter(Status == "Pending")
    portal_df <- portal_df %>% arrange(desc(`Perc BASELINE`))
    
    
    datatable(portal_df)
    }, server = TRUE)

    
  output$x2 <- DT::renderDataTable({
        
    raw_df <- read_xlsx(input$upload$datapath)
    
    portal_df <- raw_df %>% select(`First Name`, `Last Name`, `Status`, `Perc BASELINE`, 
                                   `Gender`, 
                                   `EthnicityAre you of Hispanic or Latino origin?`,
                                   `What is your race? (Only if Not Hispanic or Latino)`) 
    
    portal_df <- portal_df %>% rename(Race = `What is your race? (Only if Not Hispanic or Latino)`) #rename Race variable
    portal_df <- portal_df %>% rename(Eth_hispanic = `EthnicityAre you of Hispanic or Latino origin?`) #rename Are you of Hispanic or Latino origin variable
    portal_df$Race <- ifelse(is.na(portal_df$Race), portal_df$Eth_hispanic, portal_df$Race) #if race is left blank fill it in with response from Eth_hispanic
    portal_df <- portal_df %>% mutate(Race = replace(Race, Race == "Yes, I am Hispanic or Latino", "Hispanic or Latino")) #if original race was NA and yes to Eth_hispanic then code as Hispanic or Latino
    portal_df <- portal_df %>% mutate(Race = replace(Race, Race == "No, not of Hispanic or Latino", "Choose not to identify")) #if original race was NA and no to Eth_hispanic then code as Choose not to identify
    portal_df <- portal_df %>% mutate(Race = replace(Race, is.na(Race), "Choose not to identify")) #if both Race and Ethnic_hisp are NA then code as Choose not to identify
    portal_df <- portal_df %>% mutate(Gender = replace(Gender, is.na(Gender), "Choose not to identify"))
    portal_df <- portal_df %>% select(-Eth_hispanic)
    
    #Only select with status of pending and sort by BASELINE
    
    portal_df <- portal_df %>% filter(Status == "Pending")
    portal_df <- portal_df %>% arrange(desc(`Perc BASELINE`))
    
    
    s <-  input$x1_rows_selected
        tab1 <- portal_df[s,]
        race_tab <- tab1 %>% 
        tabyl(Race) %>%
        arrange(desc(n)) %>% 
        adorn_pct_formatting(digits = 0) %>% 
        adorn_totals("row")
        
        majority_race_total <- race_tab %>% 
          filter(Race == input$race_majority)
        majority_race_total <- majority_race_total %>% select(n) %>% pull()
        totat_total <- race_tab %>% 
          filter(Race == "Total")
        total_total <- totat_total %>% select(n) %>% pull()
        
        race_tab <- race_tab %>% 
          mutate(impact_ratio = ((n/total_total)/(majority_race_total/total_total)))
        race_tab <- race_tab %>% 
          mutate_at(vars(starts_with("impact")), round, 2)
        race_tab$impact_ratio[race_tab$Race == "Total"] <- NA
        
        datatable(race_tab, options = list(dom = 't'))
    }, server = TRUE)
    
    output$x3 <- DT::renderDataTable({
      
      raw_df <- read_xlsx(input$upload$datapath)
      
      portal_df <- raw_df %>% select(`First Name`, `Last Name`, `Status`, `Perc BASELINE`, 
                                     `Gender`, 
                                     `EthnicityAre you of Hispanic or Latino origin?`,
                                     `What is your race? (Only if Not Hispanic or Latino)`) 
      
      portal_df <- portal_df %>% rename(Race = `What is your race? (Only if Not Hispanic or Latino)`) #rename Race variable
      portal_df <- portal_df %>% rename(Eth_hispanic = `EthnicityAre you of Hispanic or Latino origin?`) #rename Are you of Hispanic or Latino origin variable
      portal_df$Race <- ifelse(is.na(portal_df$Race), portal_df$Eth_hispanic, portal_df$Race) #if race is left blank fill it in with response from Eth_hispanic
      portal_df <- portal_df %>% mutate(Race = replace(Race, Race == "Yes, I am Hispanic or Latino", "Hispanic or Latino")) #if original race was NA and yes to Eth_hispanic then code as Hispanic or Latino
      portal_df <- portal_df %>% mutate(Race = replace(Race, Race == "No, not of Hispanic or Latino", "Choose not to identify")) #if original race was NA and no to Eth_hispanic then code as Choose not to identify
      portal_df <- portal_df %>% mutate(Race = replace(Race, is.na(Race), "Choose not to identify")) #if both Race and Ethnic_hisp are NA then code as Choose not to identify
      portal_df <- portal_df %>% mutate(Gender = replace(Gender, is.na(Gender), "Choose not to identify"))
      portal_df <- portal_df %>% select(-Eth_hispanic)
      
      #Only select with status of pending and sort by BASELINE
      
      portal_df <- portal_df %>% filter(Status == "Pending")
      portal_df <- portal_df %>% arrange(desc(`Perc BASELINE`))
      
      s <-  input$x1_rows_selected
      tab1 <- portal_df[s,]
      gender_tab <- tab1 %>% 
        tabyl(Gender) %>%
        arrange(desc(n)) %>% 
        adorn_pct_formatting(digits = 0) %>% 
        adorn_totals("row")
      
      majority_gender_total <- gender_tab %>% 
        filter(Gender == input$gender_majority)
      majority_gender_total <- majority_gender_total %>% select(n) %>% pull()
      totat_total <- gender_tab %>% 
        filter(Gender == "Total")
      total_total <- totat_total %>% select(n) %>% pull()
      
      gender_tab <- gender_tab %>% 
        mutate(impact_ratio = ((n/total_total)/(majority_gender_total/total_total)))
      gender_tab <- gender_tab %>% 
        mutate_at(vars(starts_with("impact")), round, 2)
      gender_tab$impact_ratio[gender_tab$Gender == "Total"] <- NA
      
      datatable(gender_tab, options = list(dom = 't'))
    }, server = TRUE)
    
})
