library(shiny)
library(DT)
library(readxl)
library(dplyr)
library(janitor)
library(shinydashboard)
library(shinyBS)
library(writexl)
library(scales)

shinyServer(function(input, output) {
  
#table to select candidates________________________
  output$x1 <-  DT::renderDataTable({
    req(input$upload)
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
    }, server = FALSE)

#Race impact table________________________________    
  output$x2 <- DT::renderDataTable({
    req(input$upload) 
    req(input$x1_rows_selected)
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
    
    #calculate total number of pending candidates
  
    total_tab <- portal_df %>% 
      group_by(Race) %>% 
      summarise(total_pending = n()) 
    
    #calculate total number of selected candidates
    selected <-  input$x1_rows_selected
        tab1 <- portal_df[selected,]
        race_tab <- tab1 %>% 
          group_by(Race) %>% 
          summarise(total_selected = n())
        
        tab2 <- left_join(total_tab, race_tab, by = "Race") %>% 
          select(Race, total_selected, total_pending) 
        tab2$total_selected <- ifelse(is.na(tab2$total_selected), 0, tab2$total_selected)
        
        tab2 <- tab2 %>% mutate(selection_rate = (total_selected/total_pending))
        
        tab2$selection_rate <- scales::percent(tab2$selection_rate, accuracy = .1)
        majority_race_percent <- tab2 %>% 
          filter(Race == input$race_majority) %>% 
          mutate(majority_selected = total_selected/total_pending) %>% 
          select(majority_selected) %>% 
          pull()
        
        race_tab <- tab2 %>% mutate(impact_ratio = ((total_selected/total_pending)/majority_race_percent))
        race_tab$impact_ratio <- round(race_tab$impact_ratio, digits = 2)  
        
        race_tab <- race_tab %>% arrange(desc(total_selected))
        race_tab <- race_tab %>% adorn_totals(where = "row")
        race_tab$impact_ratio[race_tab$Race == "Total"] <- NA
        
        race_tab <- race_tab %>% 
          rename(`Total Selected` = total_selected, `Total Pending` = total_pending, `Selection Rate` = selection_rate, `Impact Ratio` = impact_ratio)
        
        datatable(race_tab, options = list(dom = 't', columnDefs = list(list(className = 'dt-center', targets = 1:4))), rownames = FALSE)
    }, server = FALSE)

  
#Gender impact table__________________________    
    output$x3 <- DT::renderDataTable({
      req(input$upload)
      req(input$x1_rows_selected)
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
      
      
      total_tab <- portal_df %>% 
        group_by(Gender) %>% 
        summarise(total_pending = n()) 
      
      #calculate total number of selected candidates
      selected <-  input$x1_rows_selected
      tab1 <- portal_df[selected,]
      gender_tab <- tab1 %>% 
        group_by(Gender) %>% 
        summarise(total_selected = n())
      
      tab2 <- left_join(total_tab, gender_tab, by = "Gender") %>% 
        select(Gender, total_selected, total_pending) 
      tab2$total_selected <- ifelse(is.na(tab2$total_selected), 0, tab2$total_selected)
      
      tab2 <- tab2 %>% mutate(selection_rate = (total_selected/total_pending))
      
      tab2$selection_rate <- scales::percent(tab2$selection_rate, accuracy = .1)
      majority_gender_percent <- tab2 %>% 
        filter(Gender == input$gender_majority) %>% 
        mutate(majority_selected = total_selected/total_pending) %>% 
        select(majority_selected) %>% 
        pull()
      
      gender_tab <- tab2 %>% mutate(impact_ratio = ((total_selected/total_pending)/majority_gender_percent))
      gender_tab$impact_ratio <- round(gender_tab$impact_ratio, digits = 2)  
      
      gender_tab <- gender_tab %>% arrange(desc(total_selected))
      gender_tab <- gender_tab %>% adorn_totals(where = "row")
      gender_tab$impact_ratio[gender_tab$Gender == "Total"] <- NA
      
      gender_tab <- gender_tab %>% 
        rename(`Total Selected` = total_selected, `Total Pending` = total_pending, `Selection Rate` = selection_rate, `Impact Ratio` = impact_ratio)
      
      datatable(gender_tab, options = list(dom = 't', columnDefs = list(list(className = 'dt-center', targets = 1:4))), rownames = FALSE)
    }, server = FALSE)
      

#File download_______________________________
    
    output$Candidate_download <- downloadHandler(
      
      filename = function(){
        paste("Selected_Candidates.xlsx")
      },
      
      content = function(file){
      
      req(input$upload)
      req(input$x1_rows_selected)
  
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
      downloaded_data <- portal_df[s,]
      write_xlsx(downloaded_data, file)
      }
       )
    
})
