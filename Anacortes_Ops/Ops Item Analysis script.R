#library(readxl)
#library(readr)
#library(dplyr)
#library(psych)

#User entry
#Results are in ClientsShared/Tesoro/Assessment Results/JST Results/Item level data/Item analysis

#________________________________
file_name <- "Tesoro Anacortes Ops JST Item Level COMBINED 4.28.17sb.xlsx"
location <- "Tesoro Anacortes"
job <- "Operator"
#________________________________

#Data Prep
sheet_names <- excel_sheets(file_name)
sheet_names <- sheet_names[-1]

item_analysis <- list()
df_output <- list()

for (i in sheet_names){
  df_raw <- read_excel(file_name, 
                 sheet = i, na = c("*", 0))
  n_items <- read_excel(file_name, 
                      sheet = i, range = "E1:E2")
  n_items <- n_items$Score
  key_raw <- read_excel(file_name,
                  sheet = i, cell_rows(2), col_names = FALSE)

#take out "key" row and extra columns from data import
  df <- df_raw %>% filter(!`ID Number` == "KEY")  
  df <- df[ ,1:(n_items + 5)]

#separate out question data and answer key
  item_data <- df[ ,6:(n_items + 5)]
  key <- key_raw[1, (6:(n_items + 5))]
  key <- as.numeric(key[1, ])

#Scoring responses
  a <- psych::score.multiple.choice(key, item_data, missing = FALSE)
  item_analysis[[i]] <- a
  j <- which(sheet_names == i)
  b <- item_analysis[[j]][[1]]
  #col_set <- ncol(b) - 3 #use this line if use response frequency datas
  #df_output[[j]] <- b[ ,col_set:ncol(b)]  #Use this line if do not use response frequency data
  df_output[[j]] <- b[ ,1:ncol(b)] #Use this line if use response frequency data
  df_output[[j]]$item <- paste(i, "_", rownames(df_output[[j]]), sep = "")  #add item name
}

#Create final output to save as csv in working directory
final_output <- bind_rows(df_output)  #consolidates sheets from the df_output list

final_output <- final_output %>% rename(itemtotal_r = r,
                                        total_n = n, 
                                        item_diff = mean,
                                        item_sd = sd) 
final_output$location <- location
final_output$job <- job
final_output <- final_output %>% arrange(item_diff)
path_name <- paste(location, "_", job, "_item analysis.csv", sep = "")
write_csv(final_output, path_name)





