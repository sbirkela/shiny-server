#library(readxl)
#library(readr)
#library(dplyr)
#library(psych)


#The purpose of this script is to examine the overall JST results from the Anacortes Ops testing. One goal is to look at difference between Proctor U and in person scores.

#User entry
#________________________________
file_name <- "Tesoro Anacortes Ops JST Results 4.14.17cg REVISED.xls"
location <- "Tesoro Anacortes"
job <- "Operator"
#________________________________

test_location <- read_excel(file_name, 
                          sheet = "Sorted Results", range = "B2:B509")
applicantID <- read_excel(file_name,
                          sheet = "Sorted Results", range = "G2:G509")
total_score <- read_excel(file_name,
                          sheet = "Sorted Results", range = "X2:X509")
jst_pass <- read_excel(file_name,
                       sheet = "Sorted Results", range = "Y2:Y509")

listening <- read_excel(file_name,
                        sheet = "Sorted Results", range = "J2:J509") 
gauges <- read_excel(file_name,
                     sheet = "Sorted Results", range = "L2:L509")
logic <- read_excel(file_name,
                    sheet = "Sorted Results", range = "N2:N509")
math <- read_excel(file_name,
                   sheet = "Sorted Results", range = "P2:P509")
pipes <- read_excel(file_name,
                    sheet = "Sorted Results", range = "R2:R509")
reading <- read_excel(file_name,
                      sheet = "Sorted Results", range = "T2:T509")
mech <- read_excel(file_name,
                   sheet = "Sorted Results", range = "V2:V509")

listening <- listening %>% rename(raw_lu = `# Correct - 11`)
gauges <- gauges %>% rename(raw_gauges = `# Correct - 16`)
logic <- logic %>% rename(raw_logic = `# Correct - 15`)
math <- math %>% rename(raw_math = `# Correct - 17`)
pipes <- pipes %>% rename(raw_pipes = `# Correct - 13`)
reading <- reading %>% rename(raw_read = `# Correct - 15`)
mech <- mech %>% rename(raw_mech = `# Correct - 34`)

jst_results <- bind_cols(applicantID, test_location, total_score, jst_pass, listening,
                         gauges, logic, math, pipes, reading, mech)
rm(applicantID, test_location, total_score, jst_pass, listening,
   gauges, logic, math, pipes, reading, mech)
jst_results <- jst_results %>% rename(ApplicantID = `ID #`,
                                      Total_Score = `Composite Score`, 
                                      JST_Pass = `Composite Pass - 91`
                                      )


x <- jst_results %>% group_by(Location) %>% summarise(
    ave_lu = mean(raw_lu, na.rm = TRUE),
    ave_gauges = mean(raw_gauges, na.rm = TRUE),
    ave_logic = mean(raw_logic, na.rm = TRUE),
    ave_math = mean(raw_math, na.rm = TRUE),
    ave_pipes = mean(raw_pipes, na.rm = TRUE),
    ave_reading = mean(raw_read, na.rm = TRUE),
    ave_mech = mean(raw_mech, na.rm = TRUE),
    ave_total = mean(Total_Score, na.rm = TRUE)
)


