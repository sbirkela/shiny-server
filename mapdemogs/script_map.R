raw_df <- data.frame(read_excel("~/R/data/LAR Operator data 12.6.16sb.xlsx", sheet = "Operator Oct 2016"))

#Create demographic dataframe and change applicant ID to all uppercase
colnames(raw_df)[78] <- "ApplicantID"
a <- raw_df$ApplicantID  #need to make sure all Applicant ID numbers start with capital letter
a <- toupper(a)
raw_df$ApplicantID <- a
raw_df$What.is.your.race...Only.if.Not.Hispanic.or.Latino. <- ifelse(is.na(raw_df$What.is.your.race...Only.if.Not.Hispanic.or.Latino.),raw_df$EthnicityAre.you.of.Hispanic.or.Latino.origin.,raw_df$What.is.your.race...Only.if.Not.Hispanic.or.Latino.)
colnames(raw_df)[228] <- "Race"
raw_df$Race <- ifelse(raw_df$Race=="Yes, I am Hispanic or Latino","Hispanic or Latino",raw_df$Race)
raw_df$Race <- ifelse(is.na(raw_df$Race), "Choose not to identify",raw_df$Race)
raw_df$Gender <- ifelse(is.na(raw_df$Gender), "Choose not to identify",raw_df$Gender)

#make sure applicant ID and Education variables are in correct columns
df <- raw_df[,c(78,5,8,45,82:85,226,228)]
colnames(df)[1] <- "ApplicantID"
colnames(df)[4] <- "Education"


#this creates the address variable that was used to geocode the latitude and longitude information...needs to have it be in the same order as when geocoded
df1 <- unite(df,Address,c(Address.Street,Address.City,Address.State),sep = ", ")
df1 <- filter(df1,!is.na(Address))

#Use this below line if you need to get the latitude and longitude. This takes a while. I have already done that and saved data to a csv
#latlng_all <- geocode(df1$Address)
latlng_all <- data.frame(read.csv("~/R/data/Ops All latlongs.csv"))
latlng_all <- latlng_all[,c(2:3)]

#colnames(latlng_all)[1] <- "lng"
#write.csv(latlng_all,file = "~/Documents/R/Clients/Tesoro/Ops All latlongstest.csv")
df1 <- cbind(df1,latlng_all)

#Dynamic map using leaflet
#Need Dplyr
latlng_df <- select(df1,Race,lng,lat)
latlng_df <- filter(latlng_df,!is.na(lng))

#create datasets by the major races
asian <- filter(latlng_df, Race == "Asian")
blck <- filter(latlng_df, Race == "Black or African-American")
hispanic <- filter(latlng_df, Race == "Hispanic or Latino")
white <- filter(latlng_df, Race == "White")
two <- filter(latlng_df, Race == "Two or More Races")
notid <- filter(latlng_df, Race == "Choose not to identify")

latlng_df %>%
  leaflet() %>%
  setView(lng = -118.2000, lat = 33.8000, zoom = 7) %>%
  addTiles() %>% 
  addCircleMarkers(data=latlng_df,radius=1,group="All", color = "Blue") %>%
  addCircleMarkers(data=asian,radius=1,group="Asian", color = "Blue") %>%
  addCircleMarkers(data=blck,radius=1,group="Black or African-American", color = "Blue") %>%
  addCircleMarkers(data=hispanic,radius=1,group="Hispanic", color = "Blue") %>%
  addCircleMarkers(data=white,radius=1,group="White", color = "Blue") %>%
  addCircleMarkers(data=two,radius=1,group="Two or more races", color = "Blue") %>%
  addCircleMarkers(data=notid,radius=1,group="Choose not to identify", color = "Blue") %>%
  addLayersControl(
    baseGroups = c("All", "Asian", "Black or African-American", "Hispanic", "White", "Two or more races","Choose not to identify"),
    options = layersControlOptions(collapsed=FALSE)
  )  


