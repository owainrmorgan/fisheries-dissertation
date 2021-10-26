library('tidyverse')
library('lubridate')

# First load in the data we're intereseted in
landings_data <- read_csv("_data/sample_landings_data_raw.csv")

# Then observe the data
landings_data

# Give the columns some intuitive names
landings_data <- landings_data %>%
  # Rename the columns
  rename(Year = yy,
         Date = dat,
         Trip_ID = trip,
         Effort_Hours = effort,
         Gear = gr,
         Species = sp,
         Length_cm = l_cm,
         Weight_g = w_cm) %>%
  # Turn the date column into a date format that R recognizes
  mutate(Date = mdy(Date)) 

landings_data

#Now find incomplete datapoints
landings_data[!complete.cases(landings_data),]

#Remove incomplete datapoints
landings_data <- na.omit(landings_data)

landings_data

#Now consider any typos
unique(landings_data$Gear)

#We lower case all Gear values
landings_data <- landings_data %>%
  mutate(Gear = tolower(Gear))
  
unique(landings_data$Gear)

#Now check for uniqueness among species name
unique(landings_data$Species)

#Count number of rows with either spelling
landings_data %>%
  filter(Species == "Caesoi cunning") %>%
  nrow()

landings_data %>%
  filter(Species == "Caesio cuning") %>%
  nrow()

#Clearly "Caesio cuning" is the correct spelling so we amend all records
landings_data <- landings_data %>%
  mutate(Species = replace(Species, Species =="Caesoi cunning", "Caesio cuning"))
unique(landings_data$Species)

#Now we check for accuracy of data entry
summary(landings_data$Length_cm)

#There seems to be an anomaly,
#Investigate by preparing a plot
plot(landings_data$Length_cm)

#There's clearly an outlier so we remove this
landings_data <- landings_data%>%
  filter(Length_cm < 100)
plot(landings_data$Length_cm)

#We also check for accuracy of weight and Effort Hours
summary(landings_data$Effort_Hours)
plot(landings_data$Effort_Hours)

summary(landings_data$Weight_g)
plot(landings_data$Weight_g)

#We now save these data as clean
write_csv(landings_data, "_data/sample_landings_data_clean.csv")

