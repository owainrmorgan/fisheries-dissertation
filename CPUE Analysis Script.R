#Analysis as directed by Gustav regarding CPUE

#Verbatim from Gustav email:
#Produce an RMarkdown document that compares the median CPUE to the mean CPUE. 
#Ideally you would plot the time series for both in the same figure. 
#This will require some skills beyond what 
#"Introductory Fisheries Analyses with R" introduces.
#Chapters 3 and 4 of R for Data Science at https://r4ds.had.co.nz/ may be helpful. 
#Perhaps also make a boxplot of the distribution of CPUE to understand the 
#origin of the difference between mean and median. 

#Guided by "An introduction to basic fisheries analysis with R"

#NB; summarise() and summarize() are synonyms.
#I will tend to use the French (rather than Greek) spelling

library('tidyverse')
library('lubridate')

#Load data
cpue_data <- read_csv("_data/sample_landings_data_clean.csv")

#Check it's correct
cpue_data

#Add a column for Weight/kg
cpue_data <- cpue_data %>%
  mutate(Weight_kg = Weight_g/1000.0)

#Now calculate CPUE for each trip
cpue_data <- cpue_data %>%
  #Group by Year and Trip ID
  group_by(Year,Trip_ID) %>%
  #For each trip, calculate CPUE by dividing weight of catch by time spent
  summarise(Trip_CPUE = sum(Weight_kg) / mean(Effort_Hours))


#I assume we want the time series (to compare mean and median)
#for the year rather than for each individual trip.
cpue_data_ann_median <- cpue_data %>%
  group_by(Year) %>%
  summarise(Median_CPUE_kg_per_hr = median(Trip_CPUE)) 

cpue_data_ann_mean <- cpue_data %>%
  group_by(Year) %>%
  summarise(Mean_CPUE_kg_per_hr = mean(Trip_CPUE)) 

#Fortunately cpue_data_ann_median matches with the published results.

#Now we plot the data,
#first plot just median like tutorial
#to ensure consistency
cpue_data_ann_median %>%
  #Initialise a ggplot of median CPUE vs year
  ggplot(aes(x=Year, y=Median_CPUE_kg_per_hr)) +
  #Scatter plot
  geom_point() +
  #Line connecting points
  geom_line() +
  #Set y-axis title
  ylab("Median CPUE [kg/hr]")

#Now do the same for mean to have an idea of the comparison
cpue_data_ann_mean %>%
  #Initialise a ggplot of median CPUE vs year
  ggplot(aes(x=Year, y=Mean_CPUE_kg_per_hr)) +
  #Scatter plot
  geom_point() +
  #Line connecting points
  geom_line() +
  #Set y-axis title
  ylab("Mean CPUE [kg/hr]")

ggplot() +
  geom_point(shape = 24, data = cpue_data_ann_mean, aes(x=Year, y=Mean_CPUE_kg_per_hr), 
             color = "red", show.legend = TRUE) +
  geom_line(data = cpue_data_ann_mean, aes(x=Year, y=Mean_CPUE_kg_per_hr), 
            color = "red", show.legend = TRUE) +
  geom_point(shape = 22, data = cpue_data_ann_median, aes(x=Year, y=Median_CPUE_kg_per_hr), 
             color = "blue") +
  geom_line(data = cpue_data_ann_median, aes(x=Year, y=Median_CPUE_kg_per_hr), 
             color = "blue") +
  #xlab("Year") +
  #ylab("Comparison of CPUE [kg/hr]") +
  ylab("CPUE [kg/hr]") +
  ggtitle("Comparison of Mean and Median for CPUE") +
  #theme(legend.justification = "left")
  theme(legend.position = c(.05,.95))


