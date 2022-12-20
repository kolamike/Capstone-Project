#load all packages needed for the cleaning and analysis process
library(tidyverse)
library(janitor)
library(here)
library(skimr)
library(lubridate)

#import the files singly via read_csv
jan <- read_csv("C:/Users/HP/Downloads/202201-divvy-tripdata.csv")
feb <- read_csv("C:/Users/HP/Downloads/202102-divvy-tripdata.csv")
mar <- read_csv("C:/Users/HP/Downloads/202103-divvy-tripdata.csv")
apr <- read_csv("C:/Users/HP/Downloads/202104-divvy-tripdata.csv")
may <- read_csv("C:/Users/HP/Downloads/202105-divvy-tripdata.csv")
jun <- read_csv("C:/Users/HP/Downloads/202106-divvy-tripdata.csv")
jul <- read_csv("C:/Users/HP/Downloads/202107-divvy-tripdata.csv")
aug <- read_csv("C:/Users/HP/Downloads/202108-divvy-tripdata.csv")
sep <- read_csv("C:/Users/HP/Downloads/202109-divvy-tripdata.csv")
oct <- read_csv("C:/Users/HP/Downloads/2021010-divvy-tripdata.csv")
nov <- read_csv("C:/Users/HP/Downloads/2021011-divvy-tripdata.csv")
dec <- read_csv("C:/Users/HP/Downloads/2021012-divvy-tripdata.csv")

#merging the individual data sets into one using rbind for easy view and analysis

all_trips <- rbind(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)

#preview the merged data
ncol(all_trips) #to view number of columns
nrow(all_trips) #to view number of rows
str(all_trips) # to view columns and data types
colnames(all_trips) #list of column names
head(all_trips) #to preview the first 6 rows
tail(all_trips) #to preview the last 6 rows

#Having reviewed the data set and fields, there is a need to create two new columns
#named "ride_length" and "day_of_week", this is to give more insight into the
#data set

#create new column named "ride_length" in mins
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at, units = "mins")

#round up ride_length to 1 decimal place
all_trips$ride_length <- round(all_trips$ride_length, digits = 1)

#create a new column - "day_of_week"
all_trips$day_of_week <- wday(all_trips$started_at)

#assiging the dataframe to a new variable
all_trips1 <- all_trips

#For easy analysis, let's create a column for month
all_trips1$month <- format(as.Date(all_trips1$started_at), "%m")

#The month column has been created but each month is represented by its number
#Let's change it from number to character i.e from 06 to "June"

all_trips1 <- all_trips1 %>% 
  mutate(month = case_when(month == "01" ~ "January",
                           month == "02" ~ "February",
                           month == "03" ~ "March",
                           month == "04" ~ "April",
                           month == "05" ~ "May",
                           month == "06" ~ "June",
                           month == "07" ~ "July",
                           month == "08" ~ "August",
                           month == "09" ~ "September",
                           month == "10" ~ "October",
                           month == "11" ~ "November",
                           month == "12" ~ "December"))

#Let's do the same for day_of_week column
all_trips1 <- all_trips1 %>%
  mutate(day_of_week = case_when(day_of_week == "1" ~ "Sunday",
                                 day_of_week == "2" ~ "Monday",
                                 day_of_week == "3" ~ "Tuesday",
                                 day_of_week == "4" ~ "Wednesday",
                                 day_of_week == "5" ~ "Thursday",
                                 day_of_week == "6" ~ "Friday",
                                 day_of_week == "7" ~ "Saturday"))

#previewing the data to confirm all columns have the correct data types
str(all_trips1)

#Remove rows with NA values
all_trips1 <- na.omit(all_trips1)

#Remove duplicate rows
all_trips1 <- distinct(all_trips1)

#reomve row with negative ride_length
all_trips1 <- all_trips1 [!(all_trips1$ride_length <= 0),]

#Preview cleaned data
View(all_trips1)


summary(all_trips1)

#change ride_length data type to numeric
all_trips1$ride_length <- as.numeric(as.character(all_trips1$ride_length))

#Descriptive analysis of cleaned data
#mean, median, max, min of the ride_length

mean(all_trips1$ride_length)
max(all_trips1$ride_length)
median(all_trips1$ride_length)
min(all_trips1$ride_length)

#Average ride_length for members and casuals

all_trips1 %>% 
  group_by(member_casual) %>% 
  summarize(avg_ride_length = mean(ride_length))

#Average ride_length by day_of_week

all_trips1 %>% 
  group_by(day_of_week) %>% 
  summarise(avg_ride_length = mean(ride_length))

#Average ride_length for members and casuals by day_of_week

all_trips1 %>% 
  group_by(day_of_week, member_casual) %>% 
  summarise(avg_ride_length = mean(ride_length))

#The day_of_week needs to be ordered, it should be arranged
#since it is a categorical data, so our results doesn't look distorted.

all_trips1$day_of_week <- ordered(all_trips1$day_of_week, levels = 
                                    c("Sunday", "Monday",
                                      "Tuesday", "Wednesday",
                                      "Thursday", "Friday",
                                      "Saturday"))

#Let's Check if the day_of_week is now ordered
is.ordered(all_trips1$day_of_week)

#count of number of rides per user by day_of_week.
all_trips1 %>% 
  group_by(day_of_week, member_casual) %>% 
  summarise(no_of_rides = n())

#membership size
all_trips1 %>% 
  group_by(member_casual) %>% 
  summarise(no_of_memberships = n())


#Let"s export the summary file in form of csv for visualiaztion in Tableau

write.csv(cyclistic_data, "cyclistic_trips.csv")

