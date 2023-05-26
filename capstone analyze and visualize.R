# installing Packages

install.packages('tidyverse')
install.packages('ggplot2')
install.packages('janitor')
install.packages('lubridate')

# Loading Libraries

library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(data.table)
library(hms)

# Setting working Directory

getwd()
setwd("C:\\Users\\nerin\\OneDrive\\Documents\\csv_data_original")
getwd()

# Loading original .csv data (from February 2022 to January 2023) into individual data frame 

df1_jan23 <- read_csv('202301-divvy-tripdata.csv')
df2_dec22 <- read_csv('202212-divvy-tripdata.csv')
df3_nov22 <- read_csv('202211-divvy-tripdata.csv')
df4_oct22 <- read_csv('202210-divvy-tripdata.csv')
df5_sep22 <- read_csv('202209-divvy-tripdata.csv')
df6_aug22 <- read_csv('202208-divvy-tripdata.csv')
df7_jul22 <- read_csv('202207-divvy-tripdata.csv')
df8_jun22 <- read_csv('202206-divvy-tripdata.csv')
df9_may22 <- read_csv('202205-divvy-tripdata.csv')
df10_apr22 <- read_csv('202204-divvy-tripdata.csv')
df11_mar22 <- read_csv('202203-divvy-tripdata.csv')
df12_feb22 <- read_csv('202202-divvy-tripdata.csv')

# Viewing each data-frame individually 
View(df1_jan23)
View(df2_dec22)
View(df3_nov22)
View(df4_oct22)
View(df5_sep22)
View(df6_aug22)
View(df7_jul22)
View(df8_jun22)
View(df9_may22)
View(df10_apr22)
View(df11_mar22)
View(df12_feb22)

# Checking column names for consistency

colnames(df1_jan23)
colnames(df2_dec22)
colnames(df3_nov22)
colnames(df4_oct22)
colnames(df5_sep22)
colnames(df6_aug22)
colnames(df7_jul22)
colnames(df8_jun22)
colnames(df9_may22)
colnames(df10_apr22)
colnames(df11_mar22)
colnames(df12_feb22)

# Displaying the structure of each data frame

str(df1_jan23)
str(df2_dec22)
str(df3_nov22)
str(df4_oct22)
str(df5_sep22)
str(df6_aug22)
str(df7_jul22)
str(df8_jun22)
str(df9_may22)
str(df10_apr22)
str(df11_mar22)
str(df12_feb22)

#--------Testing columns------------

# Testing to see if columns match in classes

sapply(df12_feb22, class) == sapply(df4_oct22, class)
View(sapply(df12_feb22, class))
View(sapply(df1_jan23, class))
View(sapply(df4_oct22, class))

# Transforming started_at and ended_at data for df4_oct22 stored as character to date and time format

df4_oct22 <-  mutate(df4_oct22, started_at 
                     = mdy_hms(started_at),ended_at = mdy_hms(ended_at))

str(df4_oct22)

# viewing the data frame after data parsing 

View(df4_oct22)

# checking the data type 

class(df4_oct22$started_at)
class(df4_oct22$ended_at)
class(df1_jan23$started_at)
class(df1_jan23$ended_at)
class(df1_jan23$start_station_name)
class(df2_dec22$started_at)

#-------creating combined new data frame------

#Comparing column names of all the data frames 

compare_df_cols(df1_jan23,df2_dec22,df3_nov22,df4_oct22,
                df5_sep22,df6_aug22,df7_jul22,df8_jun22,
                df9_may22,df10_apr22,df11_mar22,df12_feb22,
                return = "mismatch")

#Combining 12 data frames into one single data frame 

data_df <- rbind(df1_jan23,df2_dec22,df3_nov22,df4_oct22,
                    df5_sep22,df6_aug22,df7_jul22,df8_jun22,
                    df9_may22,df10_apr22,df11_mar22,df12_feb22)


# inspecting the new data frame 

View(data_df)
colnames(data_df)
str(data_df)
dim(data_df)

# removing individual data frames to clear up space in environment

remove(df1_jan23,df2_dec22,df3_nov22,df4_oct22,df5_sep22,df6_aug22,
       df7_jul22,df8_jun22,df9_may22,df10_apr22,df11_mar22,df12_feb22)

# creating new data frame to contain new columns

divvy_trips <- data_df
View(divvy_trips)

# creating the table for : date, day of week, month, day, year

#default format is yyyy-mm-dd, using start date

divvy_trips$date <- as.Date(divvy_trips$started_at)

# creating the column for day of week

divvy_trips$day_of_week <- format(as.Date(divvy_trips$date), "%A")

#creating column for day

divvy_trips$day <- format(as.Date(divvy_trips$date), "%d")

#creating column for month

divvy_trips$month <- format(as.Date(divvy_trips$date), "%m")

#creating column for year

divvy_trips$year <- format(as.Date(divvy_trips$date), "%y")

# calculating ride_length (in minutes and seconds) by subtracting ended_at time from started_at time 

divvy_trips$ride_length_m <- difftime(divvy_trips$ended_at, divvy_trips$started_at,units = 'mins')

divvy_trips$ride_length <- difftime(divvy_trips$ended_at, divvy_trips$started_at)

# inspecting the new data frame created

View(divvy_trips)
colnames(divvy_trips)
str(divvy_trips)
dim(divvy_trips)
head(divvy_trips)


#Converting c(ride_length, ride_length_m, day and month) to numeric to execute calculations

divvy_trips$ride_length <- as.numeric(as.character(divvy_trips$ride_length))
divvy_trips$ride_length_m <- as.numeric(as.character(divvy_trips$ride_length_m))
divvy_trips$month <- as.numeric(divvy_trips$month)
divvy_trips$day <- as.numeric(divvy_trips$day)


is.numeric(divvy_trips$ride_length)
is.numeric(divvy_trips$ride_length_m)
is.numeric(divvy_trips$month)
is.numeric(divvy_trips$day)

trips_data <- divvy_trips[!( divvy_trips$ride_length < 0),]
trips_data <- distinct(trips_data)
trips_data <- na.omit(trips_data)

View(trips_data)

#Descriptive analysis on ride length

trips_data %>% 
  summarise(max(ride_length_m), min(ride_length_m), median(ride_length_m), mean(ride_length_m))

trips_data %>% 
  summarise(max(ride_length), min(ride_length), median(ride_length), mean(ride_length))

#comparing members and casual users
aggregate(trips_data$ride_length ~ trips_data$member_casual, FUN = mean)
aggregate(trips_data$ride_length ~ trips_data$member_casual, FUN = median)
aggregate(trips_data$ride_length ~ trips_data$member_casual, FUN = max)
aggregate(trips_data$ride_length ~ trips_data$member_casual, FUN = min)


trips_data$day_of_week <- ordered(trips_data$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

cyclist_tableau <- trips_data

fwrite(cyclist_tableau,"cyclist_data.csv")





#------Data Visualization------------

#plot for number of rides for weekdays

trips_data %>%
  group_by(day_of_week) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(mapping = aes(x = day_of_week, y = number_of_rides)) +
  geom_col(position = "dodge")

#plot for every day of the week for members and casual riders

trips_data %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_m)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
 
#Plot for number of rides per day for every rider type
 
trips_data %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_m)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Plot for average ride length depending on rider type and number of each rider type

trips_data %>%
  group_by(member_casual)%>%
  summarise(max(ride_length_m), min(ride_length_m), avg_ride_length = mean(ride_length_m)) %>%
  ggplot(aes(x = member_casual, y = avg_ride_length, fill = member_casual)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0, 40, by = 5))


trips_data %>%
  group_by(member_casual) %>%
  summarise(rider_count = n()) %>%
  ggplot(aes(x = member_casual, y = rider_count, fill = member_casual)) +
  geom_col()

trips_data <- trips_data %>%
  mutate(
    season = case_when(
      month %in%  9:11 ~ "Fall",
      month %in%  c(12, 1, 2)  ~ "Winter",
      month %in%  3:5  ~ "Spring",
      TRUE ~ "Summer"))


trips_data%>%
  group_by(season, day_of_week, member_casual) %>%   
  summarise(number_of_rides = n()						 
            ,avg_ride_length = mean(ride_length_m)) %>% 
  #print(n=56) %>%
  ggplot() + 
  geom_col(mapping = aes(x = day_of_week, y = number_of_rides, fill = member_casual), position = "dodge") + facet_wrap(~season) + 
  scale_y_continuous(breaks = seq(0, 400000, by = 50000))


trips_data %>%
  group_by(season, day_of_week, member_casual) %>%
  summarise(number_of_rides = n(),
            avg_ride_length = mean(ride_length_m)) %>%
  #print(n=56)
  ggplot() +
  geom_col(mapping = aes(x = day_of_week, y= avg_ride_length, fill = member_casual),
           position = "dodge") +
  facet_wrap(~season) +
  scale_y_continuous(breaks = seq(0,50, by = 10))


trips_data %>%
  group_by(month, member_casual) %>%
  summarise(number_of_rides = n(),
             avg_ride_length = mean(ride_length_m)) %>%
  #print(n=24)
  ggplot()+
  geom_line(mapping = aes(x= month, y= number_of_rides, color = member_casual))+
  scale_x_continuous(breaks = seq(1, 12, by = 1))


counts <- aggregate(trips_data$ride_length ~ trips_data$member_casual + 
                      trips_data$day_of_week, FUN = mean)
write.csv(counts, file = "C:\\Users\\nerin\\OneDrive\\Documents\\cap_anavis_arl.csv")