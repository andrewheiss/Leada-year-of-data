# Title:          bike_rentals.R
# Description:    Answer questions about bike sharing dataset
# Author:         Andrew Heiss
# Last updated:   2015-01-11

# Load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Load and clean data
bikes.raw <- read.csv("bike_trip_data.csv")
bikes <- bikes.raw %>%
  mutate(Start.Date = mdy_hms(as.character(Start.Date), truncated=1),
         End.Date = mdy_hms(as.character(End.Date), truncated=1),
         Bike = factor(Bike..),
         Start.Day = wday(Start.Date, label=TRUE, abbr=FALSE))


#------------
# Problem 1
#------------
# What was the average total time (in minutes) used by a bicycle in the data?

# Get average duration for each bike
bike.duration <- bikes %>% 
  group_by(Bike) %>% 
  summarize(average = mean(Duration) / 60)

# Plot, just for fun
ggplot(bike.duration, aes(x=average)) + 
  geom_histogram(binwidth=10) + 
  geom_vline(xintercept=mean(bike.duration$average), colour="darkred", size=1) + 
  # coord_cartesian(xlim=c(0, 250)) + 
  theme_bw()

# Get answer
as.duration(mean(bike.duration$average) * 60)

# [1] "1784.43089330139s (~29.74 minutes)"


#------------
# Problem 2
#------------
# What was the most popular day by trip frequency in this dataset?

# Calculate number of trips per weekday
bike.weekdays <- bikes %>% 
  group_by(Start.Day) %>% 
  summarize(trips = n()) %>%
  mutate(day.rev = factor(Start.Day, levels=rev(levels(Start.Day))))

# Plot, again just for fun
ggplot(bike.weekdays, aes(x=day.rev, y=trips)) + 
  geom_bar(stat="identity") + 
  coord_flip() + theme_bw()

# Get answer
bike.weekdays %>% filter(trips == max(trips))

#   Start.Day trips  day.rev
# 1  Thursday 25265 Thursday


#------------
# Problem 3
#------------
# Assuming there are 30 bikes per station, find what date and time the bikes FIRST need to be rebalanced. As in, there are 0 bikes at a terminal for a customer to rent. 

# Convert dataset to long, sorted by event time
bikes.tidy <- bikes %>%
  arrange(Start.Date) %>%
  # Start with a small subset of columns
  select(Trip.ID, Start.Date, Start.Station, End.Date, End.Station) %>%
  # Convert to long
  gather(Event.Type, Event.Date, Start.Date, End.Date, 
         -Trip.ID, -Start.Station, -End.Station) %>%
  # Clean up resultant columns
  mutate(Event.Type = factor(Event.Type, labels=c("Out", "In")),
         Event.Station = ifelse(Event.Type == "Out", 
                                as.character(Start.Station), 
                                as.character(End.Station))) %>%
  # Remove these, since Event.Station covers it
  select(-Start.Station, -End.Station) %>%
  arrange(Event.Date)


# Set up station inventory
bikes.per.station <- 30
stations <- data.frame(station=levels(bikes$Start.Station), 
                       bikes=bikes.per.station)

# Loop through all the ordered events and add/subtract from the station inventories
for(i in 1:nrow(bikes.tidy)) {
  # Select the station that matches the current row in bike.tidy
  station.row <- stations$station == as.character(bikes.tidy[i, "Event.Station"])
  
  # Modify the count of the bikes in the stations depending 
  # on if bike was borrowed or returned
  if(bikes.tidy[i, "Event.Type"] == "Out") {
    stations[station.row, "bikes"] <- stations[station.row, "bikes"] - 1
  } else {
    stations[station.row, "bikes"] <- stations[station.row, "bikes"] + 1
  }
  
  if(any(stations$bikes < 1)) {
    rebalance.point <- i
    break
  }
}

# Get row where rebalancing should happen
bikes.tidy[rebalance.point,]

#      Trip.ID Event.Type          Event.Date         Event.Station
# 5881    8713        Out 2013-09-02 11:33:00 Embarcadero at Bryant
