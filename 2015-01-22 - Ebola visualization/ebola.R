# Title:          ebola.R
# Description:    Visualize and answer questions about Ebola
# Author:         Andrew Heiss
# Last updated:   2015-01-22

# Load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


#----------------------
# Step 1: Aggregation
#----------------------
# Each CSV is formatted with variables as rows, not columns. This function 
# saves the date of the file, removes the total column, and rearranges the 
# data to be useful.
clean.csv <- function(csv) {
  day.raw <- read.csv(csv, stringsAsFactors=FALSE)
  
  df.day <- day.raw$Date[1]
  
  day.clean <- day.raw %>% select(-c(Date, Totals)) %>%
    gather(city, value, -Description) %>% 
    mutate(Description = tolower(gsub("[ \\+\\(\\)]", ".", Description)),
           Description = gsub("\\.+", ".", Description)) %>%
    mutate(value = as.numeric(gsub("%", "", value))) %>%
    spread(key = Description, value) %>%
    mutate(report.date = ymd(df.day, tz="America/New_York")) %>%
    mutate(city = as.character(city))

  day.clean
}

# Clean all the CSV files in the data directory and save to list
# Original data from https://github.com/tristantao/ebola
raw.files <- dir("guinea_data", pattern = "\\.csv$", full.names = TRUE)
all.days <- lapply(raw.files, clean.csv)

# Combine list of data frames
ebola <- bind_rows(all.days) %>%
  mutate(fatality.rate.for.confirmed.and.probables = 
           fatality.rate.for.confirmed.and.probables / 100,
         city = factor(city))


#------------------------
# Step 2: Visualization
#------------------------
# Build visualizations for the aggregated data. There are different visualizations you can build (a line graph is a good start). Also, there are different variables you can visualize (Total death/s in confirmed cases, New Case/s (Probable), total confirmed cases, and a lot more). Think about which variables might be interesting / meaningful. You're also free to find additional data (geographical data etc), but make sure you cite your sources!
 # There are some sample visualizations here: http://www.bbc.com/news/world-africa-28755033


#---------------------
# Step 3: Conclusion
#---------------------
# So you got the data and built some visualizations. Now interpret it! Note a few things about the graph. Can you conclude anything? Is the worst over? Maybe it's yet to come? Or not enough data? Let us know your thoughts.
# Tweet at us with your visualizations! @LeadaHQ
