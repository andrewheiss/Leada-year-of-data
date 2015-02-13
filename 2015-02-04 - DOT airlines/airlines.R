# Load libraries
library(dplyr)

# We will only work with the first million rows. Download year 2008.
airlines.raw <- read.csv(file="2008.csv.bz2", nrows=1000000)


#------------
# Problem 1
#------------
# Find the IATA code that is in the Origin column, but is NOT is the Dest column.
origin <- levels(airlines.raw$Origin)
dest <- levels(airlines.raw$Dest)

dest[!(dest %in% origin)]

# "OGD", or Ogden


#------------
# Problem 2
#------------
# Which airport, as defined by the IATA code, has at least 10,000 flights and had the lowest probability for a delayed flight in the data?

# Define a delay as:
#   If ArrDelay > 0, then count as a delayed flight to Origin airport.
#   If DepDelay > 0, then count as a delayed flight to Dest airport.
# Total delayed flights equals the sum of the two above.
# Probability of delay equals total delayed flights divided by total flights (Origin & Dest) that go through that airport.
dep.delays <- airlines.raw %>%
  mutate(delay.dep = ifelse(DepDelay > 0, TRUE, FALSE)) %>%
  group_by(Origin) %>%
  summarize(delays.dep = sum(delay.dep, na.rm=TRUE),
            flights.dep = n()) %>%
  rename(airport = Origin)

arr.delays <- airlines.raw %>%
  mutate(delay.arr = ifelse(ArrDelay > 0, TRUE, FALSE)) %>%
  group_by(Dest) %>%
  summarize(delays.arr = sum(delay.arr, na.rm=TRUE),
            flights.arr = n()) %>%
  rename(airport = Dest)

all.delays <- arr.delays %>%
  left_join(dep.delays, by="airport") %>%
  mutate(delays.total = delays.dep + delays.arr,
         flights.total = flights.dep + flights.arr,
         prob = delays.total / flights.total) %>%
  filter(flights.total > 10000) %>%
  arrange(prob)

all.delays %>% slice(1) %>% 
  select(airport, delays=delays.total, flights=flights.total, prob)

#   airport delays flights      prob
# 1     HNL   4293   17626 0.2435606


#------------
# Problem 3
#------------
# Create a spreadsheet of data which calculates the historical probability of flight delay in each of the twenty carriers based off of two characteristics.
# DayofWeek - Weekday (1,2,3,4,5) or Weekend (6,7)
# DepTime - Day Time (0501 to 1700), Night Time (1701 to 2400), or Red Eye (0000 to 0500)
# UniqueCarrier - Carrier (20)
# For example, for an AA day time flight on the weekday, the percentage of delayed flights was ~29.3%.
air.fancy <- airlines.raw %>%
  mutate(delay = ifelse(DepDelay > 0 | ArrDelay > 0, TRUE, FALSE)) %>%
  mutate(Weekday = factor(ifelse(DayOfWeek %in% 1:5, "Weekday", "Weekend")),
         DepTimeType = cut(DepTime, breaks=c(0, 500, 1700, 2400), 
                           include.lowest=TRUE, 
                           labels=c("Red Eye", "Day", "Night"))) %>%
  group_by(UniqueCarrier, Weekday, DepTimeType) %>%
  summarize(delays = sum(delay, na.rm=TRUE),
            flights = n()) %>%
  mutate(delay.prob = delays / flights) %>%
  filter(!is.na(DepTimeType))

# Check to see if it works
air.fancy %>% filter(UniqueCarrier == "AA")

# Output spreadsheet
write.csv(air.fancy, "delay_probs.csv", row.names=FALSE)
