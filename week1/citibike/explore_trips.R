library(dplyr)

load('trips.RData')

# count the number of trips (= rows in the data frame)

nrow(trips)

  #5370361

# find the earliest and latest birth years (see help for max and min to deal with NAs)

min(trips$birth_year, na.rm = TRUE)

  #1899

max(trips$birth_year, na.rm = TRUE)

  #1998

# use filter and grepl to find all trips that either start or end on broadway

the_tibble <- tbl_df(head(trips, n=30))

grepl("Broadway", the_tibble$start_station_name) & grepl("Broadway", the_tibble$end_station_name)

the_main_tibble <- tbl_df(trips)
grepl("Broadway", the_main_tibble$start_station_name) & grepl("Broadway", the_main_tibble$end_station_name)
  #the end finds those that starts and ends on broadway

filter(the_main_tibble, grepl("Broadway", the_main_tibble$start_station_name) | grepl("Broadway", the_main_tibble$end_station_name))

# do the same, but find all trips that both start and end on broadway
filter(the_main_tibble, grepl("Broadway", the_main_tibble$start_station_name) & grepl("Broadway", the_main_tibble$end_station_name))

# use filter, select, and distinct to find all unique station names

distinct(the_main_tibble, start_station_id)

filter(select(distinct(the_tibble, start_station_id), starts_with("start_station_name")), TRUE)

# count trips by gender

the_main_tibble %>% group_by(gender) %>% summarize (total = n())

# find the 10 most frequent station-to-station trips

the_main_tibble %>% group_by(start_station_name, end_station_name) %>% summarize("total" = n()) %>% ungroup %>% arrange(-total)


# use awk to count all trips that start and end on broadway

startend<- filter(the_main_tibble, grepl("Broadway", the_main_tibble$start_station_name) & grepl("Broadway", the_main_tibble$end_station_name))
nrow(startend)

