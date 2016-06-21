########################################
# load libraries
########################################

# load some packages that we'll need
library(dplyr)
library(ggplot2)
library(reshape)
library(scales)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides

ggplot(trips, aes(x=trips$tripduration)) + geom_histogram() + xlim (0, 7500)

# plot the distribution of trip times by rider type

ggplot(trips, aes(x=trips$tripduration, fill=trips$usertype)) + geom_density() + xlim (0, 7500)

# plot the number of trips over each day

ggplot(trips, aes(x=trips$ymd)) + geom_bar()

# plot the number of trips by gender and age

gender_age <- group_by(trips, birth_year, gender) %>% summarize( total = n())
ggplot(gender_age, aes(x=birth_year, y= total, color = as.factor(gender))) + geom_point() + ylim(0,150000)

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the minimum temperature over each day

ggplot(trips_with_weather, aes(x= ymd, y= tmin)) + geom_point()

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this

ntrip <- group_by(trips_with_weather, tmin, ymd) %>% summarize(total = n())
ggplot(ntrip, aes(x=tmin, y=total, color= as.factor(ymd))) + geom_point()

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this
#prcp>8

mutate(trips_with_weather, prcp >= 8, high_prcp)



# add a smoothed fit on top of the previous plot, using geom_smooth



# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package

# plot the above

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package


