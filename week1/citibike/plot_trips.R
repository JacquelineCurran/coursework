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

#plot the ratio of male to female trips by age
#hint use the spread() function to reshape things to make it easier to compute this ratio



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


precipitation<- group_by(trips_with_weather, tmin, ymd) %>% filter(prcp >=8) %>% summarize(total=n())
View(precipitation)
ggplot(ntrip, aes(x=tmin, y=total, color=as.factor(ymd))) + geom_point()

# add a smoothed fit on top of the previous plot, using geom_smooth
ntrip <- group_by(trips_with_weather, tmin, ymd) %>% summarize(total = n())
ggplot(ntrip, aes(x=tmin, y=total, color= as.factor(ymd))) + geom_point() + geom_smooth()


# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package

install.packages("lubridate")
library(lubridate)

Time <- format(as.POSIXct(strptime(trips_with_weather$starttime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")
trips_with_weather$time <- Time
View(trips_with_weather)

hour <-format(as.POSIXct(strptime(trips_with_weather$time,"%H:%M:%S",tz="")) ,format = "%H")
trips_with_weather$hour <- hour
View(trips_with_weather)

by_hour <- group_by(trips_with_weather, hour, ymd) %>% summarize(total = n()) %>% ungroup () %>% group_by(hour) %>% summarize(average = mean(total), standard_deviation = sd(total))
View(by_hour)

#plot this
ggplot(by_hour, aes(x=hour, y=average)) + geom_point()
ggplot(by_hour, aes(x=hour, y=standard_deviation)) + geom_point()


# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
trips_with_weather$day <- wday(trips_with_weather$ymd)
View(trips_with_weather)

#IDK how to do because IDK how to do the las two