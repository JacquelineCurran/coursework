load("trips_model.RData")

library(dplyr)
library(ggplot2)
library(reshape)
library(scales)
library(dplyr)
library(readr)

#then load the new weather data
weather2015 <- read.table('weather_2015.csv', header=T, sep=',')
weather2015 <- select(weather2015, DATE, PRCP, SNWD, SNOW, TMAX, TMIN)
names(weather2015) <- tolower(names(weather2015))
weather2015 <- mutate(weather2015,
                  tmin = tmin / 10,
                  tmax = tmax / 10,
                  ymd = as.Date(parse_datetime(date, "%Y%m%d")))
weather2015 <- tbl_df(weather2015)



##new trips data##

# load each month of the trip data into one big data frame
csvs <- c('201504-citibike-tripdata.csv', '201505-citibike-tripdata.csv', '201507-citibike-tripdata.csv', '201508-citibike-tripdata.csv', '201509-citibike-tripdata.csv', '201510-citibike-tripdata.csv', '201511-citibike-tripdata.csv', '201512-citibike-tripdata.csv')
trips <- data.frame()
for (csv in csvs) {
  print(csv)
  tmp <- read_csv(csv, na='\\N')
  
  # the date format changed to something ugly in 2014-09 which read_csv doesn't recognize as a datetime,
  # so manually convert the date from a string to a datetime
  if (typeof(tmp$starttime) == "character")
    tmp <- mutate(tmp,
                  starttime=parse_datetime(starttime, "%m/%d/%Y %H:%M:%S"),
                  stoptime=parse_datetime(stoptime, "%m/%d/%Y %H:%M:%S"))
  
  trips <- rbind(trips, tmp)
}

# replace spaces in column names with underscores
names(trips) <- gsub(' ', '_', names(trips))

# add a column for year/month/day (without time of day)
trips <- mutate(trips, ymd=as.Date(starttime))

# recode gender as a factor 0->"Unknown", 1->"Male", 2->"Female"
trips <- mutate(trips, gender=factor(gender, levels=c(0,1,2), labels=c("Unknown","Male","Female")))


#to try and get first 7 ish months
csvs2 <- c('201501-citibike-tripdata.csv', '201502-citibike-tripdata.csv', '201503-citibike-tripdata.csv', '201506-citibike-tripdata.csv')
trips2 <- data.frame()
for (csv2 in csvs2) {
  print(csv2)
  tmp2 <- read_csv(csv2, na='\\N')
  
  # the date format changed to something ugly in 2014-09 which read_csv doesn't recognize as a datetime,
  # so manually convert the date from a string to a datetime
  if (typeof(tmp2$starttime) == "character")
    tmp2 <- mutate(tmp2,
                  starttime=parse_datetime(starttime, "%m/%d/%Y %H:%M"),
                  stoptime=parse_datetime(stoptime, "%m/%d/%Y %H:%M"))
  
  trips2 <- rbind(trips2, tmp2)
}

# replace spaces in column names with underscores
names(trips2) <- gsub(' ', '_', names(trips2))

# add a column for year/month/day (without time of day)
trips2 <- mutate(trips2, ymd=as.Date(starttime))

# recode gender as a factor 0->"Unknown", 1->"Male", 2->"Female"
trips2 <- mutate(trips2, gender=factor(gender, levels=c(0,1,2), labels=c("Unknown","Male","Female")))


trips2015 <- rbind(trips, trips2)

#now have to join the trips and weather
trips2015_per_day <- trips2015 %>% group_by(ymd) %>% summarise(count =n())
trips2015_n_weather <- inner_join(trips2015_per_day, weather2015, by = "ymd")
View(trips2015_n_weather)

save(trips2015_n_weather, file = "trips2015.RData")

#to get day of week
library(lubridate)
trips2015_n_weather$day_of_week <- wday(trips2015_n_weather$ymd)
trips2015_n_weather <- mutate(trips2015_n_weather, day_of_week=factor(day_of_week, levels = c(1:7), labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
View(trips2015_n_weather)

#now have to say if weekend
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
trips2015_n_weather$weekend <- factor((trips2015_n_weather$day_of_week %in% weekdays), 
                                  levels=c(TRUE, FALSE), labels=c('0', '1'))


#holidays
holidays_2015 <- c("20151126", "20151224", "20151225")
trips_holiday_2015 <- mutate(trips2015_n_weather, is_holiday = date %in% holidays_2015)
trips2015_n_weather <- as.factor(trips_holiday_2015$is_holiday)

#extreme_prcp
trips2015_extreme <- if trips2015_precip > 2, TRUE

#predict
##newdata##$predicted <- predict(model, ##new data##)

#find the Rsquared value
cor(#newdata#$count, ##newdata##$predicted)^2

  
#calculate RMSE
#newdata#$RMSE <- (#newdata#$count - #newdata#$predicted)^2
  #newdata#_RMSE <- sqrt(mean(##newdata##$RMSE))
  #newdata#_RMSE