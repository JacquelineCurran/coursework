load("trips_model.Rdata")

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

#to get day of week
library(lubridate)
weather2015$day_of_week <- wday(weather2015$ymd)
weather2015 <- mutate(weather2015, day_of_week=factor(day_of_week, levels = c(1:7), labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

# if weekend
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weather2015$weekend <- factor((weather2015$day_of_week %in% weekdays), 
                                      levels=c(TRUE, FALSE), labels=c('0', '1'))

#holidays
holidays_2015 <- c("20151126", "20151224", "20151225")
trips_holiday_2015 <- mutate(weather2015, is_holiday = date %in% holidays_2015)
weather2015$is_holiday <- as.numeric(trips_holiday_2015$is_holiday)

#extreme_prcp
extreme_prcp <- mutate (weather2015, extreme_prcp = prcp > 2)
weather2015$extreme_prcp <- as.numeric(extreme_prcp$extreme_prcp)

#med_prcp
med_prcp <- mutate (weather2015, med_prcp = prcp >0 & prcp < 2)
weather2015$med_prcp <- as.numeric(med_prcp$med_prcp)

#predict
weather2015$predicted <- predict(model, weather2015)
