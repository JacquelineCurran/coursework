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

#to get day of week
library(lubridate)
weather2015$day_of_week <- wday(weather2015$ymd)
weather2015 <- mutate(weather2015, day_of_week=factor(day_of_week, levels = c(1:7), labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
View(weather2015)

# if weekend
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weather2015$weekend <- factor((weather2015$day_of_week %in% weekdays), 
                                      levels=c(TRUE, FALSE), labels=c('0', '1'))


#holidays
holidays_2015 <- c("20151126", "20151224", "20151225")
trips_holiday_2015 <- mutate(weather2015, is_holiday = date %in% holidays_2015)
weather2015$is_holiday <- trips_holiday_2015$is_holiday

#extreme_prcp
trips2015_extreme <- if trips2015_precip > 2, TRUE

#med_prcp

#predict
##newdata##$predicted <- predict(model, ##new data##)

#find the Rsquared value
cor(#newdata#$count, ##newdata##$predicted)^2
  
  
  #calculate RMSE
  #newdata#$RMSE <- (#newdata#$count - #newdata#$predicted)^2
  #newdata#_RMSE <- sqrt(mean(##newdata##$RMSE))
  #newdata#_RMSE