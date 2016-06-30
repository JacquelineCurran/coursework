load("trips_and_weather.RData")
library(dplyr)
library(ggplot2)
library(reshape)
library(scales)
#set wd to week 2 > citibike

#set indexes and test and train set
indexes <- sample(1:nrow(trips_n_weather), size=0.2*nrow(trips_n_weather))
Tripstest=trips_n_weather[indexes, ]
Tripstrain=trips_n_weather[-indexes, ]

#here is the final model
model <- lm(count ~ tmax + weekend + snwd + extreme_prcp + med_prcp+ is_holiday, data = Tripstrain)

#then load the new data
##new data##

#predict
##newdata##$predicted <- predict(model, ##new data##)

#find the Rsquared value
cor(#newdata#$count, ##newdata##$predicted)^2

  
#calculate RMSE
#newdata#$RMSE <- (#newdata#$count - #newdata#$predicted)^2
  #newdata#_RMSE <- sqrt(mean(##newdata##$RMSE))
  #newdata#_RMSE