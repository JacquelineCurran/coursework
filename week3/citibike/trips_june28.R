load("trips_and_weather.RData")
indexes <- sample(1:nrow(trips_n_weather), size=0.2*nrow(trips_n_weather))
Tripstest=trips_n_weather[indexes, ]
Tripstrain=trips_n_weather[-indexes, ]


library(dplyr)
library(ggplot2)
library(reshape)
library(scales)

model <- lm(count ~ tmax + weekend + snwd:tmin + extreme_prcp + no_prcp + is_holiday, data = Tripstrain)
summary(model)
#R-squared 0.8582
Tripstrain$predicted <- fitted(model)
ggplot(data = Tripstrain, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted)) +facet_wrap(~weekend)
#Then the test set
predicted_test <- predict(model, Tripstest)
Tripstest$predicted <- predicted_test
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted)^2
#R-squared is 0.844354

Tripstrain$RMSE <-(Tripstrain$count - Tripstrain$predicted)^2
Train_RMSE <- sqrt(mean(Tripstrain$RMSE))
Train_RMSE
#3909.875
Tripstest$RMSE <- (Tripstest$count - Tripstest$predicted)^2
Test_RMSE <- sqrt(mean(Tripstest$RMSE))
Test_RMSE
#3815.935


#plot predicted v actual
ggplot(data = Tripstest, aes(x = predicted, y = count, color= tmax)) + geom_point() +geom_abline(slope = 1)
ggplot(data = Tripstrain, aes(x = predicted, y = count,  color= tmax)) + geom_point() +geom_abline(slope = 1)



indexes2 <- sample(1:nrow(trips_n_weather), size=0.2*nrow(trips_n_weather))
Tripstrain2=trips_n_weather[-indexes2, ]
Tripstest2=trips_n_weather[indexes2, ]
Tripstest2$predicted <- predict(model, Tripstest2)

ggplot(data = Tripstest2, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted)) + facet_wrap(~weekend)
cor(Tripstest2$count, Tripstest2$predicted)^2
#.8792008

Tripstest2$RMSE <- (Tripstest2$count - Tripstest2$predicted)^2
Test2_RMSE <- sqrt(mean(Tripstest2$RMSE))
Test2_RMSE
#3609.446

indexes3 <- sample(1:nrow(trips_n_weather), size=0.2*nrow(trips_n_weather))
Tripstrain3=trips_n_weather[-indexes3, ]
Tripstest3=trips_n_weather[indexes3, ]
Tripstest3$predicted <- predict(model, Tripstest3)
cor(Tripstest3$count, Tripstest3$predicted)^2
#.8959349

Tripstest3$RMSE <- (Tripstest3$count - Tripstest3$predicted)^2
Test3_RMSE <- sqrt(mean(Tripstest3$RMSE))
Test3_RMSE
#3266.292



