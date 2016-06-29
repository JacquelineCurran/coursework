# Cross-validation for Citibike trips
# 


load("trips_and_weather.RData")
#### Now lets work on the linear model for day 5 ### 

indexes2 <- sample(1:nrow(trips_n_weather), size=0.2*nrow(trips_n_weather))
Tripstest=trips_n_weather[indexes2, ]
Tripstrain=trips_n_weather[-indexes2, ]


library(dplyr)
library(ggplot2)
library(reshape)
library(scales)

train_cor <- c()
test_cor <- c()

for (h in 1:20) {
  model <- lm(count ~ poly(tmax, h), data = Tripstrain)
  Tripstrain$predicted <- predict(model, Tripstrain)
  Tripstest$predicted <-  predict(model, Tripstest)
  train_cor[h] <- cor(Tripstrain$predicted, Tripstrain$count)^2
  test_cor[h] <- cor(Tripstest$predicted, Tripstest$count)^2
}

correlation <- data.frame(test_cor, train_cor)
correlation$k <- c(1:20)
View(correlation)
ggplot(data= correlation, aes(x= k, y=train_cor)) + geom_line() + geom_line(aes(x = k, y = test_cor, color= "red"))

#Best is 3

indexes2 <- sample(1:nrow(trips_n_weather), size=0.2*nrow(trips_n_weather))
Tripstrain2=trips_n_weather[-indexes2, ]
Tripstest2=trips_n_weather[indexes2, ]
train_cor2 <- c()
test_cor2 <- c()

for (h in 1:20) {
  model <- lm(count ~ poly(tmax, h), data = Tripstrain2)
  Tripstrain2$predicted <- predict(model, Tripstrain2)
  Tripstest2$predicted <-  predict(model, Tripstest2)
  train_cor2[h] <- cor(Tripstrain2$predicted, Tripstrain2$count)^2
  test_cor2[h] <- cor(Tripstest2$predicted, Tripstest2$count)^2
}

correlation <- data.frame(test_cor2, train_cor2)
correlation$k <- c(1:20)
View(correlation)
#Best is 9

model2 <- lm(count ~ poly(tmax, 3), data = Tripstrain)
summary(model2)
#.7209

Tripstrain$predicted2 <- fitted(model2)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted2))
#Then the test set
predicted_test2 <- predict(model2, Tripstest)
Tripstest$predicted2 <- predicted_test2
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted2)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted2)^2
#R-squared is 0.79

model3 <- lm(count~ tmax + tmax^3, data = Tripstrain)
summary(model3)
#.7031
Tripstrain$predicted3 <- fitted(model3)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted3))
#Then the test set
predicted_test3 <- predict(model3, Tripstest)
Tripstest$predicted3 <- predicted_test3
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted3)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted2)^2
#R-squared is 0.7951

model4 <- lm(count~ tmax + tmax^3 + prcp, data = Tripstrain)
summary(model4)
#.7767
Tripstrain$predicted4 <- fitted(model4)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted4))
#Then the test set
predicted_test4 <- predict(model4, Tripstest)
Tripstest$predicted4 <- predicted_test4
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted4)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted4)^2
#R-squared is 0.7847794


model5 <- lm(count~ tmax + tmax^3 + weekend, data = Tripstrain)
summary(model5)
#.7601
Tripstrain$predicted5 <- fitted(model5)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted5))
#Then the test set
predicted_test5 <- predict(model5, Tripstest)
Tripstest$predicted5 <- predicted_test5
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted5)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted5)^2
#R-squared is 0.7677531

model6 <- lm(count~ tmax + tmax^3 + weekend + prcp, data = Tripstrain)
summary(model6)
#.8407
Tripstrain$predicted6 <- fitted(model6)
ggplot(data = Tripstrain, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted6)) +facet_wrap(~weekend)
#Then the test set
predicted_test6 <- predict(model6, Tripstest)
Tripstest$predicted6 <- predicted_test6
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted6)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted6)^2
#R-squared is 0.8109706

#winner winner model 7
model7 <- lm(count ~ tmax + weekend + prcp + snwd:tmin + extreme_prcp + no_prcp + is_holiday, data = Tripstrain)
summary(model7)
#R-squared 0.8872
Tripstrain$predicted <- fitted(model7)
ggplot(data = Tripstrain, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted)) +facet_wrap(~weekend)
#Then the test set
predicted_test <- predict(model7, Tripstest)
Tripstest$predicted <- predicted_test
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted)^2
#R-squared is 0.8822155



Tripstrain$RMSE <-(Tripstrain$count - Tripstrain$predicted)^2
Train_RMSE <- sqrt(mean(Tripstrain$RMSE))
Train_RMSE
  #3483.901
Tripstest$RMSE <- (Tripstest$count - Tripstest$predicted)^2
Test_RMSE <- sqrt(mean(Tripstest$RMSE))
Test_RMSE
  #300.536




indexes3 <- sample(1:nrow(trips_n_weather), size=0.2*nrow(trips_n_weather))
Tripstrain3=trips_n_weather[-indexes3, ]
Tripstest3=trips_n_weather[indexes3, ]
Tripstest3$predicted <- predict(model7, Tripstest3)
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted)) + facet_wrap(~weekend)
cor(Tripstest3$count, Tripstest3$predicted)^2
Tripstest3$RMSE <- (Tripstest3$count - Tripstest3$predicted)^2
Test3_RMSE <- sqrt(mean(Tripstest3$RMSE))
Test3_RMSE
  #3465.363


#plot predicted v actual
ggplot(data = Tripstest, aes(x = predicted, y = count, color= tmax)) + geom_point()
ggplot(data = Tripstrain, aes(x = predicted, y = count,  color= tmax)) + geom_point()


##TRY AGAIN BUT SIMPLIFY
model8 <- lm(count ~ tmax + weekend + prcp, data = Tripstrain)
summary(model8)
#R-squared 0.8666
Tripstrain$predicted2 <- fitted(model8)
ggplot(data = Tripstrain, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted2)) +facet_wrap(~weekend)
#Then the test set
predicted_test2 <- predict(model8, Tripstest)
Tripstest$predicted2 <- predicted_test2
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted2)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted2)^2
#R-squared is 0.810976

Tripstest2$predicted <- predict(model7, Tripstest2)
cor(Tripstest2$count, Tripstest2$predicted)^2

Tripstrain$RMSE <-(Tripstrain$count - Tripstrain$predicted)^2
Train_RMSE <- sqrt(mean(Tripstrain$RMSE))
Train_RMSE
#3950.82
Tripstest$RMSE <- (Tripstest$count - Tripstest$predicted)^2
Test_RMSE <- sqrt(mean(Tripstest$RMSE))
Test_RMSE
#4335.796
Tripstest2$RMSE <- (Tripstest2$count - Tripstest2$predicted)^2
Test2_RMSE <- sqrt(mean(Tripstest2$RMSE))
Test2_RMSE
#3207.84


indexes3 <- sample(1:nrow(trips_n_weather), size=0.2*nrow(trips_n_weather))
Tripstrain3=trips_n_weather[-indexes3, ]
Tripstest3=trips_n_weather[indexes3, ]
Tripstest3$predicted <- predict(model7, Tripstest3)
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted)) + facet_wrap(~weekend)
cor(Tripstest3$count, Tripstest3$predicted)^2
Tripstest3$RMSE <- (Tripstest3$count - Tripstest3$predicted)^2
Test3_RMSE <- sqrt(mean(Tripstest3$RMSE))
Test3_RMSE
#3465.363


#plot predicted v actual
ggplot(data = Tripstest, aes(x = predicted, y = count, color= tmax)) + geom_point()
ggplot(data = Tripstrain, aes(x = predicted, y = count,  color= tmax)) + geom_point()

##### more work on the model#####
View(trips_n_weather)
ggplot(trips_n_weather, aes(x = prcp)) + geom_histogram()
# I think there should be an added column of extreme precipitation: >2 

trips_no_precip <- mutate(trips_n_weather, no_prcp = prcp == 0)
trips_extreme_weather$no_prcp <- as.numeric(trips_no_precip$no_prcp)
trips_n_weather$no_prcp <- trips_extreme_weather$no_prcp
save(trips_n_weather, file = 'trips_and_weather.RData')
 #maybe no precipitation

trips_extreme_weather <- mutate(trips_n_weather, extreme_prcp = prcp >=2)
trips_extreme_weather$extreme_prcp <- as.numeric(trips_extreme_weather$extreme_prcp)
trips_n_weather$extreme_prcp <- trips_extreme_weather$extreme_prcp

#some precipitation?
trips_some_precip <- mutate(trips_n_weather, med_prcp = (prcp < 2 & prcp > 0))
trips_some_precip$med_prcp <- as.numeric(trips_some_precip$med_prcp)
trips_n_weather$med_prcp <- trips_some_precip$med_prcp

##now lets scope out our model again

model9 <- lm(count ~ tmax + weekend + no_prcp + extreme_prcp + prcp + is_holiday, data = Tripstrain)
summary(model9)
#R-squared 0.8388
Tripstrain$predicted <- fitted(model9)
ggplot(data = Tripstrain, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted)) +facet_wrap(~weekend)
#Then the test set
predicted_test <- predict(model9, Tripstest)
Tripstest$predicted <- predicted_test
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted)^2
#R-squared is 0.890947


Tripstrain$RMSE <-(Tripstrain$count - Tripstrain$predicted)^2
Train_RMSE <- sqrt(mean(Tripstrain$RMSE))
Train_RMSE
#3826.981
Tripstest$RMSE <- (Tripstest$count - Tripstest$predicted)^2
Test_RMSE <- sqrt(mean(Tripstest$RMSE))
Test_RMSE
#3439.416


ggplot(data = Tripstest, aes(x = predicted, y = count, color= tmax)) + geom_point()
ggplot(data = Tripstrain, aes(x = predicted, y = count,  color= tmax)) + geom_point()


##what if its christmas##
holidays <- c("20141225", "20141224", "20141127")
is_holiday <- mutate(trips_n_weather, is_holiday = trips_n_weather$date %in% holidays)
trips_n_weather$is_holiday <- is_holiday$holiday


save(trips_n_weather, file = "trips_and_weather.RData")


