load("trips_and_weather.RData")

save(trips_n_weather, file='trips_and_weather.RData')


indexes <- sample(1:nrow(trips_n_weather), size=0.2*nrow(trips_n_weather))
Tripstest=trips_n_weather[indexes, ]
Tripstrain=trips_n_weather[-indexes, ]


library(dplyr)
library(ggplot2)
library(reshape)
library(scales)

model <- lm(count ~ tmax + weekend + snwd + extreme_prcp + med_prcp+ is_holiday, data = Tripstrain)
summary(model)
#R-squared 0.8589
Tripstrain$predicted <- fitted(model)
ggplot(data = Tripstrain, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted)) +facet_wrap(~weekend)
#Then the test set
predicted_test <- predict(model, Tripstest)
Tripstest$predicted <- predicted_test
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted)^2
#R-squared is 0.8376083

Tripstrain$RMSE <-(Tripstrain$count - Tripstrain$predicted)^2
Train_RMSE <- sqrt(mean(Tripstrain$RMSE))
Train_RMSE
#3897.984
Tripstest$RMSE <- (Tripstest$count - Tripstest$predicted)^2
Test_RMSE <- sqrt(mean(Tripstest$RMSE))
Test_RMSE
#3944.169


#plot predicted v actual
ggplot(data = Tripstest, aes(x = predicted, y = count, color= tmax)) + geom_point() +geom_abline(slope = 1)
ggplot(data = Tripstrain, aes(x = predicted, y = count,  color= tmax)) + geom_point() +geom_abline(slope = 1)

###################################5-fold cross validation#################################################################
trips_n_weather$fold <- sample(1:5, nrow(trips_n_weather), replace = T)
test_RMSE <- c()
test_MSE <- c()

#I want to keep track of the MSE in the test data
for (h in 1:5) {
  train <- filter(trips_n_weather, fold != h)
  test <- filter(trips_n_weather, fold == h)
  model <- lm(count ~ tmax + weekend + snwd + extreme_prcp + med_prcp+ is_holiday, data = train)
  test_predicted <- predict(model, test)
  test_RMSE[h] <- sqrt(mean((test$count - test_predicted)^2))
  test_MSE[h] <- mean((test$count - test_predicted)^2)
}

test_RMSE
#above gave us all the MSE's so now we want to find the mean
mean(test_RMSE)
  #3929.146
#now want to find standard error of the RMSE
sd(test_RMSE)/sqrt(5)
  #183.2209
  #that is sd divided by sqare root of n
test_MSE
mean(test_MSE)
  #16266890
#find standard error of the MSE
sd(test_MSE)/sqrt(5)
  #1487016
#that is sd divided by sqare root of n


for (h in 1:20) {
  model3 <- lm(count ~ poly(tmax, h) + weekend, data = Tripstrain)
  Tripstrain$predicted_day5_3 <- predict(model3, Tripstrain)
  Tripstest$predicted_day5_3 <-  predict(model3, Tripstest)
  train_cor[h] <- cor(Tripstrain$predicted_day5_3, Tripstrain$count)^2
  test_cor[h] <- cor(Tripstest$predicted_day5_3, Tripstest$count)^2
}


#############NOW TRY GLMNET#########################
library(glmnet)
trips_n_weather_matrix <- trips_n_weather
trips_n_weather_matrix$day_of_week <- NULL
trips_n_weather_matrix$snow <- NULL
trips_n_weather_matrix$ymd <- NULL
trips_n_weather_matrix$date <- NULL
trips_n_weather_matrix$tmin <- NULL
trips_n_weather_matrix$no_prcp <- NULL
View(trips_n_weather_matrix)

x=model.matrix(count~.,trips_n_weather_matrix)
y=trips_n_weather_matrix$count
grid = 10^seq(1, 02, length = 100)
indexes <- sample(1:nrow(x), size=0.2*nrow(x))
Tripstest=x[indexes, ]
Tripstrain=x[-indexes, ]
count.test <- y[indexes]
count.train <- y[-indexes]
ridge.mod <- glmnet(Tripstrain, count.train, alpha = 0, lambda = grid)
cv.out <- cv.glmnet(Tripstrain, count.train, alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
  #970.2486
ridge.pred <- predict(ridge.mod, s=bestlam, newx = Tripstest)
mean((ridge.pred - count.test)^2)
   #16,188,366
      #RMSE would be 4023.477
out = glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)


#lasso
lasso.mod <- glmnet(Tripstrain, count.train, alpha = 1, lambda = grid)
plot(lasso.mod)
set.seed(2)
cv.out <- cv.glmnet(Tripstrain, count.train, alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
#52.99767
lasso.pred <- predict(lasso.mod, s = bestlam, newx = Tripstest)
mean((lasso.pred - count.test)^2)
  #16,038,162
    #RMSE would be 4,004.767
out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)
lasso.coef


###final trained model
model <- lm(count ~ tmax + weekend + snwd + extreme_prcp + med_prcp+ is_holiday, data = trips_n_weather)
save(model, file = "trips_model.Rdata")
