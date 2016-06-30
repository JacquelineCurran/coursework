library(dplyr)
library(readr)

load('trips.RData')

trips_per_day <- trips %>% group_by(ymd) %>% summarise(count =n())
trips_n_weather <- inner_join(trips_per_day, weather, by = "ymd")
View(trips_n_weather)

########
# Cross-validation for Citibike trips
# 
# In this assignment we'll predict number of trips per day as a function of the weather on that day.
# 
#     Create a data frame with one row for each day, the number of trips taken on that day, 
      #and the minimum temperature on that day.


trips_per_day <- trips %>% group_by(ymd) %>% summarise(count =n())
trips_n_weather <- inner_join(trips_per_day, weather, by = "ymd")
View(trips_n_weather)

#     Split the data into a randomly selected training and test set, as in the above exercise, 
      # with 80% of the data for training the model and 20% for testing.

indexes <- sample(1:nrow(trips_n_weather), size=0.2*nrow(trips_n_weather))
Tripstest=trips_n_weather[indexes, ]
Tripstrain=trips_n_weather[-indexes, ]


#     Fit a model to predict the number of trips as a (linear) function of the minimum temperature, 
      #and evaluate the fit on the training and testing data sets. Do this first visually by plotting the predicted 
      #and actual values as a function of the minimum temperature. Then do this with R^2, as above. 
      #You'll want to use the predict and cor functions for this.

#First the training
model <- lm(count ~ tmin, data = Tripstrain)
summary(model)
    #This gives us an R-squared of 0.6699
Tripstrain$predicted <- fitted(model)
View(Tripstrain)
ggplot(data = Tripstrain, aes(x= tmin, y=count)) + geom_point() + geom_line(aes(x = tmin, y=predicted)
) + xlab("Minimum Temperature") + ylab("Number or Riders")

#Then the test set
predicted_test <- predict(model, Tripstest)
Tripstest$predicted <- predicted_test
View(Tripstest)
ggplot(data = Tripstest, aes(x= tmin, y=count)) + geom_point() + geom_line(aes(x = tmin, y=predicted)
) + xlab("Minimum Temperature") + ylab("Number or Riders")
cor(Tripstest$count, Tripstest$predicted)^2
  #0.7000482 
  
# Repeat this procedure, but add a quadratic term to your model (e.g., + tmin^2, or equivalently + poly(k,2)). 
#How does the model change, and how do the fits between the linear and quadratic models compare?

#First the training
model2 <- lm(count ~ tmin + poly(tmin,2), data = Tripstrain)
summary(model2)
  #R-squared is 0.6706
Tripstrain$predicted_poly <- fitted(model2)
ggplot(data = Tripstrain, aes(x = tmin, y = count)) + geom_point() + geom_line(aes(x = tmin, y = predicted_poly))

#Then the test set
predicted_test_poly <- predict(model2, Tripstest)
Tripstest$predicted_poly <- predicted_test_poly
ggplot(data = Tripstest, aes(x = tmin, y = count)) + geom_point() + geom_line(aes(x = tmin, y = predicted_poly))
cor(Tripstest$count, Tripstest$predicted_poly)^2
  #R-squared is 0.6939525
#The R-squared increases more for the linear then the polynomial

# Now automate this, extending the model to higher-order polynomials with a for loop over the degree k. 
#For each value of k, fit a model to the training data and save the R^2 on the training data to one vector 
#and test vector to another. Then plot the training and test R^2 as a function of k. What value of k has the best performance?

#create two vectors to store R^2 = cor()^2
  #train_cor
  #test_cor
#loop over degree of polynomial

train_cor <- c()
test_cor <- c()

for (h in 1:20) {
  model3 <- lm(count ~ poly(tmin, h), data = Tripstrain)
  Tripstrain$predicted2 <- predict(model3, Tripstrain)
  Tripstest$predicted2 <-  predict(model3, Tripstest)
  train_cor[h] <- cor(Tripstrain$predicted2, Tripstrain$count)^2
  test_cor[h] <- cor(Tripstest$predicted2, Tripstest$count)^2
}

correlation <- data.frame(test_cor, train_cor)
View(correlation)


correlation$k <- c(1:20)

ggplot(data= correlation, aes(x= k, y=train_cor)) + geom_line() + geom_line(aes(x = k, y = test_cor, color= "red"))
#The red line shows our test R^2
#k=7 is the best


# Finally, fit one model for the value of k with the best performance in 6), and plot the actual and predicted values for 
#this model.

#on the training
model4 <- lm(count ~ poly(tmin, 7), data = Tripstrain)
Tripstrain$predicted3 <- fitted(model4)

#on the test
Tripstest$predicted3 <- predict(model4, Tripstest)
ggplot(data = Tripstest, aes(x=tmin, y=count)) + geom_point() + geom_line(aes(x= tmin, y=predicted3))
ggplot(data = Tripstest, aes(x=tmin, y=count)) + geom_point(color= "red") + geom_line(aes(x= tmin, y=predicted3)) +geom_point(data = Tripstrain, aes(x=tmin, y=count))



#################################DAY 5######################3
library(lubridate)
trips_n_weather$day_of_week <- wday(trips_n_weather$ymd)
trips <- mutate(trips, gender=factor(gender, levels=c(0,1,2), labels=c("Unknown","Male","Female")))

#got day of week listed as day, not number
trips_n_weather <- mutate(trips_n_weather, day_of_week=factor(day_of_week, levels = c(1:7), labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
View(trips_n_weather)

#now have to say if weekend
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
trips_n_weather$weekend <- factor((trips_n_weather$day_of_week %in% weekdays), 
                   levels=c(TRUE, FALSE), labels=c('0', '1'))

save(trips_n_weather, file='trips_and_weather.RData')

load("trips_and_weather.RData")
#### Now lets work on the linear model for day 5 ### 


library(dplyr)
library(ggplot2)
library(reshape)
library(scales)
indexes <- sample(1:nrow(trips_n_weather), size=0.2*nrow(trips_n_weather))
Tripstest=trips_n_weather[indexes, ]
Tripstrain=trips_n_weather[-indexes, ]

#The first go around
model <- lm (count ~ ymd + tmax + weekend, data = Tripstrain)
summary(model)
#We get an R^2 value of .7475
Tripstrain$predicted_day5 <- fitted(model)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5))

#Then the test set
predicted_test_day5 <- predict(model, Tripstest)
Tripstest$predicted_day5 <- predicted_test_day5
ggplot(data = Tripstest, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5))
cor(Tripstest$count, Tripstest$predicted_day5)^2
#We get an R^2 value of .8303033.... but its super jagged so I'm not sure I buy this



##TAKE TWO##
model2 <- lm(count ~ tmax + weekend, data = Tripstrain)
summary(model2)
#r-squared is 0.7448
Tripstrain$predicted_day5_2 <- fitted(model2)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_2))
#Then the test set
predicted_test_day5_2 <- predict(model2, Tripstest)
Tripstest$predicted_day5_2 <- predicted_test_day5_2
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_2)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted_day5_2)^2
#The correlation is 0.8308182
#This one is better, but i'm still not sold, is it just me??

##TAKE THREE##

train_cor <- c()
test_cor <- c()

for (h in 1:20) {
  model3 <- lm(count ~ poly(tmax, h) + weekend, data = Tripstrain)
  Tripstrain$predicted_day5_3 <- predict(model3, Tripstrain)
  Tripstest$predicted_day5_3 <-  predict(model3, Tripstest)
  train_cor[h] <- cor(Tripstrain$predicted_day5_3, Tripstrain$count)^2
  test_cor[h] <- cor(Tripstest$predicted_day5_3, Tripstest$count)^2
}

correlation <- data.frame(test_cor, train_cor)
correlation$k <- c(1:20)
View(correlation)

ggplot(data= correlation, aes(x= k, y=train_cor)) + geom_line() + geom_line(aes(x = k, y = test_cor, color= "red"))
max(correlation$test_cor)
#This is for k=10
#let's see how it goes

model4 <- lm(count ~ poly(tmax, 10) + weekend, data = Tripstrain)
summary(model4)
#r-squared is 0.7851
Tripstrain$predicted_day5_4 <- fitted(model4)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_4))
#Then the test set
predicted_test_day5_4 <- predict(model4, Tripstest)
Tripstest$predicted_day5_4 <- predicted_test_day5_4
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_4)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted_day5_4)^2
#The correlation is 0.8531349


##Maybe we need precipitation up in here
model5 <- lm(count ~tmax + weekend + prcp, data = Tripstrain)
summary(model5)
#R-squared is 0.8243
Tripstrain$predicted_day5_5 <- fitted(model5)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_5))
#Then the test set
predicted_test_day5_5 <- predict(model5, Tripstest)
Tripstest$predicted_day5_5 <- predicted_test_day5_5
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_5)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted_day5_4)^2
#R-squared is 0.8531349

#and snow?
model6 <- lm(count ~tmax + weekend + prcp + snow, data = Tripstrain)
summary(model6)
#R-squared is 0.8264
Tripstrain$predicted_day5_6 <- fitted(model6)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_6))
#Then the test set
predicted_test_day5_6 <- predict(model6, Tripstest)
Tripstest$predicted_day5_6 <- predicted_test_day5_6
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_6)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted_day5_4)^2
#R-squared is 0.8531349

#maybe snow isnt the answer, what about interaction beween weekend & precipitation
model7 <- lm(count ~tmax + weekend*prcp, data = Tripstrain)
summary(model7)
#R-squared is 0.826
Tripstrain$predicted_day5_7 <- fitted(model7)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_7)) + facet_wrap(~weekend)
#Then the test set
predicted_test_day5_7 <- predict(model7, Tripstest)
Tripstest$predicted_day5_7 <- predicted_test_day5_7
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_7)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted_day5_7)^2
#R-squared is 0.887987
#This one seems better, so what is going on during the week?

#maybe interact everybody
model8 <- lm(count ~ tmax*prcp + weekend, data = Tripstrain)
summary(model8)
#R-squared is 0.82
Tripstrain$predicted_day5_8 <- fitted(model8)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_8)) + facet_wrap(~weekend)
#Then the test set
predicted_test_day5_8 <- predict(model8, Tripstest)
Tripstest$predicted_day5_8 <- predicted_test_day5_8
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_8)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted_day5_8)^2
#R-squared is 0.8908188



### a ton of stuff wasnt significant##
#In this model all factors were significant
model5 <- lm(count ~tmax + weekend + prcp, data = Tripstrain)
summary(model5)
#R-squared is 0.8243
Tripstrain$predicted_day5_5 <- fitted(model5)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_5))
#Then the test set
predicted_test_day5_5 <- predict(model5, Tripstest)
Tripstest$predicted_day5_5 <- predicted_test_day5_5
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_5)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted_day5_5)^2
#R-squared is 0.8897091

#maybe now lets try making it a polynomial?
train_cor <- c()
test_cor <- c()

for (h in 1:20) {
  model6 <- lm(count ~ poly(tmax, h) + weekend + prcp, data = Tripstrain)
  Tripstrain$predicted_day5_6 <- predict(model6, Tripstrain)
  Tripstest$predicted_day5_6 <-  predict(model6, Tripstest)
  train_cor[h] <- cor(Tripstrain$predicted_day5_6, Tripstrain$count)^2
  test_cor[h] <- cor(Tripstest$predicted_day5_6, Tripstest$count)^2
}

correlation <- data.frame(test_cor, train_cor)
correlation$k <- c(1:20)
View(correlation)
ggplot(data= correlation, aes(x= k, y=train_cor)) + geom_line() + geom_line(aes(x = k, y = test_cor, color= "red"))
#so the red line represents our test
#It tells us that tmax to the 10th degree is the highest, lets scope this out

model7 <- lm(count ~ poly(tmax, 4) + weekend + prcp, data = Tripstrain)
summary(model7)
#R-squared is 0.8511
Tripstrain$predicted_day5_7 <- fitted(model7)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_7))
#Then the test set
predicted_test_day5_7 <- predict(model7, Tripstest)
Tripstest$predicted_day5_7 <- predicted_test_day5_7
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_7)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted_day5_7)^2
#R-squared is 0.8993922
#This is good but still has weird stuff happening, I'm gonna try a second test set and scope it out

indexes2 <- sample(1:nrow(trips_n_weather), size=0.2*nrow(trips_n_weather))
Tripstest2=trips_n_weather[indexes2, ]
#Then the test set take 2
predicted_test2_day5_7 <- predict(model7, Tripstest2)
Tripstest2$predicted_day5_7 <- predicted_test2_day5_7
ggplot(data = Tripstest2, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_7)) + facet_wrap(~weekend)
cor(Tripstest2$count, Tripstest2$predicted_day5_7)^2
#0.8290744

#Just visually it looks like it it has a upside down u (poly 2) so i'm trying that
model9 <- lm(count ~ tmax + weekend, data = Tripstrain)
summary(model9)
#R-squared is 0.7601
Tripstrain$predicted_day5_9 <- fitted(model9)
ggplot(data = Tripstrain, aes(x = ymd, y = count)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_9))
#Then the test set
predicted_test_day5_9 <- predict(model9, Tripstest)
Tripstest$predicted_day5_9 <- predicted_test_day5_9
ggplot(data = Tripstest, aes(x = ymd, y = count, color = tmax)) + geom_point() + geom_line(aes(x = ymd, y = predicted_day5_9)) + facet_wrap(~weekend)
cor(Tripstest$count, Tripstest$predicted_day5_9)^2
#R-squared is 0.7677531