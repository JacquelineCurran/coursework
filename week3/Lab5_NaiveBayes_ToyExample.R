#create example data
train <- data.frame(class = c("spam", "ham", "ham", "ham"), viagra = c("yes", "no", "no", "yes"))
train
#estimating naive bayes model
install.packages("e1071")
library(e1071)
classifier <- naiveBayes(class ~ viagra, train)
classifier
#Using estimated model to calculate conditional probability
test <- data.frame(viagra = c("yes"))
test$viagra <- factor(test$viagra, levels = c("no", "yes"))
test
prediction <- predict(classifier, test, type = "raw")
prediction
#Doing the same with two predictors (what makes Naive Bayes naive)
train <- data.frame(type = c("spam", "ham", "ham", "ham"), viagra = c("yes", "no", "no", "yes"), meet=c("yes", "yes", "yes", "no"))
train
#Estimating naive Bayes model
library(e1071)
classifier <- naiveBayes(type ~ viagra + meet, train)
classifier
#Using estimated model to calculate conditional probability
test <- data.frame(viagra = c("yes"), meet=c("yes"))
test$viagra <- factor(test$viagra, levels = c("no", "yes"))
test$meet <- factor(test$meet, levels = c("no", "yes"))
test
prediction <- predict(classifier, test, type = "raw")
prediction

##Exercises##
#1 Create a dataframe
train <- data.frame(buy = c("yes", "no", "no", "yes"), income = c("high", "high", "medium", "low"))
train
classifier <- naiveBayes(buy ~ income, train)
classifier

#5
train2 <- data.frame(buy = c("yes", "no", "no", "yes"), income = c("high", "high", "medium", "low"), gender = c("male", "female", "female", "male"))
train2
classifier2 <- naiveBayes(buy ~ income + gender, train2)
classifier2
