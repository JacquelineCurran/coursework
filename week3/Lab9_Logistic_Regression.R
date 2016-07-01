library(dplyr)
loan <- read.csv("https://www.dropbox.com/s/vljs4z2r4wixful/LoanStats3a_securev1.csv?raw=1", skip=1)
library(dplyr)
install.packages("stargazer")
library(stargazer)
library(caret)
install.packages("caret")
loan <- read.csv("https://www.dropbox.com/s/89g1yyhwpcqwjn9/lending_club_cleaned.csv?raw=1")
loan <- read.csv("LoanStats3a_securev1.csv", skip=1)
loan <- filter(loan, loan_status!="")
loan$good <- ifelse(loan$loan_status=="Current" | 
                      loan$loan_status=="Fully Paid" |
                      loan$loan_status=="Does not meet the credit policy.  Status:Fully Paid","good","bad")
loan$good <- as.factor(loan$good)
loan$fico <- (loan$fico_range_high+loan$fico_range_low)/2

#consolidate some of the purpose categories
#create a character vector with level 'labels' where 'renewable energy' is replaced with 'other'
loan$test <- ifelse(loan$purpose=="renewable_energy" | loan$purpose=="moving", 
                    "other", as.character(loan$purpose))
loan$test <- ifelse(loan$purpose=="house", "home_improvement", loan$test)
loan$test <- ifelse(loan$purpose=="vacation", "vacation_wedding", loan$test)
loan$test <- ifelse(loan$purpose=="wedding", "vacation_wedding", loan$test)
loan$test <- ifelse(loan$purpose=="credit_card", "debt_consolidation", loan$test)
loan$test <- ifelse(loan$purpose=="car", "major_purchase", loan$test)
#turn the character vector back into a factor
loan$purpose <- as.factor(loan$test)
table(loan$purpose)
levels(loan$purpose)
loan$income <- ifelse(loan$is_inc_v=="Not Verified", NA, loan$annual_inc)

table(loan$is_inc_v)

###now lab 9##
summary(loan)
logit1 <- glm(good ~ fico, data = loan, family = "binomial")
summary(logit1)
#Interpreting coefficients in logit regression 1: odds ratio
exp(coef(logit1))
#4 interpreting coefficients in logit regression II: probability
test <- data.frame(fico =c(700, 750))
test$pred <- predict(logit1, test, type = "response")
test

#5 Interpreting coefficients in a multiple
logit2 <- glm(good ~ fico + loan_amnt, data = loan, family = "binomial")
summary(logit2)
exp(coef(logit2))

#6 Working with categorical/factor variables
logit3 <- glm(good ~ fico + loan_amnt + purpose, data = loan, family = "binomial")
summary(logit3)
round(exp(coef(logit3)),3)
loan <- loan %>% group_by(purpose) %>% mutate(nobs=n())
loan$purpose <- reorder(loan$purpose, -loan$nobs)
levels(loan$purpose)

#Dealing with missing values
logit4 <- glm(good~ fico + loan_amnt + income + purpose, data = loan, family = "binomial")
summary(logit4)

#Presenting Regression models in a compact table
stargazer(logit1, logit2, logit3, logit4, type = "text")

#Testing logistic model out of sample
set.seed(264)
sample <- sample(nrow(loan), floor(nrow(loan)*0.8))
train <- loan[sample,]
test <- loan[-sample,]
logit4 <- glm(good ~ fico + dti + loan_amnt + purpose, data = train, family = "binomial")
test$pred <- predict(logit4, test, type = "response")
test$good_pred <- ifelse(test$pred > 0.80, "good", "bad")
confusionMatrix(test$good_pred, test$good)


#Exercises
View(Titanic)
logit1 <- glm(Survived ~ Sex, data = Titanic, family = "binomial")
summary(logit1)
  #no
logit2 <- glm(Survived ~ Age, data = Titanic, family = "binomial")
summary(logit2)
  #no
logit3 <- glm(Survived ~ Class, data = Titanic, family = "binomial")
summary(logit3)
  #no
