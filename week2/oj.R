<<<<<<< HEAD
#elasticity notes
#Price elasticity of demand measures the responsiveness of demand to changes in price for a particular good. 
#If the price elasticity of demand is equal to 0, demand is perfectly inelastic (i.e., demand does not change when price changes). 
#Values between zero and one indicate that demand is inelastic (this occurs when the percent change in demand is less than the 
#percent change in price). When price elasticity of demand equals one, demand is unit elastic (the percent change in demand is 
#equal to the percent change in price). Finally, if the value is greater than one, demand is perfectly elastic 
#(demand is affected to a greater degree by changes in price).

#Between 0 and 1  inelastic (demand doesnt change when price changes)
#Greater than one elastic (demand changes when price changes)

#Sales Data Week 2 Day 2

# 1. Load the orange juice data
oj <- read.table('oj.csv', header=T, sep=',')
View(oj)
head(oj, 0)

#2 Visualizing price
# i. Make a plot of the distribution of prices
ggplot(data = oj, aes(x= price))+geom_histogram(binwidth = .10)

#ii. Change the x-axis on this plot ot use a logarithmic scale using scale_x_log10()
ggplot(data = oj, aes(x= price))+geom_histogram()+scale_x_log10()

#iii. Repeat i) faceted by brand
ggplot(data = oj, aes(x= price)) + geom_histogram(binwidth = .10) + facet_grid(brand ~ .)

#iv. Repeat ii) faceted by brand
ggplot(data = oj, aes(x= price)) + geom_histogram()+scale_x_log10() + facet_grid(brand ~ .)

#v. What do these graphs tell you about the variation in price? Why do the log pots look different? 
#Do you find them more or less informative?

#Price varies across brands, dominicks is the least expensive (on average) while tropicana is typically more expensive
#the log plots look different because it scales the prices in terms of log
#I find the normal one most informative for this because it is in terms of price

#3 Visualizing the quantity/price relationship
#i Plot logmove (the log of quantity sold) vs log price
ggplot(data = oj, aes(x = logmove, y=logprice)) + geom_point()

#i Color each point by brand. What new insights can you derive that were not apparent before?
logprice <- log(oj$price)
ggplot(data = oj, aes(x = logmove, y=logprice, color=brand)) + geom_point()
#This allows us to see the dispersion of quantity sold and price based on brand.
#Higher price Tropicana seems to sell a more concentrated quantity while dominicks has a wide variety

#4 Estimating the relationship
#i Do a regression of logmove on log price
mymodel <- lm(logmove ~ logprice, data = oj)
summary(mymodel)
#How well does the model fit?
#The model fits ok, the p-value for log-price and intercept are significant
#However the R value is very low and log price only explains about 21% of the variation in logmove

#What is the elasticity (the coefficient on logprice) and does it make sense?
#The elasticity is -1.6, this makes sense because as price increases, a buyers willingness to purchase declines
#Therefore, the logmove should decrease since this is a value representative of sales volume
#This is elastic

#ii. now add in an intercept term for each brand (by adding brand to the regression formula)
mymodel2 <- lm(logmove ~ logprice + brand, data = oj)
summary(mymodel2)

#How do the results change?
#The brand and price is able to explain about 40% of the variation in price according to the adjusted r-squared
#However, the brand dominicks is not statistically significant so has been left out
#Based on the coefficients, people increase the quantity they buy of tropicana by 1.53 and 0.87

#How should we interpret these coefficients
#We are able to interpret these coefficient to show that the minute maid brands and tropicana brands are related to quantity
#however, This still only explains 40%
#But the jump in sales volume is largest for tropicana
#minumte maid is inelastic, tropicana is elastic and so is price

#iii. Now add interaction terms to allow the elasticities to differ by brand, by including a brand:log price term in the regression 
#formula. Note the estimate coefficients will "offset" the base estimates. 
mymodel3 <- lm(logmove ~ logprice + brand + brand:logprice -1, data = oj)
summary(mymodel3)

#What is the insights we get from this regression? 
#interaction between minute maid and price is not significant

#What is the elasticity for each firm? 
#elasticity for tropicana is greater than minute maid
#nothing but price are elastic 

#Do the elasticities make sense?
#yes, tropicana is "luxury" more effected by price
#demand pretty much doesnt change otherwise

#5 Impact of featuring in store
#i Which brand is featured the most? make a plot to show this?

oj_brand <- oj %>% filter(feat == 1) %>% group_by(brand) 
View(oj_brand)
ggplot(oj_brand, aes(x=brand)) + geom_bar()
#Minute maid

#ii.How should we incorporate the "featured in store" variable into our regression? 
#Start with an additive formulation (e.g. feature impacts sales, but not through price)
mymodel4 <- lm(logmove ~ feat + logprice, data = oj)
summary(mymodel4)
#both are elastic

#iii: Now run a model where features can impact sales and price sensitivity.
mymodel5 <- lm(logmove ~ feat + logprice + feat:logprice, data = oj)
summary(mymodel5)
#only feat is elastic others are close but not exactly >1

#iv: Now run a model where each brand can have a different impact of being featured and a different impact on price sensitivity. 
#Produce a table of elasticties for each brand, one row for "featured" and one row for "not featured" (you need 6 estimates).
mymodel6 <- lm(logmove ~ brand*logprice+ brand*feat, data = oj)
summary(mymodel6)
ggplot(oj, aes(x=logprice, y=logmove, color= as.factor(feat))) + geom_smooth(method = "lm") + facet_wrap(~brand) + geom_point()


dominicks <- c((-3.20262+0.80265), -3.20262)
minutemaid <- c((-2.27032+1.13811), -2.27032)
tropicana <- c((-1.65643+0.74484), -1.65643)
results_table <- data_frame(dominicks, minutemaid, tropicana)
row.names(results_table) <- c("featured", "not featured")
View(results_table)
#all but featured tropicana are elastic

#tbl_df
#as_data_frame
#to make a tibble
=======
library(readr)
library(ggplot2)
library(scales)
library(broom)

# read the data
oj <- read_csv('oj.csv')

### VISUALIZING BRAND EFFECTS ###

# fit a model with an offset for brand
model <- lm(logmove ~ log(price) + brand, data=oj)

# add the predicted values to the original data frame
oj$predicted <- fitted(model)

# plot the original data points and overlay the predicted values as a line
ggplot(oj, aes(x=log(price), y=logmove, color=brand)) +
  geom_point(alpha=0.1) +
  geom_line(aes(x=log(price), y=predicted, color=brand)) +
  xlab('log(price)') +
  ylab('log(sales)')

# use geom smooth, note the different slopes
# this is equivalent to fitting a different model (intercept and slope) for each brand
# similar to logmove ~ log(price)*brand
ggplot(oj, aes(x=log(price), y=logmove, color=brand)) +
  geom_point(alpha=0.1) +
  geom_smooth(method="lm") +
  xlab('log(price)') +
  ylab('log(sales)')


### ADDING FEATURED PRODUCT EFFECTS ###

# fit a model with lots of interaction terms
model <- lm(logmove ~ log(price)*brand*feat - 1, data=oj)

# add the predicted values to the original data frame
oj$predicted <- fitted(model)

# plot the original data points and overlay the predicted values as a line
ggplot(oj, aes(x=price, y=exp(1)^logmove, color=brand, shape=as.factor(feat))) +
  geom_point(alpha=0.1) +
  geom_line(aes(x=price, y=exp(1)^predicted, color=brand, linetype=as.factor(feat))) +
  facet_wrap(~ brand) +
  scale_x_log10(breaks=c(1,2,3)) +
  scale_y_log10(label=comma, breaks=c(1e3,1e4,1e5)) +
  xlab('Price') +
  ylab('Sales')

# use geom smooth to do similar
ggplot(oj, aes(x=price, y=exp(1)^logmove, color=brand, shape=as.factor(feat), linetype=as.factor(feat))) + 
  geom_point(alpha=0.1) + 
  geom_smooth(method="lm") + 
  facet_wrap(~ brand) +
  scale_x_log10(breaks=c(1,2,3)) +
  scale_y_log10(label=comma, breaks=c(1e3,1e4,1e5)) +
  xlab('Price') +
  ylab('Sales')


### PLOTTING COEFFICIENTS FOR DEMOGRAPHICS ON OVERALL DEMAND ###

# fit a model with lots of interaction terms plus demographic features
model <- lm(logmove ~ log(price)*brand*feat - 1 + AGE60 + EDUC + ETHNIC + INCOME + HHLARGE + WORKWOM + HVAL150, data=oj)

# view the effects of demographics on overall sales
# do this the "old way" by manually extracting coefficients
coefficients <- coef(model)[6:12]
plot_data <- data.frame(variable = as.factor(names(coefficients)), value = as.numeric(coefficients)) %>%
  mutate(variable = reorder(variable, value))
# effect in log sales space
ggplot(plot_data, aes(x = value, y = variable)) +
  geom_point() +
  geom_vline(xintercept=0, linetype=2) +
  xlab('Change in log(sales)') +
  ylab('')
# transformed from log sales to sales
ggplot(plot_data, aes(x = exp(1)^value, y = variable)) +
  geom_point() +
  geom_vline(xintercept=1, linetype=2) +
  scale_x_log10(breaks=c(0.3,1,3)) +
  xlab('Relative change in sales') +
  ylab('')

# repeat this, with the help of broom to get a tidy data frame summarizing the model
plot_data <- tidy(model) %>%
  mutate(term = reorder(term, estimate))
# effect in log sales space
ggplot(plot_data[6:12, ], aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - 2*std.error, xmax = estimate + 2*std.error), height=0) +
  geom_vline(xintercept=0, linetype=2) +
  xlab('Change in log(sales)') +
  ylab('')
# transformed from log sales to sales
ggplot(plot_data[6:12, ], aes(x = exp(1)^estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = exp(1)^(estimate - 2*std.error), xmax = exp(1)^(estimate + 2*std.error)), height=0) +
  geom_vline(xintercept=0, linetype=2) +
  scale_x_log10(breaks=c(0.3,1,3)) +
  xlab('Relative change in sales') +
  ylab('')


### PLOTTING DEMOGRAPHIC EFFECTS FOR   HOUSEHOLD SIZE AND EDUCATION ###

# fit a model focusing on the effects of two demographic features
model <- lm(logmove ~ log(price)*HHLARGE + log(price)*EDUC, data=oj)


# construct a new data frame with just the variables and values we'd like to look at
plot_data <- expand.grid(price = unique(oj$price),
                         HHLARGE = quantile(oj$HHLARGE, c(.25,0.5, 0.75)),
                         EDUC = quantile(oj$EDUC, c(.25,0.5, 0.75)))
plot_data$predicted <- predict(model, plot_data)
plot_data <- mutate(plot_data,
                    EDUC = factor(EDUC, labels=c('Low education','Typical education','High education')),
                    HHLARGE = factor(HHLARGE, labels=c('Low','Median','High')))
# color by large households, facet by education
ggplot(plot_data, aes(x=price, y=exp(1)^predicted, color=as.factor(HHLARGE))) +
  geom_line() +
  scale_x_log10(breaks=c(1,2,3), lim=c(.8, 4)) +
  scale_y_log10(label=comma, breaks=c(1e3,1e4,1e5), lim=c(1e3,1e5)) +
  scale_color_discrete(name='Fraction of large households') +
  xlab('Price') +
  ylab('Sales') +
  facet_wrap(~ EDUC)
>>>>>>> f8d31944ef7f7fd8909dea8ef4fc78989d0f6b40
