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