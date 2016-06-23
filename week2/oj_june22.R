### More sales data

oj <- read.table('oj.csv', header=T, sep=',')


#1. Let’s return to the orange juice assignment and investigate how store demographics are related to demand.

  #i. Let’s start with the following model: ``logmove ~ log(price)*brand*feat`` and add in the store 
  #demographics as linear features (e.g., ``+ AGE60 + EDUC + ETHNIC + INCOME``). Try them individually and then all together.

model1 <- lm(logmove ~ log(price)*brand*feat, data = oj)
model1 <- lm(logmove ~ log(price)*brand*feat + AGE60, data = oj)
model1 <- lm(logmove ~ log(price)*brand*feat + EDUC, data = oj)
model1 <- lm(logmove ~ log(price)*brand*feat + ETHNIC, data = oj)
model1 <- lm(logmove ~ log(price)*brand*feat + INCOME, data = oj)
model1 <- lm(logmove ~ log(price)*brand*feat + AGE60 + EDUC + ETHNIC + INCOME, data = oj)

summary(model1)

  #ii: What demographics are significantly ``(t > 2 standard deviations)`` related to demand?
    #All of them are significantly related to demand (t>2)

  #iii: How much did the adjusted R-squared improve with the addition of these variables?
    #original r-squared:.5352
    #new r-squared: .5678
      #3% improvement

#2. Let’s focus on two variables ``HHLARGE`` ("fraction of households that are large") and 
#``EDUC`` ("fraction of shoppers with advanced education").

  #i. What are the means and percentiles of each of these variables?
      mean(oj$HHLARGE)
        #0.1156024
      quantile(oj$HHLARGE)
     # 0%        25%        50%        75%       100% 
    #  0.01350636 0.09793763 0.11122120 0.13516767 0.21635434 
      
      mean(oj$EDUC)
        #0.2252196
      
      quantile(oj$EDUC)
     # 0%        25%        50%        75%       100% 
    #  0.04955029 0.14598491 0.22939040 0.28439465 0.52836201  
  
  #ii. Using your coefficient estimates from the regression in 1b:

    #a. If we move from the median value of ``HHLARGE`` to the 75th percentile (3rd quartile), how much does ``logmove`` 
    #change each week on average? You can estimate this visually if you plot the fitted model, or you can compare the 
    #predicted values for rows that have the median and 75th percentiles for ``HHLARGE``.

      model1 <- lm(logmove ~ log(price)*brand*feat, data = oj)
      oj$predicted <- fitted(model1)  
      filter(oj, oj$HHLARGE == median(HHLARGE))      
        #8.75
      #75 
      filter(oj, oj$HHLARGE == quantile(HHLARGE, .75))
      #8.47
      8.75 - 8.47
      #.28
      
    #b. If we move from the median value of ``EDUC`` to the 75th percentile (3rd quartile), how much does ``logmove`` 
    #change each week on average?
      #median
      filter(oj, oj$EDUC == median(EDUC))      
      #.875
      #75
      filter(oj, oj$EDUC == quantile(EDUC, .75))
      #8.75

      #no change!
      
    #c. Based on this analysis, which is the more important predictor of demand?
        #Size of household HHLARGE is the more important predictor of demand
      
  #ii. Now let’s see if these variables impact price sensitivity. Add two interaction terms (with logprice) to the model 
  #to test this.

      model1 <- lm(logmove ~ log(price)*brand*feat*HHLARGE*EDUC, data = oj)
      model2 <- lm(logmove ~ log(price)*HHLARGE*EDUC, data = oj)
      
    #a. What are the coefficients on the interaction terms?

      summary(model1)
      summary(model2)
      
      #                        Estimate Std. Error t value Pr(>|t|)    
     # (Intercept)              10.5572     0.1370  77.082  < 2e-16 ***
    #  log(price)               -1.6725     0.1602 -10.438  < 2e-16 ***
      # HHLARGE                   4.6602     1.0649   4.376 1.21e-05 ***
      #EDUC                     -0.6981     0.5138  -1.359  0.17426    
      
      ###############################################################
      #log(price):HHLARGE       -6.4933     1.2442  -5.219 1.81e-07 ***
      # log(price):EDUC           2.1552     0.5947   3.624  0.00029 ***
      #  HHLARGE:EDUC            -20.2578     4.3595  -4.647 3.39e-06 ***
      #  log(price):HHLARGE:EDUC  12.7063     5.0518   2.515  0.01190 *  
      ################################################################
      
    #b. Recall, positive values indicate lower price sensitivity and negative values indicate greater price sensitivity. 
    #Do your estimates make sense based on your intuition?
      #Price & Household size is negative because need to buy more, spending a lot, price increasing is bad
      #Price & Education looking for quality less price sensitive
      #All three: more educated more money? 
      

    #c. What are the coefficient estimates on the constants EDUC and HHLARGE? How do they compare to your regression from 1b?
    #new model
      # HHLARGE                   4.6602     1.0649   4.376 1.21e-05 ***
      #EDUC                     -0.6981     0.5138  -1.359  0.17426    
      
          # Education isn't significant because as we saw when we move from median value to the 3rd quartile, the elasticity 
        #doesnt change 
      
    #d. Similar to 2b, if we move from the median value of each variable to the 3rd quartile, how much does elasticity change? 
    #Based on this, which is more important to price sensitivity?

      #log(price):HHLARGE       -6.4933     1.2442  -5.219 1.81e-07 ***
      # log(price):EDUC           2.1552     0.5947   3.624  0.00029
      # HHLARGE is more important in terms of price sensitivity as we saw when we moved from the median to the 3rd quartile
      
  #iv. You should notice that the coefficients on ``EDUC`` and ``HHLARGE`` have flipped sign once we include interaction terms 
  #with price. HHLARGE now appears to be a positive demand shifter and increases price sensitivity. Explain in words or pictures 
  #what is going on.

      #When alone v interaction terms flips the terms
        #HHLARGE: alone, just direct effect, intercept large because demand more
        ##interaction slope: negative because more price sensitive because they respond to prices
      
#3. Let’s split our data into a training set and a test set. An easy way to do this is with the sample command. 
#The following will randomly select 20% of the rows in our data frame: ``indexes <- sample(1:nrow(oj), size=0.2*nrow(oj))``

      oj_test <- indexes <- sample(1:nrow(oj), size=0.2*nrow(oj))
  
      #i. Now let’s use this index to create a training and a test set, try:
  #``OJtest=oj[indexes, ]`` and ``Ojtrain=oj[-indexes, ]``. What did this do? How many rows does the test set have? 
  #How many rows does the training set have?
      OJtest=oj[indexes, ]
      Ojtrain=oj[-indexes, ]
      #The training set has 23158 rows and the test set has 5789
      
#4. Now let’s run the very simple model ``logmove ~ log(price) + brand`` on the training data.

  #i. Use LM on this model and report the R-squared.
    model <- lm(logmove ~ log(price) + brand, data = Ojtrain)
    summary(model)    
      #Rsquared was .3968
    
  #ii. Use ``predict(model, Ojtest)`` to predict log sales for the test set.
    predicted_sales <- predict(model, OJtest)
    
  #iii. Compute ``cor(predicted_sales,logmove)^2`` on the test set. This is our "honest R-squared". 
  #How does it compare to the value in (a)?
    cor(predicted_sales,OJtest$logmove)^2
    #0.3833419
    
#5. Now let’s run better models.

  #i. Run our "previous favorite" ``logmove ~ brand*log(price)*feat`` on the training data. Use LM to get regular R-squared. 
  #Now, follow the procedure in (3) to compute "honest R-squared". What is it? How do they compare?
    model5 <- lm(logmove ~ brand*log(price)*feat, data = Ojtrain)
    summary(model5)    
      #Regular r-sqared is 0.5398
    predicted_sales5 <- predict(model5, OJtest)  
    cor(predicted_sales5,OJtest$logmove)^2
  
    #ii. Now add in all the demographics. What is the regular R-squared on training data? 
  #What is the honest R-squared on the test set?
        #On the training data it was 0.5398
        #The honest Rwquared on the test set was 0.5179023
