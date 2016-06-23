# Cross-validation for Citibike trips
# 

load('trips.RData')
trips_with_weather <- inner_join(trips, weather, by="ymd")


# In this assignment we'll predict number of trips per day as a function of the weather on that day.
# 
#     Create a dataframe with one row for each day, the number of trips taken on that day, and the minimum temperature on that day.

day_ntrips <- trips_with_weather %>% group_by(ymd) %>% summarize(total = n())


#     Split the data into a randomly selected training and test set, as in the above exercise, 
      #with 80% of the data for training the model and 20% for testing.

#     Fit a model to predict the number of trips as a (linear) function of the minimum temperature, and evaluate 
      #the fit on the training and testing data sets. Do this first visually by plotting the predicted and actual values. 
      # Then do this with R^2, as above. You'll want to use the predict and cor functions for this.

# Repeat this procedure, but add a quadratic term to your model (e.g., + tmin^2, or equivalently + poly(k,2)). 
#How does the model change, and how do the fits between the linear and quadratic models compare?

# Now automate this, extending the model to higher-order polynomials with a for loop over the degree k. 
#For each value of k, fit a model to the training data and save the R^2 on the training data to one vector 
#and test vector to another. Then plot the training and test R^2 as a function of k. What value of k has the best performance?

# Finally, fit one model for the value of k with the best performance in 6), and plot the actual and predicted values for 
#this model.
