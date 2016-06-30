#6.6 Lab 2: Ridge Regression and the Lasso

install.packages("glmnet")
library(glmnet)
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
x=model.matrix(Salary~., Hitters)[,-1]
  #Take the salary and regress everything, on hitters data set
  # -1 (removes that column, grabs everything but) not column one because that lists the names of the people

y=Hitters$Salary
  #pulls out outcomes --> salary

#6.6.1 Ridge Regression
grid=10^seq(10, -2, length=100)
  #a big vector of different lambda values
  #glmnet does this for you
ridge.mod=glmnet(x,y,alpha=0, lambda=grid)
  #but x should be X because matrix
  #alpha is 0 ridge
  #alpha is 1 lasso (by default runs lasso)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
  #pulls out the 50th lambda value
coef(ridge.mod)[,50]
  #pulls out coefficients for 50th lambda value
  #when the output is very low, not putting a lot of weight on it
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod, s=50, type = "coefficients")[1:20,]
  #another way to get coefficients
  #this just pulls off the first 20 coefficients
  #s= picks a particular lambda value, lambda = 50
  #broom may do this even more nicely
set.seed(1)
  #if don't do this you'll keep gettting different answers
train=sample(1:nrow(x), nrow(x)/2)
  #this has us sample half the road
  #sample half the rows (1: 1/2 row)
test=(-train)
  #this is like a not on the index
  #gives us everything but these rows
y.test=y[test]
  #pulls out labels for test data
ridge.mod=glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred=predict(ridge.mod, s=4, newx = x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
  #if you just guessed the average, this would be the amount off (but need to square root it first)
ridge.pred=predict(ridge.mod, s=1e10, newx = x[test,])
  #this has them predict with a different lambda
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod, s=0, newx = x[test,], exact = T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset = train)
predict(ridge.mod, s=0, exact = T, type = "coefficients")[1:20,]
set.seed(1)
cv.out=cv.glmnet(x[train,], y[train], alpha=0)
  #this search overs the lambdas for you
  #while if just do glmnet, then you need to specify a lambda
  #the x[train,] and y[train,] are because they didnt create different dataframes for the test and train
    #need the comma so that you get all of the columns but only those rows
  #internally it computes MSE for every lambda
plot(cv.out)
  #2 dashed lines show you wehre the best lambda occured
  #used lambda min
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod, s= bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha = 0)
predict(out, type = "coefficients", s=bestlam)[1:20,]

#6.6.2 The Lasso
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx = x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef= predict(out, type = "coefficients", s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef != 0]
