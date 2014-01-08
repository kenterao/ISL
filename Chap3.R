# Chapter 3 Lab: Linear Regression

setwd("C:/Users/Ken/Documents/ISL")
getwd()
library(MASS)
library(ISLR)

# Simple Linear Regression

fix(Boston)
names(Boston)
lm.fit=lm(medv~lstat)
lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")
plot(lstat,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
library(car)
vif(lm.fit)
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)

# Interaction Terms

summary(lm(medv~lstat*age,data=Boston))

# Non-linear Transformations of the Predictors

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)
summary(lm(medv~log(rm),data=Boston))

# Qualitative Predictors

fix(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)

# Writing Functions

LoadLibraries
LoadLibraries()
LoadLibraries=function(){
 library(ISLR)
 library(MASS)
 print("The libraries have been loaded.")
 }
LoadLibraries
LoadLibraries()

## Exercises 3.7
# 1
beta_1 = beta_1_hat
beta_2 = beta_2_hat
beta_3 = 0
The probability that confidence interval of the coefficient on newspaper 
contains 0.

# 2
KNN classifiers take a vote on a 2-class classifier to predict.  
KNN regression take a mean on a continuous variable like a regression.

# 3
#3a
For a fixed value of IQ and GPA, males earn more on average than females
provided that the GPA is high enough.
#3b
50 + 20*X_1 +0.07*X_2 + 35*X_3 + 0.01*X_4 - 10*X_5
50 + 80 + 7.7 + 35 + 4.4 - 40
#3c
False.  Even if the coefficient is small, the coefficient may be significant.
It depends upon the p-value and t-stat of the coefficient. 

# 4
#4a
The training error would be lower for the cubic in sample since it has more 
degree of freedom.  
# 4b
When out of sample, since the linear model is closer to the true model it
should have a lower error.  The cubic function will most likely be overfit
and will not generalize well.
# 4c
Again, in sample error will be lower for the cubic because of the degrees
of freedom.
# 4d
It is likely that the cubic will perform better in the test set because
it is closer to the true distribution.

# 5
# See notes

# 6
# See notes

# 7
# See notes

## Applied Problems
# 8 Auto data set
# 8a
library(ISLR)
summary(Auto)
attach(Auto)
lm.fit = lm(mpg ~ horsepower)
summary(lm.fit)
# i) F-stat is significantly high
# ii) 60% R^2, p-value low enough
# iii) negative relationship
# iv) 
predict(lm.fit,data.frame(horsepower=(c(98))), interval="confidence")
predict(lm.fit,data.frame(horsepower=(c(98))), interval="prediction")
# 8b
plot(horsepower, mpg)
abline(lm.fit)
# 8c
par(mfrow=c(2,2))
plot(lm.fit)
# Residuals have pattern, relationship is nonlinear
lm.fit = lm(mpg ~ horsepower)
summary(lm.fit)
lm.fit2 = lm(mpg ~ horsepower + I(horsepower^2))
summary(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit3 = lm(mpg ~ horsepower + log(horsepower))
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)

# 9 Auto data set
# 9a
pairs(Auto[,1:8])
# 9b
cor(Auto[,1:8], use = "complete.obs")
# 9c
Auto$origin = as.factor(Auto$origin)
lm.fit = lm(mpg~.-name, data=Auto)
summary(lm.fit)
# 9ci) Yes, for some variables. F-stat is significant
# 9cii) Displacement, weight, year, and origin
# 9ciii) Positive coefficient on year suggest
# newer models have better mpg
# 9d
par(mfrow=c(2,2))
plot(lm.fit)
# Still strong nonlinearity in residuals and large outliers in right tail
# 9e
lm.fit = lm(mpg ~ displacement + weight + year + origin)
summary(lm.fit)
lm.fit=update(lm.fit, ~ . + displacement:weight)
lm.fit=update(lm.fit, ~ . + displacement:year)
lm.fit=update(lm.fit, ~ . + displacement:origin)
lm.fit=update(lm.fit, ~ . + weight:year)
lm.fit=update(lm.fit, ~ . + weight:origin)
lm.fit=update(lm.fit, ~ . + year:origin)
summary(lm.fit)
# 9f
lm.fit = lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data=Auto)
summary(lm.fit)
lm.fit=update(lm.fit, ~ . + log(displacement))
lm.fit=update(lm.fit, ~ . + log(horsepower))
lm.fit=update(lm.fit, ~ . + log(weight))
lm.fit=update(lm.fit, ~ . + log(acceleration))
lm.fit=update(lm.fit, ~ . + I(acceleration^2))
summary(lm.fit)

# 10 Carseats dataset
# 10a
summary(Carseats)
lm.fit = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit)
# 10b
# For every 1 unit of change in Price, Sales goes down by 0.054
# Urban Yes is 0.02 lower in Sales than Urban No
# US Yes is 1.20 higher in Sales than US No
# 10c
# Sales = 13.04 - 0.054 * Price - 0.022 * I(Urban=YES) + 1.201 * I(US=YES)
# 10d
# We can reject for Price and US variables
# 10e
lm.fit = update(lm.fit, ~ . - Urban)
summary(lm.fit)
# 10f
# R2 = 0.2335, R2 = 0.2354
# 10g
confint(lm.fit)
# 10h
par(mfrow=c(2,2))
plot(lm.fit)
# pretty well behaved

# 11 t-stats
# 11a
set.seed(1)
x = rnorm(100)
y = 2 * x + rnorm(100)
lm.fit = lm(y ~ x + 0)
summary(lm.fit)
# 11b
lm.fit2 = lm(x ~ y + 0)
summary(lm.fit2)
# 11c
# t-stat, p-value, and r-squared are the same
# coefficient and standard error differ, and not just inverted
# 11d

# 11e
# Argument by symmetry in x=y plane
# 11f















































