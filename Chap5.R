# Chaper 5 Lab: Cross-Validation and the Bootstrap

# for home Windows machine
#setwd("C:/Users/Ken/Documents/ISL")
# for Jump Windows machine
setwd("C:/Users/kterao/Documents/ISL")

# The Validation Set Approach

library(ISLR)
set.seed(1)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# Leave-One-Out Cross-Validation

glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)
library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
cv.error=rep(0,5)
for (i in 1:5){
 glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
 cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
 }
cv.error

# k-Fold Cross-Validation

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
 glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
 cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
 }
cv.error.10

# The Bootstrap

alpha.fn=function(data,index){
 X=data$X[index]
 Y=data$Y[index]
 return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
 }
alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio,alpha.fn,R=1000)

# Estimating the Accuracy of a Linear Regression Model

boot.fn=function(data,index)
 return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef
boot.fn=function(data,index)
 coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef


# Exercises: Conceptual

# 1: Minimum Variance
# See notes

# 2: Bootstrap samples
# 2a
# 1 - 1/n
# 2b
# 1 - 1/n
# 2c
(1 - 1/n)^n
pr = function(n) { 1 - (1 - 1/n)^n }
# 2d
pr(5)
# 2e
pr(100)
# 2f
pr(10000)
# 2g
plot(1:10000,pr(1:10000))
1 - exp(-1) # converges to this
# 2h
store=rep(NA, 10000)
for(i in 1:10000) {
	store[i] = sum(sample(1:100, rep=TRUE)==4)>0
}
mean(store)

# 3: K-fold cross-validation
# 3b: VS Validation Set
# advantage: maintain a whole data set, not waste any data
# disadvantage: computation time
# VS LOOCV
# advantage: computation time, lower variance
# disadvantage: smaller sample size

# 4: standard deviation of y-hat
# Estimate confidence intervals on beta_i into the model.
# Resample, fit model, estimate beta_i_hat and get y_hat
# Repeat with different resample, and collect output
# and calculate distribution based on these y_hats for different resamples
# Essentially bootstrapping


# Exercises: Applied
# 5: Logistic regression
set.seed(1)
summary(Default)
dim(Default)
# 5a
glm.fit=glm(default~income+balance,data=Default,family=binomial)
summary(glm.fit)
plot(Default$default, Default$income)
plot(Default$default, Default$balance)
table(Default$default, Default$student)
# 5b, c
store = rep(0,10)
for( ii in 1:length(store) ) {
train = sample(10000,8000)
glm.fit=glm(default~income+balance, data=Default, family=binomial, subset=train)
summary(glm.fit)
glm.probs=predict(glm.fit, Default[-train,])
glm.pred=rep("No",2000)
glm.pred[glm.probs>.5]="Yes"
res = table(glm.pred, Default$default[-train])
store[ii] = (res[1,2] + res[2,1]) / 2000
}
store
mean(store)
# 5d
store = rep(0,10)
for( ii in 1:length(store) ) {
train = sample(10000,8000)
glm.fit=glm(default~income+balance+student, data=Default, family=binomial, subset=train)
summary(glm.fit)
glm.probs=predict(glm.fit, Default[-train,])
glm.pred=rep("No",2000)
glm.pred[glm.probs>.5]="Yes"
res = table(glm.pred, Default$default[-train])
store[ii] = (res[1,2] + res[2,1]) / 2000
}
store
mean(store)
# Marginal difference

# 6: Logistic Regression
# 6a
glm.fit=glm(default~income+balance, data=Default, family=binomial)
summary(glm.fit)
# 6b
boot.fn = function(data, index) {
return(coef(glm(default~income+balance, data=data, subset=index, family=binomial)))
}
boot.fn(Default, sample(10000,10000,replace=TRUE))
# 6c
boot(Default, boot.fn, 1000)
# 6d
# Standard errors are approximately the same, 
# meaning the model assumptions of homeskedasticity, independence, etc.
# are relatively sound

# 7: GLM and LOOCV

# 7a

# 7b

# 7c

# 7d

# 7e


# 8: Simulated CV

# 8a

# 8b

# 8c

# 8d

# 8e

# 8f


# 9: Boston data

# 9a

# 9b

# 9c

# 9d

# 9e

# 9f

# 9g

# 9h














































