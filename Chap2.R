# Chapter 2 Lab: Introduction to R

setwd("C:/Users/Ken/Documents/ISL")

# Basic Commands

x <- c(1,3,2,5)
x
x = c(1,6,2)
x
y = c(1,4,3)
length(x)
length(y)
x+y
ls()
rm(x,y)
ls()
rm(list=ls())
?matrix
x=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x
x=matrix(c(1,2,3,4),2,2)
matrix(c(1,2,3,4),2,2,byrow=TRUE)
sqrt(x)
x^2
x=rnorm(50)
y=x+rnorm(50,mean=50,sd=.1)
cor(x,y)
set.seed(1303)
rnorm(50)
set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

# Graphics

x=rnorm(100)
y=rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")
pdf("Figure.pdf")
plot(x,y,col="green")
dev.off()
x=seq(1,10)
x
x=1:10
x
x=seq(-pi,pi,length=50)
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)

# Indexing Data

A=matrix(1:16,4,4)
A
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[1,]
A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]
dim(A)

# Loading Data

Auto=read.table("Auto.data")
fix(Auto)
Auto=read.table("Auto.data",header=T,na.strings="?")
fix(Auto)
Auto=read.csv("Auto.csv",header=T,na.strings="?")
fix(Auto)
dim(Auto)
Auto[1:4,]
Auto=na.omit(Auto)
dim(Auto)
names(Auto)

# Additional Graphical and Numerical Summaries

plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
detach(Auto)
plot(cylinders, mpg)
attach(Auto)
cylinders=as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")
hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(horsepower,mpg)
identify(horsepower,mpg,name)
summary(Auto)
summary(mpg)

# Exercises

# 1. Conceptual
1a. Flexible should do better since there is little danger in overfitting due 
to the large sample size and small feature set.  Increase in variance is small while 
decrease in bias is large as the model complexity increases.
1b. Inflexible should do better since there is danger in overfitting due to small sample size
and large feature set.  Increase in variance is large while decrease in bias is small 
as the model complexity increases.
1c. Since the true underlying relationship is complex, a flexible method is needed to model it.
1d. Since the irreducible error is high, there is risk in overfitting to the noise 
if the model is too flexible, so a simple inflexible method is best.

# 2. Conceptual
2a. Regression. Inference, not prediction. n = 500, p = 3
2b. Classification. Prediction, over inference. n = 10, p = 13
2c. Regression. Prediction, not inference. n = 52, p = 4

# 3. Conceptual









# 5. Conceptual
Flexible vs less flexible tradeoffs
If the true relationship is more complex than a flexible approach is needed to capture that.
If the sample size is small, a flexible method will overfit so a simpler model is better.
If the number of features is large, a flexible method will overfit so a simpler model is best.
If the irreducible noise is large, a flexible method will overfit so a simpler model is best.


# 6. Conceptual
Parametric vs non-parametric
Parametric ties a method to a specific functional form of the solution.
Non parametric is more flexible.
Parametric can give inference to the structure of the model while the nonparametric is black box.
Nonparametric often allows more flexibility in the model.


# 7. Conceptual
7a.
Obs. 	Dist
1	9
2	4
3	10
4	5
5	2
6	3
7b. Green
7c. Red
7d. If underlying relationship is complex, increasing K will make things smoother and 
less variance and more bias.  Smaller K will capture the true complexity better without 
giving up too much in bias.


### 8 College data set
# 8a
setwd("C:/Users/Ken/Documents/ISL")
college = read.csv("College.csv", header=T)
fix(college)
# 8b
rownames(college) = college[,1]
college = college[, -1]
fix(college)
# 8ci
summary(college)
# 8cii
dim(college)
pairs(college[, 1:10])
# 8ciii
college$Private = as.factor(college$Private)
plot(college$Private, college$Outstate)
# 8civ
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college)
plot(college$Elite, college$Outstate)
# 8cv
par(mfrow = c(2, 2))
hist(college$Outstate)
hist(college$Outstate, breaks=25)
hist(college$Accept / college$Apps, breaks=25)
hist(college$Room.Board, breaks=25)

### 9 Auto data
# 9a
Auto=read.csv("Auto.csv",header=T,na.strings="?")
fix(Auto)
Auto$origin = as.factor(Auto$origin)
#Qualitative variables are origin and name
# 9b
range(Auto$mpg, na.rm = TRUE)
range(Auto$cylinders, na.rm = TRUE)
range(Auto$displacement, na.rm = TRUE)
range(Auto$horsepower, na.rm = TRUE)
range(Auto$weight, na.rm = TRUE)
range(Auto$acceleration, na.rm = TRUE)
range(Auto$year, na.rm = TRUE)
# 9c
for (ii in 1:6) {
	print(mean(Auto[,ii], na.rm = T))
	print(sd(Auto[,ii] , na.rm = T))
}
# 9d
print(dim(Auto))
Autotemp = Auto
Auto = Auto[ -(10:85), ]
print(dim(Auto))
for (ii in 1:6) {
	print(mean(Auto[,ii], na.rm = T))
	print(sd(Auto[,ii] , na.rm = T))
}
Auto = Autotemp
# 9e
pairs(Auto[,1:7])


### 10 Boston data
# 10a
library(MASS)
Boston
dim(Boston)
summary(Boston)
pairs(Boston)
# 10b
par(mfrow = c(2, 2))
plot(log(Boston$crim), Boston$medv)
plot(log(Boston$lstat), Boston$medv)
plot(Boston$rm, Boston$medv)
plot(Boston$ptratio, Boston$medv)
# 10c
plot(Boston$rad, log(Boston$crim))
plot(Boston$tax, log(Boston$crim))
# 10d
range(Boston$crim)
hist(Boston$crim, breaks=25)
range(Boston$tax)
hist(Boston$tax, breaks=25)
range(Boston$ptratio)
hist(Boston$ptratio, breaks=25)
# 10e
table(Boston$chas)
# 10f
summary(Boston$ptratio)
# 10g
min(Boston$medv)
Boston[which(Boston$medv == min(Boston$medv)), ]
summary(Boston)
# 10h
sum( Boston$rm > 7.0 ) / dim(Boston)[1]
sum( Boston$rm > 8.0 ) / dim(Boston)[1]
Boston[which(Boston$rm>8),]
summary(Boston[which(Boston$rm>8),])
summary(Boston)




























