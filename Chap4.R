# Chapter 4 Lab: Logistic Regression, LDA, QDA, and KNN

# for home Windows machine
#setwd("C:/Users/Ken/Documents/ISL")
# for Jump Windows machine
setwd("C:/Users/kterao/Documents/ISL")
# The Stock Market Data

library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

# Logistic Regression

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred==Direction)
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

# Linear Discriminant Analysis

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)

# Quadratic Discriminant Analysis

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)

# K-Nearest Neighbors

library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

# An Application to Caravan Insurance Data

dim(Caravan)
attach(Caravan)
summary(Caravan)
summary(Purchase)
348/5822
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
9/(68+9)
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15
glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs=predict(glm.fit,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)

# Exercises
# 1
# Proof is trivial
# 2
# See notebook for proof
# 3
# See notebook for solution
# 4 Large p in KNN:
# Curse of Dimensionality for Non-parametric methods
# 4a
# 10%
# 4b
# 1% = (0.10)^2
# 4c
# (0.10)^p
# 4d
# 4e
(0.10)^(1/2)
(0.10)^(1/3)
(0.10)^(1/4)
# Cube edges have to get pretty large to encompass 10% of the data.
# 5: LDA vs QDA
# 5a
# Since QDA has more free parameters it will fit better in the training set.
# LDA should perform better on the test set, since it more closely models the linear reality
# 5b
# Since QDA has more free parameters it will fit better in the training set.
# QDA should perform better on the test set, since it more closely models the linear reality
# 5c
# test prediction for QDA should increase for more complex Bayes decision boundaries
# since QDA can capture more of the functional complexities more accurately with more data
# LDA probably will not be affected as much with increase in sample size
# 5d
# True due to more free parameters
# 6
score = -6 + 0.05*40 + 1*3.5
exp(score) / (1 + exp(score))
score = 0
(score + 6 - 1*3.5) / 0.05
# 7
# Pr(D|X=4) = Pr(X=4|D)*Pr(D) / ( Pr(X=4|D)*Pr(D) + Pr(X=4|Dc)*Pr(Dc) )
prD = 0.80
dnorm(4,10,6) * prD / ( dnorm(4,10,6) * prD + dnorm(4,0,6) * (1-prD))
0.752
# 8
# Uncertain.  Since KNN is fitting only on a training set, it is prone to overfitting, 
# especially when using a nonparametric method.  No conclusion can be drawn about 
# test error estimates.
# 9
0.37 / (1 + 0.37)
(1 - 0.16) / 0.16

























