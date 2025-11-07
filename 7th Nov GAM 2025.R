rm(list=ls())
library(MASS)
library(mgcv)
library(splines)
data = Boston
attach(data)

X = data[,c('lstat','rm')]
Y = data$medv

train_index = sample(1:506,size = 400)
train = data[train_index,]
test = data[-train_index,]
X_train = train$lstat
X_test = test$lstat
y_train = train$medv
y_test = test$medv
b=gam(medv~s(lstat)+s(rm),data=train)
par(mfrow=c(2,2))
plot(b,pages=1,residuals=TRUE)  ## show partial residuals
plot(b,pages=1,seWithMean=TRUE) ## `with intercept' CIs

gam.check(b)

model1 = b
pred = predict(model1,newdata = test)
plot(train$lstat,train$medv,col='Red',xlab = 'lstat',ylab='medv',main = 'Scatterplot of lstat and medv by GAM')
points(test$lstat,test$medv,col='blue')

plot(train$rm,train$medv,col='Red',xlab = 'rm',ylab='medv',main = 'Scatterplot of rm and medv by GAM')
points(test$rm,test$medv,col='blue')
nd_l=seq(min(lstat),max(lstat),length.out=200)
nd_r=seq(min(rm),max(rm),length.out=200)
pred_nd = predict(model1,newdata = data.frame(lstat=nd,rm=nd_r))
lines(nd,pred_nd)
points(X_test,pred,col='green')