rm(list=ls())
library(MASS)
library(splines)
data = Boston
attach(data)
X = data$lstat
Y = data$medv
train_index = sample(1:506,size = 400)
train = data[train_index,]
test = data[-train_index,]
X_train = train$lstat
X_test = test$lstat
y_train = train$medv
y_test = test$medv
par(mfrow=c(1,2))
#bs()
model1 = lm(medv ~ bs(lstat,df=5),data=train)
pred = predict(model1,newdata = test)
plot(X_train,y_train,col='Red',xlab = 'lstat',ylab='medv',main = 'Scatterplot of lstat and medv by bs()')
points(X_test,y_test,col='blue')
nd=seq(min(data$lstat),max(data$lstat),length.out=200)
pred_nd = predict(model1,newdata = data.frame(lstat=nd))
lines(nd,pred_nd)
points(X_test,pred,col='green')
#ns()
model2 = lm(medv ~ ns(lstat,df=5),data=train)
pred2 = predict(model2,newdata = test)
plot(X_train,y_train,col='Red',xlab = 'lstat',ylab='medv',main = 'Scatterplot of lstat and medv by ns()')
points(X_test,y_test,col='blue')
nd2=seq(min(data$lstat),max(data$lstat),length.out=200)
pred_nd2 = predict(model2,newdata = data.frame(lstat=nd2))
lines(nd2,pred_nd2)
points(X_test,pred2,col='green')

#HW 
#split the data set in k folds and find the best hyper parameter from 5 to 25
#find the test mse for the best degrees of freedom and plot hte graph