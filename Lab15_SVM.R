

library(e1071)
library(ROCR)


setwd("C:/Users/GG/Desktop/1115")

icsv <- read.csv("iris.csv",header = T, sep = ",")

for(i in 1:100){ #Other
  icsv[i,5] = 2
}
for(i in 101:150){#Virginica
  icsv[i,5] = 1
}
icsv <- lapply(icsv, as.numeric)

x <- data.frame(icsv$sepal.length,icsv$sepal.width,icsv$petal.length,icsv$petal.width)
y <- icsv$variety

set.seed(1233)

plot(x, col = (73 - y))


dat = data.frame(x = x, y = as.factor(y))

svmfit = svm(y ~., data = dat, kernel = "linear",
             cost = 10, scale = F) 

plot(svmfit, dat,x.icsv.petal.width ~ x.icsv.petal.length,
     slice = list(x.icsv.sepal.width = 3, x.icsv.sepal.length = 4))

svmfit$index
summary(svmfit)

set.seed(1)
tune.out = tune(svm, y~., data = dat, kernel = "linear",
                ranges = list(cost = c(0.001, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmod = tune.out$best.model
summary(bestmod)

set.seed(1)
train = sample(150, 75)

ypred = predict(bestmod, dat[-train,])
table(predict = ypred, truth = dat[-train,"y"]) 

rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}


svmfit.opt = svm(y ~., data = dat[train,], kernel = "radial",
                 gamma = 2, cost = 1, decision.values = T)

fitted = attributes(predict(svmfit.opt, dat[train,], 
                            decision.values = T))$decision.values
par(mfrow = c(1,2))
rocplot(fitted, dat[train, "y"], main = "Training Data")
svmfit.flex = svm(y ~., data = dat[train,], kernel = "radial",
                 gamma = 50, cost = 1, decision.values = T)

fitted = attributes(predict(svmfit.flex, dat[train,], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], main = "Training Data", add = T, col = "red")


fitted = attributes(predict(svmfit.opt, dat[-train,], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[-train,"y"], main = "Test data")
fitted = attributes(predict(svmfit.flex, dat[-train,], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[-train,"y"], add = T, col = "red")



