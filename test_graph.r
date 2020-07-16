library(RLT)
library(randomForest)
library(randomForestSRC)
library(ranger)

set.seed(1)
n = 1500
trainn = 1000
testn = 500

ntrees = 4
ncores = 1
nmin = 10
mtry = 100
sampleprob = 0.85
rule = "best"
nsplit = ifelse(rule == "best", 0, 3)
importance = FALSE

trainX = matrix(rnorm(trainn*trainn), trainn, trainn)
trainY = as.factor(sample(c(0,1), trainn,replace = TRUE))

testX = matrix(rnorm(testn*testn), testn, trainn)
testY = as.factor(sample(c(0,1), testn,replace = TRUE))

xorder = order(testX[, 1])
testX = testX[xorder, ]
testY = testY[xorder]

metric = data.frame(matrix(NA, 4, 4))
rownames(metric) = c("rlt", "rsf", "rf", "ranger")
colnames(metric) = c("fit.time", "pred.time", "pred.error", "obj.size")


start_time <- Sys.time()
RLTfit <- RLT(trainX, trainY, ntrees = ntrees, ncores = ncores, nmin = nmin/2, mtry = mtry,
              split.gen = rule, nsplit = nsplit, resample.prob = sampleprob, importance = importance)
metric[1, 1] = difftime(Sys.time(), start_time, units = "secs")
start_time <- Sys.time()
RLTPred <- predict(RLTfit, testX, ncores = ncores)
metric[1, 2] = difftime(Sys.time(), start_time, units = "secs")
metric[1, 3] = mean((as.numeric(RLTPred$Prediction >= 0.5) == testY))
metric[1, 4] = object.size(RLTfit)

options(rf.cores = ncores)
start_time <- Sys.time()
rsffit <- rfsrc(y ~ ., data = data.frame(trainX, "y"= trainY), ntree = ntrees, nodesize = nmin, mtry = mtry, 
                nsplit = nsplit, sampsize = trainn*sampleprob, importance = importance)
metric[2, 1] = difftime(Sys.time(), start_time, units = "secs")
start_time <- Sys.time()
rsfpred = predict(rsffit, data.frame(testX))
metric[2, 2] = difftime(Sys.time(), start_time, units = "secs")
metric[2, 3] = mean((as.numeric(rsfpred$predicted[,1] >= 0.5) == testY))
metric[2, 4] = object.size(rsffit)





par(mfrow=c(2,2))
par(mar = c(0.5, 0.5, 2, 2))

barplot(as.vector(RLTfit$VarImp), main = "RLT")
barplot(as.vector(rsffit$importance), main = "rsf")
barplot(rf.fit$importance[, 1], main = "rf")
barplot(as.vector(rangerfit$variable.importance), main = "ranger")


# predict on a subset of trees 

RLTPred_sub <- predict(RLTfit, testX, treeindex = c(1:10), keep.all = TRUE, ncores = ncores)
