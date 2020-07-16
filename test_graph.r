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
metric[1, 3] = mean((RLTPred$Prediction - testY)^2)
metric[1, 4] = object.size(RLTfit)

options(rf.cores = ncores)
start_time <- Sys.time()
rsffit <- rfsrc(y ~ ., data = data.frame(trainX, "y"= trainY), ntree = ntrees, nodesize = nmin, mtry = mtry, 
                nsplit = nsplit, sampsize = trainn*sampleprob, importance = importance)
metric[2, 1] = difftime(Sys.time(), start_time, units = "secs")
start_time <- Sys.time()
rsfpred = predict(rsffit, data.frame(testX))
metric[2, 2] = difftime(Sys.time(), start_time, units = "secs")
metric[2, 3] = mean((rsfpred$predicted - testY)^2)
metric[2, 4] = object.size(rsffit)

start_time <- Sys.time()
rf.fit <- randomForest(trainX, trainY, ntree = ntrees, mtry = mtry, nodesize = nmin, sampsize = trainn*sampleprob, importance = importance)
metric[3, 1] = difftime(Sys.time(), start_time, units = "secs")
start_time <- Sys.time()
rf.pred <- predict(rf.fit, testX)
metric[3, 2] = difftime(Sys.time(), start_time, units = "secs")
metric[3, 3] = mean((rf.pred - testY)^2)
metric[3, 4] = object.size(rf.fit)

start_time <- Sys.time()
rangerfit <- ranger(trainY ~ ., data = data.frame(trainX), num.trees = ntrees, 
                    min.node.size = nmin, mtry = mtry, num.threads = ncores, 
                    sample.fraction = sampleprob, importance = "permutation")
metric[4, 1] = difftime(Sys.time(), start_time, units = "secs")
rangerpred = predict(rangerfit, data.frame(testX))
metric[4, 2] = difftime(Sys.time(), start_time, units = "secs")
metric[4, 3] = mean((rangerpred$predictions - testY)^2)
metric[4, 4] = object.size(rangerfit)



par(mfrow=c(2,2))
par(mar = c(0.5, 0.5, 2, 2))

barplot(as.vector(RLTfit$VarImp), main = "RLT")
barplot(as.vector(rsffit$importance), main = "rsf")
barplot(rf.fit$importance[, 1], main = "rf")
barplot(as.vector(rangerfit$variable.importance), main = "ranger")


# predict on a subset of trees 

RLTPred_sub <- predict(RLTfit, testX, treeindex = c(1:10), keep.all = TRUE, ncores = ncores)
