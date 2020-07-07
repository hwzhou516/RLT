library(RLT)
library(randomForest)
library(randomForestSRC)
library(ranger)

set.seed(1)
n = 1500
trainn = 1000
testn = 500

ntrees = 200
ncores = 10
nmin = 20
mtry = p
sampleprob = 0.85
rule = "best"
nsplit = ifelse(rule == "best", 0, 3)
importance = FALSE

trainX = matrix(rnorm(trainn*trainn), trainn, trainn)
trainY = as.vector(sample(c(0,1), trainn,replace = TRUE))

testX = matrix(rnorm(testn*testn), testn, testn)
testY = as.vector(sample(c(0,1), testn,replace = TRUE))

xorder = order(testX[, 1])
testX = testX[xorder, ]
testY = testY[xorder]

metric = data.frame(matrix(NA, 4, 4))
rownames(metric) = c("rlt", "rsf", "rf", "ranger")
colnames(metric) = c("fit.time", "pred.time", "pred.error", "obj.size")


start_time <- Sys.time()
#RLTfit <- 
  RLT(trainX, trainY, ntrees = ntrees, ncores = ncores, nmin = nmin/2, mtry = mtry,
                         split.gen = rule, nsplit = nsplit, resample.prob = sampleprob, importance = importance)
metric = difftime(Sys.time(), start_time, units = "secs")
start_time <- Sys.time()

metric

par(mfrow=c(2,2))
par(mar = c(0.5, 0.5, 2, 2))

barplot(as.vector(RLTfit$VarImp), main = "RLT")
barplot(as.vector(rsffit$importance), main = "rsf")
barplot(rf.fit$importance[, 1], main = "rf")
barplot(as.vector(rangerfit$variable.importance), main = "ranger")


# predict on a subset of trees 

RLTPred_sub <- predict(RLTfit, testX, treeindex = c(1:10), keep.all = TRUE, ncores = ncores)
