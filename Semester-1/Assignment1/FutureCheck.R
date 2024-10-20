source("~/Desktop/Stat Case study/Semester1/Assignemtn1/CaseStudy Assignment1/stylometryfunctions.R")
M <- loadCorpus("~/Desktop/Stat Case study/Semester1/Assignemtn1/CaseStudy Assignment1/FunctionWords/","frequentwords70")


predictions <- NULL
KNNpredictions <- NULL 
RFpredictions <- NULL

truth <- NULL 
features <- M$features[cbind(-3,-9,-12)] #discard unknown texts 

for (i in 1:length(features)) { 
  for (j in 1:nrow(features[[i]])) {
  testdata <- matrix(features[[i]][j,],nrow=1)
  traindata <- features
  traindata[[i]] <- traindata[[i]][-j,,drop=FALSE]
  
  pred <- discriminantCorpus(traindata, testdata) 
  predictions <- c(predictions, pred)
  
  pred1 <- KNNCorpus(traindata, testdata) 
  KNNpredictions <- c(KNNpredictions, pred1)
  
  pred2 <- randomForestCorpus(traindata,testdata)
  RFpredictions <- c(RFpredictions, pred2)
  
  truth <- c(truth, i)}
}

sum(predictions==truth)/length(truth)
sum(KNNpredictions==truth)/length(truth)
sum(RFpredictions==truth)/length(truth)
confusionMatrix(factor(KNNpredictions),factor(truth))
confusionMatrix(factor(predictions),factor(truth))
confusionMatrix(factor(RFpredictions),factor(truth))