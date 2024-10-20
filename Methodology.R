source("~/Desktop/Stat Case study/Semester1/Assignemtn1/CaseStudy Assignment1/stylometryfunctions.R")
M <- loadCorpus("~/Desktop/Stat Case study/Semester1/Assignemtn1/CaseStudy Assignment1/FunctionWords/","frequentwords70")
#Discriminant and KNN and randomForest Using the whole data set
traindata <- M$features[-9]
testdata <- M$features[9]
testdata <- testdata[[1]] #needed due to how the list is structured
discriminantCorpus(traindata,testdata)
KNNCorpus(traindata,testdata)
randomForestCorpus(traindata,testdata)
#Discriminant and KNN and randomForest only Using two data
traindata <- M$features[cbind(4,6)]
testdata <- M$features[9]
testdata <- testdata[[1]] #needed due to how the list is structured
discriminantCorpus(traindata,testdata)
KNNCorpus(traindata,testdata)
randomForestCorpus(traindata,testdata)