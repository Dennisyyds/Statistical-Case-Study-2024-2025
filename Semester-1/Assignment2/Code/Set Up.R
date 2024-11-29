#Load required package
library(caret)
library(writexl)
library(class)
library(randomForest)
library(e1071)
#Set Up
source("~/Desktop/Stat Case study/Semester1/Assignment2/Assignment2 Code final version/stylometryfunctions.R")
source("~/Desktop/Stat Case study/Semester1/Assignment2/Assignment2 Code final version/reducewords.R")
GPTCorpus <- loadCorpus("~/Desktop/Stat Case study/Semester1/Assignment2/functionwords/functionwords/GPTfunctionwords/","functionwords")
HumanCorpus <- loadCorpus("~/Desktop/Stat Case study/Semester1/Assignment2/functionwords/functionwords/humanfunctionwords/","functionwords")
