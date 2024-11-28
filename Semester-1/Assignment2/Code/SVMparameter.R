set.seed (123)

all_labels <- NULL
#form the matrix of the corpus
Human_combined <- do.call(rbind, HumanCorpus$features)
GPT_combined <- do.call(rbind, GPTCorpus$features)
#combine
traindata <-rbind(Human_combined, GPT_combined)
#create vector to record the labels
y <- c(rep(1, nrow(Human_combined)), rep(2, nrow(GPT_combined)))

num_authors <- nrow(traindata)
# Normalize the training data
for (i in 1:num_authors) {
  words <- traindata[i, ]
  traindata[i, ] <- words / sum(words)
}

dat=data.frame(x=traindata, y=as.factor(y))
# Assign labels to training data
trainlabels <- factor(all_labels)
tune.out=tune(svm,y~., data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100))) 
summary(tune.out) 
bestmod=tune.out$best.model