library(class)
library(caret)
library(gridExtra)

M <- loadCorpus("/cloud/project/FunctionWords/", "frequentwords70")

DISCpredictions <- NULL
KNNpredictions <- NULL
RFpredictions <- NULL
SVMpredictions <- NULL

truth <- NULL

features <- M$features

for (i in 1:length(features)) {
  for (j in 1:nrow(features[[i]])) {
    
    if (i != 3 & i != 9 & i != 12) {
      testdata <- matrix(features[[i]][j,],nrow=1)
      traindata <- features
      traindata[[i]] <- traindata[[i]][-j,,drop=FALSE]
      
      
      pred <- discriminantCorpus(traindata, testdata)
      DISCpredictions <- c(DISCpredictions, pred)
      
      pred <- KNNCorpus(traindata, testdata)
      KNNpredictions <- c(KNNpredictions, pred)
      
      pred <- randomForestCorpus(traindata, testdata)
      RFpredictions <- c(RFpredictions, pred)
      
      pred <- SVMCorpus(traindata, testdata, 10, 0.01)
      SVMpredictions <- c(SVMpredictions, pred)
      
      truth <- c(truth, i)
    }
    
  }
}

DISC_acc <- sum(DISCpredictions==truth)/length(truth) * 100
KNN_acc <- sum(KNNpredictions==truth)/length(truth) * 100
RF_acc <- sum(RFpredictions==truth)/length(truth) * 100
SVM_acc <- sum(SVMpredictions==truth)/length(truth) * 100

df <- data.frame(DISC_acc, KNN_acc, RF_acc, SVM_acc)
colnames(df) <- c('Discriminant Analysis',
                  'K-Nearest Neighbour',
                  'Random Forest',
                  'SVM')
rownames(df) <- c('Accuracy %')

grid.table(df)
#confusionMatrix(KNNpredictions,truth)