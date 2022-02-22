library(class)
library(caret)
library(gridExtra)

#load the data
M <- loadCorpus("/cloud/project/FunctionWords/", "frequentwords70")

#extract features
features <- M$features


n <- 0 #number of texts
authors <- NULL #list of authors
texts <- NULL #number of text for each author

for (i in 1:length(features)) { #loops over each author
  for (j in 1:nrow(features[[i]])) { #and each of their texts
    n <- n + 1
    authors <- c(authors, i)
    texts <- c(texts, j)
  }
}

DISCpredictions <- NULL
KNNpredictions <- NULL
RFpredictions <- NULL
SVMpredictions <- NULL
truth <- NULL

#Create 6 equally size folds
folds <- cut(seq(1,n),breaks=6,labels=FALSE)

#Does a 6-fold cross validation
for (f in 1:6){
  
  #Segment data by fold using the which() function 
  testIndexes <- which(folds==f,arr.ind=TRUE)
  
  #start with training as everything and remove what we want to test
  traindata <- features
  testdata <- NULL
  
  #loop over each test index
  for (t in testIndexes){
    
    #we can't predict a class with only 1 text
    #because the model will know nothing about it
    #so only add to testset if more than 1 text left
    
    if (t != 3 & t != 9 & t != 12 & length(traindata[[authors[t]]])/70 > 1){
      #build test set
      testdata <- rbind(testdata, matrix(features[[authors[t]]][texts[t],],nrow=1))
      
      #remove this text from training set
      traindata[[authors[t]]] <- traindata[[authors[t]]][-texts[t],,drop=FALSE]
      
      #add truth values
      truth <- c(truth, authors[t])
    }
  }
  
  #Discriminant model
  preds <- discriminantCorpus(traindata, testdata)
  DISCpredictions <- c(DISCpredictions, preds)
  
  #KNN model
  preds <- KNNCorpus(traindata, testdata)
  KNNpredictions <- c(KNNpredictions, preds)
  
  #Random forest model
  preds <- randomForestCorpus(traindata, testdata)
  RFpredictions <- c(RFpredictions, preds)
  
  #SVM model
  preds <- SVMCorpus(traindata, testdata, 10, 0.01)
  SVMpredictions <- c(SVMpredictions, preds)
  
}

#calculates accuracies
DISC_acc <- sum(DISCpredictions==truth)/length(truth) * 100
KNN_acc <- sum(KNNpredictions==truth)/length(truth) * 100
RF_acc <- sum(RFpredictions==truth)/length(truth) * 100
SVM_acc <- sum(SVMpredictions==truth)/length(truth) * 100

#builds a dataframe with results from each model
results_df <- data.frame(DISC_acc, KNN_acc, RF_acc, SVM_acc)
colnames(results_df) <- c('Discriminant Analysis',
                  'K-Nearest Neighbour',
                  'Random Forest',
                  'SVM')

#converts predictions and truths to factor variables
truth <- factor(truth, levels = 1:12)

KNNpredictions <- factor(KNNpredictions, levels = 1:12)
SVMpredictions <- factor(SVMpredictions, levels = 1:12)
RFpredictions <- factor(RFpredictions, levels = 1:12)
DISCpredictions <- factor(DISCpredictions, levels = 1:12)

#calls confusion matrix from caret function
cmKNN <- confusionMatrix(KNNpredictions,truth)
cmSVM <- confusionMatrix(SVMpredictions,truth)
cmRF <- confusionMatrix(RFpredictions,truth)
cmDISC <- confusionMatrix(DISCpredictions,truth)

#This adds a new row of Kappa coefficients to the results df
new_row <- data.frame(cmDISC$overall['Kappa']*100,
                      cmKNN$overall['Kappa']*100,
                      cmRF$overall['Kappa']*100,
                      cmSVM$overall['Kappa']*100)

colnames(new_row) <- c('Discriminant Analysis',
                       'K-Nearest Neighbour',
                       'Random Forest',
                       'SVM')

results_df <- rbind(results_df, new_row)
rownames(results_df) <- c('Accuracy %', 'Kappa Coefficient %')


#produces sanky diagram to show which texts are classified correctly and which aren't
Sanky(truth, KNNpredictions, 'Classification of Texts by the KNN Algorithm')
Sanky(truth, SVMpredictions, 'Classification of Texts by the SVM Algorithm')
Sanky(truth, RFpredictions, 'Classification of Texts by the Random Forest Algorithm')
Sanky(truth, DISCpredictions, 'Classification of Texts via the Discriminant Method')

#creates sanky diagram to see which authors are being
#misclassified by which models
SankyIncorrect(truth, KNNpredictions, SVMpredictions, RFpredictions, DISCpredictions)

#This section is about predicting the unknown text

#test is the unknown text
testdata <- matrix(features[[9]][1,],nrow=1)
#train is everything else
traindata <- features[-9]

#predict the author of the unknown text for each model
DISCpred <- discriminantCorpus(traindata, testdata)
KNNpred <- KNNCorpus(traindata, testdata, 1)
RFpred <- randomForestCorpus(traindata, testdata)
SVMpred <- SVMCorpus(traindata, testdata, 10, 0.01)


#This adds a new row of predicted authors to the results df
new_row <- data.frame(M$authornames[DISCpred],
                      M$authornames[KNNpred],
                      M$authornames[RFpred],
                      M$authornames[SVMpred])

colnames(new_row) <- c('Discriminant Analysis',
                       'K-Nearest Neighbour',
                       'Random Forest',
                       'SVM')

results_df <- rbind(results_df, new_row)
rownames(results_df) <- c('Accuracy %', 'Kappa Coefficient %', 'Frankenstein Prediction')
View(results_df)

#prints out the confusion matricies and all that data
print(cmKNN)
print(cmDISC)
print(cmRF)
print(cmSVM)

