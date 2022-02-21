library(class)
library(caret)
library(gridExtra)

#load the corpus
M <- loadCorpus("/cloud/project/FunctionWords/", "frequentwords70")

#extract the features
features <- M$features

X <- NULL #The data
authors <- NULL #the labels

#loops over each feature
for (i in 1:length(features)) {
  
  if (i != 3 && i != 9 && i != 12) { #authors with only one text
    freq <- c(features[[i]]) #gets the data into a vector
    
    X <- rbind(X, matrix(freq, nrow=(length(freq)/70))) #adds as rows to bottom of matrix
    
    authors <- c(authors, rep(i, length(freq)/70 )) #adds the author
  }
  
}

#convert to proportion of text
for (i in 1:nrow(X)) {
  X[i,] <- X[i,] / sum(X[i,])
}

for (j in 1:ncol(X)) {
  X[,j] <- (X[,j] - mean(X[,j]))/sd(X[,j])
}


X <- cbind(authors, X)
accs <- NULL

for (x in 1:100){
  #Randomly shuffle the data
  X <- X[sample(nrow(X)),]
  
  
  #Create 6 equally size folds
  folds <- cut(seq(1,nrow(X)),breaks=6,labels=FALSE)
  
  
  for (k in 1:25){
    truth <- NULL
    KNNpredictions <- NULL
    KNN_accs <- NULL
    
    for (f in 1:6){
      
      #Segment your data by fold using the which() function 
      testIndexes <- which(folds==f,arr.ind=TRUE)
      
      testData <- X[testIndexes,]
      trainData <- X[-testIndexes,]
      
      trainLabels <- trainData[,1]
      testLabels <- testData[,1]
      
      trainData <- trainData[,-1]
      testData <- testData[,-1]
      
      predsKNN <- KNN2(trainData, testData, trainLabels, k)
      
      KNN_accs <- c(KNN_accs, sum(predsKNN==testLabels)/length(testLabels) * 100)
      
    }
    
    if (x == 1){
      accs <- c(accs, mean(KNN_accs))
    }
    else {
      accs[k] <- accs[k] + mean(KNN_accs)
    }
    
  }
  
}

accs <- accs / 100

df <- data.frame(c(1:25), accs)
colnames(df) <- c('K', 'Accuracy')


ggplot(data=df, aes(K, Accuracy, colour = Accuracy, label = K)) + 
  geom_point() +
  ggtitle('Accuracy of the KNN Algorithm for Different K Values') +
  geom_text(hjust=0.5, vjust=-0.5) +
  theme(legend.position="none") +
  ylim(0, 100) +
  ylab('Accuracy %')

