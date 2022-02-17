# We load in the data of the books.

M <- loadCorpus("./FunctionWords/", "frequentwords70")

# We can view all of the author names

M$authornames

# This is the word counts for the books written by the first author
# we have three rows, and the columns are the count words.

M$features[[1]]


set.seed(2)
traindata <- M$features
testdata <- NULL
testlabels <- NULL

for (i in 1:length(traindata)) {
  
  testind <- sample(1:nrow(traindata[[i]]), 1)
  testdata <- rbind(testdata, traindata[[i]][testind,])
  testlabels <- c(testlabels,i)
  traindata[[i]] <- traindata[[i]][-testind,,drop = FALSE]
}


preds <- discriminantCorpus(traindata, testdata)
sum(preds == testlabels)/length(testlabels)

# We set seed above since test/train split is random, 
# but we see that there is 67% accuracy.



# We will try cross validation with the discriminant corpus method

set.seed(2)
predictions <- NULL
truth  <- NULL
features <- M$features

for (i in 1:length(features)){
  
  for (j in 1:nrow(features[[i]])) {
    
    testdata <- matrix(features[[i]][j,], nrow = 1)
    traindata <- features
    traindata[[i]] <- traindata[[i]][-j,,drop = FALSE]
    
    pred <- discriminantCorpus(traindata, testdata)
    predictions <- c(predictions, pred)
    
    truth <- c(truth, i)
  }
}


# we can see that it is 79% accurate..

sum(predictions==truth)/length(truth)

confusionMatrix(as.factor(predictions), as.factor(truth))


