# Discriminant and KNN


# we create the training set by removing the unknown texts

M <- loadCorpus("./FunctionWords/", "frequentwords70")

M$authornames

# the unknown text is the 9th element of the list
# so we create the training set by removing
# the unknown text (element 9) and put this
# into the test set.

train <- M$features[-9]
test <- M$features[9]
test <- test[[1]]

discriminantCorpus(train,test)
KNNCorpus(train, test)

# This gives us two sets of results
# the discriminant method says that author 10 wrote the unknown text
# this is watler scott.
# on the other hand, the KNN method suggests that it was the fourth author
# who wrote the unknown book - this is Mary Shelley.

# We can now check to see how accurate these predictions 
# were using leave one out cross validation
# cross validation doesn't work for KNN
# but it does work for discriminant method.


predictions <- NULL
truth <- NULL
features <- M$features[-9] #discard unknown texts
for (i in 1:length(features)) {
  for (j in 1:nrow(features[[i]])) {
    testdata <- matrix(features[[i]][j,],nrow=1)
    traindata <- features
    traindata[[i]] <- traindata[[i]][-j,,drop=FALSE]
    pred <- discriminantCorpus(traindata, testdata)
    
    predictions <- c(predictions, pred)
    truth <- c(truth, i)
  
  }
}

sum(predictions==truth)/length(truth)

# We have that it is 82% correct.

# We now try to get KNN working by removing the author with only one book.

train_na <- M$features[-c(3,9,12)]
test_na <- M$features[9]
test_na  <- test_na[[1]]

KNNCorpus(train_na, test_na)

predictions <- NULL
truth <- NULL
features <- train_na #discard unknown texts
for (i in 1:length(features)) {
  for (j in 1:nrow(features[[i]])) {
    testdata <- matrix(features[[i]][j,],nrow=1)
    traindata <- features
    traindata[[i]] <- traindata[[i]][-j,,drop=FALSE]
    pred <- KNNCorpus(traindata, testdata)
    
    KNNpredictions <- c(KNNpredictions, pred)
    truth <- c(truth, i)
    
  }
}

sum(KNNpredictions==truth)/length(truth)




