library(ggalluvial)

#This produces a sanky diagram for one type of model
Sanky <- function(truth, pred, title){
  df <- get_sanky_df(truth, pred) #create the dataframe
  
  #plot
  ggplot(df,
         aes(y = Frequency, axis1 = Truth, axis2 = Predictions)) +
    geom_alluvium(aes(fill = Correct))+
    geom_stratum() +
    ggtitle(title,
            "Stratified by Authors") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    scale_x_continuous(breaks = 1:2, labels = c("Truth", "Prediction")) +
    xlab('Classification') +
    ylab('Frequency')
}

#converts truth values and predictions to a form
#which we can make a sanky diagram with
get_sanky_df <- function(truth, pred){
  #get data
  M <- loadCorpus("/cloud/project/FunctionWords/", "frequentwords70")
  
  #extract features and author names
  features <- M$features
  authors <- M$authornames
  
  #convert to factor variables
  truth <- factor(truth, levels = 1:12)
  pred <- factor(pred, levels=1:12)
  
  #make a confusion matrix
  tab <- table(pred, truth)
  
  #convert to a matrix
  X <- as.matrix(tab)
  
  
  Truths <- NULL
  Preds <- NULL
  Freq <- NULL
  Correct <- NULL
  
  #loop over each element in the confusion matrix
  for (i in 1:nrow(X)){
    for (j in 1:ncol(X)){
      
      #if their has been a prediction between these authors
      if (X[i, j] != 0) {
        
        #if its a correct prediction
        if (i == j){
          Correct <- c(Correct, 'Correct')
        }
        #an incorrect prediction
        else{
          Correct <- c(Correct, 'Incorrect')
        }
        
        Truths <- c(Truths, authors[j]) #adds the truth value
        Preds <- c(Preds, authors[i]) #adds the prediction value
        Freq <- c(Freq, X[i,j]) #adds the occurences of it
      }
    }
  }
  
  #creates dataframe
  df <- data.frame(Truth = Truths,
                   Predictions = Preds,
                   Frequency = Freq,
                   Correct = Correct)
  
  #converts to factor variables
  df$Truth <- as.factor(df$Truth)
  df$Predictions <- as.factor(df$Predictions)
  df$Correct <- as.factor(df$Correct)
  
  return(df)
}

#This function is very specific to our task
#not written to be scalable
#It take a truth vector and 4 prediction vectors
#returns the connections between incorrect predicitons
SankyIncorrect <- function(truth, p1, p2, p3, p4){
  
  #Starts by building the dataframe
  
  df2 <- get_sanky_df(truth, p1)
  df2$Model <- rep('KNN', nrow(df2))
  
  df <- df2[df2$Correct == 'Incorrect', ]
  
  df2 <- get_sanky_df(truth, p2)
  df2$Model <- rep('SVM', nrow(df2))
  
  df <- rbind(df, df2[df2$Correct == 'Incorrect', ])
  
  df2 <- get_sanky_df(truth, p3)
  df2$Model <- rep('RandomForest', nrow(df2))
  
  df <- rbind(df, df2[df2$Correct == 'Incorrect', ])
  
  df2 <- get_sanky_df(truth, p4)
  df2$Model <- rep('Discriminant', nrow(df2))
  
  df <- rbind(df, df2[df2$Correct == 'Incorrect', ])
  
  
  #converts the model type to a factor
  df$Model <- as.factor(df$Model)
  
  #removes the correct column
  df <- df[,c(1,2,3,5)]
  
  #plots our results
  ggplot(df,
         aes(y = Frequency, axis1 = Truth, axis2 = Predictions)) +
    geom_alluvium(aes(fill = Model))+
    geom_stratum() +
    ggtitle("Incorrect Classifications of Different Algorithms",
            "Stratified by Authors") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    scale_x_continuous(breaks = 1:2, labels = c("Truth", "Prediction")) +
    xlab('Classification') +
    ylab('Frequency')
  
}
