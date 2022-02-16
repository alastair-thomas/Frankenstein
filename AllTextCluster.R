#Here we apply MDS Scaling to each individual text
#Everything is plotted nicely


library(ggplot2)

M <- loadCorpus("/cloud/project/FunctionWords/", "frequentwords70")

f <- M$features
freq <- c(f[[1]])
X <- matrix(freq, nrow=(length(freq)/70))
authors <- rep(M$authornames[1], length(freq)/70)

for (i in 2:length(f)) {
  freq <- c(f[[i]])
  
  X <- rbind(X, matrix(freq, nrow=(length(freq)/70)))
  authors <- c(authors, rep(M$authornames[i], length(freq)/70))
}

for (i in 1:nrow(X)) {
  X[i,] <- X[i,] / sum(X[i,])
}
for (j in 1:ncol(x)) {
  X[,j] <- (X[,j] - mean(X[,j]))/sd(X[,j])
}

d <- dist(X)
pts <- cmdscale(d)

df <- as.data.frame(cbind(pts, matrix(authors)))
colnames(df) <- c('x', 'y', 'Author')
df[,1:2] <- sapply(df[, 1:2], as.numeric)

ggplot(data=df, aes(x, y, colour = Author)) + 
  geom_point() +
  ggtitle('MDS Scaling Plot for all the Texts')
