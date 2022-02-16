#This script combines the texts from each author
#Then does MDS scaling and plots the results
#Need to make the plotting nicer

M <- loadCorpus("/cloud/project/FunctionWords/", "frequentwords70")

f <- M$features
freq <- c(f[[1]])
X <- matrix(colSums(matrix(freq, nrow=(length(freq)/70))),
                      nrow=1, ncol=70)

for (i in 2:length(f)) {
  freq <- c(f[[i]])
  
  X <- rbind(X, colSums(matrix(freq, nrow=(length(freq)/70))))
  
}

for (i in 1:nrow(X)) {
  X[i,] <- X[i,] / sum(X[i,])
}
for (j in 1:ncol(x)) {
  X[,j] <- (X[,j] - mean(X[,j]))/sd(X[,j])
}

d <- dist(X)
pts <- cmdscale(d)

df <- data.frame(cbind(pts, M$authornames))
#need to make this plot look better using ggplot
colnames(df) <- c('x', 'y', 'Author')
df[,1:2] <- sapply(df[, 1:2], as.numeric)

ggplot(data=df, aes(x, y, colour = Author, label = Author)) + 
  geom_point() +
  ggtitle('MDS Scaling Plot for each Author') +
  geom_text(hjust=0.5, vjust=-0.5) +
  theme(legend.position="none") +
  xlim(-10, 10) +
  ylim(-6, 8)


  