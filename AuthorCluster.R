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

#need to make this plot look better using ggplot
plot(pts, type='n')
text(pts[,1],pts[,2],label=M$authornames,cex=0.8)



  