#This simply creates a png of the author and their numeric label
#used in our prediction models.

library(ggplot2)
library(gridExtra)

M <- loadCorpus("/cloud/project/FunctionWords/", "frequentwords70")

print(M$authornames)

authors <- data.frame(M$authornames, c(1:length(M$authornames)))
colnames(authors) <- c('Author', 'Numeric Label')

png("test.png", height = 50*nrow(authors), width = 200*ncol(authors))
p<-tableGrob(authors)
grid.arrange(p)
dev.off()