#Install packages 
install.packages("corrplot")
library(corrplot)

fulldts_noNA<- replace(fulldts, is.na(fulldts), 0) # set NA to 0 
M = cor(fulldts_noNA, use= "everything")
testRes = cor.mtest(fulldts_noNA, conf.level = 0.95)

png("/Users/temp/Documents/FF/corplot.png", width = 800, height=800)
corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.9, order = 'AOE', diag=FALSE)
dev.off()
