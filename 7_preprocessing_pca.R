##################################
## Coursera Johns Hopkins Univ. Data Science Course
# Preprocessing with principal components analysis
##################################
library(caret); library(kernlab); data(spam)

inTrain <- createDataPartition(y=spam$type, p=0.75, list=F)

training <-spam[inTrain,]
testing <-spam[-inTrain,]

M <-abs(cor(training[,-58]))
diag(M)<-0 # correation with itself
which(M>0.8, arr.ind=T) #which variables have high (>0.8) correlation?

names(spam)[c(34,32)]
plot(spam[,34],spam[,32])
# How to take those two highly correlated variables and turn them into a single variable that might be better?

# Basic PCA idea
        # a weighted combination of predictors might be better
        # we should pick this combination to capture the "most information" possible
        # benefits: 1) reduced number of predictors; 2)reduced noise (due to averaging)

x <- 0.71*training$num415 + 0.71*training$num857
y <- 0.71*training$num415 - 0.71*training$num857
plot(x,y) # most of the variation happens around x axis; we might want to use a variable that captures the "sum" of the two variables.

# related problems
        # find a new set of multivariate variables that are uncorrelated and explain as much variance as possible
        # if you put all the variables together in one matrix, find the best matrix created with fewer variables
                # that explains the original data
# The first goal is statistical and the second goal is data compression.

# Related solutions - PCA/SVD
# SVD: matrix decomposition 
# PCA: The principal components are equal to the right singular values if you first scale (subtract the mean,
        # divide by the standard deviation) the variables.

# Principal components in R - prcomp
smallSpam <-spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])

prComp$rotation

typeColor <- ((spam$type=="spam")*1+1)
prComp <- prcomp(log10(spam[,-58]+1)) # log transformation to make variables look more Guassian
plot(prComp$x[,1], prComp$x[,2], col=typeColor, xlab="PC1", ylab="PC2")

# PCA with caret package
preProc <- preProcess(log10(spam[,-58]+1), method="pca", pcaComp=2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1], spamPC[,2], col=typeColor)

# Preprocessing with PCA
preProc <- preProcess(log10(training[,-58]+1), method="pca", pcaComp=2)
trainPC <- predict(preProc, log10(training[,-58]+1))
modelFit <- train(training$type~., method="glm", data=trainPC)

testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit, testPC))

# Alternative (sets # of PCs)
modelFit <- train(training$type~., method="glm", preProcess="pca", data=training)
confusionMatrix(testing$type, predict(modelFit, testing))

# Final thoughts on PCs
        # Most useful for linear-type models
        # Can make it harder to interpret predictors
        # Watch out for outliers!
                # transform first (with logs/ Box Cox)
                # Plot predictors to identify problems
