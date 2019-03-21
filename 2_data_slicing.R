##################################
## Coursera Johns Hopkins Univ. Data Science Course
# Data Splitting
##################################
install.packages("caret")
install.packages("kernlab")
install.packages("e1071")
library(caret)
library(kernlab)
data(spam)

names(spam)
table(spam$type)
?createDataPartition

# Data splitting
inTrain<-createDataPartition(y=spam$type, p=0.75, list=F) # 75% Assigned to training set
training<-spam[inTrain,]
testing<-spam[-inTrain,]
dim(training)

# K-fold when returnTrain=T
set.seed(32323)
folds<-createFolds(y=spam$type, k=10, list=T, returnTrain=T)

sapply(folds,length)

folds[[1]][1:10]

# K-fold when returnTrain=F (Return Test)
set.seed(32323)
folds<-createFolds(y=spam$type, k=10, list=T, returnTrain=F)

sapply(folds,length)
folds[[1]][1:10]

# Resampling
set.seed(32323)
folds<-createResample(y=spam$type, times=10, list=T)

sapply(folds,length)
folds[[1]][1:10]


# Time Slices
set.seed(32323)
tme<-1:1000
?createTimeSlices
folds<-createTimeSlices(y=tme, initialWindow=20, horizon=10)
names(folds)

folds$train[[1]] # 20 data points
folds$test[[1]] # 10 
