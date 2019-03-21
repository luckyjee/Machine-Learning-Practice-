##################################
## Coursera Johns Hopkins Univ. Data Science Course
# Training Options
##################################
library(caret)
library(kernlab)
data(spam)

names(spam)
table(spam$type)
?createDataPartition

# Data splitting
inTrain<-createDataPartition(y=spam$type, p=0.75, list=F) 
training<-spam[inTrain,]
testing<-spam[-inTrain,]

# Model Fitting
modelFit<-train(type~., data=training, method="glm")

args(train.default)
# Metric options
# Continuous outcomes:
        # RMSE = Root mean squared error
        # RSquared
# Categorical outcomes:
        # Accuracy =Fraction correct
        # Kappa = A measure of concordance

args(trainControl)

# method
        # boot = bootstrapping
        # boot632 = bootstrapping with adjustment
        # cv = cross validation
        # repeatedcv = repeated cross validation
        # LOOCV = leave one out cross validation
# number
        # For boot/cross validation
        # Number of subsamples to take
# repeats
        # Number of times to repeat subsampling
        # If big this can slow things down
