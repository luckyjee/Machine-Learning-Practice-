##################################
## Coursera Johns Hopkins Univ. Data Science Course
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
inTrain<-createDataPartition(y=spam$type, p=0.75, list=F)
training<-spam[inTrain,]
testing<-spam[-inTrain,]
dim(training)

# Fit a model
set.seed(32343)
modelFit<-train(type~., data=training, method="glm")
modelFit

# Final Model
modelFit$finalModel

# Prediction
predictions <-predict(modelFit, newdata=testing)
predictions

# Confusion Matrix
confusionMatrix(predictions, testing$type)
