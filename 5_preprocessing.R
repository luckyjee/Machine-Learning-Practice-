##################################
## Coursera Johns Hopkins Univ. Data Science Course
# Preprocessing
        # training and test must be processed in the same way
        # test transformations will likely be imperfect
        # This section is for continuous variable (not factor)
##################################
library(caret); library(kernlab); data(spam)
install.packages("RANN")
library(RANN)

inTrain<-createDataPartition(y=spam$type, p=0.75, list=F)
training <- spam[inTrain,]
testing<-spam[-inTrain,]
hist(training$capitalAve, main="", xlab="ave. capital run length") # very skewed

mean(training$capitalAve)
sd(training$capitalAve)
# Why preprocess? Not to let algorithm to get tricked by the large variation in the predictor

### Standardizing
trainCapAve<-training$capitalAve
trainCapAveS<-(trainCapAve-mean(trainCapAve))/sd(trainCapAve) 
mean(trainCapAveS) #0
sd(trainCapAveS) # 1

### Standardizing - Test Set
# Note that whenwe are standardizing a variable, we need to use data from TRAINING SET
# use the mean and sd from the train set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve -mean(trainCapAve)) / sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

### Standardizing - preProcess function in caret
preObj <- preProcess (training[,-58], method=c("center","scale"))
trainCapAveS<-predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)
# apply this to testing set
testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)
# pass preProcess argument directly into model fitting argument
set.seed(32343)
modelFit<- train(type~., data=training, 
                 preProcess=c("center","scale"), method="glm")
modelFit

####Standardizing - Box-Cox transform: take continuous data and try to make them look like normal data using Maximum Likelihood
preObj<-preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS) # not perfect, doesn't work well for repeated data points

### Standardizing - Imputing data
set.seed(13343)
# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom (dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <-NA
# Impute and standardize: knn impurtation
preObj<-preProcess(training[-58,], method="knnImpute")
capAve <- predict(preObj, training[,-58])$capAve
# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth))/sd(capAveTruth)
# See how imputed values are off from the true values
quantile(capAve-capAveTruth)
quantile((capAve-capAveTruth)[selectNA]) #only those that are imputed
quantile((capAve-capAveTruth)[!selectNA])
