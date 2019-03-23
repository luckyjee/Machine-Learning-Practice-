##################################
## Coursera Johns Hopkins Univ. Data Science Course
# Covariate Creation
        # Level 1: From raw data to covariate
                # examples: frequency of words; frequency of phrases
                # edges, ridges, number and type of images, A/B Testing
        # Level 2: Transforming tidy covariates
                # More necessary for some methods (regression, svms) than for others (classification trees)
                # Should be done only on the training set
                # The best approach is through exploratory analysis (plotting/tables)
                # New covariates should be added to data frames
##################################
library(caret); library(kernlab); data(spam)
spam$capitalAveSq<-spam$capitalAve^2

# Load example data
library(ISLR); library(caret); data(Wage);
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

# Common covariates to add, dummy variables: convert factor variables to indicator variables
# dummyVars from caret package
table(training$jobclass)
dummies<-dummyVars(wage~jobclass, data=training)
head(predict(dummies, newdata=training))

# Removing zero covariates (no variability in the covariate)
# nearZeroVar in the caret package
nsv <- nearZeroVar(training, saveMetrics=T)
nsv # nzv (near zero variability) if TRUE, not very useful covariate

# Spline basis
library(splines)
bsBasis <- bs(training$age, df=3)
bsBasis
# Fitting curves with splines
lm1<-lm(wage~bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col="red", pch=19, cex=0.5)
# Splines on the test set
predict(bsBasis, age=testing$age)
