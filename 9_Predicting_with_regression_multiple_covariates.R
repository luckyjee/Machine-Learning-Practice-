##################################
## Coursera Johns Hopkins Univ. Data Science Course
# Predicting with Regression Multiple Covariates
##################################
library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage<-subset(Wage, select=-c(logwage))
summary(Wage)
# Get training/ test sets
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=F)
training<-Wage[inTrain,]; testing<-Wage[-inTrain,]
dim(training); dim(testing)

# feature plot
featurePlot(x=training[,c("age","education","jobclass")],
            y=training$wage,
            plot="pairs")
qplot(age,wage,data=training, color=education)

# fit a linear model
modFit <- train(wage~age+jobclass+education,,
                method="lm", data=training)
finMod<-modFit$finalModel
print(modFit)
finMod

# Diagnostics
plot(finMod, 1, pch=19, cex=0.5, col="#00000010")

# Color by variables not used in the model
qplot(finMod$fitted, finMod$residuals, color=race, data=training)

# Plot by index
plot(finMod$residuals, pch=19)

# predicted vs. truth in test set
pred <- predict(modFit, testing)
qplot(wage, pred, color=year, data=testing)

# If you want to use all covariates
modFitAll <- train(wage~., data=training, method="lm")
pred<-predict(modFitAll, testing)
qplot(wage, pred, data=testing)
