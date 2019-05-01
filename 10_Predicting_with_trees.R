# Predicting with Trees

data(iris); library(ggplot2)
names(iris)
table(iris$Species)

library(caret)
inTrain<-createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)
qplot(Petal.Width, Sepal.Width, color=Species, data=training)

modFit <-train(Species~., method="rpart", data=training)
?train
print(modFit$finalModel)

#Plot tree
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
# Prettier plots
install.packages("rattle")
library(rattle)
fancyRpartPlot(modFit$finalModel)
    
predict(modFit, newdata=testing)
