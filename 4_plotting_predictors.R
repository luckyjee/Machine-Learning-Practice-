##################################
## Coursera Johns Hopkins Univ. Data Science Course
# Plotting predictors
        # Make your plots only in the training set! (not in the test set for exploration!)
        # Imbalance in outcomes/ predictors
        # outliers?
        # groups of points not explained by a predictor
        # skewed variables
##################################
library(ISLR)
library(ggplot2)
library(caret)
library(Hmisc)
data(Wage)
summary(Wage)

# Data Splitting
inTrain<-createDataPartition(y=Wage$wage, p=0.7, list=F)
training <-Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training);dim(testing)

# Feature plot (caret package)
featurePlot(x=training[,c("age","education","jobclass")],
            y=training$wage,
            plot="pairs")
#qplot (ggplot2 package)
qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass, data=training)

qplot(age,wage, color=education, data=training)+
        geom_smooth(method="lm", formula=y~x)

#cut2, making factors (Hmisc package)
cutWage<-cut2(training$wage, g=3)
table(cutWage)

#Boxplots with cut2
p1<-qplot(cutWage, age, data=training, fill=cutWage,
      geom=c("boxplot"))

p2<-qplot(cutWage, age, data=training, fill=cutWage,
      geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)

# table
t1<-table(cutWage,training$jobclass)
t1
prop.table(t1,1)
prop.table(t1,2)

# Density plots
qplot(wage,color=education, data=training, geom="density")
