#Title: "Regularization"
#author: "Hardik Kachhwaha"
#Subject: ALY-6015-Intermediate Analytics

# Adding Libraries
library(psych)
library(dplyr)
library(corrplot)
library(caret)
library(olsrr)
library(performance)
library(Ecdat)
library(leaps)
library(lmtest)
library(visdat)
library(inspectdf)
library(skimr)
library(ggplot2)
library(ggcorrplot)
library(car)
library(nhstplot)
library(ISLR)
library(e1071)
library(Hmisc)
library(tidyverse)
library(caret)
library(pROC)
library(pls)
library(glmnet)
library(cat)
library(mltools)
library(olsrr)
library(MASS)


#1. Split the data into a train and test set – refer to the Feature_Selection_R.pdf document for information on how to split a dataset
options (max.print=100000)   
data("College") 
College
head(College) 
collegeData <- as.data.frame(College)

#Exploratory Data Analysis
dim(collegeData) 
summary(collegeData) 
glimpse(collegeData) 
skim(collegeData) 

#Categorical and Numerical Aggregation in data
vis_dat(collegeData) 

#Check NAs values
z <-inspect_na(collegeData) 
show_plot(z) 

# Split Data into train and test set
set.seed(12)
Sample <- sample(nrow(collegeData), nrow(collegeData)*.7) 
collegeTrainData <- College[Sample, ] 
head(collegeTrainData)
collegeTestData <- College[-Sample, ] 
head(collegeTestData)
dim(collegeTrainData)
dim(collegeTestData)

#****************************************Ridge Regression****************************************

#2. Use the cv.glmnet function to estimate the lambda.min and lambda.1se values. Compare and discuss the values.
# Get names of columns except graduate rate
variableNames <- colnames(collegeData)[colnames(collegeData) != "Grad.Rate"] 
variableNames

#Defining Predictor and Response Varibales
X <- data.matrix(collegeData[, variableNames]) 
X
Y <- collegeData[, "Grad.Rate"] 

#Creating Ridge model
set.seed(345)
ridgeModel <- cv.glmnet(X, Y, alpha = 0) 
Lamda.Min <- ridgeModel$lambda.min 
Lambda.1se <- ridgeModel$lambda.1se 
data.frame(Lamda.Min,Lambda.1se ) 
coef(ridgeModel)

#3. Plot the results from the cv.glmnet function provide an interpretation. What does this plot tell us?
plot(ridgeModel)
abline(v=ridgeModel$lambda.min, col = "blue", lty=2)
abline(v=ridgeModel$lambda.1se, col="grey", lty=2)

#4. Fit a Ridge regression model against the training set and report on the coefficients. Is there anything interesting?
# Get names of columns except graduate rate
variableNames1 <- colnames(collegeTrainData)[colnames(collegeTrainData) != "Grad.Rate"] 
variableNames1

#Defining Predictor and Response Varibales
X1 <- data.matrix(collegeTrainData[, variableNames1]) 
X1
Y1 <- collegeTrainData[, "Grad.Rate"] 

#Creating Ridge model for training data
set.seed(150)
lambda <- 10^seq(2,-2, by = -.1)
ridgeModelTrain <- cv.glmnet(X1, Y1, alpha = 0, lambda = lambda) 
Lamda.Min1 <- ridgeModelTrain$lambda.min 
Lambda.1se1 <- ridgeModelTrain$lambda.1se 
data.frame(Lamda.Min1,Lambda.1se1 ) 
plot(ridgeModelTrain)
coef(ridgeModelTrain)

#5. Determine the performance of the fit model against the training set by calculating the root mean square error (RMSE). sqrt(mean((actual - predicted)^2)) 
trainingDataPrediction <- predict(ridgeModelTrain, s = lambda, newx = X1) 

#Calculating RMSE - Training Data
RMSEtrainingData <- sqrt(mean((Y1 - trainingDataPrediction)^2)) 
RMSEtrainingData

#6. Determine the performance of the fit model against the test set by calculating the root mean square error (RMSE). Is your model overfit?
variableNames2 <- colnames(collegeTestData)[colnames(collegeTestData) != "Grad.Rate"] 
variableNames2

X2 <- data.matrix(collegeTestData[, variableNames1]) 
X2
Y2 <- collegeTestData[, "Grad.Rate"] 

set.seed(250)
lambda <- 10^seq(2,-2, by = -.1)
ridgeModelTest <- cv.glmnet(X2, Y2, alpha = 0, lambda = lambda) 
Lamda.Min2 <- ridgeModelTest$lambda.min 
Lambda.1se2 <- ridgeModelTest$lambda.1se 
data.frame(Lamda.Min2,Lambda.1se2 ) 
plot(ridgeModelTest)
coef(ridgeModelTest)

testDataPrediction <- predict(ridgeModelTest, s = lambda, newx = X2) 

#Calculating RMSE - Test Data
RMSEtestData <- sqrt(mean((Y2 - testDataPrediction)^2)) 
RMSEtestData

#****************************************LASSO****************************************

#7. Using cv.glmnet function to estimate the lambda.min and lambda.1se values. 
set.seed(234)
lassoModel <- cv.glmnet(X,Y, alpha = 1)
LamdaL.min <- lassoModel$lambda.min 
LambdaL.1se <- lassoModel$lambda.1se 
data.frame(LamdaL.min,LambdaL.1se ) 

#Question 8: Plot Results
plot(lassoModel)
abline(v=lassoModel$lambda.min, col = "green", lty=2)
abline(v=lassoModel$lambda.1se, col="blue", lty=2)

#9. Fit a LASSO regression model against the training set and report on the coefficients.
lassoModelTrain = glmnet(X1, Y1, alpha = 1, lambda = LamdaL.min)
coef(lassoModelTrain)

#10. Determine the performance of the fit model against the training set by calculating the root mean square error (RMSE)
trainDataPrediction1 <- predict(lassoModelTrain, s = LamdaL.min, newx = X1) 
RMSE3 <- sqrt(mean((Y1 - trainDataPrediction1)^2)) 
RMSE3

#11.  Determine the performance of the fit model against the test set by calculating the root mean square error (RMSE)
lassoModelTest = glmnet(X2, Y2, alpha = 1, lambda = LamdaL.min)
testDataPrediction1 <- predict(lassoModelTest, s = LamdaL.min, newx = X2) 
RMSE4 <- sqrt(mean((Y2 - testDataPrediction1)^2)) 
RMSE4

#12. Comparing Model Performance

#13. Performing Stepwise Selection and fitting a model
linearModel <- lm(Grad.Rate ~., data = collegeData) 
summary(linearModel)

stepWisdeModel <- stepAIC(linearModel, direction = "both", trace = FALSE) 
summary(stepWisdeModel)

ModelPlot <- ols_step_both_aic(linearModel)
plot(ModelPlot)

RMSE5 <-sqrt(sum(stepWisdeModel$residuals^2) / stepWisdeModel$df.residual)
RMSE5
