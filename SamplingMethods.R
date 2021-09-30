#Title: "Nonparametric Statistical Methods and Sampling"
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
library(rstatix)
library(BSDA)


####################################################### Section:13-2 #######################################################

#Question 1  - Game Attendance
#Paid attendancw data - 6210	3150	2700	3012	4875 3540	6127	2581	2642	2573 2792	2800	2500	3700	6030 5437	2758	3490	2851	2720

# State the hypothesis
#H0 : The median of paid attendance 20 local football games is 3000
#H1 : The median of paid attendance 20 local football game is not 3000

alpha1 = 0.05
audience <- c(6210,3150, 2700, 3012, 4875, 3540, 6127, 2581, 2642, 2573, 2792, 2800, 2500, 3700, 6030, 5437, 2758, 3490, 2851, 2720)
audienceData <- matrix(audience, nrow = 4, ncol = 5, byrow=TRUE)
audienceData


# Finding the critical value
qsignrank(0.025, 20, lower.tail = T)

# Using the Sign Test
test1 <- SIGN.test(audienceData, md = 3000, alpha = 0.05)
ifelse(test1$p.value > alpha1, "Fail to reject the Null Hypothesis", "Reject Null Hypothesis")

#Question 2 - Lottery Ticket Sales
#State the Hypothesis:
#H0 : The median is 200
#H1 : The median is less than 200

alpha = 0.05

set.seed(12)
Negative <- 15
Positive <- 25
Res <- binom.test(x = c(Positive, Negative), alternative = "less")
Res
ifelse(Res$p.value > alpha1, "Fail to reject the Null Hypothesis", "Reject Null Hypothesis")

####################################################### Section:13-3 #######################################################

#Question 3 - Lengths of Prison Sentences
#State the Hypothesis:
#H0 : Difference in the sentence received by Gender
#H1 : No Difference in the sentence received by Gender

alpha = 0.05

malesData <- c(8,12,6,14,22,27,32,24,26,19,15,13)
femalesData <- c(7,5,23,21,26,30,9,4,17,23,12,11,16)

#Calculating Critical Value
qwilcox(0.025, 14, 12, lower.tail = T)

#Using Wilcox test
wiltest <- wilcox.test(malesData,femalesData, exact = FALSE)
ifelse(wiltest$p.value > alpha1, "Fail to reject the Null Hypothesis", "Reject Null Hypothesis")

#Question 4 - Winning Baseball games
#State the Hypothesis:
#H0: There is no difference in wins count
#H1: There is difference in wins count

NLdata <- c(89,96,88,101,90,91,92,96,108,100,95)
ALdata <- c(108,86,91,97,100,102,95,104,95,89,88,101)

#Calculating Critical Value
qwilcox(0.025, 11, 12, lower.tail = T)
ifelse(wiltest1$p.value > alpha1, "Fail to reject the Null Hypothesis", "Reject Null Hypothesis")

#Using Wilcox test
wiltest1 <- wilcox.test(NLdata,ALdata, exact = FALSE)
wiltest1
ifelse(wiltest1$p.value > alpha1, "Fail to reject the Null Hypothesis", "Reject Null Hypothesis")

####################################################### Section:13-4 #######################################################

#Question 5 - ws = 13, n = 15, α = 0.01, two-tailed
qsignrank(0.005, 15, lower.tail = T)

#Question 6 - ws = 32, n = 28, α = 0.025, one-tailed
qsignrank(0.025, 28, lower.tail = T)

#Question 7 - ws = 65, n = 20, α = 0.05, one-tailed
qsignrank(0.05, 20, lower.tail = T)

#Question 8 - ws = 22, n = 14, α = 0.10, two-tailed
qsignrank(0.05, 14, lower.tail = T)


####################################################### Section:13-5 #######################################################

#Question 9 - Mathematics Literacy Score
#State the Hypothesis:
#H0 : There is no difference in the mean
#H1 : There is a difference in the mean

westernHemiData <- c(527,406,474,381,411)
europeData <- c(520,510,513,548,496)
easternAsiaData <- c(523,547,547,391,549)

#Kruskal-Wallis test
ktest <- kruskal.test(list(westernHemiData, europeData, easternAsiaData))
ifelse(ktest$p.value > 0.05, "Fail to reject the Null Hypothesis", "Reject Null Hypothesis")

####################################################### Section:13-6 #######################################################

#Question 10 - Subway and Commuter Rail Passengers
#State the Hypothesis:
#H0: correlation relationship exists between subway and rail.
#H1: correlation relationship does not exists between subway and rail.

subwayData <- c(845,494,425,313,108,41)
railData <- c(39,291,142,103,33,38)

correlTest <- cor.test(x = subwayData, y =railData, method = 'spearman')
correlTest
ifelse(correlTest$p.value > 0.05, "Fail to reject the Null Hypothesis", "Reject Null Hypothesis")

####################################################### Section:14-3 #######################################################

#Question 11 - Prizes in Caramel Corn Boxes 

expNumWins = 40
simData <- matrix(list(), nrow = expNumWins, ncol = 1)
crmlBoxes <- vector()
for(i in 1:expNumWins)
{
  win <- vector()
  a=0
  b=0
  c=0
  d=0
  while(a*b*c*d == 0){
    data <- sample(1:4,1)
    if(data == 1)
    {
      a = 1
      win <- append(win, "a")
    } 
    else if(data == 2)
    {
      b = 2
      win <- append(win, "b")
    }
    else if(data == 3)
    {
      c = 3
      win <- append(win, "c")
    }
    else
    {
      d = 1
      win <- append(win, "d")
    }
  }
  
  simData[[i,1]] <- win
  crmlBoxes <- append(crmlBoxes, length(win))
}

simData
averageBoxes <-  sum(crmlBoxes)/expNumWins
round(averageBoxes, 0)

#Question 12 - Lottery Winner

outcomes = 30
simData2 <- matrix(list(), nrow = outcomes, ncol = 1)

ticket <- vector()
for(j in 1:outcomes){
  data2 <- vector()
  x = 0
  y = 0
  z = 0
  while (x*y*z == 0) {
    df <- sample(0:9, 1)
    if (df <= 5){
      x = 1
      data2 <- append(data2,"d")
    }
    else if(df <= 8){
      y = 1 
      data2 <- append(data2,"e")
    }
    else{
      z = 1
      data2 <- append(data2,"f")
    }
  }
  simData2[[j,1]] <- data2
  ticket <- append(ticket, length(data2))
}

averageTickets <-  sum(ticket)/outcomes
averageTickets
round(averageTickets , 0)