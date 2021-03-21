getwd()
data <- read.csv("R Codes/Start_Ups.csv", header = T)  #50_Startups.csv <- this is new csv file.
View(data)
data_re <- data[-1,]
View(data_re)
sum(is.na(data_re))
str(data_re)
summary(data_re)
#Giving names to columns
names(data_re)<- c("R_D_Spend", "Administration", "Marketing_Spend", "State", "Profit")
View(data_re)

#Transforming Data Types
recast <- transform(data_re,
                    R_D_Spend = as.integer(R_D_Spend),
                    Administration = as.integer(Administration),
                    Marketing_Spend = as.integer(Marketing_Spend),
                    State = as.factor(State),
                    Profit = as.integer(Profit)
)
str(recast)
summary(recast)
View(recast)

# Taking continuous variable
library(dplyr)
continuous <-select_if(recast, is.numeric)
summary(continuous)

#Checking Outliers 
boxplot(continuous)
boxplot(recast$Marketing_Spend)
abline(h=quantile(recast$Marketing_Spend))  
  #No outliers
                
#Shows Error if we include state column so need to remove it because it is a factor
library(usdm)
vif(recast[-4])
#Since R_D_Spend has high vif value need to remove it.

#Spliting Data Set
trainDataIndex <- sample(1:nrow(recast),0.7*nrow(recast), replace = F)
trainData <-recast[trainDataIndex, ]
testData <- recast[-trainDataIndex, ]
View(trainData)
View(testData)

#first running model with all the variables
lmMod <- lm(Profit ~.
            ,data = trainData)
summary(lmMod)

#Starting backward elemination by firstly removing R_D_Spend
lmMod <- lm(Profit ~ #R_D_Spend 
            + Marketing_Spend 
            #+ Administration 
            #+ State
            ,data = trainData)
summary(lmMod)
#eq :e is comming
#Accuracy is 84%

testData$predProfit <- predict(lmMod, testData)
View(testData)

#Calculating MAPE
mape <- mean(abs((testData$predProfit - testData$Profit))/testData$Profit)
mape

library(Metrics)
mape(testData$predProfit,testData$Profit)

