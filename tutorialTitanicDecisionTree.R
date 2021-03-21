library(dplyr)
data <- read.csv("R Codes/tutorialTitanic.csv")

View(data)
summary(data)
str(data)

sum(is.na(data))
data[data == "?"] <-NA
sum(is.na(data))

#converting data types
data <- transform(data,
                  age=as.integer(age),
                  fare=as.integer(fare),
                  cabin=as.factor(cabin),
                  embarked=as.factor(embarked),
                  home.dest=as.factor(home.dest),
                  pclass=as.factor(pclass),
                  sex=as.factor(sex),
                  sibsp=as.factor(sibsp),
                  parch=as.factor(parch),
                  ticket=as.integer(ticket),
                  survived=as.factor(survived))

str(data)

#some values are converted to NA's if we keep ticket column
summary(data)
View(data)
#give Col with NA's
list_na <- colnames(data[ apply(data, 2, anyNA) ])
list_na
#How many NA's in each Col.
colSums(is.na(data))
#Gives continuous variable
continuous <-select_if(data, is.numeric)
#Gives NA's in conituous variable
list_na_cont <- colnames(continuous[ apply(continuous, 2, anyNA) ])
list_na_cont
#Mean imputation for Continuous variable
average_missing <- apply(data[,list_na_cont],2, mean, na.rm =TRUE)
average_missing
#replace NA's with mean in continuous variable
data_replace <- data %>%
  mutate(age  = ifelse(is.na(age), average_missing[1], age),
         ticket = ifelse(is.na(ticket), average_missing[2], ticket),
         fare = ifelse(is.na(fare), average_missing[3], fare))
View(data_replace)
#rempve 'x' & 'name' columns
data_replace_again <- data_replace[,c(-1,-4)]
View(data_replace_again)
sum(is.na(data_replace_again))
#NA's in data_replace_again
list_na_again <- colnames(data_replace_again[ apply(data_replace_again, 2, anyNA) ])
list_na_again

#handling missing values of embarked column
m<-table(data_replace_again$embarked)
m
data_replace_again$embarked[which(is.na(data_replace_again$embarked))] <- as.factor("S")

summary(data_replace_again)
#'?'is still there, so converting back to factor
data_replace_again$embarked <- factor(data_replace_again$embarked)
summary(data_replace_again)
#Checking NA's in data_replace_again col.
colSums(is.na(data_replace_again))

# removing cabin and home.sate col from data_replace_again since it contains many categories

recast <- data_replace_again[,c(-9,-11)]
View(recast)
sum(is.na(recast))
str(recast)
summary(recast)

trainDataIndex <- sample(1:nrow(recast),0.7*nrow(recast), replace = F)
trainDatare <-recast[trainDataIndex, ]
testDatare <- recast[-trainDataIndex, ]
View(testDatare)
View(trainDatare)

library("rpart")
library("rpart.plot")

ctree = rpart(survived ~., data = trainDatare, method = "class")
rpart.plot(ctree)

#Prediction :-
testDatare$PredSurvived <- predict(ctree,testDatare, type="class")
View(testDatare)

length(testDatare$PredSurvived)


accu_table <- table(testDatare$PredSurvived,testDatare$survived)
accuracy_Test <- sum(diag(accu_table)) / sum(accu_table)
accuracy_Test


prun <- rpart(survived~., trainDatare, control = rpart.control(minsplit = 100))
rpart.plot(prun, main = "minsplit=100")

prun <- rpart(survived~., trainDatare, control = rpart.control(minsplit = 50))
rpart.plot(prun, main = "minsplit=50")


rm(list = ls())

