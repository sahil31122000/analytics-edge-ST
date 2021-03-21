#----NOTE----
#ticket contains no NA's and gives error on converting from char to numeric
#age and fare contains NA's and can be converted from char to numeric
#Ticket column contains values which have numeric, char together so cant be converted.
#To convert its type delete those values and replace by NA's
#----START----

rm(list = ls())

data <- read.csv("R Codes/tutorialTitanic.csv")

View(data)
summary(data)
str(data)

data[data == "?"] <-NA
sum(is.na(data))

#give Col with NA's
list_na <- colnames(data[ apply(data, 2, anyNA) ])
list_na
#How many NA's in each Col.
colSums(is.na(data))

#cabin and home.dest contains many categories
table(data$cabin)
table(data$home.dest)
#Removing insignificant columns but why to remove ticket
library(dplyr)
library(raster)
clean_data <- data %>%
  select(-c(home.dest, cabin, name, x,ticket))
View(clean_data)
sum(is.na(clean_data))
str(clean_data)

#Try & Error----
  clean_data$fare <- as.numeric(as.character(clean_data$fare))
  clean_data$age <- as.numeric(as.character(clean_data$age))
  str(clean_data)

  continuous <-select_if(clean_data, is.numeric)
  #Gives NA's in conituous variable
  list_na_cont <- colnames(continuous[ apply(continuous, 2, anyNA) ])
  list_na_cont
  average_missing <- apply(data[,list_na_cont],2, mean, na.rm =TRUE)
  average_missing  #<- this gives error

#continue----
#Checking NA's in Clean data
list_na_clean <- colnames(clean_data[ apply(clean_data, 2, anyNA) ])
list_na_clean

#Need to delete NA's since imputation not possible because changing data type and handling NA's
#not possible as same time.

recast <- clean_data %>% 
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
          survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  na.omit()
View(recast)
sum(is.na(recast))
str(recast)
table(recast$embarked)
#Changing remaining data types
recast$fare <- as.numeric(as.character(recast$fare))
recast$age <- as.numeric(as.character(recast$age))
# data$ticket <- as.numeric(as.character(data$ticket)) => coercion error
# data$ticket <- as.numeric(data$ticket) => coercion error

recast <- recast %>% mutate(embarked = factor(embarked, levels = c('C','Q','S')),
                          sex = factor(sex, levels = c('female', 'male')))
str(recast)

#Splitting dataset into training and testing data set
trainDataIndex <- sample(1:nrow(recast),0.7*nrow(recast), replace = F)
trainDatare <-recast[trainDataIndex, ]
testDatare <- recast[-trainDataIndex, ]
View(testDatare)
View(trainDatare)

# Change Y values to 1's and 0's
trainDatare$survived <- ifelse(trainDatare$survived == "Yes", 1, 0)
str(trainDatare)
trainDatare$survived <- factor(trainDatare$survived, levels = c(0, 1))
str(trainDatare)
View(trainDatare)

logit <- glm(survived~., data = trainDatare, family = 'binomial')
summary(logit)

#Removing fare, parch
logit <- glm(survived ~ pclass + sex + age + sibsp + #parch 
               + embarked,
             data = trainDatare, family = 'binomial')
summary(logit)

#Change Dependent Var "Income" values to 1's and 0's----
testDatare$survived <- ifelse(testDatare$survived == "Yes", 1, 0)
View(testDatare)

#----------Predict--------------------
testDatare$Pred_survived <- predict(logit,testDatare,type =c("response"))
View(testDatare)

table(testDatare$survived)/nrow(testDatare)
#from above cmd 0 is 65% and 1 is 34%

quantile(testDatare$Pred_survived, probs = seq(0,1,0.05))

#Quantile level at 65% is 0.54. Thus converting all the values above 0.54 to 1 and rest to 0
testDatare$Pred_survived <- ifelse(testDatare$Pred_survived > 0.54,1,0)
View(testDatare)

#In prediction 0's are more and 1's are less
table(testDatare$Pred_survived)/nrow(testDatare)

#Checking Accuracy of Model using: Confusion Matrix----
table_mat<-table(testDatare$Pred_survived,testDatare$survived)
table_mat

#Accuracy is comming to be 78.5%
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

