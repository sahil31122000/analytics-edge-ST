data <- read.csv("R Codes/HR_Data.csv")
View(data)
sum(is.na(data))
str(data)
summary(data)

#Converting data types
data$left <- factor(data$left, levels = c(0,1))
data$role <- factor(data$role)       #table(data$role)
data$salary <- factor(data$salary, levels = c('high','low','medium'))   #table(data$salary)
data$Work_accident <- factor(data$Work_accident, levels = c(0,1))
data$promotion_last_5years <- factor(data$promotion_last_5years, levels = c(0,1))
  str(data)
  summary(data)
  
#----Checking Outliers----
  # Taking continuous variable
  library(dplyr)
  continuous <-select_if(data, is.numeric)
  summary(continuous)
  
  #Checking Outliers in all continous variable
  boxplot(continuous)
  boxplot(continuous$number_project)
  boxplot(continuous$exp_in_company)  #<- here we found outlier
  abline(h=quantile(continuous$exp_in_company))
  
  #Outlier is there in exp_in_company
  #So removing top 2%
  top_one_percent <- quantile(continuous$exp_in_company, .98)
  top_one_percent
  
  data_drop <-data %>%
    filter(exp_in_company<top_one_percent)
  dim(data_drop)
  View(data_drop)
  boxplot(data_drop$exp_in_company)
  quantile(data_drop$exp_in_company)
  
#Creating dummy variable for role and salary
table(data_drop$role)
library(sqldf)
sqldf("select role, COUNT (*) as obs from data_drop GROUP BY 1")
dummy_data <- data_drop
dummy_data$role_1 <- ifelse(data_drop$role=='sales',1,0)
dummy_data$role_2 <- ifelse(data_drop$role=='technical',1,0)
View(dummy_data)

table(dummy_data$salary)
sqldf("select salary, COUNT (*) as obs from data_drop GROUP BY 1")
dummy_data$salary_1 <- ifelse(data_drop$salary=='low',1,0)
dummy_data$salary_2 <- ifelse(data_drop$salary=='medium',1,0)
View(dummy_data)
str(dummy_data)
summary(dummy_data)

#Removing role and salary columns 
re_data <- dummy_data[,c(-9,-10)]
View(re_data)
str(re_data)

#Create training and testing data
trainDataIndex <- sample(1:nrow(re_data),0.7*nrow(re_data), replace = F)
trainData <-re_data[trainDataIndex, ]
testData <- re_data[-trainDataIndex, ]
View(trainData)
View(testData)

#Preparing model
logit1 <- glm(left ~., data = trainData, family = 'binomial')
summary(logit1)

#Backward elemination starting from role 1
logit1 <- glm(left ~ satisfaction_level 
              +last_evaluation 
              +number_project  
              +average_montly_hours
              +exp_in_company
              +Work_accident
              +promotion_last_5years
              +role_2
              +salary_1    
              +salary_2
              ,data = trainData, family = 'binomial')
summary(logit1)

#Predicting on test data set
testData$Pred_left <- predict(logit1,testData,type =c("response"))
View(testData)

#Converting probabilities
table(testData$left)/nrow(testData)
quantile(testData$Pred_left, probs = seq(0,1,0.05))
testData$Pred_left <- ifelse(testData$Pred_left > 0.38,1,0)
View(testData)
#table(testData$Pred_left)/nrow(testData)

#Accuracy
table_mat<-table(testData$Pred_left,testData$left)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#Hence the accuracy is 80%
#Before removing Outliers accuracy was 79%

#----Solved by using bucketing for Col exp & proj----
View(data_drop)
data_drop$exp_in_company <- factor(data_drop$exp_in_company)   
str(data_drop)
summary(data)
table(data_drop$exp_in_company)
table(data_drop$number_project)
library(dplyr)
recast_data <- data_drop %>%
  mutate(exp_in_company = 
        factor(ifelse(exp_in_company == "2" | 
                      exp_in_company == "3" , "Fresher", 
               ifelse(exp_in_company == "4" |
                      exp_in_company == "5" |
                      exp_in_company == "6", "Experience",
               ifelse(exp_in_company == "7" |
                      exp_in_company == "8" |
                      exp_in_company == "10", "High_Exp","No_Exp")))))
str(recast_data)      
table(recast_data$exp_in_company)

recast_data <- recast_data %>%
  mutate(number_project = 
           factor(ifelse(number_project == "2" | 
                         number_project == "3" |
                         number_project == "4", "Less_Proj",  "More_Proj")))
str(recast_data)      
table(recast_data$number_project)

#----Rest is same----

#Creating dummy variable for role and salary
table(data$role)
#library(sqldf)
#sqldf("select role, COUNT (*) as obs from data GROUP BY 1")
dummy_data <- recast_data
dummy_data$role_1 <- ifelse(data_drop$role=='sales',1,0)
dummy_data$role_2 <- ifelse(data_drop$role=='technical',1,0)
View(dummy_data)

table(dummy_data$salary)
sqldf("select salary, COUNT (*) as obs from data_drop GROUP BY 1")
dummy_data$salary_1 <- ifelse(data_drop$salary=='low',1,0)
dummy_data$salary_2 <- ifelse(data_drop$salary=='medium',1,0)
View(dummy_data)
str(dummy_data)
summary(dummy_data)

#Removing role and salary columns 
re_data <- dummy_data[,c(-9,-10)]
View(re_data)
str(re_data)

#Create training and testing data
trainDataIndex <- sample(1:nrow(re_data),0.7*nrow(re_data), replace = F)
trainData <-re_data[trainDataIndex, ]
testData <- re_data[-trainDataIndex, ]
View(trainData)
View(testData)

#Preparing model
logit2 <- glm(left ~., data = trainData, family = 'binomial')
summary(logit2)

#Backward elemination starting from exp

logit2 <- glm(left ~ satisfaction_level 
              #+last_evaluation 
              +number_project  
              #+average_montly_hours
              #+exp_in_company
              +Work_accident
              +promotion_last_5years
              #+role_1
              #+role_2
              +salary_1    
              +salary_2
              ,data = trainData, family = 'binomial')
summary(logit2)

#Predicting on test data set
testData$Pred_left <- predict(logit2,testData,type =c("response"))
View(testData)

#Converting probabilities
table(testData$left)/nrow(testData)
quantile(testData$Pred_left, probs = seq(0,1,0.05))
testData$Pred_left <- ifelse(testData$Pred_left > 0.33,1,0)
View(testData)
#table(testData$Pred_left)/nrow(testData)

#Accuracy
table_mat<-table(testData$Pred_left,testData$left)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#Hence the accuracy is 77.7%



