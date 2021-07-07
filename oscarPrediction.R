data <- read.csv("R Codes/oscars-demographics1.csv")
View(data)
summary(data)

#Extracting few columns
library(dplyr)
step_1_df <- select(data, birthplace,date_of_birth,race_ethnicity,year_of_award,award,country,YOB,award_age)
View(step_1_df)

#Showing first three rows of subset
df <- head(step_1_df, n=3)

#Distinct values of categorical columns
table(step_1_df$award)
class(step_1_df$date_of_birth)
table(step_1_df$country)
table(step_1_df$award_age)

#Replacing NA's in place of corrupted values
step_1_df[ step_1_df == "#VALUE!" ] <- NA
#Now removing NA's
step_2_df<-na.omit(step_1_df)
View(step_2_df)

#Changing datatype of award_age col from char to int
step_2_df$award_age = as.integer(step_2_df$award_age)
str(step_2_df)

#Bucketing award age column
recast_data <- step_2_df %>%
  mutate(award_age = 
           factor(ifelse(award_age<35 , "Bucket 1", 
                         ifelse(award_age >= 35 && award_age < 45, "Bucket 2",
                                ifelse(award_age >= 45 && award_age < 55, "Bucket 3","Bucket 4")))))
View(recast_data)

#Need to bucket award col since it is dependent variable
table(recast_data$award)
recast_data1 <- recast_data %>%
  mutate(Naward = 
           factor(ifelse(award == "Best Actor" , "0", 
                         ifelse(award == "Best Actress", "1",
                                ifelse(award == "Best Director", "2",
                                       ifelse(award == "Best Supporting Actor", "3","4"))))))
table(recast_data1$Naward)
View(recast_data1)

#Bucketing country col since there are too many categories. Now there will be only 2 category
#USA and other.
library(dplyr)
recast_data1 <-recast_data1 %>%
  mutate(Ncountry = 
           factor(ifelse(country == "Al" | 
                           country == "Ar" |
                           country == "Ca" |
                           country == "Fl" |
                           country == "Ga" |
                           country == "Hi" |
                           country == "Il" |
                           country == "In" |
                           country == "Ks" |
                           country == "Ky" |
                           country == "La" |
                           country == "Me" |
                           country == "Md" |
                           country == "Ma" |
                           country == "Mi" |
                           country == "Mn" |
                           country == "Mo" |
                           country == "Mt" |
                           country == "Ne" |
                           country == "New York City" |
                           country == "Nj" |
                           country == "Ny" |
                           country == "Oh" |
                           country == "Ok" |
                           country == "Pa" |
                           country == "Tn" |
                           country == "Tx" |
                           country == "Ut" |
                           country == "Va" |
                           country == "Wa" |
                           country == "Wi", "USA","other")))
View(recast_data1)
table(recast_data1$Ncountry)
str(recast_data1)

#Changing remaining data types
recast_data1$race_ethnicity = as.factor(recast_data1$race_ethnicity)
str(recast_data1)

#Creating dummy variable for race_ehnicity col because only white race has enough count.
table(recast_data1$race_ethnicity)
recast_data1$race1 <- ifelse(recast_data1$race_ethnicity =='White',1,0)
table(recast_data1$race1)
recast_data1$race1 <- as.factor(recast_data1$race1)
str(recast_data1)

View(recast_data1)

#----Data exploration:----

#Most oscar winner are from USA
barplot(table(recast_data1$Ncountry)) 

#Max oscar winner are white
barplot(table(step_2_df$race_ethnicity)) 

#Best director tend to be older than best actor or actress
library(ggplot2)
ggplot(step_2_df, aes(x = award, y = award_age)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()

#Since we have 5 levels in dependent variable award, We will cuild logistic regression  
#model for each level

#----Making seprate col for each category of award----
table(recast_data1$award)
recast_data1$best_director <- ifelse(recast_data1$award == "Best Director", 1, 0)
recast_data1$best_actor <- ifelse(recast_data1$award == "Best Actor", 1, 0)
recast_data1$best_actress <- ifelse(recast_data1$award == "Best Actress", 1, 0)
recast_data1$best_sup_actor <- ifelse(recast_data1$award == "Best Supporting Actor", 1, 0)
recast_data1$best_sup_actress <- ifelse(recast_data1$award == "Best Supporting Actress", 1, 0)

recast_data1$best_director <- factor(recast_data1$best_director, levels = c(0, 1))
recast_data1$best_actor <- factor(recast_data1$best_actor, levels = c(0, 1))
recast_data1$best_actress <- factor(recast_data1$best_actress, levels = c(0, 1))
recast_data1$best_sup_actor <- factor(recast_data1$best_sup_actor, levels = c(0, 1))
recast_data1$best_sup_actress <- factor(recast_data1$best_sup_actress, levels = c(0, 1))

str(recast_data1)



#----Split data into tarin and test----
trainDataIndex <- sample(1:nrow(recast_data1),0.7*nrow(recast_data1), replace = F)
trainData <-recast_data1[trainDataIndex, ]
testData <- recast_data1[-trainDataIndex, ]
View(trainData)
View(testData)


#----Building model for best director----
logit <- glm(best_director ~
               award_age +
               race1 +
               Ncountry, data = trainData, family = 'binomial')
summary(logit)

#Removing insignificant col starting from Ncountry
logit <- glm(best_director ~
               award_age 
               #race1 
               #Ncountry
               , data = trainData, family = 'binomial')
summary(logit)

testData$Pred_director <- predict(logit,testData,type =c("response"))
View(testData)

table(testData$best_director)/nrow(testData)

quantile(testData$Pred_director, probs = seq(0,1,0.05))

testData$Pred_director <- ifelse(testData$Pred_director > 0.2,1,0)
View(testData)

#Accuracy 
table_mat<-table(testData$Pred_director,testData$best_director)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#Accuracy is 41% to predict best director
#----Building model for best actor----
logit1 <- glm(best_actor ~
               award_age +
               race1 +
               Ncountry, data = trainData, family = 'binomial')
summary(logit1)

#Removing insignificant col starting from race1
logit1 <- glm(best_actor ~
               award_age 
             #race1 
             #+Ncountry
             , data = trainData, family = 'binomial')
summary(logit1)

testData$Pred_actor <- predict(logit1,testData,type =c("response"))
View(testData)

table(testData$best_actor)/nrow(testData)

quantile(testData$Pred_actor, probs = seq(0,1,0.05))

testData$Pred_actor <- ifelse(testData$Pred_actor > 0.24,1,0)
View(testData)

#Accuracy 
table_mat<-table(testData$Pred_actor,testData$best_actor)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#Accuracy is 37% to predict best actor

#----Building model for best actress----
logit2 <- glm(best_actress ~
               award_age +
               race1 +
               Ncountry, data = trainData, family = 'binomial')
summary(logit2)

#Removing insignificant col starting from race1
logit2 <- glm(best_actress~
              
               award_age 
             #race1 
             #+Ncountry
             , data = trainData, family = 'binomial')
summary(logit2)

testData$Pred_actress <- predict(logit2,testData,type =c("response"))
View(testData)

table(testData$best_actress)/nrow(testData)

quantile(testData$Pred_actress, probs = seq(0,1,0.05))

testData$Pred_actress <- ifelse(testData$Pred_actress > 0.11,1,0)
View(testData)

#Accuracy 
table_mat<-table(testData$Pred_actress,testData$best_actress)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#Accuracy is 82% to predict best actress

#----Building model for best sup actor----
logit3 <- glm(best_sup_actor ~
               award_age +
               race1 +
               Ncountry, data = trainData, family = 'binomial')
summary(logit3)

#Removing insignificant col starting from Ncountry
logit3 <- glm(best_sup_actor ~
               award_age 
             #+race1 
             #Ncountry
             , data = trainData, family = 'binomial')
summary(logit3)

testData$Pred_sup_actor <- predict(logit3,testData,type =c("response"))
View(testData)

table(testData$best_sup_actor)/nrow(testData)

quantile(testData$Pred_sup_actor, probs = seq(0,1,0.05))

testData$Pred_sup_actor <- ifelse(testData$Pred_sup_actor > 0.22,1,0)
View(testData)

#Accuracy 
table_mat<-table(testData$Pred_sup_actor,testData$best_sup_actor)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#Accuracy to predict best supporting actor is 78%

#----Building model for best sup actress----
logit4 <- glm(best_sup_actress ~
               award_age +
               race1 +
               Ncountry, data = trainData, family = 'binomial')
summary(logit4)

testData$Pred_sup_actress <- predict(logit4,testData,type =c("response"))
View(testData)

table(testData$best_sup_actress)/nrow(testData)

quantile(testData$Pred_sup_actress, probs = seq(0,1,0.05))

testData$Pred_sup_actress <- ifelse(testData$Pred_sup_actress > 0.25,1,0)
View(testData)

#Accuracy 
table_mat<-table(testData$Pred_sup_actress,testData$best_sup_actress)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#Accuracy to predict best supporting actress is 71%

#----Since we have more than 2 parameter to to predict our dependent variable we need to 
#apply PCA technique to visualise our clusters----

pladf <- recast_data1
pladf <- pladf[c(8,10,11)]
View(pladf)

pladf1 <- step_2_df %>%
  mutate(award_age = 
           factor(ifelse(award_age<35 , "1", 
                         ifelse(award_age >= 35 && award_age < 45, "2",
                                ifelse(award_age >= 45 && award_age < 55, "3","4")))))
pladf1$award_age <- as.integer(pladf1$award_age)
pladf1$Ncountry <- pladf$Ncountry
pladf1$Ncountry <- ifelse(pladf1$Ncountry =='USA',1,0)
pladf1$Ncountry <- as.integer(pladf1$Ncountry)
pladf1$rac1 <- pladf$race1
pladf1 <- pladf1[c(8,9,10)]
pladf1$rac1 <- as.integer(pladf1$rac1)

View(pladf1)
str(pladf1)
pladf1 = scale(pladf1)

trainDataIndex <- sample(1:nrow(pladf1),0.7*nrow(pladf1), replace = F)
trainData <-pladf1[trainDataIndex, ]
View(trainData)

# Applying PCA
# install.packages('caret')
library(caret)
# install.packages('e1071')
library(e1071)
pca = preProcess(x = trainData, method = 'pca', pcaComp = 2)

#Making new data set with pca
pca_set = predict(pca, pladf1)
View(pca_set)

#----Since there are 5 types of awards we will have 5 clusters----
# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = pca_set, centers = 5)
y_kmeans = kmeans$cluster

# Visualising the clusters
library(cluster)
clusplot(pca_set,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of award'),
         xlab = 'PC1',
         ylab = 'PC2')
