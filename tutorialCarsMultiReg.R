cars_data <- read.csv("R Codes/Luxury_Cars.csv")
View(cars_data)
sum(is.na(cars_data))
str(cars_data)

#Covert data types
recast_cars <- transform(cars_data,
                         Make = as.factor(Make),
                        Type = as.factor(Type),
                         Origin = as.factor(Origin),
                        DriveTrain = as.factor(DriveTrain),
                        Cylinders = as.factor(Cylinders),
                        Engine_Size = as.numeric(Engine_Size),
                        Horsepower = as.numeric(Horsepower),
                        Weight_LBS = as.numeric( Weight_LBS),
                        Wheelbase_inch = as.numeric(Wheelbase_inch),
                        Length_inch = as.numeric(Length_inch ),
                        MPG_Mileage = as.numeric( MPG_Mileage )
                        )
str(recast_cars)
sum(is.na(recast_cars))
View(recast_cars)

#NO need of model and make col.
library(dplyr)
recast <- recast_cars %>% select(-c(Model,Make))
View(recast)

#Creating Dummy variable for DriveTrain
table(recast$DriveTrain)
library(sqldf)
sqldf("select DriveTrain, COUNT (*) as obs, avg(MPG_Mileage) from recast GROUP BY 1")
dataset_1 <- recast
dataset_1$Drive1 <- ifelse(dataset_1$DriveTrain =='Front',1,0)
dataset_1$Drive2 <- ifelse(dataset_1$DriveTrain =='Rear',1,0)
View(dataset_1)
str(dataset_1)
  #Remove drivetrain col
dataset_1 <- dataset_1[-3]

#Creating Dummy variable for cylinders
table(dataset_1$Cylinders)
dataset_1$cylinder1 <- ifelse(dataset_1$Cylinders =='4',1,0)
dataset_1$cylinder2 <- ifelse(dataset_1$Cylinders =='6',1,0)
dataset_1 <- dataset_1[-4]

#Remove Origin col also because it does not matters which origin it is
dataset_1 <- dataset_1[-2]
str(dataset_1)
table(dataset_1$Type)

# Training and Testing
trainDataIndex <- sample(1:nrow(dataset_1),0.7*nrow(dataset_1), replace = F)
trainData <-dataset_1[trainDataIndex, ]
testData <- dataset_1[-trainDataIndex, ]
View(trainData)
View(testData)

#Model:-
lmMod <- lm(MPG_Mileage ~.
            ,data = trainData)
summary(lmMod)

#Backward elemination starting from cylinder1
lmMod <- lm(MPG_Mileage ~ Type + #Engine_Size 
            + Horsepower + Weight_LBS + #Wheelbase_inch
            #+ Length_inch 
            + Drive1  + #Drive2 
              + cylinder2
            ,data = trainData)
summary(lmMod)
  #Accuracy is 84%
#Prediction :-
testData$predMPG <- predict(lmMod, testData)
View(testData)

#Calculating MAPE
mape <- mean(abs((testData$predMPG - testData$MPG_Mileage))/testData$MPG_Mileage)
mape

library(Metrics)
mape(testData$predMPG,testData$MPG_Mileage)

#Error is 6-7%