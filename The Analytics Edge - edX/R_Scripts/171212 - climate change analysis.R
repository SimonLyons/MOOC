# From edX course - The Analytics Edge
# Unit 2: Linear Regression 
# Assignment 2 > Climate Change

# https://courses.edx.org/courses/course-v1:MITx+15.071x+2T2017/courseware/f8d71d64418146f18a066d7f0379678c/60d93a44280348d7a0a16663f92af0f7/?child=first

# Load required packages
# require(dplyr)
# require(ggplot2)

# set working directory & read in working wine case files
setwd("/home/a_friend/data_analysis/projects/MOOC/The Analytics Edge - edX/working_data")

climate_change <- read.csv("climate_change.csv", header = TRUE)
View(climate_change)

# Create training and test datasets
climate_change_train <- subset(climate_change, Year < 2007)
climate_change_test <- subset(climate_change, Year > 2006)

length(climate_change$Year)
max(climate_change_test$Year)
max(climate_change_train$Year)

# Create prediction linear model
Pred_Temp <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_change_train)
summary(Pred_Temp)

# Problem 2.2 - Understanding the Mode
# Compute the correlations between all the variables in the training set.
# Which of the independent variables is N2O highly correlated with (absolute correlation greater than 0.7)?
cor(climate_change_train)

# Problem 3 - Simplifying the Model
# Build a model with only MEI, TSI, Aerosols and N2O as independent variables
Pred_Temp2 <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data = climate_change_train)
summary(Pred_Temp2)

# Problem 4 - Automatically Building the Model
Pred_Temp3 <- step(Pred_Temp)
summary(Pred_Temp3)

# Problem 5 - Testing on Unseen Data
# Using the model produced from the step function, calculate temperature predictions 
# for the testing data set, using the predict function.
TempPrediction <- predict(Pred_Temp3, climate_change_test)

# Enter the testing set R2
SSE_test <- sum((TempPrediction - climate_change_test$Temp)^2)
SST_test <- sum((mean(climate_change_train$Temp) - climate_change_test$Temp)^2)
R2 <- 1 - SSE_test/SST_test
