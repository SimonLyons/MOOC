# From edX course - The Analytics Edge
# Unit 2: Linear Regression 
# Assignment 2 > Reading Test Scores

# https://courses.edx.org/courses/course-v1:MITx+15.071x+2T2017/courseware/f8d71d64418146f18a066d7f0379678c/60d93a44280348d7a0a16663f92af0f7/?child=first

# Load required packages
# require(dplyr)
# require(ggplot2)

# set working directory & read in working Pisa Test case files
setwd("/home/a_friend/data_analysis/projects/MOOC/The Analytics Edge - edX/working_data")

pisa2009train <- read.csv("pisa2009train.csv", header = TRUE)
pisa2009test <- read.csv("pisa2009test.csv", header = TRUE)
View(pisa2009train)

# Problem 1.1 - Dataset size
# How many students are there in the training set?
length(pisa2009train$male)
nrow(pisa2009train)
nrow(pisa2009test)


# Problem 1.2 - Summarizing the dataset
# Using tapply() on pisaTrain, what is the average reading test score of males?
tapply(pisa2009train$readingScore, pisa2009train$male, mean)

# Problem 1.3 - Locating missing values
# Which variables are missing data in at least one observation in the training set?
summary(pisa2009train)

# Problem 1.4 - Removing missing values
# How many observations are now in the training set?
pisa2009train <- na.omit(pisa2009train)
nrow(pisa2009train)
# How many observations are now in the testing set?
pisa2009test <- na.omit(pisa2009test)
nrow(pisa2009test)

# Problem 2.1 - Factor variables
# Which of the following variables is an unordered factor with at least 3 levels? A - raceeth
# Which of the following variables is an ordered factor with at least 3 levels? A - grade
str(pisa2009train)

# Problem 2.2 - Unordered factors in regression models
# Which binary variables will be included in the regression model?

# Problem 2.3 - Example unordered factors
# For a student who is Asian, which binary variables would be set to 0? All remaining variables will be set to 1.
# For a student who is white, which binary variables would be set to 0?

# Problem 3.1 - Building a model
# Set the reference level of the factor by typing the following two lines in your R console:
pisa2009train$raceeth = relevel(pisa2009train$raceeth, "White")
pisa2009test$raceeth = relevel(pisa2009test$raceeth, "White")

# It's apparently for the test set data to either specify "newdata = ...." or to just
# insert the name of the data.
lmScore <- lm(readingScore ~ ., pisa2009train)
summary(lmScore)

# Problem 3.2 - Computing the root-mean squared error of the model
# What is the training-set root-mean squared error (RMSE) of lmScore?
# Calculate the Sum of Squared Errors
SSE <- sum(lmScore$residuals ^2)
# Calculate the Sum of Total Error
SST <- sum((mean(pisa2009train$readingScore) - lmScore$fitted.values)^2)
# Calculate the RMSE of the training set.
RMSE <- sqrt(SSE/nrow(pisa2009train))

# Problem 3.3 - Comparing predictions for similar students
# Consider two students A and B. They have all variable values the same, except that 
# student A is in grade 11 and student B is in grade 9. What is the predicted reading 
# score of student A minus the predicted reading score of student B?
readingscoredelta <- lmScore$coefficients[2] * (11 -9)

# Problem 4.1 - Predicting on unseen data
# What is the range between the maximum and minimum predicted reading score on the TEST set?
predTest <- predict(lmScore, newdata = pisa2009test)
summary(predTest)
diff(range(predTest))
# Answer: 303.6437, which differs from the 284.5 on the EdX website. My max and min are different.

# Problem 4.2 - Test set SSE and RMSE
# What is the sum of squared errors (SSE) of lmScore on the testing set?
SSE_test <- sum((predTest - pisa2009test$readingScore)^2)
# What is the root-mean squared error (RMSE) of lmScore on the testing set?
SST_test <- sum((mean(pisa2009train$readingScore) - pisa2009test$readingScore)^2)
R2 <- 1 - SSE_test/SST_test
RMSE <- sqrt(mean((predTest-pisa2009test$readingScore)^2))

# Problem 4.3 - Baseline prediction and test-set SSE
# What is the predicted test score used in the baseline model? 
# Remember to compute this value using the training set and not the test set.
baseline = mean(pisa2009train$readingScore)
