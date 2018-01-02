# From edX course - The Analytics Edge
# Unit 2: Linear Regression 
# Assignment 2 > State Data (OPTIONAL)

# https://courses.edx.org/courses/course-v1:MITx+15.071x+2T2017/courseware/f8d71d64418146f18a066d7f0379678c/60d93a44280348d7a0a16663f92af0f7/?activate_block_id=block-v1%3AMITx%2B15.071x%2B2T2017%2Btype%40sequential%2Bblock%4060d93a44280348d7a0a16663f92af0f7

# Load the dataset and convert it to a data frame
elantra_url <- "https://prod-edxapp.edx-cdn.org/assets/courseware/v1/78f6bc572ffdf2bca928179d83723fb0/asset-v1:MITx+15.071x+2T2017+type@asset+block/elantra.csv"

# set working directory & read in working Pisa Test case files
setwd("/home/a_friend/data_analysis/projects/MOOC/The Analytics Edge - edX/working_data")
# Load the dataset and convert it to a data frame
elantra_data <- read.csv("elantra.csv", header = TRUE)
summary(elantra_data)
str(elantra_data)
View(elantra_data)

# Problem 1 - Loading the Data
# Sort into training and test sets. Training set contains all data in years 2012 and earlier. 
elantra_train <- subset(elantra_data, Year < 2013)
elantra_test <- subset(elantra_data, Year > 2012)
# How many observations are in the training set?
nrow(elantra_train)   # 36  (3 years x 12 months)

# Problem 2.1 - A Linear Regression Model
# Build a linear regression model to predict monthly Elantra sales
# What is the model R-squared?
elantra_model1 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = elantra_train)
summary(elantra_model1)

# Problem 3.1 - Modeling Seasonality
View(elantra_train)
# Order the training data by date and add a row for this sequence
elantra_train_sorted <- elantra_train[order(elantra_train$Year, elantra_train$Month), ]
elantra_train_sorted$Month_seq <- 1:nrow(elantra_train_sorted)
plot(elantra_train_sorted$Month_seq, elantra_train_sorted$ElantraSales)
# Build a new linear regression model that predicts monthly Elantra sales using Month as 
# well as Unemployment, CPI_all, CPI_energy and Queries.
elantra_model2 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + Month, data = elantra_train)
summary(elantra_model2)

# Problem 3.3 - Understanding the Model
# In the new model, given two monthly periods that are otherwise identical in Unemployment, 
# CPI_all, CPI_energy and Queries, what is the absolute difference in predicted Elantra sales 
# given that one period is in January and one is in March?
# SIMON: The answer is the difference in the input variable, in this case March (3) - January (1) = 2, multiplied
# by the coefficient for Month = 2 x 110.69 = 221.3705
2 * elantra_model2$coefficients[6]
# what is the absolute difference in predicted Elantra sales given that one period is in January and one is in May?
# May (5) - January (1) = 4
4 * elantra_model2$coefficients[6]

# Problem 4.1 - A New Model
# Re-run the regression with the Month variable modeled as a factor variable.
elantra_train$Month_as_factor <- as.factor(elantra_train$Month)
elantra_model3 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + Month_as_factor, data = elantra_train)
summary(elantra_model3)

# Problem 5.1 - Multicolinearity
# Which of the following variables is CPI_energy highly correlated with?
cor(elantra_train$Unemployment, elantra_train$CPI_all)
cor(elantra_train[ , c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

# Problem 6.1 - A Reduced Model
# Remove the variable with the highest p-value. Repeat this until there are no variables that are insignificant. 
# Which variables, and in what order, are removed by this process?
elantra_model4 <- lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Month_as_factor, data = elantra_train)
summary(elantra_model4)

#  Problem 6.2 - Test Set Predictions
# Using the model from Problem 6.1, make predictions on the test set. 
# What is the sum of squared errors of the model on the test set?
elantra_test$Month_as_factor <- as.factor(elantra_test$Month)
View(elantra_test)
model4_test_predict <- predict(elantra_model4, newdata = elantra_test)
model4_SSE <- sum((model4_test_predict - elantra_test$ElantraSales)^2)

# Problem 6.3 - Comparing to a Baseline
# What would the baseline method predict for all observations in the test set?
mean(elantra_train$ElantraSales)

# Problem 6.4 - Test Set R-Squared
# What is the test set R-Squared?
model4_SST <- sum((mean(elantra_train$ElantraSales) - elantra_test$ElantraSales)^2)
model4_R2 <- 1 - model4_SSE / model4_SST

# Problem 6.5 - Absolute Errors
# What is the largest absolute error that we make in our test set predictions?
which.max(abs(elantra_test$ElantraSales - model4_test_predict))

# I'm now up to Unit 3
# https://courses.edx.org/courses/course-v1:MITx+15.071x+2T2017/courseware/5893e4c5afb74898b8e7d9773e918208/030bf0a7275744f4a3f6f74b95169c04/?child=first

