# From edX course - The Analytics Edge
# Unit 2: Linear Regression 
# Assignment 2 > Detecting Flu Epidemics via Search Engine Query Data

# https://courses.edx.org/courses/course-v1:MITx+15.071x+2T2017/courseware/f8d71d64418146f18a066d7f0379678c/60d93a44280348d7a0a16663f92af0f7/?child=first

# Load required packages
# require(dplyr)
# require(ggplot2)

# set working directory & read in working Pisa Test case files
setwd("/home/a_friend/data_analysis/projects/MOOC/The Analytics Edge - edX/working_data")

FluTrain <- read.csv("FluTrain.csv", header = TRUE)
FluTest <- read.csv("FluTest.csv", header = TRUE)
View(FluTrain)
View(FluTest)

# Look at the correlation between the Google Search data and physician visits:
cor(FluTrain$ILI, FluTrain$Queries)
plot(FluTrain$ILI, FluTrain$Queries)

# Problem 1.1 - Understanding the Data
# Which week corresponds to the highest percentage of ILI-related physician visits?
FluTrain[FluTrain$Queries == max(FluTrain$Queries), ]

# Problem 1.2 - Understanding the Data
# Plot the histogram of the dependent variable, ILI. What best describes the distribution of values of ILI?
hist(FluTrain$ILI)

# Problem 1.3 - Understanding the Data
plot(log(FluTrain$ILI), FluTrain$Queries)

# Problem 2.2 - Linear Regression Model
# What is the training set R-squared value for FluTrend1 model (the "Multiple R-squared")?
FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

# Problem 2.3 - Linear Regression Model
# For a single variable linear regression model, there is a direct relationship between the 
# R-squared and the correlation between the independent and the dependent variables. 
# What is the relationship we infer from our problem?
cor(log(FluTrain$ILI), FluTrain$Queries)
log(1/(cor(FluTrain$ILI, FluTrain$Queries)))
exp(-0.5*(cor(FluTrain$ILI, FluTrain$Queries)))
FluTrend2 <- lm(ILI ~ Queries, data = FluTrain)
summary(FluTrend2)


# Problem 3.1 - Performance on the Test Set
PredTest1 <- exp(predict(FluTrend1, newdata = FluTest))
# What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?
FluTest$Week
target_date <- "2012-03-11"
FluTest$Week[grepl(target_date, FluTest$Week)]
PredTest1[grepl(target_date, FluTest$Week)]

# Problem 3.2 - Performance on the Test Set
# What is the relative error betweeen the estimate (our prediction) and the observed value for the week 
# of March 11, 2012? Note that the relative error is calculated as:
# (Observed ILI - Estimated ILI)/Observed ILI

rel_err <- abs(FluTest$ILI - PredTest1)/FluTest$ILI
rel_err[11]

# Problem 3.3 - Performance on the Test Set
# What is the Root Mean Square Error (RMSE) between our estimates and the actual observations for the 
# percentage of ILI-related physician visits, on the test set?

FluTest_residuals <- abs(FluTest$ILI - PredTest1)

SSE_Flu <- sum(FluTest_residuals^2)
RMSE_FluTest <- sqrt(SSE_Flu/nrow(FluTest))

# Problem 4.1 - Training a Time Series Model
# We will build a variable called ILILag2 that contains the ILI value from 2 weeks before the current observation.
require(zoo)
ILILag2 <- lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
# How many values are missing in the new ILILag2 variable?

# Problem 4.2 - Training a Time Series Model
# Use the plot() function to plot the log of ILILag2 against the log of ILI. 
# Which best describes the relationship between these two variables?
plot(ILILag2, log(FluTrain$ILI))

# Problem 4.3 - Training a Time Series Model
# Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using 
# the Queries variable as well as the log of the ILILag2 variable. 
# Call this model FluTrend2.
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)
# Which coefficients are significant at the p=0.05 level in this regression model?


# Problem 5.1 - Evaluating the Time Series Model in the Test Set
# Modify the code from the previous subproblem to add an ILILag2 variable to the FluTest data frame. 
FluTest$ILILag2 <- lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
# How many missing values are there in this new variable?
summary(FluTest$ILILag2)

# Problem 5.3 - Evaluating the Time Series Model in the Test Set
# Fill in the missing values for ILILag2 in FluTest.
FluTrain_nrow <- nrow(FluTrain)
# What is the new value of the ILILag2 variable in the first row of FluTest?
FluTest$ILILag2[1] <- FluTrain$ILI[FluTrain_nrow-1]
# What is the new value of the ILILag2 variable in the second row of FluTest?
FluTest$ILILag2[2] <- FluTrain$ILI[FluTrain_nrow]

# Problem 5.4 - Evaluating the Time Series Model in the Test Set
# Obtain test set predictions of the ILI variable from the FluTrend2 model
