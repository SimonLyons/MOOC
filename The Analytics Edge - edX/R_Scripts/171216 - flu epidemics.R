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
View(FluTrain)

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

