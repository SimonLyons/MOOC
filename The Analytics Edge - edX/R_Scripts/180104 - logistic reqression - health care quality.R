# From edX course - The Analytics Edge
# Unit 3: Logistic Regression 
# Modeling the Expert: An Introduction to Logistic Regression 

# https://courses.edx.org/courses/course-v1:MITx+15.071x+2T2017/courseware/5893e4c5afb74898b8e7d9773e918208/030bf0a7275744f4a3f6f74b95169c04/?child=first

# set working directory & read in working Pisa Test case files
setwd("/home/a_friend/data_analysis/projects/MOOC/The Analytics Edge - edX/working_data")
# Load the dataset and convert it to a data frame
quality <- read.csv("quality.csv", header = TRUE)
summary(quality)
str(quality)
View(quality)

# Plot the number of office visits versus narcotics prescribed
plot(quality$OfficeVisits, quality$Narcotics)
require(ggplot2)
ggplot(quality, aes(OfficeVisits, Narcotics)) + geom_point(aes(colour = factor(PoorCare))) + theme(legend.position = "bottom")

# Load 'caTaools' package
require(caTools)
set.seed(88)
# Split data into training and testing sets
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
quality_train <- quality[split, ]
quality_test <- quality[!split, ]

# Build logistic regression model to predict level of care
QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = quality_train, family = binomial)
summary(QualityLog)

# Predict outcome of care on testing dataset
predict(QualityLog, newdata = quality_test)
ifelse(predict(QualityLog, newdata = quality_test) > 0.5 ,1 , 0)
quality_test$predict <- ifelse(predict(QualityLog, newdata = quality_test) > 0.5 ,1 , 0)
data.frame(quality_test$PoorCare, quality_test$predict)
quality_test$accuracy <- abs(quality_test$PoorCare - quality_test$predict)
predict_accuracy <- 1 - sum(quality_test$accuracy)/nrow(quality_test)

# Quick Question One
# create a logistic regression model to predict "PoorCare" using the 
# independent variables "StartedOnCombination" and "ProviderCount".
PoorCareLog <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data = quality_train, family=binomial)
summary(PoorCareLog)

# Video 5: Thresholding
predictTrain <- predict(QualityLog, newdata = quality_train)
predictTrain > 0.5
quality_train$PoorCare
sens_table <- table(quality_train$PoorCare, predictTrain > 0.5)
sens_table[2,2]
sensitivity <- sens_table[2,2] / (sens_table[2,2] + sens_table[2, 1])
specificity <- sens_table[1,1] / (sens_table[1,1] + sens_table[1, 2])

# Quick Question
# What is the sensitivity of Confusion Matrix #1?
20/25
# What is the specificity of Confusion Matrix #1?
15/25

# Video 6: ROC Curves
install.packages("ROCR")
require(ROCR)
ROCRpred <- prediction(predictTrain, quality_train$PoorCare)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))

# Quick Question
# Given this ROC curve, which threshold would you pick if you wanted to correctly identify 
# a small group of patients who are receiving the worst care with high confidence?

# Up to Video 7 - 8JAN18
# Video 7: Interpreting the Model
# https://courses.edx.org/courses/course-v1:MITx+15.071x+2T2017/courseware/5893e4c5afb74898b8e7d9773e918208/030bf0a7275744f4a3f6f74b95169c04/?child=first

