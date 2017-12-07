# From edX course - The Analytics Edge
#  Unit 2: Linear Regression
# The Statistical Sommelier: An Introduction to Linear Regression 

# https://courses.edx.org/courses/course-v1:MITx+15.071x+2T2017/courseware/f8d71d64418146f18a066d7f0379678c/35b789067e9b469caed457cfff1645b7/?child=first



# Load necessary packages


# set working directory & read in working wine case files
setwd("/home/a_friend/data_analysis/projects/MOOC/The Analytics Edge - edX/working_data")
wine <- read.csv("wine.csv", header = TRUE)
wine_test <- read.csv("wine_test.csv", header = TRUE)

# R commands from Video 4
str(wine)
summary(wine)
# Determine the fitted model for Price based on the AGST variable using a linear regression approach.
model1 <- lm(Price ~ AGST, data = wine)
# Show the residuals (or errors)
model1$residuals
model1_SSE <- sum(model1$residuals^2)

model2 <- lm(Price ~ AGST + HarvestRain, data = wine)
model2$residuals
model2_SSE <- sum(model2$residuals^2)
plot(model2)
summary(model1)
summary(model2)

model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)
model3_SSE <- sum(model3$residuals^2)

# Now up to Video 5
# https://courses.edx.org/courses/course-v1:MITx+15.071x+2T2017/courseware/f8d71d64418146f18a066d7f0379678c/35b789067e9b469caed457cfff1645b7/?child=first

model4 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model4)
model4_SSE <- sum(model4$residuals^2)

# Video 6: Correlation and Multicollinearity
plot(wine$FrancePop, wine$Age)
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

model5 <- lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)
summary(model5)
model5_SSE <- sum(model5$residuals^2)

# Video 7: Making Predictions

predictTest <- predict(model4, wine_test)
str(wine_test)
predictTest_SSE <- sum((wine_test$Price - predictTest)^2)
predictTest_SST <- sum((wine_test$Price - mean(wine$Price))^2)
RSquared <- 1 - predictTest_SSE/predictTest_SST
