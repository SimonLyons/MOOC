
# From edX course - The Analytics Edge
# Unit 2: Linear Regression Playing Moneyball in the NBA (Recitation)  

# https://courses.edx.org/courses/course-v1:MITx+15.071x+2T2017/courseware/f8d71d64418146f18a066d7f0379678c/6248c2ecbbcb40cfa613193e8f1873c1/?child=first

# Load required packages
require(dplyr)
require(ggplot2)

# set working directory & read in working wine case files
setwd("/home/a_friend/data_analysis/projects/MOOC/The Analytics Edge - edX/working_data")
NBA <- read.csv("NBA_train.csv", header = TRUE)
str(NBA)
summary(NBA)
head(NBA)

plot(NBA$W, NBA$Playoffs)

NBA$PTSdiff <- NBA$PTS - NBA$oppPTS
cor(NBA$W, NBA$PTSdiff)
plot(NBA$W, NBA$PTSdiff)
WinsReg <- lm(W ~ PTSdiff, data = NBA)
summary(WinsReg)

PointsReg <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary(PointsReg)

SSE <- sum(PointsReg$residuals ^2)
RMSE <- sqrt(SSE/length(PointsReg$residuals))
mean(NBA$PTS)

PointsReg2 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA)
summary(PointsReg2)

PointsReg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB+ STL + BLK, data = NBA)
summary(PointsReg3)


PointsReg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary(PointsReg4)

SSE4 <- sum(PointsReg4$residuals ^2)
RMSE4 <- sqrt(SSE4/length(PointsReg4$residuals))

# I'm up to Video 4: Making Predictions
# https://courses.edx.org/courses/course-v1:MITx+15.071x+2T2017/courseware/f8d71d64418146f18a066d7f0379678c/6248c2ecbbcb40cfa613193e8f1873c1/?child=first


