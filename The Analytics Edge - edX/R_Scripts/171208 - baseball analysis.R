# From edX course - The Analytics Edge
#  Unit 2: Linear Regression
#  Moneyball: The Power of Sports Analytics 

# https://courses.edx.org/courses/course-v1:MITx+15.071x+2T2017/courseware/f8d71d64418146f18a066d7f0379678c/6324edb8a22c4e35b937490647bfe203/?child=first

# Load required packages
require(dplyr)

# set working directory & read in working wine case files
setwd("/home/a_friend/data_analysis/projects/MOOC/The Analytics Edge - edX/working_data")
baseball <- read.csv("baseball.csv", header = TRUE)

summary(baseball)
str(baseball)
View(baseball)
unique(baseball$Team)

# Create a plot of Oakland wins after 1997
plot(baseball$Year[baseball$Team == "OAK" & baseball$Year > 1997], baseball$W[baseball$Team == "OAK" & baseball$Year > 1997], 
     col = "red", type = 'l')

# Create dataset limiting years to between 1997 and 2001
baseball_lim <- baseball %>% filter(Year > 1996 & Year < 2002 & Team == "OAK")

plot(baseball_lim$Year, baseball_lim$W, col = "red", type = 'l')

str(baseball_lim)

##################################################
# Video 2: Making it to the Playoffs

# Subset data to prior to 2002
moneyball <- subset(baseball, Year < 2002)
str(moneyball)
summary(moneyball)
write.csv(moneyball, "moneyball.csv", row.names = FALSE)
# Create a new variable showing the difference in Runs Scored versus Runs Against
moneyball$RD <- moneyball$RS - moneyball$RA
cor(moneyball$W, moneyball$RD)
plot(moneyball$RD, moneyball$W, type = 'p')

# Create linear regression model for Wins dependent on Runs Difference (RD)
WinsReg <- lm(W ~ RD, data = moneyball)
summary(WinsReg)

# Calculate the RD required to win at least 95 games
RD_95 <- (95 - WinsReg$coefficients[1])/WinsReg$coefficients[2]

# If a baseball team scores 713 runs and allows 614 runs, how many games do we expect the team to win?
RD <- 713 - 614
Q_Wins <- WinsReg$coefficients[1] + WinsReg$coefficients[2] * RD

##################################################
# Video 3: Predicting Runs

RunsReg <- lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(RunsReg)
cor(moneyball$RS, moneyball$OBP, moneyball$SLG, moneyball$BA)

# Predicting Runs Allowed
RunsAllowedReg <- lm(RA ~ OOBP + OSLG, data = moneyball)
summary(RunsAllowedReg)

# If a baseball team's OBP is 0.311 and SLG is 0.405, how many runs do we expect the team to score?
RunsReg$coefficients[1] + 0.311 * RunsReg$coefficients[2] + 0.405 *RunsReg$coefficients[3]

# If a baseball team's opponents OBP (OOBP) is 0.297 and oppenents SLG (OSLG) is 0.370, how many runs do we expect the team to allow?
RunsAllowedReg$coefficients[1] + 0.297 * RunsAllowedReg$coefficients[2] + 0.370 * RunsAllowedReg$coefficients[3]

##################################################
# Video 4: Using the Models to Make Predictions



