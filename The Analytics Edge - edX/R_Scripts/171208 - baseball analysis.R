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

# Team On Base Percentage (OBP): 0.339
# Team Slugging Percentage (SLG): 0.430

team_runs_2002 <-  RunsReg$coefficients[1] + 0.339 * RunsReg$coefficients[2] + 0.430 *RunsReg$coefficients[3]
team_runs_2002

# Opposing Team On Base Percentage (OBP): 0.307
# Opposing Team Slugging Percentage (SLG): 0.373
opposing_team_runs_2002 <- RunsAllowedReg$coefficients[1] + 0.307 * RunsAllowedReg$coefficients[2] + 0.373 * RunsAllowedReg$coefficients[3]
opposing_team_runs_2002

RD <- team_runs_2002 - opposing_team_runs_2002
Q_Wins <- WinsReg$coefficients[1] + WinsReg$coefficients[2] * RD

##################################################
# Quick Question
# You have a budget of $1,500,000, and you have the choice between the following players:

# First, forecast how many runs each player will score
player_selection_data <- read.csv("player_seletion_Q.csv", header = TRUE)
player_selection_data$RS <- RunsReg$coefficients[1] + player_selection_data$OBP * RunsReg$coefficients[2] + player_selection_data$SLG *RunsReg$coefficients[3]

# Then determine dollars of salary spent per run scored
player_selection_data$Salary_clean <- as.numeric(gsub(",", "", gsub("\\$", "", player_selection_data$Salary)))
player_selection_data$value <- player_selection_data$Salary_clean / player_selection_data$RS

# We see that Carlos Pena costs the least amount of salary per run scored, followed by Frank
# Menechino. The two salaries together add up to only $595k. However, the highest number of 
# runs inside the $1.5M spend cap can be achieved by combining Carlos Pena ($0.3M) and Jeremy
# Giambi ($1.065M)

# I'm now looking for my own method for automatically calculating the combinations
# of salary and runs scored. This would be useful if the list of players was extremely
# long, instead of just being five players.

# This use of the 'combn' function for creating permutations works quite
# well, except when it came to pasting names together.
combn(player_selection_data$Player.Name, 2, FUN = paste)
do.call(paste0, expand.grid(player_selection_data$Player.Name, player_selection_data$Player.Name))
combn(player_selection_data$Salary_clean, 2, FUN = sum)
combn(player_selection_data$RS, 2, FUN = sum)

players_comb <- expand.grid(player_selection_data$Player.Name, player_selection_data$Player.Name)
colnames(players_comb) <- c("Player_1", "Player_2")
salary_comb <- expand.grid(player_selection_data$Salary_clean, player_selection_data$Salary_clean)
colnames(salary_comb) <- c("Salary_1", "Salary_2")
rs_comb <- expand.grid(player_selection_data$RS, player_selection_data$RS)
colnames(rs_comb) <- c("rs_1", "rs_2")

anal_comb <- cbind(players_comb, salary_comb, rs_comb)
View(anal_comb)

anal_comb$Salary_comb <- anal_comb$Salary_1 + anal_comb$Salary_2
anal_comb$rs_comb <- anal_comb$rs_1 + anal_comb$rs_2

anal_filter <- anal_comb %>% filter(Salary_comb <= 1500000) %>% arrange(desc(rs_comb))
View(anal_filter)



##################################################
# Quick Question - Playoffs
# https://courses.edx.org/courses/course-v1:MITx+15.071x+2T2017/courseware/f8d71d64418146f18a066d7f0379678c/6324edb8a22c4e35b937490647bfe203/?child=first

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 <- c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
wins2013 <- c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)
length(wins2013)
wins_table <- cbind(teamRank, wins2012, wins2013)
cor(wins_table)
cor(teamRank, wins2012)
cor(teamRank, wins2013)
