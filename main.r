library(Hmisc)

dataset <- read.csv("COVID19_line_list_data.csv")
View(dataset)
describe(dataset)

# Cleaned Up Death Column
dataset$death_dummy <- as.integer(dataset$death != 0)
unique(dataset$death_dummy)

# Death Rate
sum(dataset$death_dummy) / nrow(dataset)

# AGE
# Claim : People who die are older than people who survive
dead = subset(dataset, death_dummy == 1)
alive = subset(dataset, death_dummy == 0)
mean(dead$age, na.rm=TRUE)
mean(alive$age, na.rm=TRUE)
# Therefore, mean age of people who are dead is greater than mean age of people who are alive

# To prove the above statistically, we'll do t-test
t.test(alive$age, dead$age, alternative="two.sided", conf.level=0.95)
# From the 95% confidence interval, we can see that age of person who survives is much less than the one who lives (almost 16-24 years less)
# If p-value < 0.5, reject Null hypothesis
# here p-value ~ 0, so we reject the null hypothesis

# GENDER
# Claim : Gender has not effect
men = subset(dataset, gender == "male")
women = subset(dataset, gender == "female")
mean(men$death_dummy, na.rm=TRUE)   # 8.5%
mean(women$death_dummy, na.rm=TRUE) # 3.7%

# To prove the above statistically, we'll do t-test
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level=0.95)
# 99% Confidence
# Men have 0.8% to 8.8% higher chance of dying
# p-value = 0.002 < 0.5, so Reject Null Hypothesis 