library(ggplot2)

pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

# boxplot of gender and age by gender with stat_summary(mean)
ggplot(data = subset(pf, !is.na(gender)), aes(x = gender, y = age)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = 'point', shape = 4)

# median of friends of each gender by age
ggplot(data = subset(pf, !is.na(gender)), aes(x = age, y = friend_count)) +
  geom_line(aes(color = gender), stat = "summary", fun.y = median)

library(dplyr)

# group by age and gender using filter function
group_by_age_gender <- group_by(filter(pf, !is.na(gender)), age, gender)

# get the mean and median of friends of each gender by age
pf.fc_by_age_gender <- summarise(group_by_age_gender, 
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n())

# ungorup the data
pf.fc_by_age_gender <- ungroup(pf.fc_by_age_gender)

# sort by age
pf.fc_by_age_gender <- arrange(pf.fc_by_age_gender, age)

head(pf.fc_by_age_gender, 10)

ggplot(data = pf.fc_by_age_gender, aes(x = age, y = friend_count_mean)) +
  geom_line(aes(color = gender))

library(reshape2)
install.packages("tidyr")
library(tidyr)

# dcast function. get the median of number of friends of each gender grouped by age 
pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender, 
                                  age ~ gender, 
                                  value.var = 'friend_count_median')
head(pf.fc_by_age_gender.wide)

# plot the ratio of female and male users
ggplot(data = pf.fc_by_age_gender.wide, aes(x = age, y = female/male)) +
  geom_line() +
  geom_hline(yintercept = 1, alpha = 0.3, linetype = 2)

# year that each user joined facebook.
pf$year_joined <- floor(2014 - pf$tenure/365)
summary(pf$year_joined)
table(pf$year_joined)

# cut function ... year that each user joined facebook grouped by year "buckets"
pf$year_joined.bucket <- cut(pf$year_joined, 
                             c(2004, 2009, 2011, 2012, 2014))

table(pf$year_joined.bucket, useNA = 'ifany')

ggplot(data = subset(pf, !is.na(year_joined.bucket)), aes(x = age, y = friend_count)) +
  geom_line(aes(color = year_joined.bucket), stat = "summary", fun.y = mean) +
  geom_line(stat = "summary", fun.y = mean, linetype = 2)


# how many friends users have for each day since they joined the facebook
labels(pf)

with(data = subset(pf, tenure >= 1), summary(friend_count / tenure))


# Create a line graph of mean of friendships_initiated per day (of tenure)
# vs. tenure colored by year_joined.bucket.

# You need to make use of the variables tenure,
# friendships_initiated, and year_joined.bucket.

# You also need to subset the data to only consider user with at least
# one day of tenure.

ggplot(data = subset(pf, tenure >= 1), 
       aes(x = 7*round(tenure/7), y = friendships_initiated / tenure)) +
  geom_line(aes(color = year_joined.bucket), stat = "summary", fun.y = mean)

# with smooth
ggplot(data = subset(pf, tenure >= 1), 
       aes(x = 7*round(tenure/7), y = friendships_initiated / tenure)) +
  geom_smooth(aes(color = year_joined.bucket))


#====================================================

# yogurt dataset

yo <- read.csv("yogurt.csv")
summary(yo)
str(yo)

yo$id <- factor(yo$id)

labels(yo)

#histogram of prices
ggplot(data = yo, aes(x = price)) +
  geom_histogram()
