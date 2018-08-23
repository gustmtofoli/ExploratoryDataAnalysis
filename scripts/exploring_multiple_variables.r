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

# histogram of prices
ggplot(data = yo, aes(x = price)) +
  geom_histogram()

yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)
summary(yo$all.purchases)

ggplot(data = yo, aes(x = time, y = price)) +
  geom_jitter(alpha = 1/4)

set.seed(1356)
sample.ids <- sample(levels(yo$id), 16)

ggplot(data = subset(yo, id %in% sample.ids), aes(x = time, y = price)) +
  facet_wrap( ~ id) +
  geom_line() +
  geom_point(aes(sizes = all.purchases), pch = 1)


# scatterplot matrices
install.packages('GGally')
library(GGally)

theme_set(theme_minimal(20))
set.seed(1836)
pf_subset <- pf[, c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset), 10), ])

# genes data - heat map
library(reshape2)

nci <- read.table('nci.tsv')
nci.long.samp <- melt(as.matrix(nci[1:200, ]))
names(nci.long.samp) <- c('gene', 'case', 'value')
head(nci.long.samp)

# make the heat map
ggplot(data = nci.long.samp, aes(y = gene, x = case, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = colorRampPalette(c('blue', 'red'))(100))


# diamonds data set
library(ggplot2)
data(diamonds)
summary(diamonds)

ggplot(data = diamonds, aes(y = price, x = carat)) +
  geom_point(alpha = 1/4) +
  stat_smooth(method = 'lm') +
  xlim(0, quantile(diamonds$carat, 0.99)) +
  ylim(0, quantile(diamonds$price, 0.99))
