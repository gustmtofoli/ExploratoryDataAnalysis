getwd()

statesInfo <- read.csv('stateData.csv')
statesInfo

stateSubset <- subset(statesInfo, state.region == 1)
stateSubset <- subset(stateSubset, highSchoolGrad > 50)
stateSubset

head(stateSubset, 1) # segundo parametro é a quantidade de linhas
dim(stateSubset)

stateSubsetBracket <- statesInfo[statesInfo$state.region == 1, ]
stateSubsetBracket

qplot(data = reddit, x = income.range)

# ================================================

library(ggplot2)
library(ggthemes)
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
names(pf) # exibe todas as variáveis
qplot(data = pf, x = dob_day) +
  scale_x_continuous(breaks = 1:31) +
  facet_wrap(~dob_month, ncol = 4)

qplot(data = pf, x = friend_count, xlim = c(1,1000))

qplot(data = pf, x = friend_count) +
  scale_x_continuous(limits = c(1, 1000))

