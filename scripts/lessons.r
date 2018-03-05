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

# exibe todas as variáveis
names(pf) 

# gráficos para cada mês em função do dia do aniversário dos usuários
qplot(data = pf, x = dob_day) +
  scale_x_continuous(breaks = 1:31) + # quebra eixo x de 1 a 31 (dias do mês)
  facet_wrap(~dob_month, ncol = 4)

# gráfico com intervalo no eixo x definido
qplot(data = pf, x = friend_count, xlim = c(1,1000))
# or
qplot(data = pf, x = friend_count) +
  scale_x_continuous(limits = c(1, 1000))

# gráfico com escala no eixo x de 50 em 50 e "colunas" quebrando de 25 em 25
qplot(data = pf, x = friend_count, binwidth = 25) +
  scale_x_continuous(limits = c(1, 1000), breaks = seq(0, 1000, 50))
  
# gráfico de número de amigos para cada gênero
qplot(data = pf, x = friend_count, binwidth = 25) +
  scale_x_continuous(limits = c(1, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender)

# gráfico de número de amigos para cada gênero, exceto NA
qplot(data = subset(pf, !is.na(gender)), x = friend_count, binwidth = 25) +
  scale_x_continuous(limits = c(1, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender)
  