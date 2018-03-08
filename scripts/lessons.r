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

# total de homens e mulheres
table(pf$gender)  

# apresenta dados estatísticos sobre a quantidade de amigos para cada genero
by(pf$friend_count, pf$gender, summary)

# apresenta dados estatísticos sobre uma variável
summary(pf$age)

# gráfico azul com borda preta de tenure em função dos dias
qplot(data = pf, x = tenure, binwidth = 30, color = I('black'), fill = I('#099DD9'))

# adicionando cor de preenchimento e borda e labels no gráfico
qplot(data = pf, x = tenure/365, binwidth = .25, 
      xlab = 'Numbers of years using facebook',
      ylab = 'Number of users in sample',
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(breaks = seq(1, 7, 1), limits = c(0, 7))
 
# gráfico da idade dos usuários do facebook com mudança de cor na borda e fill
qplot(data = pf, x = age, binwidth = 1, color = I('#0D47A1'), fill = I('#2196F3'), 
      xlab = 'Age', ylab = 'Number of users in sample') +
  scale_x_continuous(breaks = seq(13, 113, 5), limits = c(13, 113))

# cria gráticos com escalas de log e sqrt e plota vários vários gráficos em uma só janela
library(gridExtra)
summary(log10(pf$friend_count+1))
summary(sqrt(pf$friend_count))
p1 <- qplot(data = pf, x = friend_count)
p2 <- qplot(data = pf, x = log10(friend_count) + 1)
p3 <- qplot(data = pf, x = sqrt(friend_count))
grid.arrange(p1, p2, p3)
#or
pl1 <- ggplot(data = pf, aes(x = friend_count)) + geom_histogram()
pl2 <- pl1 + scale_x_log10()
pl3 <- pl1 + scale_x_sqrt()
grid.arrange(pl1, pl2, pl3)

# gráfico de polígonos de frequencia para gênero
qplot(data = subset(pf, !is.na(pf$gender)), x = friend_count, binwidth = 10,
      geom = 'freqpoly', color = gender) +
  scale_x_continuous(breaks = seq(0, 1000, 50), limits = c(0, 1000))

# mudando a escala do gráfico para obter mais detalhes
qplot(data = subset(pf, !is.na(pf$gender)), x = www_likes, geom = 'freqpoly', color = gender) +
  scale_x_continuous() +
  scale_x_log10()

# soma dos likes de agrupada por genero
by(pf$www_likes, pf$gender, sum)

# cria um gráfico boxplot di bpyneri de amigos por genero com correção das coordenadas cartesianas quando limita os dados
qplot(data = subset(pf, !is.na(pf$gender)), x = gender, y = friend_count, geom = "boxplot") +
  coord_cartesian(ylim = c(0, 250))

# 75% das mulheres tem menos de 244 amigos (terceiro quartil)
# 75% dos homens tem menos de 182 amigos (terceiro quartil)
by(pf$friend_count, pf$gender, summary)

# cria um gráfico boxplot do número de inicializçaões de amizade por genero
qplot(data = subset(pf, !is.na(pf$gender)), x = gender, y = friendships_initiated, geom = "boxplot") +
  coord_cartesian(ylim = c(0, 150))

by(pf$friendships_initiated, pf$gender, summary)

