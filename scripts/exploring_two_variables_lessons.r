library(ggplot2)

pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

# por default, quando atribuimos duas variáveis no qplot, é utilizado gráfico de dispersão
qplot(data = pf, x = age, y = friend_count)

# usando ggplot'
# 'jitter' para excesso de plotagem no gráfico
# 'alpha' é o grau de transparência dos pontos. (1 ponto completamente preto a cada 20 ocorrencias)
ggplot(data = pf, aes(x = age, y = friend_count)) + 
  geom_jitter(alpha = 1/20) +
  xlim(13, 90)

summary(pf$age)

# adicionando uma transformação raiz quadrada no gráfico.
# 'position' permite adicionar jitter juntamente com transformações de escala
ggplot(data = pf, aes(x = age, y = friend_count)) + 
  geom_jitter(alpha = 1/20, position = position_jitter(h = 0), color = 'orange') +
  xlim(13, 90) +
  coord_trans(y = 'sqrt') +
  geom_line(stat = 'summary', fun.y = mean)



ggplot(data = pf, aes(x = age, y = friendships_initiated)) +
  geom_jitter(alpha = 1/20, position = position_jitter(h = 0)) +
  xlim(13, 113) +
  coord_trans(y = 'sqrt')
  


library(dplyr)

# agrupa por alguma variável, no caso, 'age'
age_groups <- group_by(pf, age)

# mostra a idade, mediana do número de amigos, média do número de amigos e número de usuários que pertence a esse grupo
pf.fc_by_age <- summarise(age_groups, 
          friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),
          n = n())

# ordena por idade
pf.fc_by_age <- arrange(pf.fc_by_age, age)

# mostra 20 primeiras linhas
head(pf.fc_by_age, 20)
#or
pf.fc_by_age_2 <- pf %>% 
  group_by(age) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  arrange(age)

head(pf.fc_by_age_2, 20)

# adicionando linha entre os pontos do gráfico de dispersão
ggplot(data = pf.fc_by_age, aes(x = age, y = friend_count_mean)) +
  geom_line()

# adicionando linhas da média, 10%, 50%, 90% no gráfico de dispersão
ggplot(data = pf, aes(x = age, y = friend_count)) + 
  geom_jitter(alpha = 1/20, position = position_jitter(h = 0), color = 'orange') +
  xlim(13, 90) +
  coord_trans(y = 'sqrt') +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = 'blue')

# adicionando linhas da média, 10%, 50%, 90% no gráfico de dispersão e dando zoom
ggplot(data = pf, aes(x = age, y = friend_count)) + 
  geom_jitter(alpha = 1/20, position = position_jitter(h = 0), color = 'orange') +
  coord_cartesian(xlim = c(13, 70), ylim = c(0, 1000)) +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = 'blue')


# CORRELAÇÃO

# maior que 0.3 - significativa, mas pequena
# 0.5 - moderada
# 0.7 ou maior - grande

?cor.test

cor.test(pf$age, pf$friend_count, method = 'pearson')
#or
with(pf, cor.test(age, friend_count, method = 'pearson'))

# retirando idade mais avançada, porque a probabilidade de serem dados incorretos é grande
with(subset(pf, age <= 70), cor.test(age, friend_count, method = 'pearson'))

# é imprudente dizer que a medida que a idade decresce o número de amigos tbm (relação negativa)
# seria necessário usar estatística inferencial ao invés de estatística descritiva

# utlizando a relação de spearman
with(subset(pf, age <= 70), cor.test(age, friend_count, method = 'spearman'))

# likes_received vs www_likes_received
summary(pf$likes_received)
summary(pf$www_likes_received)

ggplot(data = pf, aes(x = www_likes_received, y = likes_received)) +
  geom_point(color = 'black')

ggplot(data = pf, aes(x = www_likes_received, y = likes_received)) +
  geom_point(color = 'black') +
  xlim(0, quantile(pf$www_likes_received, .95)) +
  ylim(0, quantile(pf$likes_received, .95)) +
  geom_smooth(method = 'lm', color = 'red')

# correlação de 0.948 - não significa que as duas variáveis são tão correlatas. Na verdade esse
# número é alto porque um conjunto é um subconjunto do outro. (natureza das variáveis)
with(pf, cor.test(likes_received, www_likes_received))

# =========================================================

library(lme4)
library(alr3)

data(Mitchel)


# =========================================================

# média do número de amigos pela idade por mês
# agrupa por alguma variável, no caso, 'age'
age_groups <- group_by(pf, age)

# mostra a idade, mediana do número de amigos, média do número de amigos e número de usuários que pertence a esse grupo
pf.fc_by_age <- summarise(age_groups, 
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n())

# ordena por idade
pf.fc_by_age <- arrange(pf.fc_by_age, age)

plot_by_age <- ggplot(data = subset(pf.fc_by_age, age < 71), aes(x = age, y = friend_count_mean)) +
  geom_line() +
  geom_smooth()

head(pf.fc_by_age, 10)

pf.fc_by_age[17:19, ]



# média do número de amigos pela idade por mês
pf$age_with_months <- pf$age + (12 - pf$dob_month)/12

month_groups <- group_by(pf, age_with_months)

pf.fc_by_month <- summarise(month_groups, 
                            friend_count_mean_month = mean(friend_count),
                            friend_count_median_month = median(friend_count),
                            n = n())

arrange(pf.fc_by_month, age_with_months)

head(pf.fc_by_month, 10)

plot_by_month <- ggplot(data = subset(pf.fc_by_month, age_with_months < 71), aes(x = age_with_months, y = friend_count_mean_month)) +
  geom_line() +
  geom_smooth()

plot_by_month_five <-  ggplot(data = subset(pf, age < 71), aes(x = round(age / 5) * 5, y = friend_count)) +
  geom_line(stat = "summary", fun.y = mean)


library(gridExtra)

grid.arrange(plot_by_age, plot_by_month, plot_by_month_five, ncol = 1)
