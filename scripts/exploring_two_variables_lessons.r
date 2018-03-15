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
  geom_jitter(alpha = 1/20, position = position_jitter(h = 0)) +
  xlim(13, 90) +
  coord_trans(y = 'sqrt')


ggplot(data = pf, aes(x = age, y = friendships_initiated)) +
  geom_jitter(alpha = 1/20, position = position_jitter(h = 0)) +
  xlim(13, 113) +
  coord_trans(y = 'sqrt')


library(dplyr)

group_by(pf, age)

