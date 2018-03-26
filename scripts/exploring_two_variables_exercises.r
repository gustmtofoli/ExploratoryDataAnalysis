library(ggplot2)
data(diamonds)

ggplot(data = diamonds, aes(x = price, y = x)) +
  geom_point()

with(diamonds, cor.test(price, x, method = 'pearson')) # 0.88

with(diamonds, cor.test(price, y, method = 'pearson')) # 0.86

with(diamonds, cor.test(price, z, method = 'pearson')) # 0.86

ggplot(data = diamonds, aes(x = price, y = depth)) +
  geom_point()

ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(0, 80, 2))

with(diamonds, cor.test(depth, price, method = 'pearson'))

ggplot(data = diamonds, aes(x = price, y = carat)) +
  geom_point() +
  xlim(0, quantile(diamonds$price, .99)) +
  ylim(0, quantile(diamonds$carat, .99))

