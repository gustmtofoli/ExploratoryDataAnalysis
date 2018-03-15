library(ggplot2)
data(diamonds)

data(diamonds)

qplot(data = diamonds, x = diamonds$price, binwidth = 10,
      color = I("black")) +
  scale_x_continuous(limits = c(300, 2000), breaks = seq(300, 2000, 100))

max(diamonds$price)

qplot(data = diamonds, x = diamonds$price, binwidth = 100,
      color = I("black")) +
  facet_wrap(~cut)

by(diamonds$price, diamonds$cut, summary)
