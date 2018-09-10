install.packages('GGally')
install.packages('scales')
install.packages('memisc')
install.packages('lattice')
install.packages('MASS')
install.packages('car')
install.packages('reshape')
install.packages('plyr')

library(ggplot2)
library(GGally)
library(scales)
library(memisc)
library(gridExtra)

set.seed(20022012)

diamond_samp <- diamonds[sample(1:length(diamonds$price), 10)]


plot1 <- qplot(data = diamonds, x = diamonds$price, binwidth = 100) +
  ggtitle('Price')

plot2 <- qplot(diamonds, x = diamonds$price, binwidth = 0.01) +
  ggtitle('Price log 10') +
  scale_x_log10()

grid.arrange(plot1, plot2)

