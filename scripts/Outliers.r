values <- read.csv('test-outliers.csv')
outliers_values <- boxplot.stats(values$valor.procedimento)$out # get outliers
outliers_values

outliers_info <- subset(values, values$valor.procedimento == outliers_values)