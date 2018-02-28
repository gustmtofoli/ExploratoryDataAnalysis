values <- read.csv('test-outliers.csv')
outliers_values <- boxplot.stats(values$valor.procedimento)$out # get outliers
outliers_values

outliers_info <- subset(values, values$valor.procedimento == outliers_values)

# =========================================================================

# obter outliers de um grupo de valores já filtrado
procedures <- read.csv('exportar_procedimento_executado.csv', sep = '\t')
procedures_sub <- subset(procedures,procedures$CODIGO_PROCEDIMENTO == "40302423" & !is.null(procedures$VALOR_TOTAL))
procedures_sub$VALOR_TOTAL

n <- length(procedures_sub$VALOR_TOTAL) # quantidade de elementos no vetor

index <- c(factor(1:n)) # cria vetor de 1 até quantidade de elementos no vetor

outliers_values <- boxplot.stats(procedures_sub$VALOR_TOTAL)$out
outliers_values

# TODO agrupar grupos automaticamente