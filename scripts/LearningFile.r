#==============================================================================
# ler arquivo csv
values <- read.csv('test-outliers.csv')

# ler arquivo csv com separador customizado
procedures <- read.csv('exportar_procedimento_executado.csv', sep = '\t')
#==============================================================================

#==============================================================================
# subconjunto de um dataframe
outliers_info <- subset(values, values$valor.procedimento == outliers_values) 

# combinação de condições para extrair um subconjunto
procedures_sub <- subset(procedures,procedures$CODIGO_PROCEDIMENTO == "40302423" & !is.null(procedures$VALOR_TOTAL))
#==============================================================================

#==============================================================================
# quantidade de elementos no vetor
n <- length(procedures_sub$VALOR_TOTAL) 

# cria vetor de 1 até quantidade de elementos no vetor
index <- c(factor(1:n)) 
#==============================================================================

#==============================================================================
# get outliers
outliers_values <- boxplot.stats(procedures_sub$VALOR_TOTAL)$out
#==============================================================================

#==============================================================================
# retorna um "distinct"
unique_group_procedures <- unique(procedures$CODIGO_PROCEDIMENTO)

# ordenar por coluna
procedures_sub[order(procedures_sub$CODIGO_PROCEDIMENTO, decreasing = FALSE), ] 

# soma do VALOR_TOTAL de todas as vezes que determinado CODIGO_PROCEDIMENTO ocorreu
aggregate(procedures$VALOR_TOTAL, by=list(procedures$CODIGO_PROCEDIMENTO), FUN = sum)  

# número de vezes que uma variável ocorreu
count(procedures, "CODIGO_PROCEDIMENTO") 
count(procedures, "VALOR_TOTAL")
