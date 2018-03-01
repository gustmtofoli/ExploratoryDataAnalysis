library(plyr)

procedures_with_outliers = 0
procedures <- read.csv('exportar_procedimento_executado.csv', sep = '\t')
for(cp in sort(unique(procedures$CODIGO_PROCEDIMENTO))) {
  procedures_subset <- subset(procedures, procedures$CODIGO_PROCEDIMENTO == cp)
  outliers <- boxplot.stats(procedures_subset$VALOR_TOTAL)$out
  if (length(outliers) != 0) {
    procedures_with_outliers <- procedures_with_outliers + 1
    df_codproc_outliers <- data.frame(CODIGO_PROCEDIMENTO = c(cp), OUTLIERS = c(outliers))
    df_codproc_outliers_count <- count(df_codproc_outliers, c("CODIGO_PROCEDIMENTO", "OUTLIERS"))
    df_codproc_outliers_aggregate <- aggregate(df_codproc_outliers_count$freq, by = list(df_codproc_outliers_count$CODIGO_PROCEDIMENTO), FUN = sum)
    print(df_codproc_outliers_count)
  }
}
procedures_with_outliers