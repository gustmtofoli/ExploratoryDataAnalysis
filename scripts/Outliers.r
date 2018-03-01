library(plyr)

L_procedures <- c()
L_values <- c()
L_freq <- c()
procedures <- read.csv('exportar_procedimento_executado.csv', sep = '\t')
for(cp in sort(unique(procedures$CODIGO_PROCEDIMENTO))) {
  procedures_subset <- subset(procedures, procedures$CODIGO_PROCEDIMENTO == cp)
  outliers <- boxplot.stats(procedures_subset$VALOR_TOTAL)$out
  if (length(outliers) != 0) {
    df_codproc_outliers <- data.frame(CODIGO_PROCEDIMENTO = c(cp), OUTLIERS = c(outliers))
    df_codproc_outliers_count <- count(df_codproc_outliers, c("CODIGO_PROCEDIMENTO", "OUTLIERS"))
    #df_codproc_outliers_aggregate <- aggregate(df_codproc_outliers_count$freq, by = list(df_codproc_outliers_count$CODIGO_PROCEDIMENTO), FUN = sum)
    L_procedures <- c(L_procedures, df_codproc_outliers_count$CODIGO_PROCEDIMENTO)
    L_values <- c(L_values, df_codproc_outliers_count$OUTLIERS)
    L_freq <- c(L_freq, df_codproc_outliers_count$freq)
  }
}

df_codproc_ouliers_freq <- data.frame(CODIGO_PROCEDIMENTO = L_procedures, 
                                      OUTLIERS = L_values, 
                                      FREQ = L_freq)
df_codproc_ouliers_freq
df_codproc_outliers_aggregate <- aggregate(df_codproc_ouliers_freq$FREQ, by = list(df_codproc_ouliers_freq$CODIGO_PROCEDIMENTO), FUN = sum)
df_codproc_outliers_aggregate[order(df_codproc_outliers_aggregate$x, decreasing = TRUE), ]