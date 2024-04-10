rm(list = ls())

library(ggplot2)

df_combined = NA
for(rep_id in c('011','050','075','083','105','282','343','400','408','415')){
  df_rep = read.csv(paste0('../data/processed/processed_summary_rep_', rep_id, '.csv'))
  df_rep$rep_id = rep_id
  if(!is.data.frame(df_combined)){
    df_combined = df_rep
  } else {
    df_combined = rbind(df_combined, df_rep)
  }
}

ggplot(df_combined, aes(x = frac_crossed)) + 
  geom_histogram(bins=30)


df_second_cross_combined = NA
for(rep_id in c('011','050','075','083','105','282','343','400','408','415')){
  df_rep = read.csv(paste0('../data/processed/processed_second_cross_summary_rep_', rep_id, '.csv'))
  df_rep$rep_id = rep_id
  if(!is.data.frame(df_second_cross_combined)){
    df_second_cross_combined = df_rep
  } else {
    df_second_cross_combined = rbind(df_second_cross_combined, df_rep)
  }
}

ggplot(df_second_cross_combined, aes(x = frac_crossed)) + 
  geom_histogram(bins=30) +
  scale_x_continuous(limits = c(0,1))
