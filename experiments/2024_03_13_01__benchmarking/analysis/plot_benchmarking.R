rm(list = ls())

library(ggplot2)
library(dplyr)

df_all = read.csv('../data/combined_cross_info.csv', header = F)

colnames(df_all) = c('slurm_task_id', 'leading_edge_index', 'leading_edge_val', 'relative_update', 'cross_id')

df_summary = df_all[df_all$cross_id == 1,] %>% dplyr::group_by(leading_edge_index, leading_edge_val) %>% dplyr::summarize(count = dplyr::n())

ggplot(df_summary, aes(x = leading_edge_index, y = count, color = as.factor(leading_edge_val))) + 
  geom_line() + 
  geom_point()
