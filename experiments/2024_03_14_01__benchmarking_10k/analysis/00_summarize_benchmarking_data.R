rm(list = ls())

library(dplyr)

df_all = read.csv('../data/combined_cross_info.csv')
num_trials = 10000

df_summary = df_all[df_all$cross_counter == 1,] %>% dplyr::group_by(leading_edge_index, leading_edge_val) %>% dplyr::summarize(count = dplyr::n())
df_summary$crossed_frac = df_summary$count / num_trials

write.csv(df_summary, '../data/processed_summary.csv', row.names = F)

df_second_cross_summary = df_all[df_all$cross_counter == 2,] %>% dplyr::group_by(leading_edge_index, leading_edge_val) %>% dplyr::summarize(count = dplyr::n())
df_second_cross_summary$crossed_frac = df_second_cross_summary$count / num_trials

write.csv(df_second_cross_summary, '../data/processed_second_cross_summary.csv', row.names = F)
