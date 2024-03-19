rm(list = ls())

library(dplyr)

total_count = 2 * (10^4)

df_batch_1 = read.csv('../../2024_03_14_01__benchmarking_10k/data/processed_summary.csv')
df_batch_2 = read.csv('../../2024_03_15_01__benchmarking_10k_batch_2/data/processed_summary.csv')

df_combined = rbind(df_batch_1, df_batch_2)
df_summary = df_combined %>%
  dplyr::group_by(leading_edge_val, leading_edge_index) %>%
  dplyr::summarize(count = sum(count))

df_summary$crossed_frac = df_summary$count / total_count

data_dir = '../data'
if(!dir.exists(data_dir)){
  dir.create(data_dir)
}

write.csv(df_summary, paste0(data_dir, '/combined_benchmarking_data.csv'), row.names = F)
