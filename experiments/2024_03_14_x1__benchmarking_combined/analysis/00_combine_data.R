rm(list = ls())

library(dplyr)

data_dir = '../data'
if(!dir.exists(data_dir)){
  dir.create(data_dir)
}

total_count = 2 * (10^4)

df_batch_1 = read.csv('../../2024_03_14_01__benchmarking_10k/data/processed_summary.csv')
df_batch_1$extra_orgs = 7
df_batch_2 = read.csv('../../2024_03_15_01__benchmarking_10k_batch_2/data/processed_summary.csv')
df_batch_2$extra_orgs = 7
df_single_org = read.csv('../../2024_03_21_01__benchmarking_1_org/data/processed_summary.csv')
df_single_org$extra_orgs = 0
df_12_org = read.csv('../../2024_04_07_01__benchmarking_12_orgs/data/processed_summary.csv')
df_12_org$extra_orgs = 11

df_combined = rbind(df_batch_1, df_batch_2, df_single_org, df_12_org)
write.csv(df_combined, paste0(data_dir, '/combined_benchmarking_data.csv'), row.names = F)

df_summary = df_combined[df_combined$extra_orgs == 7,] %>%
  dplyr::group_by(leading_edge_val, leading_edge_index) %>%
  dplyr::summarize(count = sum(count))

df_summary$crossed_frac = df_summary$count / total_count


write.csv(df_summary, paste0(data_dir, '/summarized_benchmarking_data.csv'), row.names = F)
