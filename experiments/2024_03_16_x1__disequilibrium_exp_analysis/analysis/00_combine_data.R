rm(list = ls())

library(Hmisc)

df_exp = read.csv('../../2024_03_16_01__disequilibrium_exp/data/combined_disequilibirium_exp_cross_info.csv')
df_control = read.csv('../../2024_03_16_02__disequilibrium_exp_control/data/combined_disequilibirium_control_cross_info.csv')

trials_per_job = 50
num_jobs = 200
total_trials = trials_per_job * num_jobs

df_cross_summary = data.frame(data = matrix(nrow = 0, ncol = 5))
colnames(df_cross_summary) = c('treatment', 'first_cross_count', 'first_cross_frac', 'second_cross_count', 'second_cross_frac')

exp_first_crosses = sum(df_exp$cross_counter == 1)
exp_first_frac = exp_first_crosses / total_trials
exp_second_crosses = sum(df_exp$cross_counter == 2)
exp_second_frac = exp_second_crosses / total_trials

df_cross_summary[nrow(df_cross_summary) + 1,] = c('Disequilibrium', exp_first_crosses, exp_first_frac, exp_second_crosses, exp_second_frac)

control_first_crosses = sum(df_control$cross_counter == 1)
control_first_frac = control_first_crosses / total_trials
control_second_crosses = sum(df_control$cross_counter == 2)
control_second_frac = control_second_crosses / total_trials

df_cross_summary[nrow(df_cross_summary) + 1,] = c('Control (equilibrium)', control_first_crosses, control_first_frac, control_second_crosses, control_second_frac)

df_cross_summary$first_cross_count = as.numeric(df_cross_summary$first_cross_count)
df_cross_summary$first_cross_frac = as.numeric(df_cross_summary$first_cross_frac)
df_cross_summary$second_cross_count = as.numeric(df_cross_summary$second_cross_count)
df_cross_summary$second_cross_frac = as.numeric(df_cross_summary$second_cross_frac)

df_cross_summary$first_cross_lower_ci_95 = Hmisc::binconf(df_cross_summary$first_cross_count, total_trials, alpha = 0.05, method = 'exact')[,2]
df_cross_summary$first_cross_upper_ci_95 = Hmisc::binconf(df_cross_summary$first_cross_count, total_trials, alpha = 0.05, method = 'exact')[,3]
df_cross_summary$first_cross_lower_ci_99 = Hmisc::binconf(df_cross_summary$first_cross_count, total_trials, alpha = 0.01, method = 'exact')[,2]
df_cross_summary$first_cross_upper_ci_99 = Hmisc::binconf(df_cross_summary$first_cross_count, total_trials, alpha = 0.01, method = 'exact')[,3]
df_cross_summary$second_cross_lower_ci_95 = Hmisc::binconf(df_cross_summary$second_cross_count, total_trials, alpha = 0.05, method = 'exact')[,2]
df_cross_summary$second_cross_upper_ci_95 = Hmisc::binconf(df_cross_summary$second_cross_count, total_trials, alpha = 0.05, method = 'exact')[,3]
df_cross_summary$second_cross_lower_ci_99 = Hmisc::binconf(df_cross_summary$second_cross_count, total_trials, alpha = 0.01, method = 'exact')[,2]
df_cross_summary$second_cross_upper_ci_99 = Hmisc::binconf(df_cross_summary$second_cross_count, total_trials, alpha = 0.01, method = 'exact')[,3]


contingency_table = matrix(
  c(exp_first_crosses, control_first_crosses, total_trials - exp_first_crosses, total_trials - control_first_crosses), 
  nrow = 2
)
cat('Contingency table: ')
print(contingency_table)
cat('Fisher\'s exact:\n')
print(fisher.test(contingency_table))

data_dir = '../data'
if(!dir.exists(data_dir)) dir.create(data_dir)

output_file = paste0(data_dir, '/disequilibrium_exp_summary_data.csv')
write.csv(df_cross_summary, output_file, row.names = F)
print(df_cross_summary)
cat('Table saved to file: ', output_file, '\n')

