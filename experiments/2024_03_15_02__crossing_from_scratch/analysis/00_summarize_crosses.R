rm(list = ls())

df_combined = read.csv('../data/combined_cross_info.csv')

num_trials = 1000
num_reps = 500
num_total_trials = num_reps * num_trials

df_cross_summary = data.frame(data = matrix(nrow = 0, ncol = 4))
colnames(df_cross_summary) = c('cross_count', 'num_crossed', 'total_trials', 'frac_crossed')

for(cross_count in 1:6){
  num_crossed = sum(df_combined$cross_counter == cross_count)
  cat('Number of trials that crossed ', cross_count, ' times: ', num_crossed, '\n')
  frac_crossed = num_crossed / num_total_trials
  cat('Fraction of trials that crossed ', cross_count, ' times: ', frac_crossed, '\n')
  df_cross_summary[nrow(df_cross_summary) + 1,] = c(cross_count, num_crossed, num_total_trials, frac_crossed)
}

data_dir = '../data'
if(!dir.exists(data_dir)){ dir.create(data_dir) }

write.csv(df_cross_summary, paste0(data_dir, '/cross_summary.csv'), row.names = F)