rm(list = ls())

df_combined = read.csv('../data/combined_cross_info.csv')

num_trials = 1000
num_reps = 500
num_total_trials = num_reps * num_trials

df_cross_summary = data.frame(data = matrix(nrow = 0, ncol = 8))
colnames(df_cross_summary) = c('cross_count', 'num_crossed', 'total_trials', 'frac_crossed', 'cross_time_mean', 'cross_time_median', 'cross_time_min', 'cross_time_max')

for(cross_count in 1:6){
  num_crossed = sum(df_combined$cross_counter == cross_count)
  if(num_crossed == 0){
    df_cross_summary[nrow(df_cross_summary) + 1,] = c(cross_count, 0, num_total_trials, 0, 0, 0, 0, 0)
    break
  }
  cat('Number of trials that crossed ', cross_count, ' times: ', num_crossed, '\n')
  frac_crossed = num_crossed / num_total_trials
  cat('Fraction of trials that crossed ', cross_count, ' times: ', frac_crossed, '\n')
  mean_cross_time = mean(df_combined[df_combined$cross_counter == cross_count,]$relative_update)
  cat('Mean time of cross #', cross_count, ': ' , mean_cross_time, '\n')
  median_cross_time = median(df_combined[df_combined$cross_counter == cross_count,]$relative_update)
  cat('Median time of cross #', cross_count, ': ' , median_cross_time, '\n')
  min_cross_time = min(df_combined[df_combined$cross_counter == cross_count,]$relative_update)
  cat('Minimum time of cross #', cross_count, ': ' , min_cross_time, '\n')
  max_cross_time = max(df_combined[df_combined$cross_counter == cross_count,]$relative_update)
  cat('Maximum time of cross #', cross_count, ': ' , max_cross_time, '\n')
  
  df_cross_summary[nrow(df_cross_summary) + 1,] = c(cross_count, 
                                                    num_crossed, 
                                                    num_total_trials, 
                                                    frac_crossed, 
                                                    mean_cross_time,
                                                    median_cross_time,
                                                    min_cross_time, 
                                                    max_cross_time
                                                    )
}

data_dir = '../data'
if(!dir.exists(data_dir)){ dir.create(data_dir) }

write.csv(df_cross_summary, paste0(data_dir, '/cross_summary.csv'), row.names = F)
