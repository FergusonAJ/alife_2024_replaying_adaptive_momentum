rm(list = ls())

library(ggplot2)

script_id = '02'

summarize_benchmarking = function(df_benchmarking){
  df_benchmarking_summary = df_benchmarking[df_benchmarking$cross_counter == 1,] %>% 
    dplyr::group_by(slurm_task_id, leading_edge_index, leading_edge_val) %>%
    dplyr::summarize(count = dplyr::n())
  df_benchmarking_summary$num_total = 10^4
  df_benchmarking_summary$frac_crossed = df_benchmarking_summary$count / df_benchmarking_summary$num_total
  return(df_benchmarking_summary)
}

plot_replay_with_benchmark = function(rep_id, processed_data_dir, plot_dir){
  rep_plot_dir = paste0(plot_dir, '/reps/', rep_id)
  
  # Load processed data
  df_rep_summary = read.csv(paste0(processed_data_dir, '/processed_summary_rep_', rep_id,'.csv'))  
  # Load and process benchmarking data
  df_benchmarking_summary = read.csv('../../2024_03_14_01__benchmarking_10k/data/processed_summary.csv')

  # Plot and save!
  ggplot(df_rep_summary, aes(x = replay_gen, y = frac_crossed)) + 
    geom_line(data = df_benchmarking_summary, aes(x = leading_edge_index, y = crossed_frac, color = as.factor(leading_edge_val))) + 
    geom_line() + 
    geom_point() + 
    scale_y_continuous(limits = c(0,1)) + 
    xlab('Update') + 
    ylab('Potential to cross')
  ggsave(paste0(rep_plot_dir, '/script_01__plot_', script_id, '__replays_with_naive_benchmarking.png'), units = 'in', width = 8, height = 6)
  
}


processed_data_dir = '../data/processed'
plot_dir = '../plots'

## Plot the replicates that cross twice
#for(rep_id in c('093', '124', '138', '263')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_replay_with_benchmark(rep_id, processed_data_dir, plot_dir)
#}

## Plot some replicates that cross once
#for(rep_id in c('011', '014', '023', '026', '049')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_replay_with_benchmark(rep_id, processed_data_dir, plot_dir)
#}

## Plot our 10 randomly-selected no-cross reps
#for(rep_id in c('134',  '158', '164', '175', '252', '339', '365', '394', '446', '450')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_replay_with_benchmark(rep_id, processed_data_dir, plot_dir)
#}

# Plot our 10 randomly-selected single-cross reps
for(rep_id in c('011','050','075','083','105','282','343','400','408','415')){
  cat('Rep id: ', rep_id, '\n')
  plot_replay_with_benchmark(rep_id, processed_data_dir, plot_dir)
}