rm(list = ls())

library(ggplot2)

script_id = '03'

plot_replay_with_leading_edge = function(rep_id, processed_data_dir, plot_dir){
  rep_plot_dir = paste0(plot_dir, '/reps/', rep_id)
  
  # Load processed data
  df_rep_summary = read.csv(paste0(processed_data_dir, '/processed_summary_rep_', rep_id, '.csv'))  
  
  # Load snapshot data for leading edge info
  df_snapshot = read.csv(paste0('../data/reps/', rep_id, '/snapshot_data.csv'))

  # Plot and save! 
  ggplot(df_snapshot, aes(x = update, y = leading_edge_index)) + 
    geom_abline(aes(intercept = 8, slope = 1), linetype = 'dashed', alpha = 0.5) + 
    geom_line(alpha = 0.5) + 
    xlab('Update') + 
    ylab('Index of the leading edge')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_00__leading_edge.png'), units = 'in', width = 8, height = 6)
  
  ggplot(df_rep_summary, aes(x = replay_gen, y = frac_crossed)) + 
    geom_abline(aes(intercept = 8 / 512, slope = 1/512), linetype = 'dashed', alpha = 0.5) + 
    geom_line(data = df_snapshot, aes(x = update, y = leading_edge_index / 512), alpha = 0.5) + 
    geom_line() + 
    geom_point() + 
    scale_y_continuous(limits = c(0,1)) + 
    xlab('Update') + 
    ylab('Potential to cross')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_01__replays_with_leading_edge.png'), units = 'in', width = 8, height = 6)
}



processed_data_dir = '../data/processed'
plot_dir = '../plots'

## Plot the replicates that cross twice
#for(rep_id in c('093', '124', '138', '263')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_replay_with_leading_edge(rep_id, processed_data_dir, plot_dir)
#}

## Plot some replicates that cross once
#for(rep_id in c('011', '014', '023', '026', '049')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_replay_with_leading_edge(rep_id, processed_data_dir, plot_dir)
#}

# Plot our 10 randomly-selected no-cross reps
for(rep_id in c('134',  '158', '164', '175', '252', '339', '365', '394', '446', '450')){
  cat('Rep id: ', rep_id, '\n')
  plot_replay_with_leading_edge(rep_id, processed_data_dir, plot_dir)
}