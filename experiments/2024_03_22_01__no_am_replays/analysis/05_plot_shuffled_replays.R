rm(list = ls())

library(ggplot2)
library(dplyr)

script_id = '05'

summarize_replay_data = function(df_rep, num_total_reps){
  df_summary = df_rep[df_rep$cross_counter == 1,] %>% 
    dplyr::group_by(slurm_task_id, replay_gen) %>%
    dplyr::summarize(count = dplyr::n())
  df_summary$frac_crossed = df_summary$count / num_total_reps
  return(df_summary)
}

create_dirs = function(processed_data_dir, plot_dir){
  if(!dir.exists(processed_data_dir)){
    dir.create(processed_data_dir)
  }
  if(!dir.exists(plot_dir)){
    dir.create(plot_dir, recursive = T)
  }
}


plot_shuffled_replay_data = function(rep_id, num_total_reps, processed_data_dir, plot_dir){
  # Create our output directory structure
  rep_plot_dir = paste0(plot_dir, '/reps/', rep_id)
  create_dirs(processed_data_dir, rep_plot_dir)
  
  # Load our standard replay data (Already processed)
  df_base_summary = read.csv(paste0(processed_data_dir, '/processed_summary_rep_', rep_id, '.csv'))
  
  # Load our new shuffled replay data and annotate which crosses were first crosses
  df_shuffled = read.csv(paste0('../data/reps/', rep_id, '/combined_shuffled_replay_cross_data.csv'))
  
  # Summarize shuffled replay data
  df_shuffled_summary = summarize_replay_data(df_shuffled, num_total_reps)
  
  # Plot the standard one valley potentiation
  ggplot(df_base_summary, aes(x = replay_gen, y = frac_crossed, color = 'Base')) + 
    geom_line() + 
    geom_point() + 
    geom_line(data = df_shuffled_summary, aes(color = 'Shuffled')) + 
    geom_point(data = df_shuffled_summary, aes(color = 'Shuffled')) + 
    scale_y_continuous(limits = c(0,1)) + 
    xlab('Update') + 
    ylab('Potential to cross') + 
    labs(color = 'Treatment')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_00__shuffled_potentiation.png'), units = 'in', width = 8, height = 6)
}



num_total_reps = 1000
processed_data_dir = '../data/processed'
plot_dir = '../plots'

# Plot our 10 randomly-selected single-cross reps
for(rep_id in c('011','050','075','083','105','282','343','400','408','415')){
  cat('Rep id: ', rep_id, '\n')
  plot_shuffled_replay_data(rep_id, 1000, processed_data_dir, plot_dir)
}
