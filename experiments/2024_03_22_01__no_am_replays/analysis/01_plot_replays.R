rm(list = ls())

library(ggplot2)
library(dplyr)

script_id = '01'

summarize_replay_data = function(df_rep, num_total_reps){
  df_summary = df_rep[df_rep$cross_counter == 1,] %>% 
    dplyr::group_by(slurm_task_id, replay_gen) %>%
    dplyr::summarize(count = dplyr::n())
  df_summary$frac_crossed = df_summary$count / num_total_reps
  return(df_summary)
}

summarize_second_cross_data = function(df_rep, num_total_reps){
  df_summary = df_rep[df_rep$cross_counter == 2,] %>% 
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


plot_replay_data = function(rep_id, num_total_reps, processed_data_dir, plot_dir){
  # Create our output directory structure
  rep_plot_dir = paste0(plot_dir, '/reps/', rep_id)
  create_dirs(processed_data_dir, rep_plot_dir)
  
  # Process our standard replay data and save
  filename = paste0('../data/reps/', rep_id, '/combined_replay_cross_data.csv')
  df_rep = read.csv(filename)
  
  # Summarize our data into a more usable format and save
  df_rep_summary = summarize_replay_data(df_rep, num_total_reps)
  write.csv(df_rep_summary, paste0(processed_data_dir, '/processed_summary_rep_', rep_id, '.csv'), row.names = F)
  
  # Summarize our second cross data into a more usable format and save
  df_second_cross_summary = summarize_second_cross_data(df_rep, num_total_reps)
  write.csv(df_second_cross_summary, paste0(processed_data_dir, '/processed_second_cross_summary_rep_', rep_id, '.csv'), row.names = F)
  
  # Plot the standard one valley potentiation
  ggplot(df_rep_summary, aes(x = replay_gen, y = frac_crossed)) + 
    geom_line() + 
    geom_point() + 
    scale_y_continuous(limits = c(0,1)) + 
    xlab('Update') + 
    ylab('Potential to cross')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_00__single_valley_potentiation.png'), units = 'in', width = 8, height = 6)
  
  ggplot(df_second_cross_summary, aes(x = replay_gen, y = frac_crossed)) + 
    geom_line() + 
    geom_point() + 
    scale_y_continuous(limits = c(0,1)) + 
    xlab('Update') + 
    ylab('Potential to cross')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_01__second_valley_potentiation.png'), units = 'in', width = 8, height = 6)
  
  ggplot(df_rep_summary, aes(x = replay_gen, y = frac_crossed)) + 
    geom_line() + 
    geom_point() + 
    geom_line(data = df_second_cross_summary, aes(x = replay_gen, y = frac_crossed)) + 
    geom_point(data = df_second_cross_summary, aes(x = replay_gen, y = frac_crossed)) + 
    scale_y_continuous(limits = c(0,1)) + 
    xlab('Update') + 
    ylab('Potential to cross')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_02__both_valley_potentiation.png'), units = 'in', width = 8, height = 6)
}



num_total_reps = 1000
processed_data_dir = '../data/processed'
plot_dir = '../plots'

## Plot the replicates that cross twice
#for(rep_id in c('093', '124', '138', '263')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_replay_data(rep_id, num_total_reps, processed_data_dir, plot_dir)
#}

## Plot some replicates that cross once
#for(rep_id in c('011', '014', '023', '026', '049')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_replay_data(rep_id, num_total_reps, processed_data_dir, plot_dir)
#}

## Plot our 10 randomly-selected no-cross reps
#for(rep_id in c('134',  '158', '164', '175', '252', '339', '365', '394', '446', '450')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_replay_data(rep_id, num_total_reps, processed_data_dir, plot_dir)
#}

# Plot our 10 randomly-selected single-cross reps
for(rep_id in c('011','050','075','083','105','282','343','400','408','415')){
  cat('Rep id: ', rep_id, '\n')
  plot_replay_data(rep_id, num_total_reps, processed_data_dir, plot_dir)
}