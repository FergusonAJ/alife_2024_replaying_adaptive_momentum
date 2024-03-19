rm(list = ls())

library(ggplot2)
library(dplyr)

script_id = '05'

mark_first_crosses = function(df_rep){
  df_rep$is_first_cross = T
  df_rep$cross_id = 1
  df_summary = df_rep %>% 
    dplyr::group_by(rep_id, replay_gen, trial_id) %>%
    dplyr::summarize(count = dplyr::n())
  num_rows_to_check = sum(df_summary$count > 1)
  cat('Iterating over ', num_rows_to_check, ' rows to mark first crosses\n')
  counter = 0
  for(row_idx in which(df_summary$count > 1)){
    replay_gen = df_summary[row_idx,]$replay_gen
    trial_id = df_summary[row_idx,]$trial_id
    relative_updates = df_rep[df_rep$replay_gen == replay_gen & df_rep$trial_id == trial_id,]$relative_update
    mask = df_rep$replay_gen == replay_gen & df_rep$trial_id == trial_id
    df_rep[df_rep$replay_gen == replay_gen & df_rep$trial_id == trial_id,]$cross_id = 1:sum(mask)
    #row_counter = 1
    #for(relative_update in sort(relative_updates)){
    #  df_rep[df_rep$replay_gen == replay_gen & df_rep$trial_id == trial_id & df_rep$relative_update != relative_update,]$cross_id = row_counter
    #  if(row_counter != 1){
    #    df_rep[df_rep$replay_gen == replay_gen & df_rep$trial_id == trial_id & df_rep$relative_update != relative_update,]$is_first_cross = F
    #  }
    #  row_counter = row_counter + 1
    #}
    counter = counter + 1
    if(counter %% floor(num_rows_to_check / 10) == 0){
      cat('#')
    }
  }
  cat('\n')
  df_rep$is_first_cross = df_rep$cross_id == 1
  return(df_rep)
}

summarize_replay_data = function(df_rep, num_total_reps){
  df_summary = df_rep[df_rep$is_first_cross,] %>% 
    dplyr::group_by(rep_id, replay_gen) %>%
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
  df_shuffled = read.csv(paste0('../data/reps/', rep_id, '/shuffled_combined_cross_info.csv'))
  df_shuffled = df_shuffled[df_shuffled$replay_gen != 'replay_gen',]
  df_shuffled$replay_gen = as.numeric(df_shuffled$replay_gen)
  df_shuffled$trial_id = as.numeric(df_shuffled$trial_id)
  df_shuffled$relative_update = as.numeric(df_shuffled$relative_update)
  df_shuffled = mark_first_crosses(df_shuffled)
  
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

rep_id = '015'
for(rep_id in paste0('0', c(15, 16, 18, 22, 26, 65, 69, 74, 80, 81, 83, 85, 87, 89, 92, 93, 97))){
  cat('Rep id: ', rep_id, '\n')
  plot_shuffled_replay_data(rep_id, num_total_reps, processed_data_dir, plot_dir)
}