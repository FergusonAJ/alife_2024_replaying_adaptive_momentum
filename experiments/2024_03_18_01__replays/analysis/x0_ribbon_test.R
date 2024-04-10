rm(list = ls())

library(ggplot2)
library(tidyr)
library(khroma)
library(cowplot)

script_id = '07'

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

adjust_benchmarking = function(df_benchmarking, df_snapshot){
  df_adjusted_benchmarking = data.frame(data = matrix(nrow = 0, ncol = 4))
  colnames(df_adjusted_benchmarking) = c('update', 'leading_edge_val', 'leading_edge_index', 'frac_crossed')
  for(leading_edge_val in unique(df_benchmarking$leading_edge_val)){
    prev_leading_edge_index = -1
    for(update in sort(unique(df_snapshot$update))){
      leading_edge_index = df_snapshot[df_snapshot$update == update,]$leading_edge_index
      if(leading_edge_index != prev_leading_edge_index){
        frac_crossed = df_benchmarking[
          df_benchmarking$leading_edge_index <= leading_edge_index & 
            df_benchmarking$leading_edge_index > leading_edge_index - 8 & 
            df_benchmarking$leading_edge_val == leading_edge_val,]$crossed_frac
        df_adjusted_benchmarking[nrow(df_adjusted_benchmarking) + 1,] = c(
          update, 
          leading_edge_val, 
          leading_edge_index,
          frac_crossed
        )
        prev_leading_edge_index = leading_edge_index
      }
    }
  }
  return(df_adjusted_benchmarking) 
}

adjust_benchmarking_ribbon = function(df_benchmarking_ribbon, df_snapshot){
  df_adjusted_benchmarking = data.frame(data = matrix(nrow = 0, ncol = 5))
  colnames(df_adjusted_benchmarking) = c('update', 'leading_edge_val', 'leading_edge_index', 'frac_crossed_0', 'frac_crossed_7')
  for(leading_edge_val in unique(df_benchmarking_ribbon$leading_edge_val)){
    prev_leading_edge_index = -1
    for(update in sort(unique(df_snapshot$update))){
      leading_edge_index = df_snapshot[df_snapshot$update == update,]$leading_edge_index
      if(leading_edge_index != prev_leading_edge_index){
        frac_crossed_0 = df_benchmarking_ribbon[
          df_benchmarking_ribbon$leading_edge_index <= leading_edge_index & 
            df_benchmarking_ribbon$leading_edge_index > leading_edge_index - 8 & 
            df_benchmarking_ribbon$leading_edge_val == leading_edge_val,]$crossed_frac_0
        frac_crossed_7 = df_benchmarking_ribbon[
          df_benchmarking_ribbon$leading_edge_index <= leading_edge_index & 
            df_benchmarking_ribbon$leading_edge_index > leading_edge_index - 8 & 
            df_benchmarking_ribbon$leading_edge_val == leading_edge_val,]$crossed_frac_7
        df_adjusted_benchmarking[nrow(df_adjusted_benchmarking) + 1,] = c(
          update, 
          leading_edge_val, 
          leading_edge_index,
          frac_crossed_0, 
          frac_crossed_7
        )
        prev_leading_edge_index = leading_edge_index
      }
    }
  }
  return(df_adjusted_benchmarking) 
}

adjust_benchmarking_for_second_cross = function(df_benchmarking, df_snapshot){
  df_adjusted_benchmarking = data.frame(data = matrix(nrow = 0, ncol = 7))
  colnames(df_adjusted_benchmarking) = c('update', 'left_leading_edge_val', 'left_leading_edge_index', 'left_frac_crossed', 'right_leading_edge_val', 'right_leading_edge_index', 'right_frac_crossed')
  for(update in sort(unique(df_snapshot$update))){
    snapshot_row = df_snapshot[df_snapshot$update == update,]
    if(snapshot_row$second_leading_edge_left_index == 'None'){
      df_adjusted_benchmarking[nrow(df_adjusted_benchmarking) + 1,] = c( update, NA, NA, NA, NA, NA, NA)
      next
    }
    updates_remaining = 768 - update
    # Calculate cross probability on the left edge (remember it's moving left)
    left_index = as.numeric(snapshot_row$second_leading_edge_left_index)
    # Invert the direction
    left_index = 512 - left_index
    left_index = max(left_index, (512 - updates_remaining * 0.66))
    closest_left_index = left_index - (left_index %% 8)
    left_val = as.numeric(snapshot_row$second_leading_edge_left_val)
    left_frac_crossed = 0
    if(left_val >= 18 & closest_left_index <= 504 & left_val < 24){
      left_frac_crossed = df_benchmarking[df_benchmarking$leading_edge_index == closest_left_index & df_benchmarking$leading_edge_val == left_val - 6,]$crossed_frac
    }
    if(left_val >= 24){
      left_frac_crossed = 1
    }
    # Calculate cross probability on the right edge
    right_index = as.numeric(snapshot_row$second_leading_edge_right_index)
    right_index = max(right_index, 512 - updates_remaining)
    closest_right_index = right_index - (right_index %% 8)
    right_val = as.numeric(snapshot_row$second_leading_edge_right_val)
    right_frac_crossed = 0
    if(right_val >= 18 & right_val < 24 & right_index <= 504){
      right_frac_crossed = df_benchmarking[df_benchmarking$leading_edge_index == closest_right_index & df_benchmarking$leading_edge_val == right_val - 6,]$crossed_frac
    } 
    if(right_val >= 24){
      right_frac_crossed = 1
    }
    
    df_adjusted_benchmarking[nrow(df_adjusted_benchmarking) + 1,] = c(
      update, 
      left_val, left_index, left_frac_crossed, 
      right_val, right_index, right_frac_crossed
    )
  }
  df_adjusted_benchmarking$full_estimate = 1 - ((1 - df_adjusted_benchmarking$left_frac_crossed) * (1 - df_adjusted_benchmarking$right_frac_crossed))
  return(df_adjusted_benchmarking) 

}

pivot_snapshot_data_longer = function(df_snapshot){
  df_snapshot_longer = tidyr::pivot_longer(df_snapshot, paste0('count_', c(6:18, 'under', 'over')), names_to = 'category')
  df_snapshot_longer$category_factor = factor(df_snapshot_longer$category, levels = paste0('count_', c('under', 6:18, 'over')))
  return(df_snapshot_longer)
}

load_and_reformat_full_snapshot_data = function(){
  df_full_snapshot_data = read.csv(paste0('../data/reps/', rep_id, '/full_snapshot_data.csv'))
  df_full_snapshot_longer = tidyr::pivot_longer(df_full_snapshot_data, paste0('idx_', 0:511), 'index', names_prefix = 'idx_')
  df_full_snapshot_longer$index = as.numeric(df_full_snapshot_longer$index)
  df_full_snapshot_longer$value_str = as.character(df_full_snapshot_longer$value)
  df_full_snapshot_longer[df_full_snapshot_longer$value < 6,]$value_str = 'under'
  df_full_snapshot_longer[df_full_snapshot_longer$value > 18,]$value_str = 'over'
  df_full_snapshot_longer$value_factor = factor(df_full_snapshot_longer$value_str, levels = paste0(c('under', 6:18, 'over')))
  return(df_full_snapshot_longer)
}

get_stripped_color_map = function(){
  color_arr = khroma::color('iridescent')(21)
  color_map = c(
  'under' = color_arr[21], 
  '6'     = color_arr[1],
  '7'     = color_arr[2],
  '8'     = color_arr[3],
  '9'     = color_arr[4],
  '10'    = color_arr[5],
  '11'    = color_arr[6],
  '12'    = color_arr[13],
  '13'    = color_arr[14],
  '14'    = color_arr[15],
  '15'    = color_arr[16],
  '16'    = color_arr[17],
  '17'    = color_arr[18],
  '18'    = color_arr[7],
  '19'    = color_arr[8],
  '20'    = color_arr[9],
  '21'    = color_arr[10],
  '22'    = color_arr[11],
  '23'    = color_arr[12],
  '24'    = color_arr[19],
  'over'  = color_arr[20]
  )
  return(color_map)
}

load_and_reformat_full_snapshot_data = function(){
  df_full_snapshot_data = read.csv(paste0('../data/reps/', rep_id, '/full_snapshot_data.csv'))
  df_full_snapshot_longer = tidyr::pivot_longer(df_full_snapshot_data, paste0('idx_', 0:511), 'index', names_prefix = 'idx_')
  df_full_snapshot_longer$index = as.numeric(df_full_snapshot_longer$index)
  df_full_snapshot_longer$value_str = as.character(df_full_snapshot_longer$value)
  df_full_snapshot_longer[df_full_snapshot_longer$value < 6,]$value_str = 'under'
  df_full_snapshot_longer[df_full_snapshot_longer$value > 24,]$value_str = 'over'
  df_full_snapshot_longer$value_factor = factor(df_full_snapshot_longer$value_str, levels = paste0(c('under', 6:24, 'over')))
  return(df_full_snapshot_longer)
}

plot_replay_with_adjusted_benchmark = function(rep_id, processed_data_dir, plot_dir){
  rep_plot_dir = paste0(plot_dir, '/reps/', rep_id)
  
  # Load raw data
  df_rep = read.csv(paste0('../data/reps/', rep_id, '/combined_selected_replay_cross_data.csv'))
  
  # Summarize and save replay data
  df_rep_summary = summarize_replay_data(df_rep, 10000)
  write.csv(df_rep_summary, paste0(processed_data_dir, '/processed_selected_summary_rep_', rep_id, '.csv'), row.names = F)
  
  # Load and prep benchmarking data (as in script 01)
  df_benchmarking = read.csv('../../2024_03_14_01__benchmarking_10k/data/processed_summary.csv')
  df_benchmarking_ribbon = read.csv('../../2024_03_14_x1__benchmarking_combined/data/widened_combined_ribbon_data.csv')
  
  # Load snapshot data and also create a longer form "tidy" version
  df_snapshot = read.csv(paste0('../data/reps/', rep_id, '/snapshot_data.csv'))
  df_snapshot_longer = pivot_snapshot_data_longer(df_snapshot)
  
  # Levarage snapshot data to adjust benchmarking data
  df_adjusted_benchmarking = adjust_benchmarking(df_benchmarking, df_snapshot) 
  df_adjusted_ribbon = adjust_benchmarking_ribbon(df_benchmarking_ribbon, df_snapshot)
  
  ggplot(df_rep_summary) + 
    geom_ribbon(data = df_adjusted_ribbon, aes(x = update, ymin = frac_crossed_0, ymax = frac_crossed_7, fill = as.factor(leading_edge_val)), alpha = 0.5) + 
    geom_line(data = df_adjusted_ribbon, aes(x = update, y = frac_crossed_0, color = as.factor(leading_edge_val))) + 
    geom_line(data = df_adjusted_ribbon, aes(x = update, y = frac_crossed_7, color = as.factor(leading_edge_val))) + 
    geom_line(aes(x = replay_gen, y = frac_crossed)) + 
    #geom_point() + 
    scale_y_continuous(limits = c(0,1)) + 
    xlab('Update') + 
    ylab('Potential to cross')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_00__selected_replays_with_adjusted_benchmarks.png'), units = 'in', width = 8, height = 6)
  
  
  # Load processed data
  df_second_cross_summary = summarize_second_cross_data(df_rep, 10000)
  write.csv(df_second_cross_summary, paste0(processed_data_dir, '/processed_selected_second_cross_summary_rep_', rep_id, '.csv'), row.names = F)
  
  # Calculate the benchmarking probabilities of the second cross
  df_second_cross_benchmarking = adjust_benchmarking_for_second_cross(df_benchmarking, df_snapshot)
  
  ggplot(df_second_cross_summary, aes(x = replay_gen, y = frac_crossed)) + 
    geom_line(data = df_second_cross_benchmarking, aes(x = update, y = full_estimate), color = '#dd0000') + 
    geom_line() + 
    #geom_point() + 
    scale_y_continuous(limits = c(0,1)) + 
    xlab('Update') + 
    ylab('Potential to cross')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_01__selected_second_cross_estimate.png'), units = 'in', width = 8, height = 6)
  
  # Create a combined potentiation + muller plot
  df_full_snapshot = load_and_reformat_full_snapshot_data()
  stripped_color_map = get_stripped_color_map()
  
  first_cross_ud = min(df_full_snapshot[df_full_snapshot$value >= 18,]$update)
  
  ggp_base = ggplot(df_rep_summary, aes(x = replay_gen, y = frac_crossed)) + 
    geom_line(data = df_adjusted_benchmarking, aes(x = update, y = frac_crossed, color = as.factor(leading_edge_val))) + 
    geom_line() + 
    geom_vline(aes(xintercept=first_cross_ud), linetype = 'dashed', alpha = 0.5) + 
    #geom_point() + 
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_color_manual(values = stripped_color_map) + 
    xlab('Update') + 
    ylab('Potential to cross') +
    theme(legend.position = 'none')
  
  ggp_muller = ggplot(df_full_snapshot, aes(x = update, y = index, fill = value_factor)) + 
    geom_raster() + 
    geom_vline(aes(xintercept=first_cross_ud), linetype = 'dashed', alpha = 0.5) + 
    xlab('Update') + 
    ylab('Index in population') + 
    labs(fill = 'Value') + 
    scale_fill_manual(values = stripped_color_map) + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) + 
    theme(legend.position = 'none')

  combined_plot = cowplot::plot_grid(ggp_base, ggp_muller, nrow = 2, ncol = 1)
  show(combined_plot)
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_02__combined_plot.pdf'), units = 'in', width = 5, height = 8)

  
  df_benchmarking_wide = tidyr::pivot_wider(df_adjusted_benchmarking, names_prefix = 'frac_crossed_', names_from = c(leading_edge_val), values_from = c(frac_crossed))
  
  color_scheme = 'sunset'
  ggplot(df_benchmarking_wide, aes(x = leading_edge_index)) +
    geom_ribbon(aes(ymin = 0, ymax = frac_crossed_12, fill = 'p'), alpha = 0.5) + 
    geom_ribbon(aes(ymin = frac_crossed_12, ymax = frac_crossed_13, fill = 'p+1'), alpha = 0.5) + 
    geom_ribbon(aes(ymin = frac_crossed_13, ymax = frac_crossed_14, fill = 'p+2'), alpha = 0.5) + 
    geom_ribbon(aes(ymin = frac_crossed_14, ymax = frac_crossed_15, fill = 'p+3'), alpha = 0.5) + 
    geom_ribbon(aes(ymin = frac_crossed_15, ymax = frac_crossed_16, fill = 'p+4'), alpha = 0.5) + 
    geom_ribbon(aes(ymin = frac_crossed_16, ymax = frac_crossed_17, fill = 'p+5'), alpha = 0.5) + 
    geom_line(data = df_adjusted_benchmarking, aes(x = leading_edge_index, y = frac_crossed, color = as.factor(leading_edge_val))) + 
    geom_line(data = df_rep_summary, aes(x = replay_gen, y = frac_crossed)) + 
    scale_y_continuous(limits = c(0,1)) + 
    scale_fill_manual(values = khroma::color(color_scheme)(7)[2:7]) +
    scale_color_manual(values = khroma::color(color_scheme)(7)[2:7])
    
}

processed_data_dir = '../data/processed'
plot_dir = '../plots'

rep_id = '400'
# Plot our three selected replays
for(rep_id in c('400', '263', '339')){
  cat('Rep id: ', rep_id, '\n')
  plot_replay_with_adjusted_benchmark(rep_id, processed_data_dir, plot_dir)
}
