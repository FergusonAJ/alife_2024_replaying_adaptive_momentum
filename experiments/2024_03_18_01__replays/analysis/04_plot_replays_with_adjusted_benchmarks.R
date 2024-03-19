rm(list = ls())

library(ggplot2)
library(tidyr)

script_id = '04'

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

plot_replay_with_adjusted_benchmark = function(rep_id, processed_data_dir, plot_dir){
  rep_plot_dir = paste0(plot_dir, '/reps/', rep_id)
  
  # Load processed data
  df_rep_summary = read.csv(paste0(processed_data_dir, '/processed_summary_rep_', rep_id, '.csv'))  
  # Load and prep benchmarking data (as in script 01)
  df_benchmarking = read.csv('../../2024_03_14_01__benchmarking_10k/data/processed_summary.csv')
  
  # Load snapshot data and also create a longer form "tidy" version
  df_snapshot = read.csv(paste0('../data/reps/', rep_id, '/snapshot_data.csv'))
  df_snapshot_longer = pivot_snapshot_data_longer(df_snapshot)
  
  # Levarage snapshot data to adjust benchmarking data
  df_adjusted_benchmarking = adjust_benchmarking(df_benchmarking, df_snapshot) 
  
  ggplot(df_rep_summary, aes(x = replay_gen, y = frac_crossed)) + 
    geom_line(data = df_adjusted_benchmarking, aes(x = update, y = frac_crossed, color = as.factor(leading_edge_val))) + 
    geom_line() + 
    geom_point() + 
    scale_y_continuous(limits = c(0,1)) + 
    xlab('Update') + 
    ylab('Potential to cross')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_00__replays_with_adjusted_benchmarks.png'), units = 'in', width = 8, height = 6)
  
  ggplot(df_rep_summary, aes(x = replay_gen, y = frac_crossed)) + 
    geom_area(data = df_adjusted_benchmarking[df_adjusted_benchmarking$leading_edge_val == 17,], aes(x = update, y = frac_crossed), color = '#990022', fill = '#aa0055', alpha = 0.2) + 
    geom_area(data = df_adjusted_benchmarking[df_adjusted_benchmarking$leading_edge_val == 16,], aes(x = update, y = frac_crossed), color = '#990022', fill = '#aa0055', alpha = 0.2) + 
    geom_area(data = df_adjusted_benchmarking[df_adjusted_benchmarking$leading_edge_val == 15,], aes(x = update, y = frac_crossed), color = '#990022', fill = '#aa0055', alpha = 0.2) + 
    geom_area(data = df_adjusted_benchmarking[df_adjusted_benchmarking$leading_edge_val == 14,], aes(x = update, y = frac_crossed), color = '#990022', fill = '#aa0055', alpha = 0.2) + 
    geom_area(data = df_adjusted_benchmarking[df_adjusted_benchmarking$leading_edge_val == 13,], aes(x = update, y = frac_crossed), color = '#990022', fill = '#aa0055', alpha = 0.2) + 
    geom_area(data = df_adjusted_benchmarking[df_adjusted_benchmarking$leading_edge_val == 12,], aes(x = update, y = frac_crossed), color = '#990022', fill = '#aa0055', alpha = 0.2) + 
    geom_line() + 
    geom_point() + 
    scale_y_continuous(limits = c(0,1)) + 
    xlab('Update') + 
    ylab('Potential to cross')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_01__replays_with_adjusted_benchmarks_area.png'), units = 'in', width = 8, height = 6)
  
  # Load processed data
  df_second_cross_summary = read.csv(paste0(processed_data_dir, '/processed_second_cross_summary_rep_', rep_id, '.csv'))  
  
  # Calculate the benchmarking probabilities of the second cross
  df_second_cross_benchmarking = adjust_benchmarking_for_second_cross(df_benchmarking, df_snapshot)
  
  ggplot(df_second_cross_summary, aes(x = replay_gen, y = frac_crossed)) + 
    geom_line(data = df_second_cross_benchmarking, aes(x = update, y = full_estimate)) + 
    geom_line() + 
    geom_point() + 
    scale_y_continuous(limits = c(0,1)) + 
    xlab('Update') + 
    ylab('Potential to cross')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_02__second_cross_estimate.png'), units = 'in', width = 8, height = 6)
  
  
}

processed_data_dir = '../data/processed'
plot_dir = '../plots'

rep_id = '124'

## Plot the replicates that cross twice
#for(rep_id in c('093', '124', '138', '263')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_replay_with_adjusted_benchmark(rep_id, processed_data_dir, plot_dir)
#}

## Plot some replicates that cross once
#for(rep_id in c('011', '014', '023', '026', '049')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_replay_with_adjusted_benchmark(rep_id, processed_data_dir, plot_dir)
#}

# Plot our 10 randomly-selected no-cross reps
for(rep_id in c('134',  '158', '164', '175', '252', '339', '365', '394', '446', '450')){
  cat('Rep id: ', rep_id, '\n')
  plot_replay_with_adjusted_benchmark(rep_id, processed_data_dir, plot_dir)
}