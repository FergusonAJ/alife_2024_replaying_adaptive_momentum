rm(list = ls())

library(ggplot2)
library(tidyr)
library(khroma)
library(cowplot)
source('../../../global_shared_files/global_analysis_variables.R')

script_id = '07'

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
        if(length(frac_crossed) == 0) frac_crossed = 0
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

plot_selected_replay_with_shuffled = function(rep_id, processed_data_dir, plot_dir){
  rep_plot_dir = paste0(plot_dir, '/reps/', rep_id)
  
  # Load processed selected replay data
  df_rep_summary = read.csv(paste0(processed_data_dir, '/processed_selected_summary_rep_', rep_id, '.csv'))
  df_rep_summary = df_rep_summary[,setdiff(colnames(df_rep_summary), 'slurm_task_id')]
  
  # Load processed shuffle replay data
  df_shuffled_summary = read.csv(paste0(processed_data_dir, '/processed_shuffled_summary_rep_', rep_id, '.csv'))
  df_shuffled_summary = df_shuffled_summary[,setdiff(colnames(df_shuffled_summary), 'slurm_task_id')]
  
  # Combined shuffled and standard replay data
  combined_summary = dplyr::full_join(df_rep_summary, df_shuffled_summary, by=c('replay_gen'), suffix=c('_base', '_shuffled'))
  
  # Plot the selected replay and the shuffled replays 
  ggplot(combined_summary, aes(x = replay_gen)) + 
    geom_line(aes(y = frac_crossed_base, color = 'Standard')) + 
    geom_line(data = combined_summary[!is.na(combined_summary$frac_crossed_shuffled),], aes(y = frac_crossed_shuffled, color = 'Shuffled')) + 
    scale_y_continuous(limits = c(0,1)) + 
    xlab('Update') + 
    ylab('Potential to cross')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_00__selected_replays_with_shuffled_replays.png'), units = 'in', width = 8, height = 6)
  
  # Load snapshot data
  df_snapshot = read.csv(paste0('../data/reps/', rep_id, '/snapshot_data.csv'))
  
  # Load benchmarking data
  df_benchmarking = read.csv('../../2024_03_17_01__shuffled_benchmarking_backfill/data/processed_summary.csv')
  
  # Levarage snapshot data to adjust benchmarking data
  df_adjusted_benchmarking = adjust_benchmarking(df_benchmarking, df_snapshot) 
  
  df_benchmarking_wide = tidyr::pivot_wider(df_adjusted_benchmarking, names_prefix = 'frac_crossed_', names_from = c(leading_edge_val), values_from = c(frac_crossed))
  
  line_size = 0.75
  ggplot(df_benchmarking_wide, aes(x = update)) +
    geom_ribbon(aes(ymin = 0, ymax = frac_crossed_12, fill = '12'), alpha = 1) + 
    geom_ribbon(aes(ymin = frac_crossed_12, ymax = frac_crossed_13, fill = '13'), alpha = 1) + 
    geom_ribbon(aes(ymin = frac_crossed_13, ymax = frac_crossed_14, fill = '14'), alpha = 1) + 
    geom_ribbon(aes(ymin = frac_crossed_14, ymax = frac_crossed_15, fill = '15'), alpha = 1) + 
    geom_ribbon(aes(ymin = frac_crossed_15, ymax = frac_crossed_16, fill = '16'), alpha = 1) + 
    geom_ribbon(aes(ymin = frac_crossed_16, ymax = frac_crossed_17, fill = '17'), alpha = 1) + 
    geom_line(data = combined_summary, aes(x = replay_gen, y = frac_crossed_base), size = 0.75, color = color_map_value['18']) + 
    geom_line(data = combined_summary[!is.na(combined_summary$frac_crossed_shuffled),], aes(x = replay_gen, y = frac_crossed_shuffled), size = 0.75) + 
    scale_y_continuous(limits = c(0,1), expand = c(0,0)) + 
    scale_x_continuous(limits = c(0, 768), expand = c(0,0)) + 
    scale_fill_manual(values = color_map_value) +
    scale_color_manual(values = color_map_value) + 
    xlab('Update') + 
    ylab('Potential to cross') +
    theme(axis.title = element_text(size = 16)) + 
    theme(axis.text = element_text(size = 14)) + 
    theme(legend.position = 'none')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_00__selected_replays_with_shuffled_replays_and_shuffled_benchmark.png'), units = 'in', width = 8, height = 6)
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_00__selected_replays_with_shuffled_replays_and_shuffled_benchmark.pdf'), units = 'in', width = 5, height = 4)
  
  ggplot(combined_summary, aes(x = replay_gen)) +
    geom_line(data = combined_summary, aes(y = frac_crossed_base * 100), color = color_map_value['18'], size = line_size) + 
    geom_line(data = combined_summary[!is.na(combined_summary$frac_crossed_shuffled),], aes(y = frac_crossed_shuffled * 100), size = line_size) + 
    scale_y_continuous(limits = c(0,100), expand = c(0,0)) + 
    scale_x_continuous(limits = c(0, 768), expand = c(0,0)) + 
    scale_fill_manual(values = color_map_value) +
    scale_color_manual(values = color_map_value) + 
    xlab('Time (generations)') + 
    ylab('Potential to cross') +
    theme(axis.title = element_text(size = 16)) + 
    theme(axis.text = element_text(size = 14)) + 
    theme(legend.position = 'none')
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_01__selected_replays_with_shuffled_replays_no_bg.png'), units = 'in', width = 8, height = 6)
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_01__selected_replays_with_shuffled_replays_no_bg.pdf'), units = 'in', width = 5, height = 4)
}

processed_data_dir = '../data/processed'
plot_dir = '../plots'

rep_id = '400'
plot_selected_replay_with_shuffled(rep_id, processed_data_dir, plot_dir)

