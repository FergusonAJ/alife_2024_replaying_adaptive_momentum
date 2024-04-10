rm(list = ls())

library(ggplot2)
library(tidyr)
library(khroma)
source('../../../global_shared_files/global_analysis_variables.R')

script_id = '00'

pivot_snapshot_data_longer = function(df_snapshot){
  df_snapshot_longer = tidyr::pivot_longer(df_snapshot, paste0('count_', c(6:24, 'under', 'over')), names_to = 'category')
  df_snapshot_longer$category_factor = factor(df_snapshot_longer$category, levels = paste0('count_', c('under', 6:24, 'over')))
  return(df_snapshot_longer)
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

create_dirs = function(processed_data_dir, plot_dir){
  if(!dir.exists(processed_data_dir)){
    dir.create(processed_data_dir, recursive = T)
  }
  if(!dir.exists(plot_dir)){
    dir.create(plot_dir, recursive = T)
  }
}
  
# Mode function from Ken Williams on Stack Overflow
# https://stackoverflow.com/a/8189441
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


plot_snapshot_data = function(rep_id, processed_data_dir, plot_dir){
  rep_plot_dir = paste0(plot_dir, '/reps/', rep_id)
  create_dirs(processed_data_dir, rep_plot_dir)
  
  # Load snapshot data and also create a longer form "tidy" version
  df_snapshot = read.csv(paste0('../data/reps/', rep_id, '/snapshot_data.csv'))
  df_snapshot_longer = pivot_snapshot_data_longer(df_snapshot)
  
  df_snapshot_longer$category_num = substr(df_snapshot_longer$category, 7, 1000)
  ggplot(df_snapshot_longer, aes(x = update, y = value / 512, fill = category_num)) + 
    geom_area() + 
    xlab('Time (generations)') +
    ylab('Fraction of population') + 
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_manual(values = color_map_value)
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_00__sorted_muller.png'), units = 'in', width = 8, height = 6)
  
  df_full_snapshot = load_and_reformat_full_snapshot_data()
  ggplot(df_full_snapshot, aes(x = update, y = index, fill = value_factor)) + 
    geom_raster() + 
    xlab('Time (generations)') +
    ylab('Index in population') + 
    labs(fill = 'Value') + 
    scale_fill_manual(values = color_map_value) + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0))
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_01__raw_muller_plot.png'), units = 'in', width = 8, height = 6)
  
  #df_full_snapshot$is_marked_leading_edge = F
  #for(ud in df_snapshot$update){
  #  leading_edge_index = df_snapshot[df_snapshot$update == ud,]$leading_edge_index
  #  df_full_snapshot[df_full_snapshot$update == ud & df_full_snapshot$index == leading_edge_index + 1,]$is_marked_leading_edge = T
  #  df_full_snapshot[df_full_snapshot$update == ud & df_full_snapshot$index == leading_edge_index + 2,]$is_marked_leading_edge = T
  #  second_leading_edge_left_index = df_snapshot[df_snapshot$update == ud,]$second_leading_edge_left_index
  #  if(second_leading_edge_left_index != 'None'){
  #    df_full_snapshot[df_full_snapshot$update == ud & df_full_snapshot$index == as.numeric(second_leading_edge_left_index) - 1,]$is_marked_leading_edge = T
  #    df_full_snapshot[df_full_snapshot$update == ud & df_full_snapshot$index == as.numeric(second_leading_edge_left_index) - 2,]$is_marked_leading_edge = T
  #  }
  #  second_leading_edge_right_index = df_snapshot[df_snapshot$update == ud,]$second_leading_edge_right_index
  #  if(second_leading_edge_right_index != 'None'){
  #    df_full_snapshot[df_full_snapshot$update == ud & df_full_snapshot$index == as.numeric(second_leading_edge_right_index) + 1,]$is_marked_leading_edge = T
  #    df_full_snapshot[df_full_snapshot$update == ud & df_full_snapshot$index == as.numeric(second_leading_edge_right_index) + 2,]$is_marked_leading_edge = T
  #  }
  #}
  #
  #tmp_color_vec = khroma::color('YlOrBr')(7)
  #tmp_color_vec = khroma::color('sunset')(7)
  #tmp_color_map = c(
  #  '0' = tmp_color_vec[2],
  #  '1' = tmp_color_vec[3],
  #  '2' = tmp_color_vec[4],
  #  '3' = tmp_color_vec[5],
  #  '4' = tmp_color_vec[6],
  #  '5' = tmp_color_vec[7]
  #)
  #df_full_snapshot$value_mod = df_full_snapshot$value %% 6
  #ggplot(df_full_snapshot, aes(x = update, y = index, fill = as.factor(value_mod))) + 
  #  geom_raster() + 
  #  geom_raster(data = df_full_snapshot[df_full_snapshot$is_marked_leading_edge,], fill = '#ffffff') + 
  #  xlab('Time (generations)') +
  #  ylab('Index in population') + 
  #  labs(fill = 'Value') + 
  #  scale_fill_manual(values = tmp_color_map) + 
  #  scale_x_continuous(expand = c(0,0)) + 
  #  scale_y_continuous(expand = c(0,0))
  
  mode_vec = c()
  for(ud in 1:max(df_full_snapshot$update)){
    mode_vec = c(mode_vec, Mode(df_full_snapshot[df_full_snapshot$update == ud,]$value))
  }
  cat('Unique modes: ', unique(mode_vec), '\n')
}

processed_data_dir = '../data/processed'
plot_dir = '../plots'

## Plot the replicates that cross twice
#for(rep_id in c('093', '124', '138', '263')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_snapshot_data(rep_id, processed_data_dir, plot_dir)
#}

## Plot some replicates that cross once
#for(rep_id in c('011', '014', '023', '026', '049')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_snapshot_data(rep_id, processed_data_dir, plot_dir)
#}

## Plot our 10 randomly-selected no-cross reps
#for(rep_id in c('134',  '158', '164', '175', '252', '339', '365', '394', '446', '450')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_snapshot_data(rep_id, processed_data_dir, plot_dir)
#}

## Plot our 10 randomly-selected single-cross reps
#for(rep_id in c('011','050','075','083','105','282','343','400','408','415')){
#  cat('Rep id: ', rep_id, '\n')
#  plot_snapshot_data(rep_id, processed_data_dir, plot_dir)
#}

# Plot our 3 selected seeds
for(rep_id in c('339', '400', '263')){
  cat('Rep id: ', rep_id, '\n')
  plot_snapshot_data(rep_id, processed_data_dir, plot_dir)
}

