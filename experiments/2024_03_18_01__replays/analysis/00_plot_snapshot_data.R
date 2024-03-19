rm(list = ls())

library(ggplot2)
library(tidyr)

script_id = '00'

pivot_snapshot_data_longer = function(df_snapshot){
  df_snapshot_longer = tidyr::pivot_longer(df_snapshot, paste0('count_', c(6:24, 'under', 'over')), names_to = 'category')
  df_snapshot_longer$category_factor = factor(df_snapshot_longer$category, levels = paste0('count_', c('under', 6:24, 'over')))
  return(df_snapshot_longer)
}

get_color_map  = function(){
  color_map = c(
  'count_under' = '#000000', 
  'count_6' = '#feedde',
  'count_7' = '#fdd0a2',
  'count_8' = '#fdae6b',
  'count_9' = '#fd8d3c',
  'count_10' = '#e6550d',
  'count_11' = '#a63603',
  'count_12' = '#eff3ff',
  'count_13' = '#c6dbef',
  'count_14' = '#9ecae1',
  'count_15' = '#6baed6',
  'count_16' = '#3182bd',
  'count_17' = '#08519c',
  'count_18' = '#edf8e9',
  'count_19' = '#c7e9c0',
  'count_20' = '#edf8e9',
  'count_21' = '#a1d99b',
  'count_22' = '#74c476',
  'count_23' = '#31a354',
  'count_24' = '#ffffff',
  'count_over' = '#ff0000'
  )
  return(color_map)
}

get_stripped_color_map = function(){
  color_map = c(
  'under' = '#000000', 
  '6' = '#feedde',
  '7' = '#fdd0a2',
  '8' = '#fdae6b',
  '9' = '#fd8d3c',
  '10' = '#e6550d',
  '11' = '#a63603',
  '12' = '#eff3ff',
  '13' = '#c6dbef',
  '14' = '#9ecae1',
  '15' = '#6baed6',
  '16' = '#3182bd',
  '17' = '#08519c',
  '18' = '#edf8e9',
  '19' = '#c7e9c0',
  '20' = '#a1d99b',
  '21' = '#74c476',
  '22' = '#31a354',
  '23' = '#006d2c',
  '24' = '#ffffff',
  'over' = '#ff0000'
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

create_dirs = function(processed_data_dir, plot_dir){
  if(!dir.exists(processed_data_dir)){
    dir.create(processed_data_dir, recursive = T)
  }
  if(!dir.exists(plot_dir)){
    dir.create(plot_dir, recursive = T)
  }
}


plot_snapshot_data = function(rep_id, processed_data_dir, plot_dir){
  rep_plot_dir = paste0(plot_dir, '/reps/', rep_id)
  create_dirs(processed_data_dir, rep_plot_dir)
  
  # Load snapshot data and also create a longer form "tidy" version
  df_snapshot = read.csv(paste0('../data/reps/', rep_id, '/snapshot_data.csv'))
  df_snapshot_longer = pivot_snapshot_data_longer(df_snapshot)
  
  color_map = get_color_map()
  ggplot(df_snapshot_longer, aes(x = update, y = value / 512, fill = category_factor)) + 
    geom_area() + 
    xlab('Update') +
    ylab('Fraction of population') + 
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_manual(values = color_map)
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_00__sorted_muller.png'), units = 'in', width = 8, height = 6)
  
  df_full_snapshot = load_and_reformat_full_snapshot_data()
  stripped_color_map = get_stripped_color_map()
  ggplot(df_full_snapshot, aes(x = update, y = index, fill = value_factor)) + 
    geom_raster() + 
    xlab('Update') + 
    ylab('Index in population') + 
    labs(fill = 'Value') + 
    scale_fill_manual(values = stripped_color_map) + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0))
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_01__raw_muller_plot.png'), units = 'in', width = 8, height = 6)
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

# Plot our 10 randomly-selected no-cross reps
for(rep_id in c('134',  '158', '164', '175', '252', '339', '365', '394', '446', '450')){
  cat('Rep id: ', rep_id, '\n')
  plot_snapshot_data(rep_id, processed_data_dir, plot_dir)
}
