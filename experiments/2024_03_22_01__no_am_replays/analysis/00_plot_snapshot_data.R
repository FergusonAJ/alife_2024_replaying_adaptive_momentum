rm(list = ls())

library(ggplot2)
library(tidyr)
source('../../../global_shared_files/global_analysis_variables.R')

script_id = '00'

pivot_snapshot_data_longer = function(df_snapshot){
  df_snapshot_longer = tidyr::pivot_longer(df_snapshot, paste0('count_', c(6:24, 'under', 'over')), names_to = 'category')
  df_snapshot_longer$category = substr(df_snapshot_longer$category, 7, 1000)
  df_snapshot_longer$category_factor = factor(df_snapshot_longer$category, levels = c('under', 6:24, 'over'))
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
  df_full_snapshot_longer$value_remap = df_full_snapshot_longer$value - 6
  df_full_snapshot_longer$value_remap_str = as.character(df_full_snapshot_longer$value_remap)
  df_full_snapshot_longer[df_full_snapshot_longer$value_remap < 6,]$value_remap_str = 'under'
  df_full_snapshot_longer[df_full_snapshot_longer$value_remap > 24,]$value_remap_str = 'over'
  df_full_snapshot_longer$value_remap_factor = factor(df_full_snapshot_longer$value_remap_str, levels = paste0(c('under', 6:24, 'over')))
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
  
  df_snapshot_longer$category_remap = NA
  df_snapshot_longer[df_snapshot_longer$category != 'over' & df_snapshot_longer$category != 'under',]$category_remap = 
    as.character(as.numeric(df_snapshot_longer[df_snapshot_longer$category != 'over' & df_snapshot_longer$category != 'under',]$category) - 6)
  df_snapshot_longer[df_snapshot_longer$category == 'under',]$category_remap = 'under'
  df_snapshot_longer[df_snapshot_longer$category == 'over',]$category_remap = '19'
  
  color_map = get_color_map()
  ggplot(df_snapshot_longer, aes(x = update, y = value / 512, fill = category_remap)) + 
    geom_area() + 
    xlab('Time (generation)') + 
    ylab('Fraction of population') + 
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_manual(values = color_map_value)
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_00__sorted_muller.png'), units = 'in', width = 8, height = 6)
  
  df_full_snapshot = load_and_reformat_full_snapshot_data()
  ggplot(df_full_snapshot, aes(x = update, y = index, fill = value_remap_factor)) + 
    geom_raster() + 
    xlab('Time (generation)') + 
    ylab('Index in population') + 
    labs(fill = 'Value') + 
    scale_fill_manual(values = color_map_value) + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0))
  ggsave(paste0(rep_plot_dir, '/script_', script_id, '__plot_01__raw_muller_plot.png'), units = 'in', width = 8, height = 6)
  
  mode_vec = c()
  for(ud in 1:max(df_full_snapshot$update)){
    mode_vec = c(mode_vec, Mode(df_full_snapshot[df_full_snapshot$update == ud,]$value))
  }
  cat('Unique modes: ', unique(mode_vec), '\n')
}

processed_data_dir = '../data/processed'
plot_dir = '../plots'

## Plot our 10 randomly-selected single-cross reps and our double-cross rep
for(rep_id in c('00833', '01357', '02290', '02359', '03149', '05295', '07051', '07605', '07916', '09839', '05501')){
  cat('Rep id: ', rep_id, '\n')
  plot_snapshot_data(rep_id, processed_data_dir, plot_dir)
}

# Plot our 10 randomly-selected no-cross reps
for(rep_id in c('01164', '01435', '01572', '02581', '02711', '02961', '04390', '06116', '06583', '08366')){
  cat('Rep id: ', rep_id, '\n')
  plot_snapshot_data(rep_id, processed_data_dir, plot_dir)
}