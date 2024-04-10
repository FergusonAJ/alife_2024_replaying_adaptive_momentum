rm(list = ls())

library(ggplot2)
library(cowplot)
source('../../../global_shared_files/global_analysis_variables.R')

plot_dir = '../plots'
if(!dir.exists(plot_dir)){
  dir.create(plot_dir, recursive = T)
}

df_crossing_first = read.csv('../../2024_03_15_02__crossing_from_scratch/data/first_cross_data.csv')
df_crossing_second = read.csv('../../2024_03_15_02__crossing_from_scratch/data/second_cross_data.csv')
df_crossing = rbind(df_crossing_first, df_crossing_second)

# Extract fixation time from other exp 
df_fixation_edge = read.csv('../../2024_04_04_01__sweep_timing_edge/data/combined_sweep_info.csv')
df_fixation_center = read.csv('../../2024_04_04_02__sweep_timing_center/data/combined_sweep_info.csv')
fixation_time_center_min = min(df_fixation_center$sweep_time)
fixation_time_edge_max = max(df_fixation_edge$sweep_time)

shared_binwidth = 50
ggp_crossing = ggplot(df_crossing, aes(x = relative_update, fill = as.factor(cross_counter_str))) + 
  geom_histogram(data = df_crossing[df_crossing$cross_counter == 1 & df_crossing$relative_update <= 5000,], breaks = seq(0,5000,shared_binwidth), alpha = 1) + 
  geom_histogram(data = df_crossing[df_crossing$cross_counter == 2 & df_crossing$first_cross_update <= 5000 & df_crossing$relative_update <= 5000,], breaks = seq(0,5000,shared_binwidth), alpha = 0.75) + 
  geom_vline(aes(xintercept = fixation_time_center_min), linetype = 'dashed', size = 1, alpha = 0.5) + 
  geom_vline(aes(xintercept = fixation_time_edge_max), linetype='solid', size = 1, alpha = 0.5) + 
  xlab('Relative generation') +
  ylab('Count') + 
  labs(fill = 'Valley\ncrossed') +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title = element_text(size = 16)) + 
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) + 
  theme(legend.position = 'bottom') + 
  scale_fill_manual(values = c(color_map_value['12'][[1]], color_map_value['18'][[1]])) + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,3))



df_benchmarking = read.csv('../../2024_03_14_01__benchmarking_10k/data/processed_summary.csv')
df_benchmarking$paper_leading_edge_val = value_to_letter_map['12']
df_benchmarking[df_benchmarking$leading_edge_val == 13,]$paper_leading_edge_val = value_to_letter_map['13']
df_benchmarking[df_benchmarking$leading_edge_val == 14,]$paper_leading_edge_val = value_to_letter_map['14'] 
df_benchmarking[df_benchmarking$leading_edge_val == 15,]$paper_leading_edge_val = value_to_letter_map['15'] 
df_benchmarking[df_benchmarking$leading_edge_val == 16,]$paper_leading_edge_val = value_to_letter_map['16'] 
df_benchmarking[df_benchmarking$leading_edge_val == 17,]$paper_leading_edge_val = value_to_letter_map['17'] 

df_benchmarking_wide = tidyr::pivot_wider(df_benchmarking, names_from = c(leading_edge_val, paper_leading_edge_val), values_from = c(count, crossed_frac))

ggp_benchmarking = ggplot(df_benchmarking_wide[df_benchmarking_wide$leading_edge_index > 0,], aes(x = leading_edge_index)) +
  geom_ribbon(aes(ymin = 0, ymax = crossed_frac_12_h*100, fill = 'h')) + 
  geom_ribbon(aes(ymin = crossed_frac_12_h*100, ymax = crossed_frac_13_i*100, fill = 'i')) + 
  geom_ribbon(aes(ymin = crossed_frac_13_i*100, ymax = crossed_frac_14_j*100, fill = 'j')) + 
  geom_ribbon(aes(ymin = crossed_frac_14_j*100, ymax = crossed_frac_15_k*100, fill = 'k')) + 
  geom_ribbon(aes(ymin = crossed_frac_15_k*100, ymax = crossed_frac_16_l*100, fill = 'l')) + 
  geom_ribbon(aes(ymin = crossed_frac_16_l*100, ymax = crossed_frac_17_m*100, fill = 'm')) + 
  xlab('Leading edge index') + 
  ylab('Potential to cross') + 
  labs(fill = 'Leading edge\n     value') + 
  theme(axis.title = element_text(size = 16)) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.position = 'bottom') + 
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0), limits = c(0,512)) + 
  scale_fill_manual(values = color_map_letters, limits = force, labels = c(
    h = expression(p[2]  ), 
    i = expression(p[2] + 1),
    j = expression(p[2] + 2),
    k = expression(p[2] + 3),
    l = expression(p[2] + 4),
    m = expression(p[2] + 5)
  ))


df_shuffled = read.csv('../../2024_03_17_01__shuffled_benchmarking_backfill/data/processed_summary.csv')
df_shuffled = df_shuffled[,setdiff(colnames(df_shuffled), 'slurm_array_task_id')]
df_shuffled$paper_leading_edge_val = value_to_letter_map['12']
df_shuffled[df_shuffled$leading_edge_val == 13,]$paper_leading_edge_val = value_to_letter_map['13']
df_shuffled[df_shuffled$leading_edge_val == 14,]$paper_leading_edge_val = value_to_letter_map['14'] 
df_shuffled[df_shuffled$leading_edge_val == 15,]$paper_leading_edge_val = value_to_letter_map['15'] 
df_shuffled[df_shuffled$leading_edge_val == 16,]$paper_leading_edge_val = value_to_letter_map['16'] 
df_shuffled[df_shuffled$leading_edge_val == 17,]$paper_leading_edge_val = value_to_letter_map['17'] 

df_shuffled_wide = tidyr::pivot_wider(df_shuffled, names_from = c(leading_edge_val, paper_leading_edge_val), values_from = c(count, crossed_frac))

ggp_shuffled = ggplot(df_shuffled_wide[df_shuffled_wide$leading_edge_index > 0,], aes(x = leading_edge_index)) +
  geom_ribbon(aes(ymin = 0, ymax = crossed_frac_12_h * 100, fill = 'h')) + 
  geom_ribbon(aes(ymin = crossed_frac_12_h * 100, ymax = crossed_frac_13_i * 100, fill = 'i')) + 
  geom_ribbon(aes(ymin = crossed_frac_13_i * 100, ymax = crossed_frac_14_j * 100, fill = 'j')) + 
  geom_ribbon(aes(ymin = crossed_frac_14_j * 100, ymax = crossed_frac_15_k * 100, fill = 'k')) + 
  geom_ribbon(aes(ymin = crossed_frac_15_k * 100, ymax = crossed_frac_16_l * 100, fill = 'l')) + 
  geom_ribbon(aes(ymin = crossed_frac_16_l * 100, ymax = crossed_frac_17_m * 100, fill = 'm')) + 
  xlab('Leading edge index') + 
  ylab('Potential to cross') + 
  labs(fill = 'Leading edge\n     value') + 
  theme(axis.title = element_text(size = 16)) + 
  theme(axis.text = element_text(size = 12)) + 
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.position = 'bottom') + 
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0), limits = c(0,512)) + 
  scale_fill_manual(values = color_map_letters, limits = force, labels = c(
    h = expression(p[2]  ), 
    i = expression(p[2] + 1),
    j = expression(p[2] + 2),
    k = expression(p[2] + 3),
    l = expression(p[2] + 4),
    m = expression(p[2] + 5)
  ))

combined_plots = plot_grid(ggp_crossing, ggp_benchmarking, ggp_shuffled, nrow = 1, labels = c('A', 'B', 'C'))  
show(combined_plots)
ggsave(paste0(plot_dir, '/combined_plots.pdf'), units = 'in', width = 10, height = 3)


ggp_both_benchmarking = plot_grid(
  ggp_benchmarking + theme(legend.position = 'none') + theme(plot.margin = margin(10,10,10,10)),
  ggp_shuffled + theme(legend.position = 'none') + theme(plot.margin = margin(10,10,10,10)),
  labels = c('B', 'C'),
  nrow = 1
)

ggp_benchmarking_legend = get_legend(ggp_benchmarking)
ggp_full_benchmarking = plot_grid(
  ggp_both_benchmarking, 
  ggp_benchmarking_legend, 
  nrow = 2, rel_heights = c(1,0.2))
show(ggp_full_benchmarking)
  
ggp_crossing_legend = get_legend(ggp_crossing)  
ggp_full_crossing = plot_grid(
  ggp_crossing + theme(legend.position = 'none') + theme(plot.margin = margin(10,15,10,10)),
  ggp_crossing_legend, 
  nrow = 2, rel_heights = c(1,0.2),
  labels = c('A', '')
)

plot_grid(
  ggp_full_crossing, 
  ggp_full_benchmarking,
  nrow = 1,
  rel_widths = c(1,2)
  )

ggsave(paste0(plot_dir, '/combined_plots_full_equal.pdf'), units = 'in', width = 10, height = 4)

plot_grid(
  ggp_full_crossing, 
  ggp_full_benchmarking,
  nrow = 1,
  rel_widths = c(1.25,2)
  )

ggsave(paste0(plot_dir, '/combined_plots_full_split.pdf'), units = 'in', width = 10, height = 3.5)

