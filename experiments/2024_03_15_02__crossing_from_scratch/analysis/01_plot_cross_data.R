rm(list = ls())

library(ggplot2)
source('../../../global_shared_files/global_analysis_variables.R')

df_combined = read.csv('../data/combined_cross_info.csv')

plot_dir = '../plots'
if(!dir.exists(plot_dir)) dir.create(plot_dir)


ggplot(df_combined, aes(x = relative_update, fill = as.factor(cross_counter))) + 
  geom_histogram(binwidth = 512) + 
  facet_grid(rows = vars(as.factor(cross_counter))) + 
  xlab('Relative generation') + 
  ylab('Count') + 
  labs(fill = 'Cross ID')
ggsave(paste0(plot_dir, '/cross_updates_faceted.png'), units = 'in', width = 8, height = 8)

ggplot(df_combined, aes(x = relative_update, fill = as.factor(cross_counter))) + 
  geom_histogram(binwidth = 512) + 
  xlab('Relative generation') + 
  ylab('Count') + 
  labs(fill = 'Cross ID')
ggsave(paste0(plot_dir, '/cross_updates_stacked.png'), units = 'in', width = 8, height = 6)

ggplot(df_combined, aes(x = relative_update, fill = as.factor(cross_counter))) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 1,], binwidth = 512, alpha = 0.5) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 2,], binwidth = 512, alpha = 0.5) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 3,], binwidth = 512, alpha = 0.5) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 4,], binwidth = 512, alpha = 0.5) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 5,], binwidth = 512, alpha = 0.5) + 
  xlab('Relative generation') + 
  ylab('Count') + 
  labs(fill = 'Cross ID')
ggsave(paste0(plot_dir, '/cross_updates_overlayed.png'), units = 'in', width = 8, height = 6)

ggplot(df_combined[df_combined$cross_counter == 2 & df_combined$relative_update <= 1024,], aes(x = relative_update)) + 
  geom_vline(xintercept = 512, linetype = 'dashed', alpha = 0.5) +
  geom_vline(xintercept = 768, linetype = 'dotted', alpha = 0.5) +
  geom_histogram(binwidth = 32)
ggsave(paste0(plot_dir, '/second_cross_zoom.png'), units = 'in', width = 8, height = 6)

df_combined$cross_counter_str = NA
df_combined[df_combined$cross_counter == 1,]$cross_counter_str = 'First'
df_combined[df_combined$cross_counter == 2,]$cross_counter_str = 'Second'
ggplot(df_combined, aes(x = relative_update, fill = as.factor(cross_counter_str))) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 2,], binwidth = 512, alpha = 1) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 1,], binwidth = 512, alpha = 0.9) + 
  xlab('Relative generation') + 
  ylab('Count') + 
  labs(fill = 'Valley\ncrossed') +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title = element_text(size = 16)) + 
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) + 
  scale_fill_manual(values = c(color_map_value['12'][[1]], color_map_value['18'][[1]])) + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0))
ggsave(paste0(plot_dir, '/first_two_crosses_overlayed.png'), units = 'in', width = 8, height = 6)
ggsave(paste0(plot_dir, '/first_two_crosses_overlayed.pdf'), units = 'in', width = 5, height = 3)

# Make it a more fair comparison
df_combined$first_cross_update = NA
for(row_id in which(df_combined$cross_counter == 2)){
  slurm_task_id = df_combined[row_id,]$slurm_task_id
  trial_id = df_combined[row_id,]$trial_id
  first_cross_update = df_combined[df_combined$slurm_task_id == slurm_task_id & df_combined$trial_id == trial_id & df_combined$cross_counter == 1,]$relative_update
  df_combined[row_id,]$first_cross_update = first_cross_update
}


shared_binwidth = 100
total_reps = 500 * 1000
df_first_cross = df_combined[df_combined$relative_update <= 5000 & df_combined$cross_counter == 1,]
first_cross_reps = nrow(df_first_cross)
cat('Mean cross time for first crossers: ', mean(df_first_cross$relative_update), '\n')
cat('Median cross time for first crossers: ', median(df_first_cross$relative_update), '\n')
first_expectation = first_cross_reps / (5000 / shared_binwidth)
#first_cross_reps = 12826
#first_expectation = first_cross_reps / 2 / (5000 / shared_binwidth)
frac_cross = first_cross_reps / total_reps
second_expectation = (first_cross_reps * frac_cross) / 2 / (5000/shared_binwidth)
#second_expectation = total_reps * 0.003472 / 2 / (5000 / shared_binwidth)

df_second_cross = df_combined[df_combined$cross_counter == 2 & df_combined$first_cross_update <= 5000 & df_combined$relative_update <= 5000,]
second_cross_reps = nrow(df_second_cross)
cat('Mean cross time for second crossers: ', mean(df_second_cross$relative_update), '\n')
cat('Median cross time for second crossers: ', median(df_second_cross$relative_update), '\n')


ggplot(df_combined, aes(x = relative_update, fill = as.factor(cross_counter_str))) + 
  #geom_histogram(data = df_combined[df_combined$cross_counter == 1 & df_combined$relative_update <= 5000,], binwidth = shared_binwidth, alpha = 1) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 1 & df_combined$relative_update <= 5000,], breaks = seq(0,5000,shared_binwidth), alpha = 1) + 
  #geom_histogram(data = df_combined[df_combined$cross_counter == 2 & df_combined$first_cross_update <= 5000 & df_combined$relative_update <= 5000,], binwidth = shared_binwidth, alpha = 0.5) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 2 & df_combined$first_cross_update <= 5000 & df_combined$relative_update <= 5000,], breaks = seq(0,5000,shared_binwidth), alpha = 0.5) + 
  #geom_hline(aes(yintercept = first_expectation)) + 
  #geom_hline(aes(yintercept = second_expectation), linetype = 'dashed') + 
  xlab('Relative generation') + 
  ylab('Count') + 
  labs(fill = 'Valley\ncrossed') +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title = element_text(size = 16)) + 
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) + 
  scale_fill_manual(values = c(color_map_value['18'][[1]], color_map_value['24'][[1]])) + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0))
ggsave(paste0(plot_dir, '/first_two_crosses_overlayed_fair_comparison.png'), units = 'in', width = 8, height = 6)
ggsave(paste0(plot_dir, '/first_two_crosses_overlayed_fair_comparison.pdf'), units = 'in', width = 5, height = 3)

write.csv(df_combined[df_combined$cross_counter == 1 & df_combined$relative_update <= 5000,], '../data/first_cross_data.csv', row.names=F)
write.csv(df_combined[df_combined$cross_counter == 2 & df_combined$first_cross_update <= 5000 & df_combined$relative_update <= 5000,], '../data/second_cross_data.csv', row.names=F)

ggplot(df_combined, aes(x = relative_update, fill = as.factor(cross_counter_str))) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 1 & df_combined$relative_update <= 5000,]) + #, binwidth = shared_binwidth, alpha = 1) + 
  #geom_histogram(data = df_combined[df_combined$cross_counter == 1 & df_combined$relative_update <= 5000,], breaks = seq(0,5000,shared_binwidth), alpha = 1) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 2 & df_combined$first_cross_update <= 5000 & df_combined$relative_update <= 5000,]) + #, binwidth = shared_binwidth, alpha = 0.5) + 
  #geom_histogram(data = df_combined[df_combined$cross_counter == 2 & df_combined$first_cross_update <= 5000 & df_combined$relative_update <= 5000,], breaks = seq(0,5000,shared_binwidth), alpha = 0.5) + 
  #geom_hline(aes(yintercept = first_expectation)) + 
  #geom_hline(aes(yintercept = second_expectation), linetype = 'dashed') + 
  xlab('Relative generation') + 
  ylab('Count') + 
  labs(fill = 'Valley\ncrossed') +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title = element_text(size = 16)) + 
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) + 
  scale_fill_manual(values = c(color_map_value['18'][[1]], color_map_value['24'][[1]])) + 
  scale_x_log10(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0))
ggsave(paste0(plot_dir, '/first_two_crosses_overlayed_fair_comparison_log_x.png'), units = 'in', width = 8, height = 6)
ggsave(paste0(plot_dir, '/first_two_crosses_overlayed_fair_comparison_log_x.pdf'), units = 'in', width = 5, height = 3)


# Extract fixation time from other exp 
df_fixation_edge = read.csv('../../2024_04_04_01__sweep_timing_edge/data/combined_sweep_info.csv')
df_fixation_center = read.csv('../../2024_04_04_02__sweep_timing_center/data/combined_sweep_info.csv')

df_fixation_edge$class = 'edge'
df_fixation_center$class = 'center'

df_fixation = rbind(df_fixation_edge, df_fixation_center)

ggplot(df_fixation, aes(x = sweep_time, fill = class)) + 
  geom_histogram(bins = 100) + 
  scale_x_continuous(limits = c(0, max(df_fixation$sweep_time)))

fixation_time_center_mean = mean(df_fixation_center$sweep_time)
fixation_time_center_min = min(df_fixation_center$sweep_time)
fixation_time_edge_mean = mean(df_fixation_edge$sweep_time)
fixation_time_edge_max = max(df_fixation_edge$sweep_time)

ggplot(df_combined, aes(x = relative_update, fill = as.factor(cross_counter_str))) + 
  #geom_histogram(data = df_combined[df_combined$cross_counter == 1 & df_combined$relative_update <= 5000,], binwidth = shared_binwidth, alpha = 1) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 1 & df_combined$relative_update <= 5000,], breaks = seq(0,5000,shared_binwidth), alpha = 1) + 
  #geom_histogram(data = df_combined[df_combined$cross_counter == 2 & df_combined$first_cross_update <= 5000 & df_combined$relative_update <= 5000,], binwidth = shared_binwidth, alpha = 0.5) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 2 & df_combined$first_cross_update <= 5000 & df_combined$relative_update <= 5000,], breaks = seq(0,5000,shared_binwidth), alpha = 0.75) + 
  #geom_hline(aes(yintercept = first_expectation)) + 
  #geom_hline(aes(yintercept = second_expectation), linetype = 'dashed') + 
  #geom_vline(aes(xintercept = fixation_time_center_mean)) + 
  #geom_vline(aes(xintercept = fixation_time_edge_mean)) + 
  geom_vline(aes(xintercept = fixation_time_center_min), linetype = 'dashed', size = 1, alpha = 0.5) + 
  geom_vline(aes(xintercept = fixation_time_edge_max), linetype='solid', size = 1, alpha = 0.5) + 
  xlab('Relative generation') + 
  ylab('Count') + 
  labs(fill = 'Valley\ncrossed') +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title = element_text(size = 16)) + 
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) + 
  scale_fill_manual(values = c(color_map_value['12'][[1]], color_map_value['18'][[1]])) + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,3))
  #theme(panel.background = element_rect(fill = '#dddddd')) + 
  #theme(panel.grid.major = element_line(color = '#aaaaaa')) +
  #theme(panel.grid.minor = element_line(color = '#cccccc'))
  #scale_y_log10()
ggsave(paste0(plot_dir, '/first_two_crosses_overlayed_fair_comparison_with_fixation.png'), units = 'in', width = 8, height = 6)
ggsave(paste0(plot_dir, '/first_two_crosses_overlayed_fair_comparison_with_fixation.pdf'), units = 'in', width = 5, height = 3)

ggplot(df_combined, aes(x = relative_update, fill = as.factor(cross_counter_str))) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 1 & df_combined$relative_update <= 5000,]) + #, binwidth = shared_binwidth, alpha = 1) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 2 & df_combined$first_cross_update <= 5000 & df_combined$relative_update <= 5000,]) + #, binwidth = shared_binwidth, alpha = 0.5) + 
  geom_vline(aes(xintercept = fixation_time_center_mean)) + 
  geom_vline(aes(xintercept = fixation_time_edge_mean)) + 
  geom_vline(aes(xintercept = fixation_time_center_min), linetype = 'dashed') + 
  geom_vline(aes(xintercept = fixation_time_edge_max), linetype='dashed') + 
  xlab('Relative generation') + 
  ylab('Count') + 
  labs(fill = 'Valley\ncrossed') +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title = element_text(size = 16)) + 
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) + 
  scale_fill_manual(values = c(color_map_value['18'][[1]], color_map_value['24'][[1]])) + 
  scale_x_log10(expand = c(0,0), breaks = c(1,5,10,50,100,500,1000,5000), minor_breaks = c(1:10, seq(20, 100, by = 10), seq(200, 1000, by = 100), seq(2000, 5000, 1000))) + 
  scale_y_continuous(expand = c(0,0))
ggsave(paste0(plot_dir, '/first_two_crosses_overlayed_fair_comparison_log_x_with_fixation.png'), units = 'in', width = 8, height = 6)
ggsave(paste0(plot_dir, '/first_two_crosses_overlayed_fair_comparison_log_x_with_fixation.pdf'), units = 'in', width = 5, height = 3)
