rm(list = ls())

library(ggplot2)

df_combined = read.csv('../data/combined_cross_info.csv')

plot_dir = '../plots'
if(!dir.exists(plot_dir)) dir.create(plot_dir)


ggplot(df_combined, aes(x = relative_update, fill = as.factor(cross_counter))) + 
  geom_histogram(binwidth = 512) + 
  facet_grid(rows = vars(as.factor(cross_counter))) + 
  xlab('Relative update') + 
  ylab('Count') + 
  labs(fill = 'Cross ID')
ggsave(paste0(plot_dir, '/cross_updates_faceted.png'), units = 'in', width = 8, height = 8)

ggplot(df_combined, aes(x = relative_update, fill = as.factor(cross_counter))) + 
  geom_histogram(binwidth = 512) + 
  xlab('Relative update') + 
  ylab('Count') + 
  labs(fill = 'Cross ID')
ggsave(paste0(plot_dir, '/cross_updates_stacked.png'), units = 'in', width = 8, height = 6)

ggplot(df_combined, aes(x = relative_update, fill = as.factor(cross_counter))) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 1,], binwidth = 512, alpha = 0.5) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 2,], binwidth = 512, alpha = 0.5) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 3,], binwidth = 512, alpha = 0.5) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 4,], binwidth = 512, alpha = 0.5) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 5,], binwidth = 512, alpha = 0.5) + 
  xlab('Relative update') + 
  ylab('Count') + 
  labs(fill = 'Cross ID')
ggsave(paste0(plot_dir, '/cross_updates_overlayed.png'), units = 'in', width = 8, height = 6)

ggplot(df_combined[df_combined$cross_counter == 2 & df_combined$relative_update <= 1024,], aes(x = relative_update)) + 
  geom_vline(xintercept = 512, linetype = 'dashed', alpha = 0.5) +
  geom_vline(xintercept = 768, linetype = 'dotted', alpha = 0.5) +
  geom_histogram(binwidth = 32)
ggsave(paste0(plot_dir, '/second_cross_zoom.png'), units = 'in', width = 8, height = 6)

ggplot(df_combined, aes(x = relative_update, fill = as.factor(cross_counter))) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 1,], binwidth = 512, alpha = 0.65) + 
  geom_histogram(data = df_combined[df_combined$cross_counter == 2,], binwidth = 512, alpha = 0.65) + 
  xlab('Relative update') + 
  ylab('Count') + 
  labs(fill = 'Cross ID')
ggsave(paste0(plot_dir, '/first_two_crosses_overlayed.png'), units = 'in', width = 8, height = 6)