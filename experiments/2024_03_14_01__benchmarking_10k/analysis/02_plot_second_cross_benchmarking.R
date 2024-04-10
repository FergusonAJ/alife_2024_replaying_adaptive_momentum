rm(list = ls())

library(ggplot2)
library(scales)
source('../../../global_shared_files/global_analysis_variables.R')

plot_dir = '../plots'
if(!dir.exists(plot_dir)){
  dir.create(plot_dir, recursive = T)
}

df_summary = read.csv('../data/processed_second_cross_summary.csv')
df_summary$paper_leading_edge_val = value_to_letter_map['12']
df_summary[df_summary$leading_edge_val == 13,]$paper_leading_edge_val = value_to_letter_map['13']
df_summary[df_summary$leading_edge_val == 14,]$paper_leading_edge_val = value_to_letter_map['14'] 
df_summary[df_summary$leading_edge_val == 15,]$paper_leading_edge_val = value_to_letter_map['15'] 
df_summary[df_summary$leading_edge_val == 16,]$paper_leading_edge_val = value_to_letter_map['16'] 
df_summary[df_summary$leading_edge_val == 17,]$paper_leading_edge_val = value_to_letter_map['17'] 

ggplot(df_summary, aes(x = leading_edge_index, y = crossed_frac, color = paper_leading_edge_val)) + 
  geom_line() + 
  geom_point() + 
  xlab('Leading Edge Index') + 
  ylab('Fraction of replicates that cross') + 
  labs(color = 'Leading edge\n     value') + 
  theme(axis.title = element_text(size = 16)) + 
  theme(axis.text = element_text(size = 16)) + 
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.position = 'bottom') + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) + 
  scale_color_manual(values = color_map_letters, limits = force, labels = c(
    h = expression(p[2]  ), 
    i = expression(p[2] + 1),
    j = expression(p[2] + 2),
    k = expression(p[2] + 3),
    l = expression(p[2] + 4),
    m = expression(p[2] + 5)
  ))
ggsave(paste0(plot_dir, '/second_cross_benchmarking.png'), units = 'in', width = 8, height = 6)
ggsave(paste0(plot_dir, '/second_cross_benchmarking.pdf'), units = 'in', width = 5, height = 5)

df_wide = tidyr::pivot_wider(df_summary, names_from = c(leading_edge_val, paper_leading_edge_val), values_from = c(count, crossed_frac))

ggplot(df_wide, aes(x = leading_edge_index)) +
  geom_ribbon(aes(ymin = 0, ymax = crossed_frac_12_h, fill = 'h')) + 
  geom_ribbon(aes(ymin = crossed_frac_12_h, ymax = crossed_frac_13_i, fill = 'i')) + 
  geom_ribbon(aes(ymin = crossed_frac_13_i, ymax = crossed_frac_14_j, fill = 'j')) + 
  geom_ribbon(aes(ymin = crossed_frac_14_j, ymax = crossed_frac_15_k, fill = 'k')) + 
  geom_ribbon(aes(ymin = crossed_frac_15_k, ymax = crossed_frac_16_l, fill = 'l')) + 
  geom_ribbon(aes(ymin = crossed_frac_16_l, ymax = crossed_frac_17_m, fill = 'm')) + 
  xlab('Leading Edge Index') + 
  ylab('Fraction of replicates that cross') + 
  labs(fill = 'Leading edge\n     value') + 
  theme(axis.title = element_text(size = 16)) + 
  theme(axis.text = element_text(size = 16)) + 
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.position = 'bottom') + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = color_map_letters, limits = force, labels = c(
    h = expression(p[2]  ), 
    i = expression(p[2] + 1),
    j = expression(p[2] + 2),
    k = expression(p[2] + 3),
    l = expression(p[2] + 4),
    m = expression(p[2] + 5)
  ))
ggsave(paste0(plot_dir, '/second_cross_benchmarking_area.png'), units = 'in', width = 8, height = 6)
ggsave(paste0(plot_dir, '/second_cross_benchmarking_area.pdf'), units = 'in', width = 5, height = 5)
