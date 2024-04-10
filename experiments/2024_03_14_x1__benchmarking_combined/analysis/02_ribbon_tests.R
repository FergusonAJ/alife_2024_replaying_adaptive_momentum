rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

plot_dir = '../plots'
if(!dir.exists(plot_dir)){
  dir.create(plot_dir, recursive = T)
}

df_batch_1 = read.csv('../../2024_03_14_01__benchmarking_10k/data/processed_summary.csv')
df_batch_1$extra_orgs = 7
df_single_org = read.csv('../../2024_03_21_01__benchmarking_1_org/data/processed_summary.csv')
df_single_org$extra_orgs = 0
df_12_org = read.csv('../../2024_04_07_01__benchmarking_12_orgs/data/processed_summary.csv')
df_12_org$extra_orgs = 11
df_4_org = read.csv('../../2024_04_07_02__benchmarking_4_orgs/data/processed_summary.csv')
df_4_org$extra_orgs = 3 
df_16_org = read.csv('../../2024_04_07_03__benchmarking_16_orgs/data/processed_summary.csv')
df_16_org$extra_orgs = 15

df_combined = rbind(df_batch_1, df_single_org, df_12_org, df_4_org, df_16_org)

df_ribbon = tidyr::pivot_wider(df_combined, names_from = extra_orgs, values_from=c(crossed_frac, count))
write.csv(df_ribbon, '../data/widened_combined_ribbon_data.csv')

ggplot(df_ribbon[df_ribbon$leading_edge_index >= 8,], aes(x = leading_edge_index)) + 
  geom_ribbon(aes(ymin = crossed_frac_0 * 100, ymax = crossed_frac_11 * 100, fill = as.factor(leading_edge_val)), alpha = 0.5) + 
  geom_line(aes(y = crossed_frac_0 * 100, color = as.factor(leading_edge_val))) + 
  geom_line(aes(y = crossed_frac_3 * 100, color = as.factor(leading_edge_val)), linetype='dotted') + 
  geom_line(aes(y = crossed_frac_7 * 100, color = as.factor(leading_edge_val)), linetype='dotdash') + 
  geom_line(aes(y = crossed_frac_15 * 100, color = as.factor(leading_edge_val)), linetype='dashed') + 
  geom_line(aes(y = crossed_frac_11 * 100, color = as.factor(leading_edge_val))) + 
  scale_y_continuous(limits = c(0,100)) + 
  xlab('Leading Edge Index') + 
  ylab('Potential to cross') + 
  labs(color = 'Leading edge value') +
  labs(fill = 'Leading edge value')
ggsave(paste0(plot_dir, '/script_02__plot_00__ribbons.png'), units = 'in', width = 8, height = 4.5)

ggplot(df_ribbon[df_ribbon$leading_edge_index >= 8,], aes(x = leading_edge_index)) + 
  geom_line(aes(y = crossed_frac_0 * 100, color = '1')) + 
  geom_line(aes(y = crossed_frac_3 * 100, color = '4')) + 
  geom_line(aes(y = crossed_frac_7 * 100, color = '8')) + 
  geom_line(aes(y = crossed_frac_11 * 100, color = '12')) + 
  geom_line(aes(y = crossed_frac_15 * 100, color = '16')) + 
  scale_y_continuous(limits = c(0,100)) + 
  xlab('Leading Edge Index') + 
  ylab('Potential to cross') + 
  labs(color = 'Leading edge value') +
  labs(fill = 'Leading edge value') + 
  facet_wrap(vars(as.factor(leading_edge_val)))
ggsave(paste0(plot_dir, '/script_02__plot_01__faceted.png'), units = 'in', width = 8, height = 4.5)


ggplot(df_ribbon[df_ribbon$leading_edge_index >= 8,], aes(x = leading_edge_index)) + 
  geom_line(aes(y = 100 * crossed_frac_0 - crossed_frac_7  * 100, color = '1')) + 
  geom_line(aes(y = 100 * crossed_frac_3  - crossed_frac_7 * 100, color = '4')) + 
  geom_line(aes(y = 100 * crossed_frac_7  - crossed_frac_7 * 100, color = '8')) + 
  geom_line(aes(y = 100 * crossed_frac_11 - crossed_frac_7 * 100, color = '12')) + 
  geom_line(aes(y = 100 * crossed_frac_15 - crossed_frac_7 * 100, color = '16')) + 
  scale_y_continuous(limits = c(-45,20)) + 
  xlab('Leading Edge Index') + 
  ylab('Difference from 8 orgs') + 
  labs(color = 'Leading edge value') +
  labs(fill = 'Leading edge value') + 
  facet_wrap(vars(as.factor(leading_edge_val)))
ggsave(paste0(plot_dir, '/script_02__plot_02__faceted_diff.png'), units = 'in', width = 8, height = 4.5)
