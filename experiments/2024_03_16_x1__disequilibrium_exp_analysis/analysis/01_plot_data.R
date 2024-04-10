rm(list = ls())

library(ggplot2)

df_summary = read.csv('../data/disequilibrium_exp_summary_data.csv')

plot_dir = '../plots'
if(!dir.exists(plot_dir)) dir.create(plot_dir)

ggplot(df_summary, aes(x = as.factor(treatment), y = first_cross_frac)) + 
  geom_errorbar(aes(ymin = first_cross_lower_ci_99, ymax = first_cross_upper_ci_99), width = 0.1) + 
  geom_point() + 
  xlab('Treatment') + 
  ylab('Fraction of replicates that crossed') + 
  theme(axis.title = element_text(size = 16)) + 
  theme(axis.text = element_text(size = 14))
ggsave(paste0(plot_dir, '/disequilibrium_exp_zoomed.png'), units = 'in', width = 4, height = 6)

ggplot(df_summary, aes(x = as.factor(treatment), y = first_cross_frac)) + 
  geom_errorbar(aes(ymin = first_cross_lower_ci_99, ymax = first_cross_upper_ci_99), width = 0.15) + 
  geom_point() + 
  scale_y_continuous(limits = c(0,1)) + 
  xlab('Treatment') + 
  ylab('Fraction of replicates that crossed') +
  theme(axis.title = element_text(size = 16)) + 
  theme(axis.text = element_text(size = 14))
ggsave(paste0(plot_dir, '/disequilibrium_exp_full.png'), units = 'in', width = 5, height = 6)
