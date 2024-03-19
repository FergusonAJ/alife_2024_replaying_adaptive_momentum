rm(list = ls())

library(ggplot2)

plot_dir = '../plots'
if(!dir.exists(plot_dir)){
  dir.create(plot_dir, recursive = T)
}

df_summary = read.csv('../data/processed_summary.csv')

ggplot(df_summary, aes(x = leading_edge_index, y = crossed_frac, color = as.factor(leading_edge_val))) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(limits = c(0,1)) + 
  xlab('Leading Edge Index') + 
  ylab('Fraction of replicates that cross') + 
  labs(color = 'Leading edge value')
ggsave(paste0(plot_dir, '/benchmarking.png'), units = 'in', width = 8, height = 6)
