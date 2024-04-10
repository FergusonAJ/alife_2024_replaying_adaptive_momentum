rm(list = ls())

library(ggplot2)
library(scales)

plot_dir = '../plots'
if(!dir.exists(plot_dir)){
  dir.create(plot_dir, recursive = T)
}

df_summary = read.csv('../data/processed_summary.csv')
df_summary$paper_leading_edge_val = 'p[2]'
df_summary[df_summary$leading_edge_val == 13,]$paper_leading_edge_val = 'p[2] + 1'
df_summary[df_summary$leading_edge_val == 14,]$paper_leading_edge_val = 'p[2] + 2'
df_summary[df_summary$leading_edge_val == 15,]$paper_leading_edge_val = 'p[2] + 3'
df_summary[df_summary$leading_edge_val == 16,]$paper_leading_edge_val = 'p[2] + 4'
df_summary[df_summary$leading_edge_val == 17,]$paper_leading_edge_val = 'p[2] + 5'
df_summary$paper_leading_edge_val = 'a'
df_summary[df_summary$leading_edge_val == 13,]$paper_leading_edge_val = 'b'
df_summary[df_summary$leading_edge_val == 14,]$paper_leading_edge_val = 'c'
df_summary[df_summary$leading_edge_val == 15,]$paper_leading_edge_val = 'd'
df_summary[df_summary$leading_edge_val == 16,]$paper_leading_edge_val = 'e'
df_summary[df_summary$leading_edge_val == 17,]$paper_leading_edge_val = 'f'

ggplot(df_summary, aes(x = leading_edge_index, y = crossed_frac, color = paper_leading_edge_val)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(limits = c(0,1)) + 
  xlab('Leading Edge Index') + 
  ylab('Fraction of replicates that cross') + 
  labs(color = 'Leading edge\n     value') + 
  theme(axis.title = element_text(size = 16)) + 
  theme(axis.text = element_text(size = 16)) + 
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.position = 'bottom') + 
  scale_color_manual(values = hue_pal()(6), labels = c(
    a = expression(p[2]  ), 
    b = expression(p[2] + 1),
    c = expression(p[2] + 2),
    d = expression(p[2] + 3),
    e = expression(p[2] + 4),
    f = expression(p[2] + 5)
  ))
ggsave(paste0(plot_dir, '/benchmarking.png'), units = 'in', width = 8, height = 6)
ggsave(paste0(plot_dir, '/benchmarking.pdf'), units = 'in', width = 5, height = 5)

