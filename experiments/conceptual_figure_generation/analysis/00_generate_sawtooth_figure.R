rm(list = ls())

library(ggplot2)
source('../../../global_shared_files/global_analysis_variables.R')

plot_dir = '../plots'
if(!dir.exists(plot_dir)) dir.create(plot_dir)

sawtooth_func = function(x, k, d, a, w){
  return(k * x - (k + d) * ((x - a) %% w))
}
parameterized_sawtooth_func = function(x){
  return(sawtooth_func(x, 1/6, 0.05, 6, 6))
}

df_sawtooth = data.frame(data = matrix(nrow = 0, ncol = 2))
colnames(df_sawtooth) = c('x', 'y')

for(x in 5:25){
  df_sawtooth[nrow(df_sawtooth) + 1,] = c(x, parameterized_sawtooth_func(x))
}

df_sawtooth$label = ''
df_sawtooth[df_sawtooth$x == 6,]$label = 'p[1]'
df_sawtooth[df_sawtooth$x == 12,]$label = 'p[2]'
df_sawtooth[df_sawtooth$x == 18,]$label = 'p[3]'
df_sawtooth[df_sawtooth$x == 24,]$label = 'p[4]'

ggplot(df_sawtooth) + 
  geom_line(aes(x = x, y = y)) + 
  geom_point(aes(x = x, y = y)) +
  geom_text(aes(x = x, y = y + 0.75, label = label), size = 6, parse = T) + 
  xlab('Organism genome value') + 
  ylab('Score') + 
  scale_y_continuous(limits = c(-1.5,5), breaks = 0:4, 
                     sec.axis = sec_axis(~ 10^., name = "Fitness", breaks = c(1,10,100,1000,10000), labels = c(
                       expression(10^0),
                       expression(10^1),
                       expression(10^2),
                       expression(10^3),
                       expression(10^4)))) +
  scale_x_continuous(breaks = c(6,12,18,24)) +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title = element_text(size = 16)) + 
  theme(panel.grid.minor = element_blank()) 
ggsave(paste0(plot_dir, '/sawtooth_conceptual_figure.pdf'), units = 'in', width = 5, height = 2)
ggsave(paste0(plot_dir, '/sawtooth_conceptual_figure_taller.png'), units = 'in', width = 5, height = 4)

df_palette = data.frame(data = matrix(nrow = 0, ncol = 2))
colnames(df_palette) = c('x', 'name')
for(x in 5:25){
  adj_idx = x - 4
  df_palette[nrow(df_palette) + 1,] = c(x, letters[adj_idx])
}
df_palette$x = as.numeric(df_palette$x)

ggplot(df_sawtooth) + 
  geom_line(aes(x = x, y = y)) + 
  geom_point(aes(x = x, y = y)) +
  geom_text(aes(x = x, y = y + 0.75, label = label), size = 6, parse = T) + 
  geom_rect(data = df_palette, aes(xmin = x-0.5, xmax = x+0.5, ymin = -1.5, ymax = -0.5, fill = as.factor(name))) + 
  xlab('x') + 
  ylab('Score') + 
  scale_y_continuous(limits = c(-1.5,5), breaks = 0:4) +
  scale_x_continuous(breaks = c(6,12,18,24)) +
  scale_fill_manual(values = color_map_letters) +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title = element_text(size = 16)) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.position = 'none')
ggsave(paste0(plot_dir, '/sawtooth_conceptual_figure_palette.pdf'), units = 'in', width = 5, height = 2)

ggplot(df_sawtooth) + 
  geom_text(aes(x = x, y = 0, label = label), size = 6, parse = T) + 
  geom_rect(data = df_palette, aes(xmin = x-0.5, xmax = x+0.5, ymin = -1.5, ymax = -0.5, fill = as.factor(name))) + 
  xlab('') + 
  ylab('') + 
  scale_y_continuous(limits = c(-1.5, 0.5), breaks = c()) +
  scale_x_continuous(breaks = c(6,12,18,24)) +
  scale_fill_manual(values = color_map_letters) +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title = element_text(size = 16)) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.position = 'none') + 
  theme(axis.text.x = element_blank()) + 
  theme(axis.ticks.x = element_blank()) +
  theme(panel.background = element_rect(fill='#ffffff'))


