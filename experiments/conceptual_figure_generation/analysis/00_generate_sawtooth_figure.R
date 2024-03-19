rm(list = ls())

library(ggplot2)

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

ggplot(df_sawtooth, aes(x = x, y = y)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(y = y + 0.2, label = label), size = 6, parse = T) + 
  xlab('x') + 
  ylab('Score') + 
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title = element_text(size = 16))
ggsave(paste0(plot_dir, '/sawtooth_conceptual_figure.pdf'), units = 'in', width = 5, height = 5)

