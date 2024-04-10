rm(list = ls())

library(ggplot2)

df_combined = NA
# First load in the replicates in a momentum window
for(rep_id in c('011','050','075','083','105','282','343','400','408','415')){
  df_rep = read.csv(paste0('../../2024_03_18_01__replays/data/processed/processed_summary_rep_', rep_id, '.csv'))
  df_rep$rep_id = rep_id
  df_rep$condition = 'Disequilibrium'
  if(!is.data.frame(df_combined)){
    df_combined = df_rep
  } else {
    df_combined = rbind(df_combined, df_rep)
  }
}

# Now load in the no-window replicates
for(rep_id in c('00833', '01357', '02290', '02359', '03149', '05295', '07051', '07605', '07916', '09839')){
  df_rep = read.csv(paste0('../../2024_03_22_01__no_am_replays/data/processed/processed_summary_rep_', rep_id, '.csv'))
  df_rep$rep_id = rep_id
  df_rep$condition = 'Equilibrium'
  df_combined = rbind(df_combined, df_rep)
}

# First load in the replicates in a momentum window
for(rep_id in c('011','050','075','083','105','282','343','400','408','415')){
  df_rep = read.csv(paste0('../../2024_03_18_01__replays/data/processed/processed_shuffled_summary_rep_', rep_id, '.csv'))
  df_rep$rep_id = rep_id
  df_rep$condition = 'Shuffled'
  df_combined = rbind(df_combined, df_rep)
}

# Create plot dir
plot_dir = '../plots'
if(!dir.exists(plot_dir)) dir.create(plot_dir)

# Plot!
ggplot(df_combined[df_combined$condition != 'Shuffled',], aes(x = frac_crossed * 100)) + 
  geom_histogram(bins = 30) + 
  facet_grid(rows = vars(condition)) + 
  xlab('Potential to cross') + 
  ylab('Count') + 
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 14)) + 
  theme(strip.text = element_text(size = 14))
ggsave(paste0(plot_dir, '/combined_potentiation_histograms.pdf'), units = 'in', width = 5, height = 3.5)

ggplot(df_combined[df_combined$condition != 'Shuffled',], aes(x = frac_crossed * 100)) + 
  geom_histogram(bins = 30) + 
  facet_grid(rows = vars(condition)) + 
  xlab('Potential to cross') + 
  ylab('Count') + 
  scale_y_log10(minor_breaks = c(2:9, seq(20, 90, 10), seq(200, 900, 100))) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 14)) + 
  theme(strip.text = element_text(size = 14)) 
ggsave(paste0(plot_dir, '/combined_potentiation_histograms_log_scale.pdf'), units = 'in', width = 5, height = 3.5)

ggplot(df_combined, aes(x = frac_crossed * 100)) + 
  geom_histogram(bins = 30) + 
  facet_grid(rows = vars(condition)) + 
  xlab('Potential to cross') + 
  ylab('Count') + 
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 14)) + 
  theme(strip.text = element_text(size = 14))
ggsave(paste0(plot_dir, '/combined_potentiation_histograms_with_shuffled.pdf'), units = 'in', width = 5, height = 5)

ggplot(df_combined, aes(x = frac_crossed * 100)) + 
  geom_histogram(bins = 30) + 
  facet_grid(rows = vars(condition)) + 
  xlab('Potential to cross') + 
  ylab('Count') + 
  scale_y_log10(minor_breaks = c(2:9, seq(20, 90, 10), seq(200, 900, 100))) +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 14)) + 
  theme(strip.text = element_text(size = 14)) 
ggsave(paste0(plot_dir, '/combined_potentiation_histograms_width_shuffled_log_scale.pdf'), units = 'in', width = 5, height = 5)
  
