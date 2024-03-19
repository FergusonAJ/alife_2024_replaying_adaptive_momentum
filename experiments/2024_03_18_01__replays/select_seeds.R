rm(list = ls())

set.seed(2024031801)

library(dplyr)

df_combined = read.csv('./data/combined_evolution_cross_data.csv')

df_summary = df_combined %>%
  dplyr::group_by(slurm_task_id) %>%
  dplyr::summarize(max_cross = max(cross_counter))

  no_cross_reps = setdiff(1:500, df_summary$slurm_task_id)
  cat('Here are 10 random replicates that did not cross:\n')
  cat('  ', sort(sample(no_cross_reps, 10, replace = F)), '\n')

  single_cross_reps = df_summary[df_summary$max_cross == 1,]$slurm_task_id
  if(length(single_cross_reps) > 10){
        cat('Here are 10 random replicates that crossed exactly once:\n')
    cat('  ', sort(sample(single_cross_reps, 10, replace = F)), '\n')
  } else {
        cat('There are 10 or fewer replicates that crossed exactly once, here are all of them:\n')
    cat('  ', single_cross_reps, '\n')
  }

  double_cross_reps = df_summary[df_summary$max_cross == 2,]$slurm_task_id
  if(length(double_cross_reps) > 10){
        cat('Here are 10 random replicates that crossed exactly twice:\n')
    cat('  ', sort(sample(double_cross_reps, 10, replace = F)), '\n')
  } else {
        cat('There are 10 or fewer replicates that crossed exactly twice, here are all of them:\n')
    cat('  ', double_cross_reps, '\n')
  }



