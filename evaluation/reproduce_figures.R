library("DBI")
library("magrittr")
library("data.table")
library("ggplot2")
library("tidyverse")
library("ggpubr")
library("scales")
source("evaluation/analysis_helper.R")
TEST = FALSE

#-----
# 1) Unzip db_evals.zip
#-----
unzip("evaluation/db_evals.zip", exdir = "evaluation")

#----- 
# 2) Plot results 
#-----
# A) Main paper 

# plot objective ranks
plot_comparison_ranks_with_lines(nummin = 1L)
plot_comparison_ranks_with_lines(orientation = "dataset")

# plot hypervolume and no. counterfactuals
plot_hypervolume(log = TRUE)

# plot speed comparison
plot_speed_comparison(type = "n", savepdf = TRUE)
plot_speed_comparison(type = "p", savepdf = TRUE, log = TRUE)

# b) Appendix
all_methods = c("moc", "nice" , "whatif")
plot_comparison("diabetis", methods = all_methods)
plot_comparison("tic_tac_toe", methods = all_methods)
plot_comparison("credit_g", methods = all_methods)
plot_comparison("run_or_walk_info", methods = all_methods)
plot_comparison("bank8FM", methods = all_methods)
plot_comparison("hill_valley", methods = all_methods)

