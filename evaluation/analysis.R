library(DBI)
library(magrittr)
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(scales)
source("evaluation/analysis_helper.R")

# plot objective ranks
plot_comparison_ranks_with_lines(nummin = 1L)
plot_comparison_ranks_with_lines(orientation = "dataset")

# plot hypervolume and no. counterfactuals
plot_hypervolume(log = TRUE)

# plot speed comparison
load("evaluation/df_n.rda")
load("evaluation/df_p.rda")
plot_speed_comparison(df_n, type = "n", savepdf = TRUE, log = FALSE)
plot_speed_comparison(df_p, type = "p", savepdf = TRUE, log = TRUE)

# for all methods
all_methods = c("moc", "nice" , "whatif")
plot_comparison("diabetis", methods = all_methods)
plot_comparison("tic_tac_toe", methods = all_methods)
plot_comparison("credit_g", methods = all_methods)
plot_comparison("run_or_walk_info", methods = all_methods)
plot_comparison("bank8FM", methods = all_methods)
plot_comparison("hill_valley", methods = all_methods)
