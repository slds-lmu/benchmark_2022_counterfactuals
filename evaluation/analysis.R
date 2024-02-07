library(DBI)
library(magrittr)
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(scales)
source("evaluation/analysis_helper.R")

methods = c("moc", "countarf", "mocarf_arf_single", "mocarf_arf_multi")

# plot objective ranks
plot_comparison_ranks_with_lines(nummin = 1L, methods = methods)
plot_comparison_ranks_with_lines(orientation = "dataset", methods = methods)

# plot hypervolume and no. counterfactuals
plot_hypervolume(log = TRUE, methods = methods)

# plot speed comparison
plot_speed_comparison(type = "n", savepdf = TRUE, methods = methods)
plot_speed_comparison(type = "p", savepdf = TRUE, methods = methods)

# for all methods
all_methods = c("moc", "nice" , "whatif")
plot_comparison("diabetis", methods = all_methods)
plot_comparison("tic_tac_toe", methods = all_methods)
plot_comparison("credit_g", methods = all_methods)
plot_comparison("run_or_walk_info", methods = all_methods)
plot_comparison("bank8FM", methods = all_methods)
plot_comparison("hill_valley", methods = all_methods)
