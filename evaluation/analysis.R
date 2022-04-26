library(DBI)
library(magrittr)
library(data.table)
library(ggplot2)
library(tidyverse)
source("evaluation/analysis_helper.R")

plot_comparison("diabetis")
plot_comparison("tic_tac_toe")
plot_comparison("credit_g")
plot_comparison("run_or_walk_info")
plot_comparison("bank8FM")
plot_comparison("hill_valley")

plot_speed_comparison(type = "n")
plot_speed_comparison(type = "p")

psc_all1n  = plot_speed_comparison(methods = c("moc_icecurve_0", "moc_icecurve_1", "moc_random_0", "moc_random_1", "moc_sd_0", "moc_sd_1",
                                  "moc_traindata_0", "moc_traindata_1", "nice_sparsity", "nice_proximity", "nice_plausibility" , "whatif"), 
                                  savepdf = FALSE)
# ggsave(filename = "evaluation/figures/n_runtime_all.pdf", plot = psc_all1n, width = 4, height = 9)

psc_all1  = plot_speed_comparison(methods = c("moc_icecurve_0", "moc_icecurve_1", "moc_random_0", "moc_random_1", "moc_sd_0", "moc_sd_1",
                                              "moc_traindata_0", "moc_traindata_1", "nice_sparsity", "nice_proximity", "nice_plausibility" , "whatif"), 
                                  savepdf = FALSE, type = "p")
# ggsave(filename = "evaluation/figures/p_runtime_all.pdf", plot = psc_all1, width = 4, height = 9)
