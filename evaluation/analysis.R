library(DBI)
library(magrittr)
library(data.table)
library(ggplot2)
library(tidyverse)
source("evaluation/analysis_helper.R")

p1 = check_cfexp_generated(models = "neural_network")
# ggsave("evaluation/figures/nn_proportion.pdf", height = 4, width = 5, plot = p1)

p2 = plot_comparison("diabetis")
ggsave(filename = "evaluation/figures/diabetes_obj.pdf", plot = p2, width = 5.5, height = 3.8)
p3 = plot_comparison("credit_g")
ggsave(filename = "evaluation/figures/credit_g_obj.pdf", plot = p3, width = 5.5, height = 3.8)

all_methods = c("moc_icecurve_0", "moc_icecurve_1", "moc_random_0", "moc_random_1", "moc_sd_0", "moc_sd_1",
               "moc_traindata_0", "moc_traindata_1", "nice" , "whatif")

plot_comparison("diabetis", methods = all_methods)
plot_comparison("tic_tac_toe", methods = all_methods)
plot_comparison("credit_g", methods = all_methods)
plot_comparison("run_or_walk_info", methods = all_methods)
plot_comparison("bank8FM", methods = all_methods)
plot_comparison("hill_valley", methods = all_methods)

plot_speed_comparison(type = "n")
plot_speed_comparison(type = "p")

psc_all1n  = plot_speed_comparison(methods = c("moc_icecurve_0", "moc_icecurve_1", "moc_random_0", "moc_random_1", "moc_sd_0", "moc_sd_1",
                                  "moc_traindata_0", "moc_traindata_1", "nice_sparsity", "nice_proximity", "nice_plausibility" , "whatif"), 
                                  savepdf = FALSE)
# ggsave(filename = "evaluation/figures/n_runtime_all.pdf", plot = psc_all1n, width = 4.5, height = 9)

psc_all1  = plot_speed_comparison(methods = c("moc_icecurve_0", "moc_icecurve_1", "moc_random_0", "moc_random_1", "moc_sd_0", "moc_sd_1",
                                              "moc_traindata_0", "moc_traindata_1", "nice_sparsity", "nice_proximity", "nice_plausibility" , "whatif"), 
                                  savepdf = FALSE, type = "p")
# ggsave(filename = "evaluation/figures/p_runtime_all.pdf", plot = psc_all1, width = 4.5, height = 9)


