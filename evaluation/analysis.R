library(DBI)
library(magrittr)
library(data.table)
library(ggplot2)
library(tidyverse)
source("evaluation/analysis_helper.R")

# see if for all models cfexp were produced
p1 = check_cfexp_generated(models = "neural_network")
# ggsave("evaluation/figures/nn_proportion.pdf", height = 4, width = 5, plot = p1)

# plot objectives and n
p = plot_comparison_ranks(test = TRUE)
p2 = plot_comparison_ranks(orientation = "dataset", test = TRUE)

# for all methods
all_methods = c("moc", "nice" , "whatif")
plot_comparison("diabetis", methods = all_methods)
plot_comparison("tic_tac_toe", methods = all_methods)
plot_comparison("credit_g", methods = all_methods)
plot_comparison("run_or_walk_info", methods = all_methods)
plot_comparison("bank8FM", methods = all_methods)
plot_comparison("hill_valley", methods = all_methods)

# Speed comparison
plot_speed_comparison(type = "n", savepdf = TRUE)
plot_speed_comparison(type = "p", savepdf = TRUE)

# Coverage
t1 = get_coverage(data_set_name ="diabetis")
t2 = get_coverage(data_set_name ="tic_tac_toe")
t3 = get_coverage(data_set_name = "credit_g")
t4 = get_coverage(data_set_name = "run_or_walk_info")
t5 = get_coverage(data_set_name = "bank8FM")
t6 = get_coverage(data_set_name = "hill_valley")
covmoc = mapply(rbind, t1, t2, t3, t4, t5, SIMPLIFY=FALSE)
rownames(covmoc[[1]]) = c("diabetis", "tic_tac_toe", "credit_g", "run_or_walk_info", "bank8FM")
rownames(covmoc[[2]]) = c("diabetis", "tic_tac_toe", "credit_g", "run_or_walk_info", "bank8FM")
resmoc = do.call(cbind, covmoc)
colnames(resmoc) = c("nice", "whatif")
xtable::xtable(resmoc)

params = list(method1 = "nice", method2 = "moc", method3 = "whatif")
tt1 = do.call(get_coverage, c(list(data_set_name = "diabetis"),params)) 
tt2 = do.call(get_coverage, c(list(data_set_name = "tic_tac_toe"),params)) 
tt3 = do.call(get_coverage, c(list(data_set_name = "diabetis"),params)) 
tt4 = do.call(get_coverage, c(list(data_set_name = "run_or_walk_info"),params)) 
tt5 = do.call(get_coverage, c(list(data_set_name = "bank8FM"),params)) 
# tt6 = do.call(get_coverage, c(list(data_set_name = "hill_valley"),params)) 
covnice = mapply(rbind, tt1, tt2, tt3, tt4, tt5, SIMPLIFY=FALSE)
rownames(covnice[[1]]) = c("diabetis", "tic_tac_toe", "credit_g", "run_or_walk_info", "bank8FM")
rownames(covnice[[2]]) = c("diabetis", "tic_tac_toe", "credit_g", "run_or_walk_info", "bank8FM")
do.call(cbind, covnice)
resnice = do.call(cbind, covnice)
colnames(resnice) = c("moc_icecurve_1", "whatif")
xtable::xtable(resnice)
