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

plot_speed_comparison()
plot_speed_comparison(type = "p")
plot_speed_comparison(
  type = "n",
  methods = c(
    "moc_traindata_0", "moc_traindata_1", "moc_sd_0", "moc_sd_1", "whatif",
    "nice_sparsity", "nice_proximity", "nice_plausibility"
  )
) 
plot_speed_comparison(
  type = "p",
  methods = c(
    "moc_traindata_0", "moc_traindata_1", "moc_sd_0", "moc_sd_1", "whatif",
    "nice_sparsity", "nice_proximity", "nice_plausibility"
  )
) 
