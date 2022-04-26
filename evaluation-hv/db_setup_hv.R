library(DBI)
library(dplyr)
library(tidyr)
library(batchtools)
library(keras)
library(data.table)
library(ggplot2)

source("evaluation-hv/helper_db_setup_hv.R")

## --------------
reg_dir = "cfactuals/prod/registry/"
add_hv_results_to_db("diabetis")
add_hv_results_to_db("tic_tac_toe")
add_hv_results_to_db("credit_g")
add_hv_results_to_db("run_or_walk_info")
add_hv_results_to_db("bank8FM")
add_hv_results_to_db("run_or_walk_info")
add_hv_results_to_db("run_or_walk_info_sub_1")
add_hv_results_to_db("run_or_walk_info_sub_10")
add_hv_results_to_db("hill_valley")
add_hv_results_to_db("hill_valley_10")
add_hv_results_to_db("hill_valley_30")


