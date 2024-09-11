library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(batchtools)
library(keras)
library(data.table)
library(ggplot2)

source("evaluation/helper_db_setup.R")
source("config.R")

## --------------
if (TEST) {
  reg_dir = "cfactuals/prod/registry_TEST"
  db_file = "evaluation/db_evals_test.db"
  if (file.exists(db_file)) file.remove(db_file)
} else {
  reg_dir = "cfactuals/prod/registry/"
  db_file = "evaluation/db_evals.db"
  if (file.exists(db_file)) file.remove(db_file)
}
reg = loadRegistry(reg_dir, make.default = FALSE)
add_results_to_db("diabetis", reg)

if (!TEST) {
  add_results_to_db("tic_tac_toe", reg)
  add_results_to_db("credit_g", reg)
  add_results_to_db("bank8FM", reg)
  add_results_to_db("run_or_walk_info", reg)
  add_results_to_db("hill_valley", reg)
  add_results_to_db("hill_valley_10", reg)
  add_results_to_db("hill_valley_30", reg)
  add_results_to_db("run_or_walk_info_sub_1", reg)
  add_results_to_db("run_or_walk_info_sub_10", reg)
}

