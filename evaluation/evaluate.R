library("magrittr")
library("ggplot2")
library("tidyverse")
library("DBI")
library("data.table")
library("broom")

source("evaluation/helper_evaluate.R")
source("config.R")

# Run db_setup.R first

add_evals_to_db("diabetis")

if (!TEST) {
  add_evals_to_db("tic_tac_toe")
  add_evals_to_db("credit_g")
  add_evals_to_db("bank8FM")
  add_evals_to_db("run_or_walk_info")
  add_evals_to_db("run_or_walk_info_sub_1")
  add_evals_to_db("run_or_walk_info_sub_10")
  add_evals_to_db("hill_valley")
  add_evals_to_db("hill_valley_10")
  add_evals_to_db("hill_valley_30")
}
