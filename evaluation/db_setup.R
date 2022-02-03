library(DBI)
library(dplyr)
library(tidyr)
library(batchtools)
library(keras)
library(data.table)
library(ggplot2)

source("evaluation/helper_db_setup.R")
# 
# res = readRDS("evaluation/diabetes_data.R")
# 
# con <- dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
# dbWriteTable(con, "RESULTS", as.data.frame(res), overwrite = TRUE)
# tbl(con, "RESULTS") %>% collect()
# dbAppendTable(con, "RESULTS", res)
# 
# dbDisconnect(con)
# 
# dbSendQuery(con, "DROP TABLE RESULTS")


## --------------
reg_dir = "cfactuals/prod/registry"
reg = loadRegistry(reg_dir, make.default = FALSE)
add_results_to_db("diabetis", reg)
add_results_to_db("tic_tac_toe", reg)
add_results_to_db("credit_g", reg)
add_results_to_db("bank8FM", reg)
add_results_to_db("run_or_walk_info", reg)

add_results_to_db("run_or_walk_info_sub_1", reg)
add_results_to_db("run_or_walk_info_sub_10", reg)



source("evaluation/hill-valley/helper_db_setup_hill_valley.R")
reg_dir = "cfactuals/hill-valley/registry"
reg = loadRegistry(reg_dir, make.default = FALSE)
add_results_to_db_hill_valley("hill_valley", reg)
add_results_to_db_hill_valley("hill_valley_10", reg)
add_results_to_db_hill_valley("hill_valley_30", reg)

