library(DBI)
library(dplyr)
library(tidyr)
library(batchtools)
library(keras)
library(data.table)
library(ggplot2)

source("evaluation-hv/helper_db_setup_hv.R")
# res = readRDS("evaluation/diabetes_data.R")
# 
# con <- dbConnect(RSQLite::SQLite(), "evaluation-hv/db_evals_hv.db")
# dbWriteTable(con, "RESULTS", as.data.frame(res), overwrite = TRUE)
# tbl(con, "RESULTS") %>% collect()
# dbAppendTable(con, "RESULTS", res)
# 
# dbDisconnect(con)
# 
# dbSendQuery(con, "DROP TABLE RESULTS")


## --------------
# add_results_to_db("diabetis")
# add_results_to_db("tic_tac_toe")
# add_results_to_db("credit_g")
# add_results_to_db("run_or_walk_info")
# add_results_to_db("bank8FM")

# add_results_to_db("run_or_walk_info", reg)

# add_results_to_db("run_or_walk_info_sub_1", reg)
# add_results_to_db("run_or_walk_info_sub_10", reg)

source("evaluation-hv/hill-valley/helper_db_setup_hill_valley_hv.R")
# reg_dir = "cfactuals/hill-valley/registry"
# reg = loadRegistry(reg_dir, make.default = FALSE)
add_results_to_db_hill_valley_hv("hill_valley", reg)


