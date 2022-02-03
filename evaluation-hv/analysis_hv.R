library(DBI)
library(magrittr)
library(data.table)
library(ggplot2)
library(tidyverse)
source("evaluation-hv/hv_analysis_helper.R")

plot_hv_comparison(
  c("overall", "bank8FM", "credit_g","hill_valley", "diabetes", "run_or_walk_information", "tic_tac_toe")
)
