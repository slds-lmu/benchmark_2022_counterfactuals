#----
# Overview of required packages
#----
source("config.R")

# Packages only required when TEST = TRUE
packages = c(
  ### data: make get-data
  "mlr3oml", 
  ### models: make train-models & make resample
  "batchtools", "mlr3", "mlr3learners", "mlr3pipelines", "mlr3tuning", 
  ### counterfactuals: make find-counterfactuals
  "counterfactuals", "iml", "data.table",
  ### evaluation: make plot-results 
  "magrittr", "ggplot2", "tidyverse", "DBI", "dplyr", "tidyr", "batchtools", 
  "data.table", "broom", "ggpubr", "scales"
)

# Additional packages if TEST = FALSE
if (!TEST) {
  packages = c(packages, 
    ### models: make train-models & make resample
    "remotes", "xgboost", "ranger", "e1071", "keras", "mlr3keras"
  )
  if (!"mlr3keras" %in% installed.packages()) remotes::install_github("mlr-org/mlr3keras")
}

# Load packages or install them
new_packages = packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0L) install.packages(new_packages)