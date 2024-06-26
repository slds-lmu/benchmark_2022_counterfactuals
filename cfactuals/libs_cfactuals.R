# Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2) #switch off messages
packages = c("batchtools", "counterfactuals", "iml", "mlr3oml", "mlr3", "data.table", "tictoc")

if (!TEST) {
  packages = c(packages, "keras", "mlr3keras", "R6")
}

new_packages = packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0L) install.packages(new_packages)

sapply(packages, require, character.only = TRUE)

