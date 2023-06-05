packages = c("batchtools", "mlr3", "mlr3learners", "mlr3pipelines", "mlr3tuning", "remotes")
new_packages = packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0L) install.packages(new_packages)

if (!"mlr3keras" %in% installed.packages()) remotes::install_github("mlr-org/mlr3keras")

sapply(packages, require, character.only = TRUE)