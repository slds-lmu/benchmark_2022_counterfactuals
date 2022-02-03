tuning_config = list(
  resampling = rsmp("cv", folds = 5L),
  outer_resampling = rsmp("cv", folds = 5L),
  measure = msr("classif.ce"),
  tuner = tnr("random_search"),
  terminator = trm("evals", n_evals = 30L)
)
saveRDS(tuning_config, file.path("models/tuning_config.RDS"))


# DATA configurations
data_list <- readRDS(file.path("data/data_storage/data_list.RDS"))
hill_valley <- data_list$hill_valley$data
cold_idx_10 <- c(sample(ncol(hill_valley), 10), ncol(hill_valley))
hill_valley_10 <- hill_valley[, ..cold_idx_10]
cold_idx_30 <- c(sample(ncol(hill_valley), 30), ncol(hill_valley))
hill_valley_30 <- hill_valley[, ..cold_idx_30]

data_list <- list("hill_valley" = hill_valley, "hill_valley_10" = hill_valley_10, "hill_valley_30" = hill_valley_30)
saveRDS(data_list, "models/hill-valley/data_list.RDS")
x_interest_list <- readRDS(file.path("data/data_storage/x_interest_list.RDS"))
x_interest_hill_valley <- x_interest_list$hill_valley
x_interest_hill_valley_10 <- x_interest_hill_valley[, ..cold_idx_10]
x_interest_hill_valley_30 <- x_interest_hill_valley[, ..cold_idx_30]
x_interest_list <- list("hill_valley" = x_interest_hill_valley, "hill_valley_10" = x_interest_hill_valley_10, "hill_valley_30" = x_interest_hill_valley_30)
saveRDS(x_interest_list, "models/hill-valley/x_interest_list.RDS")

# ALGO configurations
algos <- c(
  "xgboost" = xgboost_wrapper,
  "logistic_regression" = lr_wrapper,
  "ranger" = ranger_wrapper,
  "svm" = svm_wrapper,
  "neural_network" = nn_wrapper
)

# BATCHTOOLS configurations
n_cores <- 25L
registry_dir <- "models/hill-valley/registry"
if (!dir.exists(dirname(registry_dir))) dir.create(dirname(registry_dir))