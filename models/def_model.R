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

# ALGO configurations
algos <- c(
  "ranger" = ranger_wrapper,
  "xgboost" = xgboost_wrapper,
  "svm" = svm_wrapper,
  "logistic_regression" = lr_wrapper,
  "neural_network" = nn_wrapper
)

# BATCHTOOLS configurations
n_cores <- 15L
registry_dir <- "models/prod/registry"
if (!dir.exists(dirname(registry_dir))) dir.create(dirname(registry_dir))