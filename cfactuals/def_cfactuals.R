# DATA configurations
data_list = readRDS(file.path("data/data_storage/data_list.RDS"))
data_run_or_walk = data_list$run_or_walk_info$data
data_list$run_or_walk_info_sub_1 = list(data = data_run_or_walk[sample(.N, ceiling(nrow(data_run_or_walk) * 0.01))])
data_list$run_or_walk_info_sub_10 = list(data = data_run_or_walk[sample(.N, ceiling(nrow(data_run_or_walk) * 0.1))])
data_list$hill_valley = NULL # has separate folder hill-valley (together with its subsets)

# ades configurations
models = c("ranger", "xgboost", "svm", "logistic_regression", "neural_network")
           
ades = list(
  whatif = CJ(
    n_counterfactuals = 10L,
    id_x_interest = 1:10,
    model_name = models
  ),
  nice = CJ(
    optimization = c("sparsity", "proximity", "plausibility"),
    id_x_interest = 1:10, 
    model_name = models
  ),
  moc = CJ(
    init_strategy = c("random", "icecurve", "sd", "traindata"),
    use_conditional_mutator = c(TRUE, FALSE),
    id_x_interest = 1:10,
    model_name = models
  ),
  random_search = CJ(
    id_x_interest = 1:10,
    model_name = models
  )
)

# BATCHTOOLS configurations
n_cores = 14L
registry_dir = "cfactuals/prod/registry"
if (!dir.exists(dirname(registry_dir))) dir.create(dirname(registry_dir))
