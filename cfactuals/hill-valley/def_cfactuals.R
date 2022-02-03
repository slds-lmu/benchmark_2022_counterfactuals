# DATA configurations
data_list <- readRDS(file.path("models/hill-valley/data_list.RDS"))

# ades configurations
models = c("xgboost", "logistic_regression", "ranger", "svm", "neural_network")
           
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
n_cores <- 14L
registry_dir <- "cfactuals/hill-valley/registry"
if (!dir.exists(dirname(registry_dir))) dir.create(dirname(registry_dir))
