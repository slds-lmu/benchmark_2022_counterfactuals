# DATA configurations
data_list <- readRDS(file.path("data/data_storage_test/data_list.RDS"))

# ades configurations
ades = list(
  whatif = CJ(
    n_counterfactuals = 10L,
    id_x_interest = 1:2,
    model_name = c("ranger", "logistic_regression")
  ),
  nice = CJ(
    optimization = c("sparsity", "proximity", "plausibility"),
    id_x_interest = 1:2, 
    model_name = c("ranger", "logistic_regression")
  ),
  moc = CJ(
    init_strategy = c("random", "icecurve"),
    use_conditional_mutator = c(TRUE, FALSE),
    id_x_interest = 1:2,
    model_name = c("ranger", "logistic_regression")
  )
)

# BATCHTOOLS configurations
n_cores <- 4L
registry_dir <- "cfactuals/test/registry_test"
if (!dir.exists(dirname(registry_dir))) dir.create(dirname(registry_dir))
