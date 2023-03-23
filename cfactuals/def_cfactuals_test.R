# DATA configurations
data_list <- readRDS(file.path("data/data_storage/data_list.RDS"))
data_run_or_walk = data_list$run_or_walk_info
data_list$run_or_walk_info_sub_1 = data_run_or_walk[sample(.N, ceiling(nrow(data_run_or_walk) * 0.01))]
data_list$run_or_walk_info_sub_10 = data_run_or_walk[sample(.N, ceiling(nrow(data_run_or_walk) * 0.1))]

# ades configurations
ades = list(
  whatif = CJ(
    id_x_interest = 1:2,
    model_name = c("ranger", "logistic_regression")
  ),
  nice = CJ(
    optimization = c("sparsity", "proximity", "plausibility"),
    id_x_interest = 1:2, 
    model_name = c("ranger", "logistic_regression")
  ),
  moc = CJ(
    id_x_interest = 1:2,
    model_name = c("ranger", "logistic_regression")
  )
)

# BATCHTOOLS configurations
n_cores <- 4L
registry_dir <- "cfactuals/test/registry_test"
if (!dir.exists(dirname(registry_dir))) dir.create(dirname(registry_dir))
