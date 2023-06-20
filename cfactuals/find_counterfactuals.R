rm(list = ls())
set.seed(867853)
data.table::setDTthreads(1L)
#----
# 0) Load helper functions & libraries
#----

# Setup
source("config.R")
source("cfactuals/libs_cfactuals.R")
source("cfactuals/helper_cfactuals_wrapper.R")
source("cfactuals/get_predictor_and_x_interest_pp.R")
source("cfactuals/def_cfactuals.R")

if (TEST) {
  registry_dir = paste0(registry_dir, "_TEST")
}

# Create registry
OVERWRITE = TRUE

# Create registry 
if (file.exists(registry_dir)) {
  if (OVERWRITE) {
    unlink(registry_dir, recursive = TRUE)
    reg = makeExperimentRegistry(file.dir = registry_dir, packages = packages, seed = 20210814)
  } else {
    reg = loadRegistry(registry_dir, writeable = TRUE)
  }
} else {
  reg = makeExperimentRegistry(file.dir = registry_dir, packages = packages, seed = 20210814)
  
}

if (Sys.info()["sysname"] == "Windows") {
} else {
  reg$cluster.functions = makeClusterFunctionsMulticore(n_cores)
}


# Add problems
for (i in seq_along(data_list)) {
  addProblem(name = names(data_list)[[i]], data = data_list[[i]], seed = 457485)
}

# Add algos
addAlgorithm(name = "whatif", fun = whatif_wrapper)
addAlgorithm(name = "nice", fun = nice_wrapper)
addAlgorithm(name = "moc", fun = moc_wrapper)

# Add experiments
addExperiments(algo.designs = ades)
summarizeExperiments()
unwrap(getJobPars())

# Run
if (TEST) {
  ### for TEST only run logistic regression and only one dataset
  lmdiabetisids = unwrap(getJobPars())[model_name == "logistic_regression" & problem == "diabetis" & id_x_interest == 1L, "job.id"]
  for (i in lmdiabetisids$job.id) {
    res = testJob(id = i)
    saveRDS(res, file = paste0(file.path(reg$file.dir, "results/"), i, ".rds"))
  }
} else {
  submitJobs()
  waitForJobs()
  getStatus()
}

