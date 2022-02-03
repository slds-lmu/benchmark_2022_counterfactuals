rm(list = ls())
set.seed(867853)
data.table::setDTthreads(1L)
#----
# 0) Load helper functions & libraries
#----
TEST = FALSE

# Setup
source("models/libs_models.R")
source("models/hill-valley/helper_model_wrapper.R")
source("models/hill-valley/def_model.R")


# Create registry
OVERWRITE <- TRUE

# Create registry 
if (file.exists(registry_dir)) {
  if (OVERWRITE) {
    unlink(registry_dir, recursive = TRUE)
    reg = makeExperimentRegistry(file.dir = registry_dir, packages = packages, seed = 123L)
  } else {
    reg = loadRegistry(registry_dir, writeable = TRUE)
  }
} else {
  reg = makeExperimentRegistry(file.dir = registry_dir, packages = packages, seed = 123L)
                               
}

if (Sys.info()["sysname"] == "Windows") {
  n_cores = min(n_cores, 2L)
  reg$cluster.functions = makeClusterFunctionsSocket(n_cores)
} else {
  reg$cluster.functions = makeClusterFunctionsMulticore(n_cores)
}


# Add problems
for (i in seq_along(data_list)) {
  addProblem(name = names(data_list)[[i]], data = data_list[[i]], seed = 5241541)
}

# Add algos
for (i in seq_along(algos)) {
  algo = algos[i]
  addAlgorithm(name = names(algo), fun = algo[[1]])
}

# Add experiments
addExperiments()
summarizeExperiments()

jobs = getJobNames()
experiments = summarizeExperiments()
for (r in seq_len(nrow(jobs))) {
  job = jobs[r]
  experiment = experiments[r]
  setJobNames(r, paste(experiment$problem, experiment$algorithm, sep = "_"))
}
getJobNames()


keras_save_dir = file.path(dirname(registry_dir), "keras")
if (!dir.exists(keras_save_dir)) {
  dir.create(keras_save_dir)
} else {
  unlink(keras_save_dir, recursive = TRUE)
  dir.create(keras_save_dir)
}


# Test jobs
# for (i in seq_len(nrow(jobs))) {
#   print(i)
#   testJob(id = i)
# }
# 
# unlink(keras_save_dir, recursive = TRUE)
# dir.create(keras_save_dir)

# Run
submitJobs()
waitForJobs()
getStatus()
