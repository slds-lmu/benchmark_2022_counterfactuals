rm(list = ls())
set.seed(867853)
data.table::setDTthreads(1L)
#----
# 0) Load helper functions & libraries
#----
TEST = FALSE

# Setup
source("cfactuals/hill-valley/libs_cfactuals.R")
source("cfactuals/hill-valley/helper_cfactuals_wrapper.R")

if (TEST) {
  source("cfactuals/hill-valley/def_cfactuals_test.R")
} else {
  source("cfactuals/hill-valley/def_cfactuals.R")
}

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
} else {
  reg$cluster.functions = makeClusterFunctionsMulticore(n_cores)
}


# Add problems
for (i in seq_along(data_list)) {
  addProblem(name = names(data_list)[[i]], data = data_list[[i]], seed = 5241541)
}

# Add algos
addAlgorithm(name = "whatif", fun = whatif_wrapper)
addAlgorithm(name = "nice", fun = nice_wrapper)
addAlgorithm(name = "moc", fun = moc_wrapper)
addAlgorithm(name = "random_search", fun = random_search_wrapper)

# Add experiments
addExperiments(algo.designs = ades)
summarizeExperiments()

# testJob(id = 14)

# Run
submitJobs()
waitForJobs()
getStatus()

getErrorMessages()

