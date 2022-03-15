rm(list = ls())
library(batchtools)
source("models/libs_models.R")
registry_dir <- "models/prod/registry"

loadRegistry(registry_dir, writeable = TRUE)
ids = findExperiments(algo.name = "neural_network")
submitJobs(ids)
waitForJobs()
getStatus()
