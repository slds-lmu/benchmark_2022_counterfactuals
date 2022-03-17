Sys.setenv('TF_CPP_MIN_LOG_LEVEL' = 2) #switch off messages
packages = c("batchtools", "counterfactuals", "iml", "mlr3oml", "mlr3", "data.table", "R6", "keras", "mlr3keras")
new_packages = packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0L) install.packages(new_packages)

sapply(packages, require, character.only = TRUE)


### this should be run to omit potential messages of r-reticulate 
exp_path = "models/prod/keras/neural_network/diabetis_model.hdf5"
exp_model = load_model_hdf5(exp_path)
rm(exp_model)
rm(exp_path)
