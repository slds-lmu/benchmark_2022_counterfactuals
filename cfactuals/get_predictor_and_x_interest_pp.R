get_predictor_and_x_interest_pp = function(arg_list, job, data) {
  library(batchtools)
  target_name = names(data)[ncol(data)]
  is_keras = arg_list$model_name == "neural_network"
  
  if (grepl("run_or_walk_info", job$prob.name, fixed = TRUE)) {
    prob_name = "run_or_walk_info"
  } else {
    prob_name = job$prob.name
  }
  
  if (is_keras) {
    # if (TEST) {
    #   model_dir = file.path("models/test/keras", arg_list$model_name)
    # } else {
      model_dir = file.path("models/prod/keras", arg_list$model_name)
    # }
    path_pipeline = file.path(model_dir, paste0(prob_name, "_po.rds"))
    pipeline = readRDS(path_pipeline)
    model_path = file.path(model_dir, paste0(prob_name, "_model.hdf5"))
    this_model = load_model_hdf5(model_path)
    pred = Predictor$new(
      this_model, data = data, y = target_name, 
      predict.function = function(model, newdata) {
        newdata = data.table::as.data.table(newdata)
        factor_cols = names(which(sapply(data, is.factor)))
        for (factor_col in factor_cols) {
          fact_col_data = data[[factor_col]]
          value = factor(newdata[[factor_col]], levels = levels(fact_col_data), ordered = is.ordered(fact_col_data))
          set(newdata, j = factor_col, value = value)
        }
        int_cols = names(which(sapply(data, is.integer)))
        if (length(int_cols) > 0L) {
          newdata[,(int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
        }
        newdata[, (target_name) := data[[target_name]][1]]
        newdata = pipeline$predict(as_task_classif(newdata, target = target_name))[[1]]$data()
        yhat = model %>% predict(as.matrix(newdata[, -1L]))
        yhat = data.table::as.data.table(yhat)
        names(yhat) = levels(data[[target_name]])
        yhat
      },
      type = "prob" 
    )
  } else {
    if (TEST) {
      models_path = "models/prod/registry_TEST"
    } else {
      models_path = "models/prod/registry"
    }
    model_registry = loadRegistry(models_path, make.default = FALSE)
    model_job_params = unwrap(getJobPars(reg = model_registry))  
    job_id = model_job_params[problem == prob_name & algorithm == arg_list$model_name]
    this_model = readRDS(file.path(models_path, "results", paste0(job_id$job.id, ".rds")))
    pred = Predictor$new(this_model, data = data, y = target_name, type = "prob" )
  }
  pred
}
