read_in_cfactual_obj_hill_valley_hv = function(this_job, reg) {
  
  cfactual = loadResult(this_job$job.id, reg)
  
  if (grepl("hill_valley", this_job$problem, fixed = TRUE)) {
    data_set_name = "hill_valley"
  } else {
    data_set_name = this_job$problem
  }
  is_keras = this_job$model_name == "neural_network"
  if (is_keras) {
    data = readRDS("data/data_storage/data_list.RDS")[[data_set_name]]$data
    target_name = names(data)[ncol(data)]
    model_dir = file.path("models/hill-valley/keras/neural_network")
    path_pipeline = file.path(model_dir, paste0(data_set_name, "_po.rds"))
    pipeline = readRDS(path_pipeline)
    model_path = file.path(model_dir, paste0(data_set_name, "_model.hdf5"))
    this_model = load_model_hdf5(model_path)
    cfactual$.__enclos_env__$private$predictor = Predictor$new(
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
    cfactual$.__enclos_env__$private$predictor$task = "classification"
  }
  attr(cfactual, "time_running") <- getJobStatus(this_job, reg)$time.running
  cfactual
}


add_results_to_db_hill_valley_hv = function(data_set_name, reg) {
  reg_dir = "cfactuals/hill-valley/registry"
  reg = loadRegistry(reg_dir, make.default = FALSE)
  job_overview = unwrap(getJobPars(reg = reg))
  jobs_of_this_data_set = job_overview[problem == data_set_name]
  
  template_names = c(
    names(readRDS("models/hill-valley/data_list.RDS")[[data_set_name]]$data),
    c("job.id", "algorithm", "model_name", "id_x_interest", "r1", "r2", "r3", "r4")
  )
  template = data.frame(matrix(NA, nrow = 0, ncol = length(template_names)))
  names(template) = template_names  
  
  con = dbConnect(RSQLite::SQLite(), "evaluation-hv/db_evals_hv.db")
  
  for (job_id in jobs_of_this_data_set$job.id) {
    this_job = job_overview[job.id == job_id]
    cfactuals_obj = try(read_in_cfactual_obj_hill_valley_hv(this_job, reg))
    if (inherits(cfactuals_obj, "try-error")) next
    print(job_id)
    if (nrow(cfactuals_obj$data) > 0L) {
      cfactuals = cfactuals_obj$evaluate()
      r1 = r2 = r3 = r4 = NA
      if (this_job$algorithm %in% c("moc", "random_search")) {
        y_hat_interest = cfactuals_obj$.__enclos_env__$private$predictor$predict(cfactuals_obj$x_interest)[[cfactuals_obj$desired$desired_class]]
        r1 = min(abs(y_hat_interest - cfactuals_obj$desired$desired_prob))
        r2 = 1
        r3 = ncol(cfactuals_obj$x_interest)
        r4 = 1
      }
      comb = cbind(
        cfactuals[1, ], this_job,
        "dominated_hv" = as.numeric(attr(cfactuals_obj, "dominated_hv")[["hv"]]), 
        "generation" = as.numeric(attr(cfactuals_obj, "dominated_hv")[["generations"]]),
        "r1" = r1, "r2" = r2, "r3" = r3, "r4" = r4
      )
      dt_standard = rbindlist(list(template, comb), fill = TRUE)
      dt_standard = dt_standard %>% mutate(algo_spec = paste(algorithm, init_strategy, use_conditional_mutator, sep = "_"))
      if (!toupper(data_set_name) %in% DBI::dbListTables(con)) {
        dbWriteTable(con, toupper(data_set_name), as.data.frame(dt_standard), overwrite = TRUE)
      } else {
        dbAppendTable(con, toupper(data_set_name), as.data.frame(dt_standard))
      }
    }
  }
  
  dbExecute(con, paste("ALTER TABLE", toupper(data_set_name), "ADD COLUMN ID INT"))
  res = tbl(con, toupper(data_set_name)) %>% collect()
  res$ID = 1:nrow(res) + 4800 # just an artifact
  dbWriteTable(con, toupper(data_set_name), as.data.frame(res), overwrite = TRUE)
  dbDisconnect(con)
}