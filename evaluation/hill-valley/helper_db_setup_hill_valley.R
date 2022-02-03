read_in_cfactual_obj_hill_valley = function(this_job, reg) {
  
  cfactual = readRDS(file.path("cfactuals/hill-valley/registry/results/", paste0(this_job$job.id, ".rds")))
  data_set_name = this_job$problem
  attr(cfactual, "time_running") <- getJobStatus(this_job, reg)$time.running
  cfactual
}



add_results_to_db_hill_valley = function(data_set_name, reg) {
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
  
  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  
  for (job_id in jobs_of_this_data_set$job.id) {
    this_job = job_overview[job.id == job_id]
    cfactuals_obj = try(read_in_cfactual_obj_hill_valley(this_job, reg))
    if (inherits(cfactuals_obj, "try-error")) next
    print(job_id)
    if (nrow(cfactuals_obj$data) > 0L) {
      cfactuals = cfactuals_obj$evaluate()
      r1 = r2 = r3 = r4 = NA
      if (this_job$algorithm == "moc") {
        y_hat_interest = cfactuals_obj$.__enclos_env__$private$predictor$predict(cfactuals_obj$x_interest)[[cfactuals_obj$desired$desired_class]]
        r1 = min(abs(y_hat_interest - cfactuals_obj$desired$desired_prob))
        r2 = 1
        r3 = ncol(cfactuals_obj$x_interest)
        r4 = 1
      }
      comb = cbind(cfactuals, this_job, "time_running" = as.numeric(attr(cfactuals_obj, "time_running")), 
                   "r1" = r1, "r2" = r2, "r3" = r3, "r4" = r4)
      dt_standard = rbindlist(list(template, comb), fill = TRUE)
      if (!toupper(data_set_name) %in% DBI::dbListTables(con)) {
        dbWriteTable(con, toupper(data_set_name), as.data.frame(dt_standard), overwrite = TRUE)
      } else {
        dbAppendTable(con, toupper(data_set_name), as.data.frame(dt_standard))
      }
    }
  }
  
  dbExecute(con, paste("ALTER TABLE", toupper(data_set_name), "ADD COLUMN ID INT"))
  res = tbl(con, toupper(data_set_name)) %>% collect()
  res$ID = 1:nrow(res)
  dbWriteTable(con, toupper(data_set_name), as.data.frame(res), overwrite = TRUE)
  dbDisconnect(con)
}