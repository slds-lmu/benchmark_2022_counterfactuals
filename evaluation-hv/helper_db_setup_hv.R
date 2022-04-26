add_hv_results_to_db = function(data_set_name) {
  reg = loadRegistry(reg_dir, make.default = FALSE)
  job_overview = unwrap(getJobPars(reg = reg))
  jobs_of_this_data_set = job_overview[problem == data_set_name & (grepl('^moc', algorithm) | grepl('^rand', algorithm))]
  
  con = dbConnect(RSQLite::SQLite(), "evaluation-hv/db_evals_hv.db")
  
  for (job_id in jobs_of_this_data_set$job.id) {
    this_job = job_overview[job.id == job_id]
    cfactuals = try(readRDS(file.path(reg_dir, "results", paste0(this_job$job.id, ".rds"))))
    print(job_id)
    if (nrow(cfactuals) > 0L) {
      dt_standard = cbind(
        cfactuals[1, ], this_job,
        "dominated_hv" = as.numeric(attr(cfactuals, "dominated_hv")[["hv"]]), 
        "generation" = as.numeric(attr(cfactuals, "dominated_hv")[["generations"]])
      )
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
  res$ID = 1:nrow(res)
  dbWriteTable(con, toupper(data_set_name), as.data.frame(res), overwrite = TRUE)
  dbDisconnect(con)
}
