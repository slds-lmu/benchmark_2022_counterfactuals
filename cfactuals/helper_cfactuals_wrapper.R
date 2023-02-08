# WhatIf
whatif_wrapper = function(data, job, instance, ...) {
  
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)
  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]
  
  start_time = Sys.time()
  whatif_classif = WhatIfClassif$new(pred, n_counterfactuals = 10L, distance_function = "gower_c")
  cfactuals = whatif_classif$find_counterfactuals(
    x_interest, desired_class, desired_prob = c(0.5 + sqrt(.Machine$double.eps), 1)
  )
  end_time = Sys.time()
  
  # save info on runtime and calls to fhat
  res = get_cf_table(cfactuals, job)
  attr(res, "runtime") = as.numeric(end_time - start_time)
}

# NICE
nice_wrapper = function(data, job, instance, ...) {
  
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)
  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]
  
  start_time = Sys.time()
  nice_classif = NICEClassif$new(pred, optimization = arg_list$optimization,
                                 return_multiple = TRUE, finish_early = FALSE, distance_function = "gower_c")
  cfactuals = nice_classif$find_counterfactuals(
    x_interest, desired_class, desired_prob = c(0.5 + sqrt(.Machine$double.eps), 1)
  )
  end_time = Sys.time()
  
  # save info on runtime and calls to fhat
  res = get_cf_table(cfactuals, job)
  attr(res, "runtime") = as.numeric(end_time - start_time)
}

# MOC
moc_wrapper = function(data, job, instance, ...) {
  
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)
  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]
  
  start_time = Sys.time()
  moc_classif = MOCClassif$new(
    pred, termination_crit = "genstag", n_generations = 10L, 
    init_strategy = "icecurve", use_conditional_mutator = FALSE,
    epsilon = 0L, quiet = TRUE, distance_function = "gower_c"
  )
  
  cfactuals = moc_classif$find_counterfactuals(
    x_interest, desired_class, desired_prob = c(0.5 + sqrt(.Machine$double.eps), 1)
  )
  end_time = Sys.time()
  
  # save info on runtime and calls to fhat
  res = get_cf_table(cfactuals, job)
  attr(res, "runtime") = as.numeric(end_time - start_time)
}

# Function to create results table
get_cf_table = function(cfactuals_obj, this_job) {
  dt_standard = data.table()
  if (nrow(cfactuals_obj$data) > 0L) {
    cfactuals_obj$subset_to_valid()
    if (nrow(cfactuals_obj$data) > 0L) {
    cfactuals = cfactuals_obj$evaluate()
    # get other evaluation criteria
    cfactuals_no = nrow(cfactuals)
    cfactuals_no_nondom = cfactuals_obj$evaluate_set(measures = "no_nondom")[[1]]
    if (is.null(cfactuals_no_nondom)) cfactuals_no_nondom = 0
    fitnesses = -as.matrix(rbind(
        cfactuals[, c("dist_target", "dist_x_interest", "no_changed", "dist_train")]))
     cfactuals = cfactuals[miesmuschel::rank_nondominated(fitnesses)$fronts == 1,] 
     cfactuals_hv = miesmuschel:::domhv(
      fitnesses = -as.matrix(rbind(
        cfactuals[, c("dist_x_interest", "no_changed", "dist_train")])),
         nadir = -c(1, ncol(cfactuals_obj$x_interest), 1))
     dt_standard = cbind(cfactuals, "job.id" = this_job$id, 
       "no_overall" = cfactuals_no, 
       "no_nondom" = cfactuals_no_nondom, 
       "hypervolume" = cfactuals_hv)
    } 
  }
  return(dt_standard)
}

