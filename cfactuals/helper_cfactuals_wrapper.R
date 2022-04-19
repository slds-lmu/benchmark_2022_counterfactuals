# WhatIf
whatif_wrapper = function(data, job, instance, ...) {

  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)
  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]
  
  whatif_classif = WhatIfClassif$new(pred, arg_list$n_counterfactuals, distance_function = "gower_c")
  cfactuals = whatif_classif$find_counterfactuals(
    x_interest, desired_class, desired_prob = c(0.5 + sqrt(.Machine$double.eps), 1)
  )

  get_cf_table(cfactuals, job)
}

# NICE
nice_wrapper = function(data, job, instance, ...) {

  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)
  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]
  
  nice_classif = NICEClassif$new(pred, optimization = arg_list$optimization, finish_early = FALSE, distance_function = "gower_c")
  cfactuals = nice_classif$find_counterfactuals(
    x_interest, desired_class, desired_prob = c(0.5 + sqrt(.Machine$double.eps), 1)
  )

  get_cf_table(cfactuals, job)
}

# MOC
moc_wrapper = function(data, job, instance, ...) {
  
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)
  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]
  
  moc_classif = MOCClassif$new(
    pred, init_strategy = arg_list$init_strategy, use_conditional_mutator = arg_list$use_conditional_mutator,
    epsilon = 0L, quiet = TRUE, distance_function = "gower_c"
  )
  
  cfactuals = moc_classif$find_counterfactuals(
    x_interest, desired_class, desired_prob = c(0.5 + sqrt(.Machine$double.eps), 1)
  )
  
  cfactuals_res = get_cf_table(cfactuals, job)
  attr(cfactuals_res, "dominated_hv") = moc_classif$get_dominated_hv()
  cfactuals_res


}


# Random Search
random_search_wrapper = function(data, job, instance, ...) {
  
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)
  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]
  
  rs = RandomSearchClassif$new(pred, distance_function = "gower_c")
  cfactuals = rs$find_counterfactuals(
    x_interest, desired_class = desired_class, desired_prob = c(0.5 + sqrt(.Machine$double.eps), 1)
  )
  cfactuals_res = get_cf_table(cfactuals, job)
  attr(cfactuals_res, "dominated_hv") = rs$get_dominated_hv()
  cfactuals_res
}

# Function to create results table
get_cf_table = function(cfactuals_obj, this_job) {
  dt_standard = data.table()
  if (nrow(cfactuals_obj$data) > 0L) {
    cfactuals = cfactuals_obj$evaluate()
    r1 = r2 = r3 = r4 = NA
    if (this_job$algo.name == "moc") {
      y_hat_interest = cfactuals_obj$.__enclos_env__$private$predictor$predict(cfactuals_obj$x_interest)[[cfactuals_obj$desired$desired_class]]
      r1 = min(abs(y_hat_interest - cfactuals_obj$desired$desired_prob))
      r2 = 1
      r3 = ncol(cfactuals_obj$x_interest)
      r4 = 1
    }
    dt_standard = cbind(cfactuals, "job.id" = this_job$id,
                 "r1" = r1, "r2" = r2, "r3" = r3, "r4" = r4)
  }
  return(dt_standard)
}

