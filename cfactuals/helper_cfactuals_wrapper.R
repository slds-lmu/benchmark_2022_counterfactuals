# WhatIf
whatif_wrapper = function(data, job, instance, ...) {

  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)
  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]
  
  whatif_classif = WhatIfClassif$new(pred, n_counterfactuals = 10L, distance_function = "gower_c")
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
  
  nice_classif = NICEClassif$new(pred, optimization = arg_list$optimization,
    return_multiple = TRUE, finish_early = FALSE, distance_function = "gower_c")
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
    pred, init_strategy = "icecurve", use_conditional_mutator = FALSE,
    epsilon = 0L, quiet = TRUE, distance_function = "gower_c"
  )
  
  cfactuals = moc_classif$find_counterfactuals(
    x_interest, desired_class, desired_prob = c(0.5 + sqrt(.Machine$double.eps), 1)
  )
  
  get_cf_table(cfactuals, job)
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
  get_cf_table(cfactuals, job)
}

# Function to create results table
get_cf_table = function(cfactuals_obj, this_job) {
  dt_standard = data.table()
  if (nrow(cfactuals_obj$data) > 0L) {
    cfactuals = cfactuals_obj$evaluate()
    cfactuals_sets = cfactuals_obj$evaluate_set()
    dt_standard = cbind(cfactuals, "job.id" = this_job$id)
    attr(dt_standard, "evalsets") = cfactuals_sets
  }
  return(dt_standard)
}

