# WhatIf
whatif_wrapper = function(data, job, instance, ...) {
  library(iml)
  library(counterfactuals)
  library(keras)
  library(mlr3)
  library(batchtools)
  source("cfactuals/hill-valley/get_predictor_and_x_interest_pp.R")

  arg_list = list(...)
  x_interest = readRDS(file.path("models/hill-valley/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)
  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]
  
  whatif_classif = WhatIfClassif$new(pred, arg_list$n_counterfactuals)
  cfactuals = whatif_classif$find_counterfactuals(
    x_interest, desired_class, desired_prob = c(0.5 + sqrt(.Machine$double.eps), 1)
  )

  cfactuals
}

# NICE
nice_wrapper = function(data, job, instance, ...) {
  library(iml)
  library(counterfactuals)
  library(keras)
  library(mlr3)
  library(batchtools)
  source("cfactuals/hill-valley/get_predictor_and_x_interest_pp.R")

  arg_list = list(...)
  x_interest = readRDS(file.path("models/hill-valley/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)
  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]
  
  nice_classif = NICEClassif$new(pred, optimization = arg_list$optimization, finish_early = FALSE)
  cfactuals = nice_classif$find_counterfactuals(
    x_interest, desired_class, desired_prob = c(0.5 + sqrt(.Machine$double.eps), 1)
  )

  cfactuals
  
}

# MOC
moc_wrapper = function(data, job, instance, ...) {
  library(iml)
  library(counterfactuals)
  library(keras)
  library(mlr3)
  library(batchtools)
  source("cfactuals/hill-valley/get_predictor_and_x_interest_pp.R")

  arg_list = list(...)
  x_interest = readRDS(file.path("models/hill-valley/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)
  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]
  
  moc_classif = MOCClassif$new(
    pred, init_strategy = arg_list$init_strategy, use_conditional_mutator = arg_list$use_conditional_mutator
  )
  
  cfactuals = moc_classif$find_counterfactuals(
    x_interest, desired_class, desired_prob = c(0.5 + sqrt(.Machine$double.eps), 1)
  )
  
  attr(cfactuals, "dominated_hv") <- moc_classif$get_dominated_hv()
  cfactuals

}



# Random Search
random_search_wrapper = function(data, job, instance, ...) {
  library(iml)
  library(counterfactuals)
  library(keras)
  library(mlr3)
  library(batchtools)
  library(R6)
  library(data.table)
  source("cfactuals/RandomSearch.R")
  source("cfactuals/get_predictor_and_x_interest_pp.R")
  
  arg_list = list(...)
  x_interest = readRDS(file.path("data/data_storage/x_interest_list.RDS"))[[job$prob.name]][arg_list$id_x_interest]
  pred = get_predictor_and_x_interest_pp(arg_list, job, data)
  pred_x_interest = pred$predict(x_interest)
  desired_class = names(pred_x_interest)[apply(pred_x_interest, 1L, which.min)]
  
  rs <- RandomSearch$new(pred)
  cfactuals <- rs$find_counterfactuals(
    x_interest, desired_class = desired_class, desired_prob = c(0.5 + sqrt(.Machine$double.eps), 1)
  )
  attr(cfactuals, "dominated_hv") <- rs$get_dominated_hv()
  cfactuals
  
}