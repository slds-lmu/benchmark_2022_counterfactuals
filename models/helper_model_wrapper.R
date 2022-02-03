ranger_wrapper = function(data, job, instance, ...) {
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3tuning)
  tc = readRDS(file.path("models/tuning_config.RDS"))
  this_task = as_task_classif(data, target = names(data)[ncol(data)])
  
  if (job$prob.name %in% c("diabetis", "tic_tac_toe", "credit_g")) {
    mod = po("classbalancing", adjust = "major", reference = "major", shuffle = FALSE, ratio = 1 / 2) %>>%
      po("scale") %>>%
      po("encode") %>>%
      po(lrn("classif.ranger", predict_type = "prob"))
  } else {
    mod = po("scale") %>>%
      po("encode") %>>%
      po(lrn("classif.ranger", predict_type = "prob"))
  }

  mod$keep_results = TRUE
  tune_ps = paradox::ParamSet$new(list(
    paradox::ParamDbl$new("classif.ranger.num.trees", lower = 1, upper = log(1000))
  ))
  tune_ps$trafo = function(x, param_set) {
    x$classif.ranger.num.trees = round(exp(x$classif.ranger.num.trees))
    x
  }
  mod = GraphLearner$new(mod)
  at = AutoTuner$new(mod, tc$resampling, tc$measure, tc$terminator, tc$tuner, tune_ps)
  at$train(this_task)
  at
  
}

xgboost_wrapper = function(data, job, instance, ...) {
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3tuning)
  tc = readRDS(file.path("models/tuning_config.RDS"))
  this_task = as_task_classif(data, target = names(data)[ncol(data)])
  
  if (job$prob.name %in% c("diabetis", "tic_tac_toe", "credit_g")) {
    mod = po("classbalancing", adjust = "major", reference = "major", shuffle = FALSE, ratio = 1 / 2) %>>%
      po("scale") %>>%
      po("encode") %>>%
      po(lrn("classif.xgboost", predict_type = "prob"))
  } else {
    mod = po("scale") %>>%
      po("encode") %>>%
      po(lrn("classif.xgboost", predict_type = "prob"))
  }
  
  
  mod$keep_results = TRUE
  tune_ps = paradox::ParamSet$new(list(
    paradox::ParamDbl$new("classif.xgboost.nrounds", lower = 1, upper = log(1000))
  ))
  tune_ps$trafo = function(x, param_set) {
    x$classif.xgboost.nrounds = round(exp(x$classif.xgboost.nrounds))
    x
  }
  mod = GraphLearner$new(mod)
  at = AutoTuner$new(mod, tc$resampling, tc$measure, tc$terminator, tc$tuner, tune_ps)
  at$train(this_task)
  at
}


svm_wrapper = function(data, job, instance, ...) {
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3tuning)
  
  if (nrow(data) > 10000L) data = data[sample(.N, 10000L)] 
  tc = readRDS(file.path("models/tuning_config.RDS"))
  this_task = as_task_classif(data, target = names(data)[ncol(data)])
  
  if (job$prob.name %in% c("diabetis", "tic_tac_toe", "credit_g")) {
    mod = po("classbalancing", adjust = "major", reference = "major", shuffle = FALSE, ratio = 1 / 2) %>>%
      po("scale") %>>%
      po("encode") %>>%
      po(lrn("classif.svm", predict_type = "prob", type = "C-classification", scale = FALSE))
  } else {
    mod = po("scale") %>>%
      po("encode") %>>%
      po(lrn("classif.svm", predict_type = "prob", type = "C-classification", scale = FALSE))
  }

  mod$keep_results = TRUE
  tune_ps = paradox::ParamSet$new(list(
    paradox::ParamDbl$new("classif.svm.cost", lower = 0.01, upper = 1)))
  mod = GraphLearner$new(mod)
  at = AutoTuner$new(mod, tc$resampling, tc$measure, tc$terminator, tc$tuner, tune_ps)
  at$train(this_task)
  at
}

lr_wrapper = function(data, job, instance, ...) {
  library(mlr3)
  library(mlr3learners)
  target_name = names(data)[ncol(data)] 
  this_task = as_task_classif(data, target = target_name)
  lr_learner = lrn("classif.log_reg")
  lr_learner$train(this_task)
  lr_learner
}


nn_wrapper = function(data, job, instance, ...) {
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3tuning)
  library(mlr3keras)
  reticulate::use_condaenv("mlr3keras")
  
  if (job$prob.name %in% c("diabetis", "tic_tac_toe", "credit_g")) {
    pos = po("classbalancing", adjust = "major", reference = "major", shuffle = FALSE, ratio = 1 / 2)  %>>% 
      po("scale") %>>% po("encode")
  } else {
    pos = po("scale") %>>% po("encode")
  }
  
  target_name = names(data)[ncol(data)] 
  processed_data = pos$train(as_task_classif(data, target = target_name))[[1L]]$data()
  this_task = as_task_classif(processed_data, target = target_name)
  
  tc = readRDS(file.path("models/tuning_config.RDS"))
  
  get_keras_model = function(units, lr = 3*10^-4) {
    m = keras::keras_model_sequential()
    m = keras::layer_dense(m, units = units, activation = "relu")
    m = keras::layer_dense(m, units = 1L, activation = "sigmoid")
    keras::compile(m, optimizer = keras::optimizer_adam(lr), loss = "binary_crossentropy", metrics = "accuracy")
    m
  }
  tune_ps = paradox::ParamSet$new(list(
    paradox::ParamInt$new("units", lower = 1, upper = 20, tags = "train"),
    paradox::ParamDbl$new("lr", lower = 10^-5, upper = 10^-1, tags = "train")
  ))
  tune_ps$trafo = function(x, param_set) {
    x$model = get_keras_model(x$units, x$lr)
    x$lr = x$units = NULL
    return(x)
  }
  
  nn_learner = lrn("classif.kerasff", predict_type = "prob", epochs = 500L, model = NULL, validation_split = 1/3,
                   callbacks = list(cb_es(monitor = "val_loss", patience = 5L)))
  
  at = AutoTuner$new(
    learner = nn_learner,
    resampling = tc$resampling,
    measure = tc$measure,
    search_space = tune_ps,
    terminator = tc$terminator,
    tuner = tc$tuner
  )
  at$train(this_task)
  rr = resample(this_task, at, tc$outer_resampling, store_models = TRUE)
  
  path = "models/prod/keras/neural_network"
  if (!dir.exists(dirname(path))) dir.create(dirname(path))
  if (!dir.exists(path)) dir.create(path)
  saveRDS(rr, file.path(path, paste0(job$prob.name, "_rr.rds")))
  saveRDS(pos, file.path(path, paste0(job$prob.name, "_po.rds")))
  at$learner$save(file.path(path, paste0(job$prob.name, "_model.hdf5")))
  at
}



