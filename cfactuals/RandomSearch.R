RandomSearch = R6::R6Class("RandomSearch",
  inherit = CounterfactualMethodClassif,
  public = list(
    initialize = function(predictor, mu = 20L, n_generations = 175L, p_use_orig = 0.6, lower = NULL, upper = NULL) {
      super$initialize(predictor, lower, upper)
      checkmate::assert_integerish(mu, lower = 0, len = 1L)
      checkmate::assert_integerish(n_generations, lower = 0, len = 1L)
      checkmate::assert_number(p_use_orig, lower = 0, upper = 1)
      private$mu = mu
      private$n_generations = n_generations
      private$p_use_orig = p_use_orig
    },
    get_dominated_hv = function() {
      if (is.null(private$ref_point)) {
        stop("There are no results yet. Please run `$find_counterfactuals` first.")
      }
      obj_names = c("dist_target", "dist_x_interest", "nr_changed", "dist_train")
      folds = rep(seq_len(private$n_generations), each = private$mu)
      candidates_fold = cbind(folds, private$candidates)

      data.table(
        generations = seq_len(private$n_generations),
        hv = vapply(
          seq_len(max(candidates_fold$folds)),
          function(i) {
            this_fold = candidates_fold[folds %in% seq_len(i)]
            evals = counterfactuals:::Counterfactuals$new(
              this_fold[, names(private$x_interest), with = FALSE], private$predictor, private$x_interest, private$param_set,
              desired = list("desired_class" = private$desired_class, "desired_prob" = private$desired_prob)
            )$evaluate()
            nondoms = evals[!bbotk::is_dominated(t(evals[, dist_x_interest:dist_target]))]
            ecr::computeHV(t(nondoms[, obj_names, with = FALSE]), private$ref_point)
          },
          FUN.VALUE = numeric(1L)
        )
      )
    }
  ),
  private = list(
    mu = NULL,
    n_generations = NULL,
    p_use_orig = NULL,
    ref_point = NULL,
    candidates = NULL,
    run = function() {
      pred_column = private$get_pred_column()
      y_hat_interest = private$predictor$predict(private$x_interest)[[pred_column]]
      private$ref_point = c(min(abs(y_hat_interest - private$desired_prob)), 1, ncol(private$x_interest), 1)
      n = private$mu * private$n_generations
      candidates = paradox::SamplerUnif$new(private$param_set)$sample(n)$data
      candidates = counterfactuals:::reset_columns(candidates, private$p_use_orig, max_changed = 1e7, private$x_interest)
      # Transform factor column w.r.t to original data
      factor_cols = names(which(sapply(private$predictor$data$X, is.factor)))
      for (factor_col in factor_cols) {
        fact_col_pred = private$predictor$data$X[[factor_col]]
        value =  factor(candidates[[factor_col]], levels = levels(fact_col_pred), ordered = is.ordered(fact_col_pred))
        candidates[, (factor_col) := value]
      }
      
      int_cols = names(which(sapply(private$predictor$data$X, is.integer)))
      if (length(int_cols) > 0L) {
        candidates[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]
      }
      
      evals = counterfactuals:::Counterfactuals$new(
        candidates[, names(private$x_interest), with = FALSE], private$predictor, private$x_interest, private$param_set,
        desired = list("desired_class" = private$desired_class, "desired_prob" = private$desired_prob)
      )$evaluate()

      candidates_nondom = candidates[!bbotk::is_dominated(t(evals[, dist_x_interest:dist_target]))]
      private$candidates = candidates
      unique(candidates_nondom[, names(private$x_interest), with = FALSE])
    },
    print_parameters = function() {
      cat(" - mu: ", private$mu, "\n")
      cat(" - n_generations: ", private$n_generations, "\n")
      cat(" - p_use_orig: ", private$p_mut, "\n")
    }
  )
)


set.seed(12345)
library(randomForest)
library(counterfactuals)
library(data.table)
rf = randomForest(Species ~ ., data = iris)
predictor = iml::Predictor$new(rf, type = "prob")
x_interest = iris[150L, ]
predictor = iml::Predictor$new(rf, type = "prob")
rs = RandomSearch$new(predictor)
cfactuals = rs$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = c(0.5, 1))
cfactuals$data
doms = rs$get_dominated_hv()
