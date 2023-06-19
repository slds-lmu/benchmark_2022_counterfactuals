# Benchmarking repository 

## Structure

### 1) Data (data/)

- Loads the required datasets from OpenML: https://www.openml.org/
- For the `hill-valley` dataset, data subsets are created with randomly selected features (10 features and 30 features).
- Stores the datasets and the `x_interest` as lists in `.rds` files
- Main functions: `get_data.R`

### 2) Models (models/)

- Trains, tunes, and stores 5 models for each dataset: randomForest, xgboost, svm, logistic regression, and neural network
- Performs nested resampling (5-fold CV for the inner and outer loop) for estimating the classification accuracies of each (tuned) model on each dataset
- The neural network had to be saved differently due to keras (the autotuner could not be saved as usual; the models need to be stored as `.hdf5` files)
- Main functions: `train_models.R`, `resample.R`, `get_resample_results.R`

### 3) Counterfactuals (cfactuals/)

- Runs the counterfactuals methods for all datasets, `x_interest`, and parameter configurations and stores the counterfactuals as `counterfactuals::Counterfactuals` objects.
- Counterfactual methods: WhatIf, NICE, MOC
- Main functions: `find_counterfactuals.R`

### 4) Evaluation (evaluation/)

#### 4.1) DB setup

- Reads in the `counterfactuals::Counterfactuals` objects and stores the counterfactuals and some metainfo (such as the algorithm name and configurations)
in a sql_lite database (`db_evals.db`) for quick retrieval
- Main functions: `db_setup.R`

#### 4.2) Evaluate

- Evaluates the counterfactuals and applies the strategies mentioned in the paper
- It then stores the results in a clean data format in separate `_EVAL` tables
- Main functions: `evaluate.R`

#### 4.3) Analysis

- Creates box plots for comparing the counterfactuals of the different methods w.r.t to several evaluation measures
- Creates box plots for comparing the speed of the different methods
- All data are queried from the database `db_evals.db`
- Main functions: `analysis.R`


## Runtimes & system requirements
Training the model and generating the counterfactuals was conducted in parallel on a computer with a 2.60 GHz Intel(R) Xeon(R) processor, and 32 CPUs.
Runtimes: 
1) Data: < 1 minute
2) Models: running `train_models.R` took overall 53 hours spread over 15 CPUS, resample.R took ~ 116 hours.
3) Counterfals: running `find_counterfactuals.R` took ~ 37 hours spread over 14 CPUS
4) Evaluation: running `db_setup.R` took a few minutes, `evaluate.R` < 1 minute.

## Test code
To test the functionality within reasonable time on a regular PC, test functionality is available. 
By switching `TEST = FALSE` to `TEST = TRUE` at the beginning of `train_models.R`, `resample.R`, `get_resample_results.R`, `find_counterfactuals.R`, `db_setup.R`, `evaluate.R` and `analysis.R`, the results for the diabetis dataset and logistic regression model for the first point of interest can 
be reproduced for the three available counterfactual explanation methods. 
