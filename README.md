# Benchmarking repository 
Paper: link will be shared soon.

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

- Evaluates the counterfactuals and applies the strategies mentioned in the paper to limit the number of counterfactuals for each algorithm to 10
- It then stores the results in a clean data format in separate `_EVAL` tables
- Main functions: `evaluate.R`

#### 4.3) Analysis

- Creates box plots for comparing the counterfactuals of the different methods w.r.t to several evaluation measures
- Creates box plots for comparing the speed of the different methods
- All data are queried from the database `db_evals.db`
- Main functions: `analysis.R`
