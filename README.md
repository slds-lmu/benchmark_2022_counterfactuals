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
- Counterfactual methods: WhatIf, NICE, MOC, RandomSearch
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

Quality comparison            |  Speed comparison
:-------------------------:|:-------------------------:
<img src="https://user-images.githubusercontent.com/25373845/172850210-5c4432e1-b928-4bf2-9cd6-ab6e7244dee4.png" width="100%" height="100%">  |  <img src="https://user-images.githubusercontent.com/25373845/172850933-089487c0-8915-499f-a556-6d935a4e96a3.png" width="100%" height="100%"> 

### 5) Evaluation hypervolume (evaluation-hv/)

#### 5.1) DB setup

- Reads in the `counterfactuals::Counterfactuals` objects for all MOC configurations and stores the hypervolumes for each generation and some metainfo (such as the algorithm name and configurations)
in a sql_lite database (`db_evals_hv.db`) for quick retrieval
- Main functions: `db_setup_hv.R`

#### 5.2) Analysis

- Creates a facet grid with line graphs showing the hypervolume evolution over the generations for all MOC configurations overall and for each dataset separately!

- All data are queried from the database `db_setup_hv.db`
- Main functions: `analysis_hv.R`

<p align="center">
  <img width="60%" height="60%" src="https://user-images.githubusercontent.com/25373845/172851429-78ac1940-ea01-4a3f-b951-f890535886b9.png">
</p>

