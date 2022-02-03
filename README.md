# Benchmarking repository for my master's thesis

## Structure

### 1) Data (data/)

- Loads the required datasets from OpenML: https://www.openml.org/
- Randomly extracts (and removes) 10 observations from each dataset for which the counterfactual data are later searched (`x_interest`)
- Stores the datasets and the `x_interest` as lists in `.rds` files
- Main function: `get_data.R`

### 2) Models (models/)

- Trains, tunes, and stores 5 models for each dataset: randomForest, xgboost, svm, logistic regression, and neural network
- Performs nested resampling (5-fold CV for the inner and outer loop) for estimating the classification accuracies of each (tuned) model on each dataset
- The neural network had to be saved differently due to keras (the autotuner could not be saved as usual; the models need to be stored as `.hdf5` files)
- `hill-valley` has a separate folder because its subsets (10 features, 30 features) were added later and it was more flexible to add a separate registry for it;
the structure is the same as in the upper dir.
- Main function: `train_models.R`, `resample.R`

### 3) Counterfactuals (cfactuals/)

- Runs the counterfactuals methods for all datasets, `x_interest`, and parameter configurations and stores the counterfactuals as `counterfactuals::Counterfactuals` objects.
- Counterfactual methods: WhatIf, NICE, MOC (incl. RandomSearch)
- `hill-valley` has a separate folder, as its subsets (10 features, 30 features) were added later and it was more flexible to add a separate registry for it;
the structure is the same as in the upper dir
- Main function: `find_counterfactuals.R`

### 4) Evaluation (evaluation/)

#### 4.1) DB setup

- Reads in the `counterfactuals::Counterfactuals` objects and stores the counterfactuals and some metainfo (such as the algorithm name and configurations)
in a sql_lite database (`db_evals.db`) for quick retrieval
- Main function: `db_setup.R`

#### 4.2) Evaluate

- Evaluates the counterfactuals and applies the strategies mentioned in the thesis to limit the number of counterfactuals for each algorithm to 10
- It then stores the results in a clean data format in separate `_EVAL` tables
- Main function: `evaluate.R`

#### 4.3) Analysis

- Creates box plots for comparing the counterfactuals of the different methods w.r.t to several evaluation measures
- Creates box plots for comparing the speed of the different methods
- All data are queried from the database `db_evals.db`
- Main function: `analysis.R`

Quality comparison            |  Speed comparison
:-------------------------:|:-------------------------:
<img src="https://user-images.githubusercontent.com/33908442/133095507-6d6eb8e5-fa44-4841-8e1f-4c491295b990.png" width="60%" height="80%">  |  <img src=https://user-images.githubusercontent.com/33908442/133096774-9746b45f-f4b7-4f70-9e91-922c6671086f.png width="100%" height="100%"> 

### 5) Evaluation hypervolume (evaluation-hv/)

#### 5.1) DB setup

- Reads in the `counterfactuals::Counterfactuals` objects for all MOC configurations and stores the hypervolumes for each generation and some metainfo (such as the algorithm name and configurations)
in a sql_lite database (`db_evals_hv.db`) for quick retrieval
- Main function: `db_setup_hv.R`

#### 5.2) Analysis

- Creates a facet grid with line graphs showing the hypervolume evolution over the generations for all MOC configurations overall and for each dataset separately
- All data are queried from the database `db_setup_hv.db`
- Main function: `analysis_hv.R`

<p align="center">
  <img width="60%" height="60%" src="https://user-images.githubusercontent.com/33908442/133098397-7b530d4a-0b95-45c5-8793-1be11fe598b6.png">
</p>

## Remark

Of course, there would be more efficient ways to save the counterfactuals in the database (normalization etc.), but considering the limited time period and the one-time
use, the current structure was more than sufficient for my analysis.

