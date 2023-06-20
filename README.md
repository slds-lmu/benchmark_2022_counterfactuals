# Benchmarking repository 

## Reproduce results 

###  Runtimes & system requirements
Training the model and generating the counterfactuals was conducted in parallel on a computer with a 2.60 GHz Intel(R) Xeon(R) processor, and 32 CPUs.
Runtimes: 
1) Get data: < 1 minute
2) Fit models: running `models/train_models.R` took overall 53 hours spread over 15 CPUS, `models/resample.R` took ~ 116 hours.
3) Generate counterfactuals: running `cfactuals/find_counterfactuals.R` took ~ 37 hours spread over 14 CPUS
4) Evaluation: running `evaluation/db_setup.R` took a few minutes, `evaluation/evaluate.R` < 1 minute.

### Test the code in reasonable time
A Makefile is a available to reproduce results.  
Since reproducing all experiments takes considerable time, per default only a few experiments are conducted when running any of the make commands. 
Running `make all` in your console conducts the following steps: 
1) install-packages: Install all packages required to run the benchmark
2) get-data: Scrape data from the OpenML platform
3) train-models: Train a logistic regression model on the data
4) resample: Get resampling performance results for the logistic regression models
5) find-counterfactuals: Generate counterfactuals for the diabetis data, logistric regression model and first point of interest, using all three available methods.

If any of the 5 steps fails, the corresponding `.Rout` of each of the steps can be inspected to identify the error. 

### Run all experiments (although this takes a lot of time)
To run all experiments, `TEST = FALSE` must be set in `config.R`. Afterwards, `make all` can be called again in the console.
Important: Outside testing mode (`TEST = FALSE` in `config.R`) neural networks are fit to the data, which requires the keras R package and consequently the availability of python on your local machine. 

### Reproduce figures 
To reproduce the results figures in the manuscript, the following script can be used: `evaluation/reproduce_figures.R`. 
This file unzips `evaluation/db_evals.zip` to `evaluation/db_evals.db` and calls `evaluation/evaluation.R`. 
The figures are then saved as pdfs in the folder `evaluation/figures`. 

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



