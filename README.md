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
A [Makefile](Makefile) is a available to reproduce results.  
Since reproducing all experiments takes considerable time, per default only a few experiments are conducted when running any of the make commands (this can be changed by setting `TEST = FALSE` in [config.R](config.R), further details are given in the section "[Run all experiments](#run-all-experiments)" below).

Running `make all` in your console conducts the following steps: 
1) install-packages: This runs [libs.R](libs.R), which installs all packages required to run the benchmark.
2) get-data: This runs [data/get_data.R](data/get_data.R), which scrapes data from the OpenML platform.
3) train-models: This runs [models/train_models.R](models/train_models.R), which trains models on the data (in test mode only logistic regression models are fitted).
4) resample: This runs [models/resample.R](models/resample.R), which computes resampling performance results (in test mode this is only done for the logistic regression models).
5) find-counterfactuals: This runs [cfactuals/find_counterfactuals.R](cfactuals/find_counterfactuals.R), which generates counterfactuals with the three available methods (in test mode this is only done for the diabetis data, logistic regression model and first point of interest).
6) plot-results: This runs [evaluation/db_setup.R](evaluation/db_setup.R), [evaluation/evaluate.R](evaluation/evaluate.R) and [evaluation/analysis.R](evaluation/analysis.R), which creates the data basis `evaluation/db_evals_test.db`, evaluates the counterfactuals and generates plots which are saved in the folder `evaluation/figures_test` (in test mode only the plots only show the results for the diabetis data, logistic regression model and first point of interest).

Outputs of all steps are saved in the corresponding `.Rout` files (`get_data.Rout`, `train_models.Rout`, `find_counterfactuals.Rout`, etc.). If any of the steps fail, they can be inspected to identify the error. 

### Reproduce figures 
To reproduce the results figures in the manuscript, the following script can be used: [evaluation/reproduce_figures.R](evaluation/reproduce_figures.R). 
This file unzips [evaluation/db_evals.zip](evaluation/db_evals.zip) to `evaluation/db_evals.db` and calls the plotting functions of [evaluation/analysis_helper.R](evaluation/analysis_helper.R). 
The figures are then saved as pdfs in the folder `evaluation/figures`. 

### Run all experiments 
:warning: **According to "Runtimes & system requirements", this takes a lot of time**

To run all experiments, `TEST = FALSE` must be set in [config.R](config.R). Afterwards, `make all` can be called again in the console.

Important: Outside testing mode (`TEST = FALSE` in [config.R](config.R)) neural networks are fit to the data, which requires the keras R package and consequently the availability of python on your local machine. 

## Structure 

### 1) Data (data/)

- Loads the required datasets from OpenML: https://www.openml.org/
- For the `hill-valley` dataset, data subsets are created with randomly selected features (10 features and 30 features).
- Stores the datasets and the `x_interest` as lists in `.rds` files in the directory `prod`. 
- Main functions: `get_data.R`

### 2) Models (models/)

- Trains, tunes, and stores 5 models for each dataset: randomForest, xgboost, svm, logistic regression, and neural network
- Performs nested resampling (5-fold CV for the inner and outer loop) for estimating the classification accuracies of each (tuned) model on each dataset
- The neural network had to be saved differently due to keras (the autotuner could not be saved as usual; the models need to be stored as `.hdf5` files)
- The results are saved in a `batchtools` registry in the folder `prod` and subfolder `results`
- Main functions: `train_models.R`, `resample.R`, `get_resample_results.R`

### 3) Counterfactuals (cfactuals/)

- Runs the counterfactuals methods for all datasets, `x_interest`, and parameter configurations and stores the counterfactuals as `counterfactuals::Counterfactuals` objects.
- The results are saved in a `batchtools` registry in the folder `prod` and subfolder `results`
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
- The resulting figures are saved in `evaluation/figures`
- Main functions: `analysis.R`
