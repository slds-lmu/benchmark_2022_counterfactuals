all: install-packages get-data train-models resample find-counterfactuals plot-results

install-packages: 
	R CMD BATCH libs.R

get-data:
	R CMD BATCH data/get_data.R

train-models:
	R CMD BATCH models/train_models.R

resample:
	R CMD BATCH models/resample.R

find-counterfactuals:
	R CMD BATCH cfactuals/find_counterfactuals.R
	
plot-results: 
	R CMD BATCH evaluation/db_setup.R
	R CMD BATCH evaluation/evaluate.R
	R CMD BATCH evaluation/analysis.R
