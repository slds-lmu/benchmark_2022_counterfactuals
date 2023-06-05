all: get-data train-models resample find-counterfactuals

get-data:
	R CMD BATCH data/get_data.R

train-models:
	R CMD BATCH models/train_models.R

resample:
	R CMD BATCH models/resample.R

find-counterfactuals:
	R CMD BATCH cfactuals/find_counterfactuals.R
