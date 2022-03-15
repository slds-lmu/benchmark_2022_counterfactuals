get-data:
	R CMD BATCH data/get_data.R

train-models:
	R CMD BATCH models/train_models.R

resubmit-jobs:
	R CMD BATCH models/resubmit_jobs.R

resample:
	R CMD BATCH models/resample.R

counterfactuals:
	R CMD BATCH cfactuals/find_counterfactuals.R
