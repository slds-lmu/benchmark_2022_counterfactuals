library(mlr3)
library(mlr3measures)

all_files = list.files("models/prod/resampling", recursive = TRUE)
df = as.data.frame(matrix(NA, nrow = length(all_files), ncol = 3L))
names(df) = c("dataset", "model", "classif_acc")

i = 0
for (one_file in all_files) {
  i = i + 1L
  results = readRDS(file.path("models", "prod", "resampling", one_file))
  classif_acc = results$aggregate(msr("classif.acc"))
  dataset_name = dirname(one_file)
  model_name = sub("_[^_]+$", "", basename(one_file))
  df[i, ] = c(dataset_name, model_name, classif_acc)
}

df
saveRDS(df, "models/resampling_results_agg.RDS")
