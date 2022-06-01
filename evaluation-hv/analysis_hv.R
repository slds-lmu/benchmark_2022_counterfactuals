library(DBI)
library(magrittr)
library(data.table)
library(ggplot2)
library(tidyverse)
source("evaluation-hv/hv_analysis_helper.R")
fig.path = "evaluation-hv/figures"

p1 = plot_hv_comparison(savepdf = FALSE,
  c("overall")
)
ggsave(filename = file.path(fig.path, "hv_overall.pdf"), plot = p1, width = 6, height = 2.75)

p2 = plot_hv_comparison(savepdf = FALSE,
  c("overall", "bank8FM", "credit_g","hill_valley", "diabetes", "run_or_walk_information", "tic_tac_toe")
)

ggsave(filename = file.path(fig.path, "hv.pdf"), plot = p2, width = 7, height = 6)

