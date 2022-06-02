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


# Get table of ranks after 1, 50, 100, 150 generations

tab = plot_hv_comparison(
  c("overall", "bank8FM", "credit_g","hill_valley", "diabetes", "run_or_walk_information", "tic_tac_toe"), 
  returntable = TRUE
)


tab = tab %>% ungroup()
tab = tab[tab$generation %in% c(1, 50, 100, 150),]
tab %>% arrange(algo_spec, data_name, generation) %>% 
  melt(id.vars = c("generation", "data_name"), variable.name = "algo_spec", value.name = "mean_rank")

tab = tab %>% 
   group_by(algo_spec) %>%
  #  dplyr::mutate(i1 = row_number()) %>% 
   spread(algo_spec, mean_rank) %>%
  arrange(data_name, generation) 
   # select(-i1)

xtable::xtable(tab)
