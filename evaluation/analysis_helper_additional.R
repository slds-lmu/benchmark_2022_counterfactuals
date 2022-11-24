plot_comparison_ranks_with_lines = function (methods = c("whatif", "nice", "moc"), orientation = "model", test = FALSE, savepdf = TRUE) {
  data_set_names = c("credit_g", "diabetis", "tic_tac_toe", "bank8FM",  "hill_valley", "run_or_walk_info")
  
  # loop through dataset to compute ranks of objectives, average these over the datapoints
  aggrres = lapply(data_set_names, function(datanam) {
    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, paste0(datanam, "_EVAL")) %>% collect()
    DBI::dbDisconnect(con)
    
    res = res %>% rename(no_counterfactuals = n)
    res$no_nondom = res$no_counterfactuals
    
    temp = res %>%
      select(-dist_target) %>%
      mutate(id = row_number()) %>%
      mutate(model_name = recode(model_name, logistic_regression = "logreg", neural_network = "neuralnet")) %>% 
      pivot_longer(c(dist_x_interest:dist_train), names_to = "objective") %>% 
      mutate(objective = factor(objective, levels = c("dist_x_interest", "no_changed", "dist_train"))) %>% 
      select(id, id_x_interest, model_name, algo_spec, objective, value)  %>% 
      filter(algo_spec %in% methods)
    
    # calculate ranks per objective
    temp_rank = temp %>%
      filter(objective != "no") %>%
      group_by(id_x_interest, model_name, objective)%>%
      group_modify(~ data.frame(cbind(.x, "no" = count(.x)))) %>%
      mutate(value = rank(value)/n) %>%
      arrange(model_name, id_x_interest, objective) %>%
      select(-n)
  
    return(rbind(temp_rank))
  })
  names(aggrres) = data_set_names  
  ll = dplyr::bind_rows(aggrres, .id = "dataset")
  ll$objective = factor(ll$objective, levels = c("dist_x_interest", "no_changed", "dist_train", "rank_nondom"), 
    labels = c("rank_dist_x_interest", "rank_no_changed", "rank_dist_train", "rank_nondom"))
  
  if (test) {
    create_test_df = function(data, subset = c("nice", "moc")) {
      testdata = data %>% filter(algo_spec %in% subset)
      testdata$algo_spec = factor(testdata$algo_spec, labels = subset, levels = subset)
      if (orientation == "model") {
        stratif = unique(testdata$model_name)
        names(testdata)[which(names(testdata) == "model_name")] = "stratif"
        lookup = expand.grid(stratif, unique(testdata$objective)[-4])
        names(lookup) = c("model_name", "objective")
      } else if (orientation == "dataset") {
        stratif = unique(testdata$dataset)
        names(testdata)[which(names(testdata) == "dataset")] = "stratif"
        lookup = expand.grid(stratif, unique(testdata$objective)[-4])
        names(lookup) = c("dataset", "objective")
      }
      t = apply(lookup, MARGIN = 1L, FUN = function(row) {
        wilcox.test(value ~ algo_spec, data = testdata %>% filter(stratif == row[[1]], objective == row[[2]]),
          exact = FALSE, correct = FALSE, conf.int = FALSE)$p.value
      })
      lookup$p.signif = ifelse(t > 0.1, "ns", ifelse(t > 0.05, ".", ifelse(t > 0.01, "*", ifelse(t > 0.001, "**", "***"))))
      lookup$p = ""
      lookup$group1 = subset[1]
      lookup$group2 = subset[2]
      lookup$.y. = "value"
      lookup = tibble::as_tibble(lookup)
    }
    lookup1 = create_test_df(ll)
    lookup2 = create_test_df(ll, subset = c("nice", "whatif"))
  }
  ll$dataset = factor(ll$dataset, levels = data_set_names, labels = data_set_names)
  
  n_colors = length(unique(ll$algo_spec))
  
  plt = ggplot(ll) +
    geom_boxplot(aes(x = objective, y = value, fill = objective), show.legend = FALSE) 
  #scale_x_discrete(limits = rev) 
  if (orientation == "model") {
    plt = plt +  facet_grid(model_name ~ algo_spec, scales = "free") 
    height = 5.5
  } else if (orientation == "dataset") {
    plt = plt + facet_grid(dataset ~ algo_spec, scales = "free")
    height = 6.5
  }
    plt = plt + 
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")) +
    theme_bw() +
    # coord_flip() +
    ylab("") + 
    xlab("") +
    theme(
      strip.text = element_text(size = 7, margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5, unit = "pt")), 
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(size = 7),
      panel.spacing = unit(2, "pt")
    ) + 
    geom_line(aes(x = objective, y = value, group=id), alpha=.1)

  if (savepdf) {
    fig.path = "evaluation/figures"
    dir.create(fig.path, showWarnings = FALSE)
    ggsave(filename = file.path(fig.path, paste0(paste("overall", orientation, 
      "obj_ranks", sep = "_"), ".pdf")), plot = plt, width = 6.5, height = height) # 5.5, 3.8
  }
  
  return(plt)
}


plot_hypervolume = function(methods = c("whatif", "nice", "moc")) {
  data_set_names = c("credit_g", "diabetis", "tic_tac_toe", "bank8FM",  "hill_valley", "run_or_walk_info")
  aggrres = lapply(data_set_names, function(datanam) {
    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, paste0(datanam, "_EVAL")) %>% collect()
    DBI::dbDisconnect(con)
  
    # res$hypervolume = res$n
    # res$no_counterfactuals = res$n
    # res$no_nondom = res$n
    
    res_hv = res %>% select(id_x_interest, model_name, algo_spec, hypervolume, 
      no_counterfactuals, no_nondom)  %>%
      filter(algo_spec %in% methods) %>%
      group_by(id_x_interest, model_name, algo_spec) %>%
      filter(row_number()==1) 
    
    return(res_hv)
  })
  names(aggrres) = data_set_names  
  ll = dplyr::bind_rows(aggrres, .id = "dataset")
  ll = ll %>% group_by(id_x_interest, model_name, dataset) %>%
    mutate(id = cur_group_id())
  
  ll = ll %>% pivot_longer(c(hypervolume, no_nondom, no_counterfactuals), names_to = "objective")
  
  ll$objective = factor(ll$objective, labels = c("hypervolume", "no_nondom", "no_counterfactuals"))
  
  n_colors = length(methods)
  plt = ggplot(ll) +
    geom_boxplot(aes(x = algo_spec , y = value, fill = algo_spec), show.legend = FALSE) + 
    facet_grid(dataset ~ objective) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = n_colors, name = "Paired")) +
    theme_bw() +
    ylab("") + 
    xlab("") +
    theme(
      strip.text = element_text(size = 7, margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5, unit = "pt")), 
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(size = 7),
      panel.spacing = unit(2, "pt")
    ) 
  plt = plt +
  geom_line(aes(x = algo_spec, y = value, group=id), alpha=.1)
  return(plt)
}

plot_hypervolume()
