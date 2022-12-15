plot_comparison_ranks_with_lines = function (methods = c("whatif", "nice", "moc"), orientation = "model", nummin = 1L, savepdf = TRUE) {
  data_set_names = c("credit_g", "diabetis", "tic_tac_toe", "bank8FM", "hill_valley", "run_or_walk_info")

  checkmate::assert_names(orientation, subset.of = c("model", "dataset"))

  # loop through dataset to compute ranks of objectives, average these over the datapoints
  aggrres = lapply(data_set_names, function(datanam) {
    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, paste0(datanam, "_EVAL")) %>% collect()
    DBI::dbDisconnect(con)
   # res = res %>% rename(no_counterfactuals = n)
  #   res$no_nondom = res$no_counterfactuals

    temp = res %>%
      select(-dist_target) %>%
      mutate(id = row_number()) %>%
      mutate(model_name = recode(model_name, logistic_regression = "logreg", neural_network = "neuralnet")) %>% 
      pivot_longer(c(dist_x_interest:dist_train), names_to = "objective") %>% 
      mutate(objective = factor(objective, levels = c("dist_x_interest", "no_changed", "dist_train"))) %>% 
      select(id, id_x_interest, model_name, algorithm, objective, value)  %>% 
      filter(algorithm %in% methods)
    
    # calculate ranks per objective
    temp_rank = temp %>%
      group_by(id_x_interest, model_name, objective)%>%
      # group_modify(~ data.frame(cbind(.x, "no" = count(.x)))) %>%
      mutate(value = rank(value)) %>% 
      mutate(value=scales::rescale(value,to=c(0, 1))) %>%
      #   maxvalue = max(value), 
      #   minvalue = min(value)) %>%
      # mutate(value = (value - minvalue/maxvalue - minvalue))
      arrange(model_name, id_x_interest, objective) 
  
    return(rbind(temp_rank))
  })
  names(aggrres) = data_set_names  
  ll = dplyr::bind_rows(aggrres, .id = "dataset")
  ll$objective = factor(ll$objective, levels = c("dist_x_interest", "no_changed", "dist_train", "rank_nondom"), 
    labels = c("rank_dist_x_interest", "rank_no_changed", "rank_dist_train", "rank_nondom"))
  
  # if (test) {
  #   create_test_df = function(data, subset = c("nice", "moc")) {
  #     testdata = data %>% filter(algorithm %in% subset)
  #     testdata$algorithm = factor(testdata$algorithm, labels = subset, levels = subset)
  #     if (orientation == "model") {
  #       stratif = unique(testdata$model_name)
  #       names(testdata)[which(names(testdata) == "model_name")] = "stratif"
  #       lookup = expand.grid(stratif, unique(testdata$objective)[-4])
  #       names(lookup) = c("model_name", "objective")
  #     } else if (orientation == "dataset") {
  #       stratif = unique(testdata$dataset)
  #       names(testdata)[which(names(testdata) == "dataset")] = "stratif"
  #       lookup = expand.grid(stratif, unique(testdata$objective)[-4])
  #       names(lookup) = c("dataset", "objective")
  #     }
  #     t = apply(lookup, MARGIN = 1L, FUN = function(row) {
  #       wilcox.test(value ~ algorithm, data = testdata %>% filter(stratif == row[[1]], objective == row[[2]]),
  #         exact = FALSE, correct = FALSE, conf.int = FALSE)$p.value
  #     })
  #     lookup$p.signif = ifelse(t > 0.1, "ns", ifelse(t > 0.05, ".", ifelse(t > 0.01, "*", ifelse(t > 0.001, "**", "***"))))
  #     lookup$p = ""
  #     lookup$group1 = subset[1]
  #     lookup$group2 = subset[2]
  #     lookup$.y. = "value"
  #     lookup = tibble::as_tibble(lookup)
  #   }
  #   lookup1 = create_test_df(ll)
  #   lookup2 = create_test_df(ll, subset = c("nice", "whatif"))
  # }
  ll$dataset = factor(ll$dataset, levels = data_set_names, labels = data_set_names)
  n_colors = length(unique(ll$algorithm))
  plt = ggplot(ll) +
    geom_boxplot(aes(x = objective, y = value, fill = algorithm), show.legend = FALSE) + 
    scale_x_discrete(labels=expression(o[prox], 
      o[sparse], o[plaus])) + 
      ylab("rank") + 
      xlab("objective")
  #scale_x_discrete(limits = rev) 
  if (orientation == "model") {
    minid = ll %>% 
      group_by(model_name, objective, algorithm) %>%
      slice(order(value)[1:nummin]) %>% 
      ungroup() %>% 
      select(model_name, algorithm, id, dataset)
    plt = plt +  facet_grid(model_name ~ algorithm, scales = "free") 
    height = 7
  } else if (orientation == "dataset") {
    minid = ll %>% 
      group_by(dataset, objective, algorithm) %>%
      slice(which.min(value)) %>% 
      ungroup() %>% 
      select(model_name, algorithm, id, dataset)
    plt = plt + facet_grid(dataset ~ algorithm, scales = "free")
    height = 7.5
  }
  mindata = left_join(x = minid, y = ll, by = c("model_name", "algorithm", "id", "dataset"))
    n_colors = length(methods)
    plt = plt + 
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = n_colors, name = "Paired")) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 10, margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5, unit = "pt")), 
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 8),
      panel.spacing = unit(2, "pt")
    ) 
    colorlines = "tan4"
    plt = plt + 
    geom_line(data = mindata, aes(x = objective, y = value, group = id), alpha = 0.5, lwd = 1, color = "tan4", lty = 1) +
    geom_line(aes(x = objective, y = value, group=id), alpha=.05, color = "grey20") 

  if (savepdf) {
    fig.path = "evaluation/figures"
    dir.create(fig.path, showWarnings = FALSE)
    ggsave(filename = file.path(fig.path, paste0(paste("overall", orientation,
      "obj_ranks_with_lines", sep = "_"), ".pdf")), plot = plt, width = 7, height = height) # 5.5, 3.8
  }
  
  return(plt)
}


plot_hypervolume = function(methods = c("whatif", "nice", "moc"), savepdf = TRUE) {
  data_set_names = c("credit_g", "diabetis", "tic_tac_toe", "bank8FM",  "hill_valley", "run_or_walk_info")
  aggrres = lapply(data_set_names, function(datanam) {
    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, paste0(datanam, "_EVAL")) %>% collect()
    DBI::dbDisconnect(con)


   #  res$hypervolume = res$dom_hv

    res_hv = res %>% select(id_x_interest, model_name, algorithm, hypervolume, 
      no_counterfactuals, no_nondom)  %>%
      filter(algorithm %in% methods) %>%
      group_by(id_x_interest, model_name, algorithm) %>%
      filter(row_number()==1) 
    
    return(res_hv)
  })
  names(aggrres) = data_set_names  
  ll = dplyr::bind_rows(aggrres, .id = "dataset")
  ll = ll %>% group_by(id_x_interest, model_name, dataset) %>%
    mutate(id = cur_group_id())
  
  ll = ll %>% pivot_longer(c(hypervolume, no_nondom, no_counterfactuals), names_to = "objective")
  ll$objective = factor(ll$objective, levels = c("hypervolume", "no_nondom", "no_counterfactuals"), 
    labels = c("hypervolume", "no. nondom", "no. overall"))
  ll$dataset = factor(ll$dataset, levels = data_set_names, labels = data_set_names)
  
  n_colors = length(methods)
  plt = ggplot(ll) +
    geom_boxplot(aes(x = algorithm , y = value, fill = algorithm), show.legend = FALSE) + 
    facet_grid(objective ~ dataset, scales = "free") +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = n_colors, name = "Paired")) +
    theme_bw() +
    ylab("") + 
    xlab("") +
    theme(
      strip.text = element_text(size = 9, margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5, unit = "pt")), 
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 9),
      panel.spacing = unit(2, "pt")
    ) 
  plt = plt +
  geom_line(aes(x = algorithm, y = value, group=id), alpha=.1)
  
  if (savepdf) {
    fig.path = "evaluation/figures"
    dir.create(fig.path, showWarnings = FALSE)
    ggsave(filename = file.path(fig.path, "hv_no_nondom.pdf"), plot = plt, width = 7, height = 5.5) # 5.5, 3.8
  }
  return(plt)
}
