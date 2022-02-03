plot_comparison = function(data_set_name) {
  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  res = tbl(con, paste0(data_set_name, "_EVAL")) %>% collect()
  DBI::dbDisconnect(con)

  res_long = res %>% 
    mutate(model_name = recode(model_name, logistic_regression = "logreg", neural_network = "neuralnet")) %>% 
    pivot_longer(c(dist_x_interest:dist_target, n), names_to = "objective") %>% 
    mutate(objective = factor(objective, levels = c("dist_target", "dist_x_interest", "nr_changed", "dist_train", "n"))) %>% 
    mutate(algo_spec = recode(algo_spec, nice_sparsity = "nice", nice_proximity = "nice", nice_plausibility = "nice"))
  
  if (data_set_name == "diabetis") data_set_name = "diabetes"
  ggplot(res_long) +
    geom_boxplot(aes(x = algo_spec, y = value, fill = algo_spec), show.legend = FALSE) +
    scale_x_discrete(limits = rev) +
    facet_grid(model_name ~ objective, scales = "free") +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")) +
    theme_bw() +
    coord_flip() +
    ylab("") + xlab("") +
    theme(
      strip.text = element_text(size = 7, margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5, unit = "pt")), 
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(size = 7),
      panel.spacing = unit(2, "pt")
    )
}


speed_comparison = function(type = "n", methods = c("moc_icecurve_0", "moc_icecurve_1", "moc_random_0", "moc_random_1", 
                                                     "nice_sparsity", "nice_proximity", "nice_plausibility" , "whatif")) {
  if (type == "n") {
    data_names = c("run_or_walk_info", "run_or_walk_info_sub_1", "run_or_walk_info_sub_10")
  } else {
    data_names = c("hill_valley", "hill_valley_10", "hill_valley_30")
  }
  
  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  res = list()
  for (data_name in data_names) {
    res[[data_name]] = tbl(con, paste0(data_name, "_EVAL")) %>% collect() %>% 
      filter(algo_spec %in% methods) %>% 
      select(id_x_interest, model_name, time_running, algo_spec, algorithm) %>% 
      mutate(data_name = data_name)
  }
  DBI::dbDisconnect(con)
  
  df_res = do.call(rbind, res) %>% 
    group_by(id_x_interest, model_name, data_name, algo_spec) %>% 
    summarise(time_running = mean(time_running, na.rm = TRUE))
  
  df_res_nice = df_res %>% 
    filter(algo_spec %in% c("nice_sparsity", "nice_proximity", "nice_plausibility")) %>% 
    group_by(id_x_interest, model_name, data_name) %>% 
    summarise(time_running = sum(time_running, na.rm = TRUE))  %>% 
    mutate(algo_spec = "nice")
  
  if (type == "n") {
    df_res = df_res %>% 
      filter(!algo_spec %in% c("nice_sparsity", "nice_proximity", "nice_plausibility")) %>% 
      rbind(df_res_nice) %>% 
      mutate(data_name = recode(data_name, run_or_walk_info = "n = 88,588", run_or_walk_info_sub_1 = "n = 886", 
                                run_or_walk_info_sub_10 = "n = 8,859")) %>% 
      mutate(data_name = factor(data_name, levels = c("n = 88,588", "n = 8,859", "n = 886")))
    
  } else {
    df_res = df_res %>% 
      filter(!algo_spec %in% c("nice_sparsity", "nice_proximity", "nice_plausibility")) %>% 
      rbind(df_res_nice) %>% 
      mutate(data_name = recode(data_name, hill_valley_10 = "p = 10", hill_valley_30 = "p = 30", hill_valley = "p = 100")) %>% 
      mutate(data_name = factor(data_name, levels = c("p = 100", "p = 30", "p = 10")))
  }
  df_res
}

plot_speed_comparison = function(type = "n", methods = c("moc_icecurve_0", "moc_icecurve_1", "moc_random_0", "moc_random_1", 
                                                          "nice_sparsity", "nice_proximity", "nice_plausibility" , "whatif")) {
  
  df_res = speed_comparison(type, methods)
  g = ggplot(df_res) +
    geom_boxplot(aes(x = data_name, y = time_running, fill = algo_spec), show.legend = FALSE) +
    facet_wrap(vars(algo_spec), ncol = 1) +
    ylab("runtime in seconds") +
    xlab("") +
    guides(color = guide_legend(title = "method")) +
    coord_flip() +
    theme_bw() +
    theme(
      axis.text = element_text(size = 8), 
      strip.text = element_text(size = 8, margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5, unit = "pt")), 
      axis.title = element_text(size = 9),
      plot.margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "pt")
    ) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))
  
  g
  
}



