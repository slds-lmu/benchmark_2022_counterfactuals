plot_comparison = function(data_set_name, methods = c("whatif", "nice", "moc_icecurve_0", "moc_icecurve_1"), savepdf = TRUE) {

  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  res = tbl(con, paste0(data_set_name, "_EVAL")) %>% collect()
  DBI::dbDisconnect(con)
  
  res_long = res %>% 
    mutate(model_name = recode(model_name, logistic_regression = "logreg", neural_network = "neuralnet")) %>% 
    pivot_longer(c(dist_x_interest:dist_target, n), names_to = "objective") %>% 
    mutate(objective = factor(objective, levels = c("dist_target", "dist_x_interest", "no_changed", "dist_train", "n"))) %>% 
    mutate(algo_spec = recode(algo_spec, nice_sparsity = "nice", nice_proximity = "nice", nice_plausibility = "nice"))
  
  if (!is.null(methods)) {
    res_long <- res_long %>% filter(algo_spec %in% methods)
  }
  n_colors = length(unique(res_long$algo_spec))
  
  
  if (data_set_name == "diabetis") data_set_name = "diabetes"
  plt = ggplot(res_long) +
    geom_boxplot(aes(x = algo_spec, y = value, fill = algo_spec), show.legend = FALSE) +
    scale_x_discrete(limits = rev) +
    facet_grid(model_name ~ objective, scales = "free") +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = n_colors, name = "Paired")) +
    theme_bw() +
    coord_flip() +
    ylab("") + xlab("") +
    theme(
      strip.text = element_text(size = 7, margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5, unit = "pt")), 
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(size = 7),
      panel.spacing = unit(2, "pt")
    )
  if (savepdf) {
    fig.path = "evaluation/figures"
    dir.create(fig.path, showWarnings = FALSE)
    ggsave(filename = file.path(fig.path, paste0(paste(data_set_name, "obj_all", sep = "_"), ".pdf")), plot = plt, width = 9, height = 6.5) # 5.5, 3.8
  }
  return(plt)
}


speed_comparison = function(type = "n", methods = c("moc_icecurve_0", "moc_icecurve_1", "nice_sparsity", "nice_proximity", "nice_plausibility" , "whatif")) {
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

plot_speed_comparison = function(type = "n", methods = c("moc_icecurve_0", "moc_icecurve_1", "nice_sparsity", "nice_proximity", "nice_plausibility" , "whatif"), 
                                 savepdf = FALSE) {
  
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
  if (savepdf) {
    fig.path = "evaluation/figures"
    dir.create(fig.path, showWarnings = FALSE)
    ggsave(filename = file.path(fig.path, paste0(paste(type, "runtime", sep = "_"), ".pdf")), plot = g, width = 4, height = 4)
  }
  g
  
}

check_cfexp_generated = function(data_set_name, models = NULL) {
  
  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  res = list()
  
  for (data_name in c("diabetis", "tic_tac_toe", "credit_g", "run_or_walk_info", "hill_valley", "bank8FM")) {
      if (data_name == "overall") next
      res_df = tbl(con, paste0(data_name, "_EVAL")) %>% collect() %>% 
        select(id_x_interest, algo_spec, model_name) %>% 
        mutate(algo_spec = recode(algo_spec, nice_sparsity = "nice", nice_proximity = "nice", nice_plausibility = "nice")) %>%
        distinct() %>% 
        group_by(algo_spec, model_name) %>% 
        summarise(proportion = n_distinct(id_x_interest)/10) %>% 
        mutate(data_name = data_name)
      if (!is.null(models)) {
        res[[data_name]] = res_df %>% filter(model_name %in% models)
      } else {
        res[[data_name]] = res_df
      }
  }
  
  DBI::dbDisconnect(con)
  
  names(res)[names(res) == "diabetis"] = "diabetes"
  res$diabetes$data_name = "diabetes"
  names(res)[names(res) == "run_or_walk_info"] = "run_or_walk_information"
  res$run_or_walk_information$data_name = "run_or_walk_information"
  
  if (is.null(models)) {
    ro = do.call(rbind, res)
    ro = ro %>% group_by(algo_spec, model_name) %>% summarise(proportion = mean(proportion)) %>% 
      mutate(data_name = "overall")
    res[["overall"]] = ro
    
    data_set_names = names(res)
    plt_data = do.call(rbind,res) %>% ungroup()
    
    # plt_data = plt_data %>%
    #   filter(data_name %in% data_set_names) %>%
    #   mutate(data_name = factor(data_name, levels = data_set_names)) %>% 
    #   mutate(
    #     data_name = recode(
    #       data_name,
    #       bank8FM = "bank8FM (n = 8,192 | p = 8)",
    #       credit_g = "credit_g (n = 1,000 | p = 20)",
    #       diabetes = "diabetes (n = 768 | p = 8)",
    #       hill_valley = "hill_valley (n = 1,212 | p = 100)",
    #       run_or_walk_information = "run_or_walk_information (n = 88,588 | p = 6)",
    #       tic_tac_toe = "tic_tac_toe (n = 958 | p = 9)",
    #     )
    #   )
    p = ggplot(plt_data) +
      geom_tile(aes(x = model_name, y = algo_spec, fill = proportion), color = "white",
                lwd = 1.5,
                linetype = 1) +
      theme_bw() +
      scale_fill_gradient(low = "white", high = "blue") +
      facet_wrap(vars(data_name), ncol = 3) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())
    
    p = shift_legend_bottom_right(p)
    
} else {
  
  ro = do.call(rbind, res)
  ro = ro %>% group_by(algo_spec, model_name) %>% summarise(proportion = mean(proportion)) %>% 
    mutate(data_name = "overall")
  res[["overall"]] = ro
  
  plt_data = do.call(rbind,res) %>% ungroup()
  data_set_names = unique(plt_data$data_name)
  plt_data = plt_data %>%
  #   filter(data_name %in% data_set_names) %>%
  mutate(data_name = factor(data_name, levels = data_set_names)) 
  #   mutate(
  #     data_name = recode(
  #       data_name,
  #       bank8FM = "bank8FM (n = 8,192 | p = 8)",
  #       credit_g = "credit_g (n = 1,000 | p = 20)",
  #       diabetes = "diabetes (n = 768 | p = 8)",
  #       hill_valley = "hill_valley (n = 1,212 | p = 100)",
  #       run_or_walk_information = "run_or_walk_information (n = 88,588 | p = 6)",
  #       tic_tac_toe = "tic_tac_toe (n = 958 | p = 9)",
  #     )
  #   )
  p = ggplot(plt_data) +
    geom_tile(aes(x = data_name, y = algo_spec, fill = proportion), color = "white",
              lwd = 1.5,
              linetype = 1) +
    theme_bw() +
    scale_fill_gradient(low = "white", high = "blue") +
    # facet_wrap(vars(data_name), ncol = 3) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.title.y = element_blank(), axis.title.x = element_blank())
  } 
return(p)
}

shift_legend_bottom_right = function(p) {
  gp = ggplotGrob(p)
  facet.panels = grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels = sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels = facet.panels[empty.facet.panels]
  empty.facet.panels = gp[["layout"]][empty.facet.panels, ]
  names = empty.facet.panels$name
  lemon::reposition_legend(p, 'center', panel = names)
}
