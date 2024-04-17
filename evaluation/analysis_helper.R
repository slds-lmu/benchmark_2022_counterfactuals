plot_comparison_ranks_with_lines = function (methods = c("whatif", "nice", "moc"), orientation = "model", nummin = 1L, savepdf = TRUE) {
  data_set_names = c("credit_g", "diabetis", "tic_tac_toe", "bank8FM", "hill_valley", "run_or_walk_info")
  
  checkmate::assert_names(orientation, subset.of = c("model", "dataset"))
  
  # loop through dataset to compute ranks of objectives, average these over the datapoints
  aggrres = lapply(data_set_names, function(datanam) {
    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, paste0(datanam, "_EVAL")) %>% collect()
    DBI::dbDisconnect(con)
    
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
      mutate(value = rank(value)) %>% 
      mutate(value=scales::rescale(value,to=c(0, 1))) %>%
      arrange(model_name, id_x_interest, objective) 
    
    return(rbind(temp_rank))
  })
  names(aggrres) = data_set_names  
  ll = dplyr::bind_rows(aggrres, .id = "dataset")
  ll$objective = factor(ll$objective, levels = c("dist_x_interest", "no_changed", "dist_train", "rank_nondom"), 
    labels = c("rank_dist_x_interest", "rank_no_changed", "rank_dist_train", "rank_nondom"))
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
    height = 5
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
      geom_line(aes(x = objective, y = value, group=id), alpha=.03, color = "grey20") 
    
    if (savepdf) {
      fig.path = "evaluation/figures"
      dir.create(fig.path, showWarnings = FALSE)
      ggsave(filename = file.path(fig.path, paste0(paste("overall", orientation,
        "obj_ranks_with_lines", sep = "_"), ".pdf")), plot = plt, width = 7, height = height) # 5.5, 3.8
    }
    
    return(plt)
}


plot_hypervolume = function(methods = c("whatif", "nice", "moc"), log = TRUE, savepdf = TRUE) {
  data_set_names = c("credit_g", "diabetis", "tic_tac_toe", "bank8FM", "hill_valley", "run_or_walk_info")
  aggrres = lapply(data_set_names, function(datanam) {
    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, paste0(datanam, "_EVAL")) %>% collect()
    DBI::dbDisconnect(con)
    
    
    #  res$hypervolume = res$dom_hv
    
    res_hv = res %>% select(id_x_interest, model_name, algorithm, hypervolume, 
      no_overall, no_nondom)  %>%
      filter(algorithm %in% methods) %>%
      group_by(id_x_interest, model_name, algorithm) %>%
      filter(row_number()==1) 
    
    return(res_hv)
  })
  names(aggrres) = data_set_names  
  ll = dplyr::bind_rows(aggrres, .id = "dataset")
  ll = ll %>% group_by(id_x_interest, model_name, dataset) %>%
    mutate(id = cur_group_id())
  
  ll = ll %>% pivot_longer(c(hypervolume, no_nondom, no_overall), names_to = "objective")
  
  if (log) {
    ll$value = log(ll$value)
    nams = c("log(hypervolume)", "log(no. nondom)", "log(no. overall)")
  } else {
    nams = c("hypervolume", "no. nondom", "no. overall")
  }
  
  ll$objective = factor(ll$objective, levels = c("hypervolume", "no_nondom", "no_overall"), 
    labels = nams)
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
    ggsave(filename = file.path(fig.path, "hv_no_nondom.pdf"), plot = plt, width = 7, height = 4.5) # 5.5, 3.8
  }
  return(plt)
}


plot_comparison = function(data_set_name, methods = c("whatif", "nice", "moc"), savepdf = TRUE) {
  
  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  res = tbl(con, paste0(data_set_name, "_EVAL")) %>% collect()
  DBI::dbDisconnect(con)
  
  res_long = res %>%
    select(-dist_target) %>%
    mutate(model_name = recode(model_name, logistic_regression = "logreg", neural_network = "neuralnet")) %>% 
    pivot_longer(c(dist_x_interest:dist_train, no_nondom), names_to = "objective") %>% 
    mutate(objective = factor(objective, levels = c("dist_x_interest", "no_changed", "dist_train", "no_nondom"), 
      labels = c("o[proxi]", "o[sparse]", "o[plaus]", "no.~nondom"))) 

  
  res_n = res_long %>% filter(objective == "no_nondom") %>% 
    group_by(id_x_interest, model_name, algorithm) %>%
    filter(row_number()==1)
  
  res_long = res_long %>% filter(objective != "no_nondom")
  res_long = do.call(rbind, list(res_long, res_n))
  
  if (!is.null(methods)) {
    res_long <- res_long %>% filter(algorithm %in% methods)
  }
 
   if (data_set_name == "diabetis") data_set_name = "diabetes"
  
   n_colors = length(unique(res_long$algorithm))

  plt = ggplot(res_long) +
    geom_boxplot(aes(x = algorithm, y = value, fill = algorithm), show.legend = FALSE) +
    scale_x_discrete(limits = rev) +
    facet_grid(model_name ~ objective, scales = "free", labeller = label_parsed) +
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
    ggsave(filename = file.path(fig.path, paste0(paste(data_set_name, "obj_all", sep = "_"), ".pdf")), plot = plt, width = 5.5, height = 3.8) # 5.5, 3.8
  }
  return(plt)
}


speed_comparison = function(type = "n", methods = c("moc", "nice" , "whatif")) {
  if (type == "n") {
    data_names = c("run_or_walk_info", "run_or_walk_info_sub_1", "run_or_walk_info_sub_10")
  } else {
    data_names = c("hill_valley", "hill_valley_10", "hill_valley_30")
  }
  
  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  res = list()
  for (data_name in data_names) {
    res[[data_name]] = tbl(con, paste0(data_name, "_EVAL")) %>% collect() %>% 
      filter(algorithm %in% methods) %>% 
      select(id_x_interest, model_name, time_running, algorithm, algorithm) %>% 
      mutate(data_name = data_name)
  }
  DBI::dbDisconnect(con)
  
  df_res = do.call(rbind, res) %>% 
    group_by(id_x_interest, model_name, data_name, algorithm) %>% 
    summarise(time_running = mean(time_running, na.rm = TRUE))
  
  df_res_nice = df_res %>% 
    filter(algorithm %in% c("nice")) %>% 
    group_by(id_x_interest, model_name, data_name) %>% 
    summarise(time_running = mean(time_running, na.rm = TRUE))  %>% 
    mutate(algorithm = "nice")
  
  if (type == "n") {
    df_res = df_res %>% 
      filter(!algorithm %in% c("nice")) %>% 
      rbind(df_res_nice) %>% 
      mutate(data_name = recode(data_name, run_or_walk_info = "n = 88,588", run_or_walk_info_sub_1 = "n = 886", 
        run_or_walk_info_sub_10 = "n = 8,859")) %>% 
      mutate(data_name = factor(data_name, levels = c("n = 88,588", "n = 8,859", "n = 886")))
    
  } else {
    df_res = df_res %>% 
      filter(!algorithm %in% c("nice")) %>% 
      rbind(df_res_nice) %>% 
      mutate(data_name = recode(data_name, hill_valley_10 = "p = 10", hill_valley_30 = "p = 30", hill_valley = "p = 100")) %>% 
      mutate(data_name = factor(data_name, levels = c("p = 100", "p = 30", "p = 10")))
  }
  df_res
}

plot_speed_comparison = function(df_res, type = "n", methods = c("moc", "nice" , "whatif"), log = FALSE, 
  savepdf = FALSE) {
  n_colors = length(methods)
  # df_res %>% group_by(data_name, algorithm) %>% summarise_at(vars(-id_x_interest, -model_name),  funs(mean(., na.rm=TRUE)))
  g = ggplot(df_res) +
    geom_boxplot(aes(x = algorithm, y = time_running, fill = algorithm), show.legend = FALSE) +
    facet_wrap(vars(data_name), ncol = 1) +
    ylab("runtime in seconds") +
    xlab("") +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = n_colors, name = "Paired")) +
    coord_flip() +
    theme_bw() +
    theme(
      axis.text = element_text(size = 8), 
      strip.text = element_text(size = 8, margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5, unit = "pt")), 
      axis.title = element_text(size = 9),
      plot.margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "pt"), 
      axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
    if (log) {
      g = g + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 5))
    } else {
      g = g + scale_y_continuous(labels = function(x) {
        format(x, big.mark = ",", scientific = FALSE)})
    }
  if (savepdf) {
    fig.path = "evaluation/figures"
    dir.create(fig.path, showWarnings = FALSE)
    ggsave(filename = file.path(fig.path, paste0(paste(type, "runtime", sep = "_"), ".pdf")), plot = g, width = 3.5, height = 4)
  }
  g
  
}

check_cfexp_generated = function(data_set_name, models = NULL) {
  
  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  res = list()
  
  for (data_name in c("diabetis", "tic_tac_toe", "credit_g", "run_or_walk_info", "hill_valley", "bank8FM")) {
    if (data_name == "overall") next
    res_df = tbl(con, paste0(data_name, "_EVAL")) %>% collect() %>% 
      select(id_x_interest, algorithm, model_name) %>% 
      distinct() %>% 
      group_by(algorithm, model_name) %>% 
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
    ro = ro %>% group_by(algorithm, model_name) %>% summarise(proportion = mean(proportion)) %>% 
      mutate(data_name = "overall")
    res[["overall"]] = ro
    
    data_set_names = names(res)
    plt_data = do.call(rbind,res) %>% ungroup()
    
    p = ggplot(plt_data) +
      geom_tile(aes(x = model_name, y = algorithm, fill = proportion), color = "white",
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
    ro = ro %>% group_by(algorithm, model_name) %>% summarise(proportion = mean(proportion)) %>% 
      mutate(data_name = "overall")
    res[["overall"]] = ro
    
    plt_data = do.call(rbind,res) %>% ungroup()
    data_set_names = names(res)
    plt_data = plt_data %>%
      mutate(data_name = factor(data_name, levels = data_set_names)) 
    p = ggplot(plt_data) +
      geom_tile(aes(x = data_name, y = algorithm, fill = proportion), color = "white",
        lwd = 1.5,
        linetype = 1) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
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



