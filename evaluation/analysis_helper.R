plot_comparison = function(data_set_name, methods = c("whatif", "nice", "moc"), savepdf = TRUE) {
  
  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  res = tbl(con, paste0(data_set_name, "_EVAL")) %>% collect()
  DBI::dbDisconnect(con)
  
  res_long = res %>%
    select(-dist_target) %>%
    mutate(model_name = recode(model_name, logistic_regression = "logreg", neural_network = "neuralnet")) %>% 
    pivot_longer(c(dist_x_interest:dist_train, n), names_to = "objective") %>% 
    mutate(objective = factor(objective, levels = c("dist_x_interest", "no_changed", "dist_train", "n"))) %>% 
    mutate(algo_spec = recode(algo_spec, nice_sparsity = "nice", nice_proximity = "nice", nice_plausibility = "nice"))
  
  res_n = res_long %>% filter(objective == "n") %>% 
    group_by(id_x_interest, model_name, algo_spec) %>%
    filter(row_number()==1)
  
  res_long = res_long %>% filter(objective != "n")
  res_long = do.call(rbind, list(res_long, res_n))
  
  if (!is.null(methods)) {
    res_long <- res_long %>% filter(algo_spec %in% methods)
  }
 
   if (data_set_name == "diabetis") data_set_name = "diabetes"
  
   n_colors = length(unique(res_long$algo_spec))
  # res_long$objlab <- factor(res_long$objective, labels = c("o[valid]", "o[proxi]", "o[sparse]", "o[plaus]", "n"))
  
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
    ggsave(filename = file.path(fig.path, paste0(paste(data_set_name, "obj_all", sep = "_"), ".pdf")), plot = plt, width = 5.5, height = 3.8) # 5.5, 3.8
  }
  return(plt)
}


speed_comparison = function(type = "n", methods = c("moc", "nice_sparsity", "nice_proximity", "nice_plausibility" , "whatif")) {
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
    summarise(time_running = mean(time_running, na.rm = TRUE))  %>% 
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

plot_speed_comparison = function(type = "n", methods = c("moc", "nice" , "whatif"), 
  savepdf = FALSE) {
  df_res = speed_comparison(type, methods)
  n_colors = length(methods)
  # df_res %>% group_by(data_name, algo_spec) %>% summarise_at(vars(-id_x_interest, -model_name),  funs(mean(., na.rm=TRUE)))
  g = ggplot(df_res) +
    geom_boxplot(aes(x = data_name, y = time_running, fill = algo_spec), show.legend = FALSE) +
    facet_wrap(vars(algo_spec), ncol = 1) +
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
      axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)
    ) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))
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
    data_set_names = names(res)
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

get_coverage = function(data_set_name, method1 = "moc", method2 = "nice", method3 = "whatif") {
  
  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  res = tbl(con, paste0(data_set_name, "_EVAL")) %>% collect()
  DBI::dbDisconnect(con)
  
  obj.nams = c("dist_train", "no_changed", "dist_x_interest")
  res = data.table(res)
  res_long = res %>%
    select(-dist_target, id_x_interest, model_name, algo_spec, dist_train, no_changed, dist_x_interest) %>%
    mutate(model_name = recode(model_name, logistic_regression = "logreg", neural_network = "neuralnet")) %>% 
    mutate(algo_spec = recode(algo_spec, nice_sparsity = "nice", nice_proximity = "nice", nice_plausibility = "nice")) %>% 
    filter(algo_spec %in% c(method1, method2, method3)) 
  
  resm1 = data.table(res_long %>% filter(algo_spec == method1))
  resm2 = data.table(res_long %>% filter(algo_spec == method2))
  resm3 = data.table(res_long %>% filter(algo_spec == method3))
  
  mod_nams = unique(res_long$model_name)
  emptyres = data.table(matrix(as.numeric(NA), nrow = 10, ncol = length(mod_nams)))
  colnames(emptyres) = mod_nams
  resl = list(emptyres, emptyres)
  names(resl) = c(method2, method3)
  for (mod in unique(res_long$model_name)) {
    for (row.id in unique(res_long$id_x_interest)) {
      cf.resm1 = resm1[id_x_interest == row.id & model_name == mod, obj.nams, with = FALSE]
      cf.resm2 = resm2[id_x_interest == row.id & model_name == mod, obj.nams, with = FALSE]
      cf.resm3 = resm3[id_x_interest == row.id & model_name == mod, obj.nams, with = FALSE]
      if (nrow(cf.resm1) > 0) {
        rcresm2 = relative_coverage(pf1 = cf.resm2, pf2 = cf.resm1)
        resl[[method2]][row.id, mod] = as.numeric(sum(rcresm2)/length(rcresm2))
        rcresm3 = dom.ind.resm3 = relative_coverage(pf1 = cf.resm3, pf2 = cf.resm1)
        resl[[method3]][row.id, mod] = sum(rcresm3)/length(rcresm3)
      }
    }
  }
  resla = lapply(resl, colMeans, na.rm = TRUE)
  resla = lapply(resla, mean, na.rm = TRUE)
  return(unlist(resla))
}

relative_coverage = function(pf1, pf2) {
  assertTRUE(all(class(pf1) == class(pf2)))
  pf1 = as.matrix(t(pf1))
  pf2 = as.matrix(t(pf2))
  n1 = ncol(pf1)
  ranking = ecr::doNondominatedSorting(cbind(pf1, pf2))$ranks
  minrankpf2 = min(ranking[(n1+1):length(ranking)])
  rank1 = ranking[1:n1]
  return(vapply(rank1, FUN.VALUE = logical(1), function(x) x > minrankpf2))
}

# print(xtable::xtable(cov.df, label = "tab:cov", 
#   caption = "MOC's coverage rate of methods to be compared per data set averaged over all models."),  floating = TRUE, 
#   floating.environment = "table",
#   caption.placement = "top", 
#   size = getOption("xtable.size", "small"), booktabs = TRUE)
# 

plot_comparison_ranks = function (methods = c("whatif", "nice", "moc"), orientation = "model", test = FALSE, savepdf = TRUE) {
  data_set_names = c("credit_g", "diabetis", "tic_tac_toe", "bank8FM",  "hill_valley", "run_or_walk_info")

  # loop through dataset to compute ranks of objectives, average these over the datapoints
  aggrres = lapply(data_set_names, function(datanam) {
    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
    res = tbl(con, paste0(datanam, "_EVAL")) %>% collect()
    DBI::dbDisconnect(con)
    
    temp = res %>%
      select(-dist_target) %>%
      mutate(model_name = recode(model_name, logistic_regression = "logreg", neural_network = "neuralnet")) %>% 
      pivot_longer(c(dist_x_interest:dist_train, n), names_to = "objective") %>% 
      mutate(objective = factor(objective, levels = c("dist_x_interest", "no_changed", "dist_train", "n"))) %>% 
      # mutate(algo_spec = recode(algo_spec, nice_sparsity = "nice", nice_proximity = "nice", nice_plausibility = "nice")) %>% 
      select(id_x_interest, model_name, algo_spec, objective, value)  %>% 
      filter(algo_spec %in% methods)
    
    # save info on number cfexp in extra dataset
    res_n = temp %>% filter(objective == "n") %>% 
      group_by(id_x_interest, model_name, algo_spec) %>%
      filter(row_number()==1)
    
    # calculate ranks per objective
    temp_rank = temp %>%
      filter(objective != "n") %>%
      group_by(id_x_interest, model_name, objective)%>%
      group_modify(~ data.frame(cbind(.x, "n" = count(.x)))) %>%
        mutate(value = rank(value)/n) %>%
        arrange(model_name, id_x_interest, objective) %>%
        select(-n)
    
    # compute ranks for fronts
    fronts = compute_fronts(res, methods)
  
    return(rbind(temp_rank, res_n, fronts))
  })
  names(aggrres) = data_set_names  
  ll = dplyr::bind_rows(aggrres, .id = "dataset")
  ll$objective = factor(ll$objective, levels = c("dist_x_interest", "no_changed", "dist_train", "rank_nondom", "n"), 
    labels = c("rank_dist_x_interest", "rank_no_changed", "rank_dist_train", "rank_nondom", "n"))
  
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
    geom_boxplot(aes(x = algo_spec, y = value, fill = algo_spec), show.legend = FALSE) +
    scale_x_discrete(limits = rev) 
  if (orientation == "model") {
  plt = plt +  facet_grid(model_name ~ objective, scales = "free") 
  height = 5.5
  } else if (orientation == "dataset") {
    plt = plt + facet_grid(dataset ~ objective, scales = "free")
    height = 6.5
  }
  plt = plt + scale_fill_manual(values = RColorBrewer::brewer.pal(n = n_colors, name = "Paired")) +
    theme_bw() +
    coord_flip() +
    ylab("") + 
    xlab("") +
    theme(
      strip.text = element_text(size = 7, margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5, unit = "pt")), 
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(size = 7),
      panel.spacing = unit(2, "pt")
    )
  
  if (test) {
    plt = plt + 
       stat_pvalue_manual(lookup1, label = "p", vjust = 2.2, y.position = 0.9, coord.flip = FALSE,   
         tip.length = 0, color = "gray") + 
    geom_text(
    data    = lookup1,
    mapping = aes(x = 2.1, y = .92, label = p.signif),
    hjust   = -0.1,
    vjust   = -1, 
    size = 2.5
)
    plt = plt + 
      stat_pvalue_manual(lookup2, label = "p", vjust = 2.2, y.position = 0.9, coord.flip = FALSE,   
        tip.length = 0, color = "gray") + 
      geom_text(
        data    = lookup2,
        mapping = aes(x = 1.1, y = .92, label = p.signif),
        hjust   = -0.1,
        vjust   = -1, 
        size = 2.5
      )
    
  }
  if (savepdf) {
    fig.path = "evaluation/figures"
    dir.create(fig.path, showWarnings = FALSE)
    ggsave(filename = file.path(fig.path, paste0(paste("overall", orientation, 
      "obj_ranks", sep = "_"), ".pdf")), plot = plt, width = 6.5, height = height) # 5.5, 3.8
  }

  return(plt)
}

compute_fronts = function(res, methods) {
  temp = res %>%
    select(-dist_target) %>%
    mutate(model_name = recode(model_name, logistic_regression = "logreg", neural_network = "neuralnet")) %>% 
    select(id_x_interest, model_name, algo_spec, dist_x_interest, no_changed, dist_train)  %>% 
    filter(algo_spec %in% methods)
  
  grid_large = expand.grid(unique(temp$id_x_interest), unique(temp$model_name))
  ls = apply(grid_large, MARGIN = 1L, function(row) {
    df = temp[temp$id_x_interest == as.numeric(row[1]) & temp$model_name == row[2], ]
    df$value = miesmuschel::rank_nondominated(-as.matrix(df[, c("dist_x_interest", "no_changed", "dist_train")]))$fronts
    df$value = df$value/max(df$value)
    df$objective = "rank_nondom"
    df = df %>%
      select(-dist_x_interest, -no_changed, -dist_train)
    return(df)
  })
  return(do.call(rbind, ls))
}


