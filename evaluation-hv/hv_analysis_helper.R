shift_legend_bottom_right = function(p) {
  gp = ggplotGrob(p)
  facet.panels = grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels = sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels = facet.panels[empty.facet.panels]
  empty.facet.panels = gp[["layout"]][empty.facet.panels, ]
  names = empty.facet.panels$name
  lemon::reposition_legend(p, 'center', panel = names)
}



plot_hv_comparison = function(data_set_names) {
  con = DBI::dbConnect(RSQLite::SQLite(), "evaluation-hv/db_evals_hv.db")
  res = list()
  for (data_name in c("diabetis", "tic_tac_toe", "credit_g", "run_or_walk_info", "hill_valley", "bank8FM")) {
    if (data_name == "overall") next
    res[[data_name]] = tbl(con, data_name) %>% collect() %>% 
      select(id_x_interest, model_name, generation, dominated_hv, algo_spec) %>% 
      mutate(data_name = data_name)
  }
  DBI::dbDisconnect(con)
  res[["overall"]] = do.call(rbind, res)
  
  names(res)[names(res) == "diabetis"] = "diabetes"
  names(res)[names(res) == "run_or_walk_info"] = "run_or_walk_information"
  
  process_data = function(data, data_name) {
    data %>% 
      mutate(algo_spec = recode(algo_spec, random_search_NA_NA = "random_search")) %>% 
      group_by(id_x_interest, model_name, generation, data_name) %>% 
      mutate(rank = rank(dominated_hv, ties.method = "random")) %>% 
      ungroup() %>% 
      group_by(generation, algo_spec) %>% 
      summarise(mean_rank = mean(rank)) %>% 
      mutate(data_name = data_name)
  }  
  
  
  ls_processed = lapply(names(res), function(x) {
    process_data(res[[x]], x)
  })
  
  df_processed = do.call(rbind, ls_processed) %>% 
    filter(data_name %in% data_set_names) %>% 
    mutate(algo_spec = gsub("TRUE", "1", algo_spec)) %>% 
    mutate(algo_spec = gsub("FALSE", "0", algo_spec)) %>% 
    mutate(data_name = factor(data_name, levels = data_set_names)) %>% 
    mutate(
      data_name = recode(
        data_name,
        bank8FM = "bank8FM (n = 8,192 | p = 8)",
        credit_g = "credit_g (n = 1,000 | p = 20)",
        diabetes = "diabetes (n = 768 | p = 8)",
        hill_valley = "hill_valley (n = 1,212 | p = 100)",
        run_or_walk_information = "run_or_walk_information (n = 88,588 | p = 6)",
        tic_tac_toe = "tic_tac_toe (n = 958 | p = 9)",
      )
    )
  
  
  
  p = ggplot(df_processed) +
    geom_smooth(aes(x = generation, y = mean_rank, color = algo_spec), se = FALSE, size = 0.4, method = "loess") +
    theme_bw() +
    scale_color_manual(values = c(RColorBrewer::brewer.pal(n = 8, name = "Paired"), "black")) +
    facet_wrap(vars(data_name), ncol = 2) +
    ylab("ranks w.r.t. dominated hypervolume") +
    guides(color = guide_legend(title = element_blank(), nrow = 3)) +
    theme(
      axis.text = element_text(size = 7), 
      strip.text = element_text(size = 7, margin = margin(t = 2.5, r = 2.5, b = 2.5, l = 2.5, unit = "pt")), 
      axis.title = element_text(size = 8),
      plot.margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "pt"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      legend.direction = "horizontal", 
      legend.margin = margin()
    )
  
  shift_legend_bottom_right(p)
}


