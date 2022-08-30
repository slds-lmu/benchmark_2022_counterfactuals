comp_hv_contr = function(x) {
  if (nrow(x) > 1) {
    if (anyNA(x[, c("r1", "r2", "r3", "r4")])) {
     res = NA
    } else {
      res = ecr::computeHVContr(
        t(rbind(x[, c("dist_target", "dist_train", "no_changed", "dist_x_interest")])),
        ref.point = as.numeric(x[1L, c("r1", "r2", "r3", "r4")])
      )
    }
  } else {
    res = 1L
  }
  res
}

add_evals_to_db = function(data_set_name) {
  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  res = tbl(con, toupper(data_set_name)) %>% collect()
  
  whatif = res %>% 
    filter(algorithm == "whatif") %>% 
    mutate(algo_spec = "whatif") 
  # derive nondominated
   nondomindwi = whatif %>% group_by(id_x_interest, model_name) %>% 
    group_modify(~ broom::tidy(ecr::nondominated(t(cbind(.x$dist_x_interest, .x$no_changed, .x$dist_train))))) %>% 
    ungroup()
   whatif = whatif %>% 
    mutate(nondom = nondomindwi$x) %>%
    filter(nondom) %>%
    group_by(id_x_interest, model_name) %>% 
    group_modify(~ data.frame(cbind(.x, "n" = count(.x))))
  
  lookup_runtime = res %>% 
    filter(algorithm  == "nice") %>% 
    # get overall runtime 
    group_by(id_x_interest, model_name) %>% 
    summarise(time_running = sum(unique(time_running), na.rm = TRUE)) %>% 
    ungroup()

  nice_all = res %>% 
    filter(algorithm  == "nice") %>% 
    # add overall runtime from lookup
    select(-time_running) %>%
    left_join(lookup_runtime, by = c("model_name", "id_x_interest"))
    # only keep nondominated
  nondomind = nice_all %>% group_by(id_x_interest, model_name) %>% 
    group_modify(~ broom::tidy(ecr::nondominated(t(cbind(.x$dist_x_interest, .x$no_changed, .x$dist_train))))) %>% 
    ungroup()
  nice_all = nice_all %>% 
      mutate(nondom = nondomind$x) %>%
            filter(nondom) %>%
      # remove duplicates
      distinct_at(vars(-job.id, -optimization, -ID, -time_running), .keep_all = TRUE) %>% 
      # only keep 4 counterfactuals per optimizations strategy
      # group_by(id_x_interest, model_name, optimization) %>% 
      # mutate(rank = dense_rank(desc(no_changed))) %>% 
      # arrange(desc(rank)) %>% 
      # slice_head(n = 4) %>% 
      # ungroup() %>% 
      # overall only keep 10 per instance & model
      # Version 1: Group by hypervolume contribution
      group_by(job.id) %>%
      group_modify(~ data.frame(cbind(.x, "dom_hv" = comp_hv_contr(.x)))) %>% 
      ungroup() %>% 
      mutate(algo_spec = "nice") %>% 
      group_by(id_x_interest, model_name) %>%
      arrange(dist_target, desc(dom_hv)) %>% 
      
      # Version 2: select by no_changed
      #group_by(id_x_interest, model_name) %>%
      #arrange(no_changed) %>% 
      slice_head(n = 10) %>%
      # mutate(algo_spec = "nice") %>% 
      # ungroup() %>% 
      # get number of distinct counterfactuals
      # group_by(id_x_interest, model_name) %>% 
      group_modify(~ data.frame(cbind(.x, "n" = count(.x))))

  moc = res %>% 
    filter(algorithm  == "moc") %>% 
    filter(dist_target == 0) %>%
    group_by(job.id) %>% 
    group_modify(~ data.frame(cbind(.x, "dom_hv" = comp_hv_contr(.x)))) %>% 
    ungroup() %>% 
    mutate(algo_spec = paste(algorithm, sep = "_")) %>% 
    group_by(id_x_interest, model_name, algo_spec) %>%
    arrange(dist_target, desc(dom_hv)) %>% 
    slice_head(n = 10) %>%
    group_modify(~ data.frame(cbind(.x, "n" = count(.x))))
  
  # create lookup from moc for randomsearch for maximum values for hypervolume
  lookup = res %>% filter(algorithm == "moc") %>% 
    group_by(id_x_interest, model_name, problem) %>% 
    summarize(r1 = mean(r1, na.rm = TRUE), r2 = mean(r2, na.rm = TRUE), r3 = mean(r3, na.rm = TRUE), r4 = mean(r4, na.rm = TRUE))
  
  randomsearch = res %>% 
    filter(algorithm  == "random_search") %>% 
    select(-r1, -r2, -r3, -r4) %>% 
    left_join(lookup, by = c("model_name", "problem", "id_x_interest")) %>%
    group_by(job.id) %>% 
    group_modify(~ data.frame(cbind(.x, "dom_hv" = comp_hv_contr(.x)))) %>% 
    ungroup() %>% 
    mutate(algo_spec = "randomsearch") %>% 
    group_by(id_x_interest, model_name) %>% 
    group_modify(~ data.frame(cbind(.x, "n" = count(.x))))
  
  all_methods = rbindlist(list(whatif, nice_all, moc, randomsearch), fill = TRUE)
  dbWriteTable(con, paste0(toupper(data_set_name), "_EVAL"), all_methods, overwrite = TRUE)
  dbDisconnect(con)
}

