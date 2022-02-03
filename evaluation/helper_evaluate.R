comp_hv_contr = function(x) {
  if (nrow(x) > 1) {
    res = ecr::computeHVContr(
      t(rbind(x[, c("dist_target", "dist_train", "nr_changed", "dist_x_interest")])),
      ref.point = as.numeric(x[1L, c("r1", "r2", "r3", "r4")])
    )
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
    mutate(algo_spec = "whatif") %>% 
    group_by(id_x_interest, model_name) %>% 
    group_modify(~ data.frame(cbind(.x, "n" = count(.x))))
  
  nice_all = res %>% 
    filter(algorithm  == "nice") %>% 
    group_by(id_x_interest, model_name, optimization) %>% 
    mutate(rank = dense_rank(desc(nr_changed))) %>% 
    arrange(desc(rank)) %>% 
    slice_head(n = 4) %>% 
    ungroup() %>% 
    group_by(id_x_interest, model_name) %>% 
    arrange(desc(rank), nr_changed) %>% 
    slice_head(n = 10) %>%
    mutate(algo_spec = paste(algorithm, optimization, sep = "_")) %>% 
    ungroup() %>% 
    group_by(id_x_interest, model_name) %>% 
    group_modify(~ data.frame(cbind(.x, "n" = count(.x))))
  
  moc_all = res %>% 
    filter(algorithm  == "moc") %>% 
    group_by(job.id) %>% 
    group_modify(~ data.frame(cbind(.x, "dom_hv" = comp_hv_contr(.x)))) %>% 
    ungroup() %>% 
    mutate(algo_spec = paste(algorithm, init_strategy, use_conditional_mutator, sep = "_")) %>% 
    group_by(id_x_interest, model_name, algo_spec) %>%
    arrange(dist_target, desc(dom_hv)) %>% 
    slice_head(n = 10) %>%
    group_modify(~ data.frame(cbind(.x, "n" = count(.x))))
  
  all_methods = rbindlist(list(whatif, nice_all, moc_all), fill = TRUE)
  dbWriteTable(con, paste0(toupper(data_set_name), "_EVAL"), all_methods, overwrite = TRUE)
  dbDisconnect(con)
}

