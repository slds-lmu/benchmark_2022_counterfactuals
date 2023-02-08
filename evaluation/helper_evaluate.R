# comp_hv_contr = function(x) {
#     if (anyNA(x[, c("r2", "r3", "r4")])) {
#      res = NA
#     } else {
#       res = miesmuschel:::domhv(fitnesses = -as.matrix(rbind(x[, c("dist_x_interest", "no_changed", "dist_train")])),
#         nadir = -as.numeric(x[1L, c("r2", "r3", "r4")])
#       )
#     }
#   return(res)
# }

add_evals_to_db = function(data_set_name) {

  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  res = tbl(con, toupper(data_set_name)) %>% collect()
  
  no_features = (which(names(res) == "dist_x_interest")-1)
  comp_hv_contr = function(x) {
        res = miesmuschel::domhv(fitnesses = -as.matrix(rbind(x[, c("dist_x_interest", "no_changed", "dist_train")])),
          nadir = -c(1, no_features, 1)
        )
    return(res)
  }
  
  # # create lookup from moc for randomsearch for maximum values for hypervolume
  # lookup = res %>% filter(algorithm == "moc") %>%
  #   group_by(id_x_interest, model_name) %>%
  #   summarize(r1 = 0.5, r2 = mean(r2, na.rm = TRUE), r3 = mean(r3, na.rm = TRUE), r4 = mean(r4, na.rm = TRUE))
  # # 
  # whatif = res %>%
  #   filter(algorithm == "whatif") %>%
  #   # mutate(algo_spec = "whatif") %>%
  #   group_by(id_x_interest, model_name) %>%
  #   group_modify(~ data.frame(cbind(.x, "n" = count(.x)))) %>%
  #   ungroup() %>%
  #   rename(no_counterfactuals = n) # %>%
  #   # select(-r1, -r2, -r3, -r4) %>%
  #   # left_join(lookup, by = c("model_name", "id_x_interest")) %>%
  #   # group_by(id_x_interest, model_name) %>%
  #   # group_modify(~ data.frame(cbind(.x, "dom_hv" = comp_hv_contr(.x)))) %>%
  #   # ungroup()
  # 
  # # derive nondominated
  #  nondomindwi = whatif %>% group_by(id_x_interest, model_name) %>%
  #   group_modify(~ broom::tidy(get_nondominated(cbind(.x$dist_x_interest, .x$no_changed, .x$dist_train)))) %>%
  #   ungroup()
  #  whatif = whatif %>%
  #   mutate(nondom = nondomindwi$x) %>%
  #   filter(nondom) %>%
  #   group_by(id_x_interest, model_name) %>%
  #   group_modify(~ data.frame(cbind(.x, "n" = count(.x)))) %>%
  #   ungroup %>%
  #   rename(no_nondom = n)
  
  lookup_runtime = res %>% 
    filter(algorithm  == "nice") %>% 
    # get overall runtime 
    group_by(id_x_interest, model_name, optimization) %>%
    filter(row_number()==1) %>%
    ungroup() %>% 
    group_by(id_x_interest, model_name) %>%
    summarise(time_running = sum(time_running, na.rm = TRUE), 
              no_overall = sum(no_overall, na.rm = TRUE)) %>% 
    ungroup()

  nice_all = res %>% 
    filter(algorithm  == "nice") %>% 
    # add overall runtime from lookup
    select(-time_running, -no_overall, -no_nondom) %>%
    left_join(lookup_runtime, by = c("model_name", "id_x_interest"))
    # select(-r1, -r2, -r3, -r4) %>%
    # left_join(lookup, by = c("model_name", "id_x_interest")) %>% 
    # group_by(id_x_interest, model_name) %>% 
    # group_modify(~ data.frame(cbind(.x, "dom_hv" = comp_hv_contr(.x)))) %>% 
    # ungroup()
    # only keep nondominated
  nondomind = nice_all %>% group_by(id_x_interest, model_name) %>% 
    group_modify(~ broom::tidy(get_nondominated(cbind(.x$dist_x_interest, .x$no_changed, .x$dist_train)))) %>%  
    ungroup()
  nice_all = nice_all %>% 
      mutate(nondom = nondomind$x) %>%
            filter(nondom) %>%
      # remove duplicates
      distinct_at(vars(-job.id, -optimization, -ID, -time_running), .keep_all = TRUE) %>% 
      # # only keep 4 counterfactuals per optimizations strategy
      # # group_by(id_x_interest, model_name, optimization) %>% 
      # # mutate(rank = dense_rank(desc(no_changed))) %>% 
      # # arrange(desc(rank)) %>% 
      # # slice_head(n = 4) %>% 
      # # ungroup() %>% 
      # # overall only keep 10 per instance & model
      # # Version 1: Group by hypervolume contribution
      group_by(id_x_interest, model_name) %>%
      group_modify(~ data.frame(cbind(.x, "dom_hv" = comp_hv_contr(.x)))) %>%
      # mutate(algo_spec = "nice") %>% 
      # group_by(id_x_interest, model_name) %>%
      # arrange(dist_target, desc(dom_hv)) %>% 
      # 
      # # Version 2: select by no_changed
      # #group_by(id_x_interest, model_name) %>%
      # #arrange(no_changed) %>% 
      # slice_head(n = 10) %>%
      # mutate(algo_spec = "nice") %>% 
      # ungroup() %>% 
      # get number of distinct counterfactuals
      group_modify(~ data.frame(cbind(.x, "n" = count(.x)))) %>% 
      ungroup %>% 
      rename(no_nondom = n)

  # moc = res %>% 
  #   filter(algorithm  == "moc") %>% 
  #   filter(dist_target == 0) %>%
  #   group_by(id_x_interest, model_name) %>%
  #   group_modify(~ data.frame(cbind(.x, "n" = count(.x)))) %>%
  #   ungroup() %>% 
  #   rename(no_counterfactuals = n)  %>%
  #   # group_by(id_x_interest, model_name) %>% 
  #   # group_modify(~ data.frame(cbind(.x, "dom_hv" = comp_hv_contr(.x)))) %>% 
  #   # ungroup() %>%
  #   # mutate(algo_spec = paste(algorithm, sep = "_")) %>% 
  #   # group_by(id_x_interest, model_name) %>%
  #     group_by(id_x_interest, model_name) %>% 
  #     group_modify(~ data.frame(cbind(.x, "n" = count(.x)))) %>% 
  #     ungroup %>% 
  #     rename(no_nondom = n)

  
 
  # randomsearch = res %>% 
  #   filter(algorithm  == "random_search") %>% 
  #   select(-r1, -r2, -r3, -r4) %>% 
  #   left_join(lookup, by = c("model_name", "id_x_interest")) %>%
  #   group_by(id_x_interest, model_name) %>% 
  #   group_modify(~ data.frame(cbind(.x, "dom_hv" = comp_hv_contr(.x)))) %>% 
  #   ungroup() %>% 
  #   mutate(algo_spec = "randomsearch") %>% 
  #   group_by(id_x_interest, model_name) %>% 
  #   group_modify(~ data.frame(cbind(.x, "n" = count(.x))))
  
  other_methods = res %>% 
    filter(algorithm != "nice")
  
  all_methods = rbindlist(list(other_methods, nice_all), fill = TRUE)
  dbWriteTable(con, paste0(toupper(data_set_name), "_EVAL"), all_methods, overwrite = TRUE)
  dbDisconnect(con)
}


get_nondominated = function(fitnesses) {
  idnondom = miesmuschel::rank_nondominated(-as.matrix(fitnesses))$fronts == 1
}
