add_evals_to_db = function(data_set_name) {

  if (TEST) {
    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals_test.db")
  } else {
    con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals.db")
  }
  res = tbl(con, toupper(data_set_name)) %>% collect()
  
  no_features = (which(names(res) == "dist_x_interest")-1)
  comp_hv_contr = function(x) {
        res = miesmuschel::domhv(fitnesses = -as.matrix(rbind(x[, c("dist_x_interest", "no_changed", "dist_train")])),
          nadir = -c(1, no_features, 1)
        )
    return(res)
  }
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

    # only keep nondominated
  nondomind = nice_all %>% group_by(id_x_interest, model_name) %>% 
    group_modify(~ broom::tidy(get_nondominated(cbind(.x$dist_x_interest, .x$no_changed, .x$dist_train)))) %>%  
    ungroup()

  nice_all = nice_all %>% 
      mutate(nondom = nondomind$x) %>%
            filter(nondom) %>%
      # remove duplicates
      distinct_at(vars(-job.id, -optimization, -ID, -time_running, -hypervolume), .keep_all = TRUE) %>% 
      group_by(id_x_interest, model_name) %>%
      group_modify(~ data.frame(cbind(.x, "dom_hv" = comp_hv_contr(.x)))) %>%
      # get number of distinct counterfactuals
      group_modify(~ data.frame(cbind(.x, "n" = count(.x)))) %>% 
      ungroup %>% 
      rename(no_nondom = n)


  other_methods = res %>% 
    filter(algorithm != "nice")
  
  all_methods = rbindlist(list(other_methods, nice_all), fill = TRUE)
  dbWriteTable(con, paste0(toupper(data_set_name), "_EVAL"), all_methods, overwrite = TRUE)
  dbDisconnect(con)
}


get_nondominated = function(fitnesses) {
  idnondom = miesmuschel::rank_nondominated(-as.matrix(fitnesses))$fronts == 1
}
