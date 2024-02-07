add_evals_to_db = function(data_set_name) {
  

  con = dbConnect(RSQLite::SQLite(), "evaluation/db_evals_arf.db")

  res = tbl(con, toupper(data_set_name)) %>% collect()
 
  other_methods = res 
  
  all_methods = rbindlist(list(other_methods), fill = TRUE)
  dbWriteTable(con, paste0(toupper(data_set_name), "_EVAL"), all_methods, overwrite = TRUE)
  dbDisconnect(con)
}


get_nondominated = function(fitnesses) {
  idnondom = miesmuschel::rank_nondominated(-as.matrix(fitnesses))$fronts == 1
}