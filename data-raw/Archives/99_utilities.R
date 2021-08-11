
#' Remove extra stubs on column names for potential use in mdb table names
#' @param df formatted dataframe of indicator
#'  
#' 
fmt_tbl_cols <- function(df) {
  names(df) <- names(df) %>% 
    stringr::str_replace_all("(past_|present_)", "") %>% 
    stringr::str_replace_all("(targets_|_cumulative|z_a|z_)", "") %>% 
    stringr::str_replace_all("achievement", "achv")
  return(df)
}


