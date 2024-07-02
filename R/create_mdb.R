#' Wrapper function to create a MDB table
#' 
#' @description 
#' Creates a MDB table for a specified OU or list of OUS.
#' Can be called in a purrr statement for batch creating
#' 
#' @param df data frame resulting from running [reshape_mdb_df()] or [reshape_mdb_tx_df()]
#' @param ou operating unit for which table is to be returned
#' @param type type of table to be created, main or treatment
#' @param pd period from which data are from
#' @param legend can pass a legend to subtitle if desired, default is NULL
#' @param legend_height adjusts the height of the preset legend
#' 
#' @export
#' @return mdb_gt a gt object formatted as the main or treatment table
#' 
#' @examples
#' \dontrun{
#'  create_mdb(mdb_tbl, "Global")
#'  create_mdb(mdb_tbl_tx, "Zambia", type = "treatment")
#'  
#'  # Batch produce tables
#'  
#'  ou_batch <- mdb_tbl_tx %>% 
#'  filter(agg_type == "OU") %>% 
#'  distinct(operatingunit) %>% pull()
#'  
#'  purrr::map(ou_batch, ~create_mdb(mdb_tbl, ou = .x))
#'  
#'  
#'  # More advanced example selecting single indicator across numerous OUS
#'  
#'  create_mdb(mdb_tbl_tx %>% filter(indicator == "VLC"), 
#'  c("Malawi", "Zambia"), type = "treatment") %>% 
#'  cols_unhide(operatingunit)}
#' 
#' 

create_mdb <- function(df, ou, type = "main", pd = meta, legend = NULL, legend_height = 20){
  
  #TODO: Write checks for the df to ensure they have created the wide version required
  
  if (!is.null(legend)){
    legend <- gt::md(glue::glue("<img src= '{legend}' style='height:{legend_height}px;'> "))
  }
  
  cntry <- stringr::str_to_upper(ou)
  
  if (!type %in% c("main", "treatment")) {
    stop("Please select the type of table you would like to create:", crayon::green(" main (default) or treatment"))
  }
  
  # Check that pd and msd_source have been created
  if (!exists('pd')) {
    stop("Please ensure that the meta objects have been created.")
  }
  
  
  if (type == "main"){
  
   mdb_gt <-  df %>% 
      dplyr::filter(operatingunit %in% c({{ou}})) %>% 
      gt::gt(groupname_col = "agency") %>% 
      mdb_main_theme(pd) %>% 
      gt::tab_header(
        title = glue::glue("{cntry} PERFORMANCE SUMMARY"),
        subtitle = legend
      )
  } else {
    
   mdb_gt <-  df %>% 
      dplyr::filter(operatingunit %in% c({{ou}})) %>% 
      gt::gt(groupname_col = "agency") %>% 
      mdb_treatment_theme(pd) %>% 
      gt::tab_header(
        title = glue::glue("{cntry} PERFORMANCE SUMMARY"),
        subtitle = legend
      )
  }
  return(mdb_gt)
}

  
  