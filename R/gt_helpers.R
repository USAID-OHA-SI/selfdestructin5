# GT functions for table creation
#' Create change note for gt theme
#' @keywords internal  
#' @family gt helpers
change_footnote <- function() {"Number reflects percentage change from the same quarter in the previous year."}


#' Create dedup note for gt theme
#' @keywords internal
#' @family gt helpers
dedup_footnote <- function() {"ALL OTHER AGENCIES based on aggregates excluding de-duplication."}

#' Create caveat note for gt theme
#' @keywords internal
#' @family gt helpers
caveats_footnote <- function() {"Certain mechanisms have been omitted. See the Known Issues Tracker for full list of mechanisms omitted."}

#' Create delta note for gt theme
#'  
#' @keywords internal
#' @family gt helpers
delta_footnote <- function() {"Number reflects the change between current and most recent quarter."}

#' Create vlc change note for gt theme
#'  
#' @keywords internal
#' @family gt helpers
vlc_footnote <- function() {"Viral Load Covererage = TX_PVLS_D / TX_CURR_2_period_lag"}

#' Create past_fy object for gt theme
#' @keywords internal
#' @param pd of the format FYXXQX that is returned from [gophr::identifypd()]
#' @return a string
#' @family gt helpers
#' 
past_fy <- function(pd) {paste0("FY", pd %>% substr(3, 4) %>% as.numeric() - 1, " Results") %>% glue::as_glue()}

#' Create present fy object for gt theme
#' 
#' @keywords internal
#' @param pd of the format FYXXQX that is returned from [gophr::identifypd()]
#' @return a string
#' @family gt helpers
#'  
present_fy <- function(pd) {paste(pd %>% substr(1, 4), "Cumulative") %>% glue::as_glue()}

#' Create present quarter object for gt theme
#' 
#' @keywords  internal 
#' @param pd of the format FYXXQX that is returned from [gophr::identifypd()]
#' @return a string 
#' @family gt helpers
#'  
present_qtr <- function(pd) { paste(pd, "Results") %>% glue::as_glue()}

#' Create author footnote for gt theme
#' @keywords internal
#' @param msd_source source metadata recovered from [selfdestructin5::msd_period()]
#' @return  a string
#' @family gt helpers
#' 
authors_footnote <- function(msd_source){glue::glue("Created by Core Analytics Cluster on {Sys.Date()} using {msd_source}")}

#' Return msd metadata for pd object
#' 
#' @description 
#' The derived pd object is used throughout the generation of the tables. 
#' It is used multiple times for formatting columns and column spanners.
#' 
#' @param df MSD or Genie extract
#' @export
#' @return object of the format FYXXQX
#' @family gt helpers
#' 
create_pd <- function(df){ 
  pd <- gophr::identifypd(df)
  return(pd)
}

#' Extract a vector of numeric column names
#' 
#' @description
#' This helper function is used to extract the names of all the numeric columns in the TX mdb table.
#' The result is passed to the treatment theme for use in formatting columns. 
#' 
#' @param df data frame from the [reshape_mdb_tx_df()] call
#' @export
#' @return vector of column names for all numeric vars
#' @family gt helpers
#' 
extract_num_colnames <- function(df) {
  numeric_cols <- df %>% 
    dplyr::select_if(is.numeric) %>% 
    names()
  return(numeric_cols)
}


