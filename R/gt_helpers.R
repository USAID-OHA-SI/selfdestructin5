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
#' @param pd of the format FYXXQX that is returned from [fetch_metadata()]
#' @export
#' @return a string
#' @family gt helpers
#' 
past_fy <- function(pd) {paste0("FY", pd$curr_pd %>% substr(3, 4) %>% as.numeric() - 1, " Results") %>% glue::as_glue()}

#' Create present fy object for gt theme
#' 
#' @keywords internal
#' @param pd of the format FYXXQX that is returned from [fetch_metadata()]
#' @return a string
#' @family gt helpers
#'  
present_fy <- function(pd) {paste(pd$curr_pd %>% substr(1, 4), "Cumulative") %>% glue::as_glue()}

#' Create present quarter object for gt theme
#' 
#' @keywords  internal 
#' @param pd of the format FYXXQX that is returned from [fetch_metadata()]
#' @return a string 
#' @family gt helpers
#'  
present_qtr <- function(pd) {paste(pd$curr_pd, "Results") %>% glue::as_glue()}

#' Create author footnote for gt theme
#' @keywords internal
#' @param pd source metadata recovered from from [fetch_metadata()]
#' @return  a string
#' @family gt helpers
#' 
authors_footnote <- function(pd){glue::glue("Created by Core Analytics Cluster on {Sys.Date()} using {pd$source}")}

#' Return msd metadata for pd object
#'
#' @description
#' This function is depracated and has been replaced by [`fetch_metadata()`]
#'
#' param df MSD or Genie extract
#' return object of the format FYXXQX
#' family gt helpers
#' @keywords internal
#' @return a string
create_pd <- function(df){
  .Deprecated("fetch_metadata()")
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
#' @return vector of column names for all numeric vars
#' @family gt helpers
#' 
extract_num_colnames <- function(df) {
  numeric_cols <- df %>% 
    dplyr::select_if(is.numeric) %>% 
    names()
  return(numeric_cols)
}


#' Object pointing to github location of legend for Q3
#' 
#' @description
#' Use legend instead as quarterly legends are no longer necessary
#' @family gt helpers
#' 
legend_q3 <- 'https://github.com/USAID-OHA-SI/selfdestructin5/blob/main/man/figures/q3_cumulative_legend.png?raw=true'


#' Object pointing to github location of legend for snapshot indicators and Q4
#' 
#' @description
#' Use legend instead as quarterly legends are no longer necessary
#' @family gt helpers
#' 
legend_snapshot <- 'https://github.com/USAID-OHA-SI/selfdestructin5/blob/main/man/figures/snapshot_legend.png?raw=true'

#' Object pointing to github location of legend for Q2
#' 
#' @description
#' Use legend instead as quarterly legends are no longer necessary
#' @family gt helpers
#' 
legend_q2 <- 'https://github.com/USAID-OHA-SI/selfdestructin5/blob/main/man/figures/Q2_cumulative_legend.png?raw=true'

#' Object pointing to github location of legend for Q1
#' 
#' @description
#' Use legend instead as quarterly legends are no longer necessary
#' @family gt helpers
legend_q1 <- 'https://github.com/USAID-OHA-SI/selfdestructin5/blob/main/man/figures/Q1_cumulative_legend.png?raw=true'

#' Object pointing to github location of legend for Q1
#' 
#' @description
#' This helper object returns the location of the new legend. Use this version
#' The object is passed to a legend_chunk f() that creates md for the legend.
#' This can then be inserted into the subtitle as an image.
#' @export
#' @family gt helpers
legend <- 'https://github.com/USAID-OHA-SI/selfdestructin5/blob/main/man/figures/legend.png?raw=true'


#' Make all text larger
#' Bold Agency names - used to increase stroke on row group label
#' @param gt_obj gt object pass through 
#' @param wt size (0-1000) of embiggening
#'
#' @return a modified gt object
#' @export
#' @family gt helpers
#'
#' @examples
#' \dontrun{
#'  mtcars %>% gt(groupname_col = "cyl") %>% bold_rowgroup(wt = 500)
#'  }
bold_rowgroup <- function(gt_obj, wt = 700){
  gt_obj %>% 
    gt::tab_style(
      style = list(
        gt::cell_text(weight = wt)
      ),
      locations = gt::cells_row_groups(groups = tidyselect::everything())
    )
}


#' Bold columns inside gt objects
#' Helper function to quickly make columns within table bold
#' 
#'
#' @param gt_obj gt object to be bolded
#' @param col column or columns to be bolded
#' @param wt weight of boldness can be lighter, normal, bold, or bolder or 0-1000
#'
#' @return a modified gt object
#' @export
#'
#' @examples
#' \dontrun{
#'  mtcars %>% 
#'  gt(groupname_col  = "cyl") %>% 
#'  bold_column(c(mpg, hp, drat, carb), wt = "bolder")
#'  }
bold_column <- function(gt_obj, col, wt = 700){
  gt_obj %>% 
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#e6e7e8", alpha = 0.5),
        gt::cell_text(weight = wt)
      ),
      locations = gt::cells_body(
        columns = {{col}},
      )
    )
}


#' Embiggen parts of mdb table
#' A noble spirit embiggens the smallest man
#' 
#'
#' @param gt_obj gt object to be embiggened
#' @param tbl_size font size for the core table
#' @param ftnote_size font size for the footnotes
#' @param source_size font size for the source notes
#'
#' @return a modified gt object
#' @export
#'
#' @examples
#' \dontrun{
#' # embiggen
#' mtcars %>% gt(groupname_col = "cyl") %>% embiggen(tbl_size = 15)
#' 
#' # de-embiggen
#' mtcars %>% gt(groupname_col = "cyl") %>% embiggen(tbl_size = 8)
#' }
embiggen <- function(gt_obj, tbl_size = 15, ftnote_size = 10, source_size = 10){
  gt_obj %>% 
    gt::tab_options(
      source_notes.font.size = source_size,
      table.font.size = tbl_size,
      footnotes.font.size = ftnote_size)
}


