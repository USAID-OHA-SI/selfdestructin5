#' Collapse base MSD down to a desired level for main MDB table
#' 
#'   
#' @description
#' Helper function to collapse data down by a provided grouping.
#' Used in the creation of the main MDB table.
#' 
#' @param df MSD or Genie data frame
#' @param indic_list list of indicators to be filter on
#' @param ... dot-dot-dot that can be passed to the group_by step
#' 
#' @export
#' 
#' @seealso [collapse_vlc_tbl] to see equivalent collapse sequence for VLC/VLCS variables
#' 
#' @family data frame munging
#' 
# Munge function to get tables in appendable format
# Function to repeat collapsing process

collapse_base_tbl <- function(df, indic_list, ...){
  
  df %>% 
    dplyr::filter(indicator %in% indic_list,
                  standardizeddisaggregate == "Total Numerator",
                  funding_agency != "Dedup") %>% 
    dplyr::mutate(agency = ifelse(funding_agency == "USAID", "USAID", "ALL OTHER AGENCIES")) %>% 
    dplyr::group_by_at(.vars = tidyselect::all_of(...)) %>% 
    dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 
}



