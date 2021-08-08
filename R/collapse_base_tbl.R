#' Collapse base msd down to a desired level
#' 
#'   
#' @description
#' Helper function to collapse data down by a provided grouping
#' Used in the creation of the main MDB table.
#' Called in the creation of the treatment 
#' 
#' @param df MSD or Genie data frame
#' @param indic_list list of indicators to be filter on
#' @param ... dot-dot-dot that can be passed to the group_by step
#' 
# Munge function to get tables in appendable format
# Function to repeat collapsing process

collapse_base_tbl <- function(df, indic_list, ...){
  df %>% 
    dplyr::filter(indicator %in% indic_list,
                  standardizeddisaggregate == "Total Numerator",
                  fundingagency != "Dedup") %>% 
    dplyr::mutate(agency = ifelse(fundingagency == "USAID", "USAID", "ALL OTHER AGENCIES")) %>% 
    dplyr::group_by(...) %>% 
    dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 
}



