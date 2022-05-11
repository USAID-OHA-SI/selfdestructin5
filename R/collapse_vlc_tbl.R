#' Collapse base MSD down to a desired level for core treatment indicators
#' 
#'   
#' @description
#' Helper function to collapse data down by a provided grouping.
#' Used in the creation of the treatment / viral load coverage MDB table.
#' 
#' 
#' @seealso [collapse_base_tbl] to see equivalent collapse sequence for core indicators.
#' 
#' @param df MSD or Genie data frame
#' @param ... dot-dot-dot to be used in the grouping option
#' 
#' @export
#' @return collapsed data frame of TX_CURR and TX_PVLS indicators
#' 
#' @family data frame munging
#' 
#' 
collapse_vlc_tbl  <- function(df, ...) {
  
  vlc_indics <- c("TX_CURR", "TX_PVLS")
  
  vlc_df <-  df %>% 
    dplyr::filter(indicator %in% vlc_indics,
                  disaggregate %in%  c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator", "Total Denominator") |
                    otherdisaggregate %in% c("ARV Dispensing Quantity - 6 or more months", "ARV Dispensing Quantity - 3 to 5 months"),
                  funding_agency != "Dedup") %>% 
    dplyr::mutate(agency = ifelse(funding_agency == "USAID", "USAID", "ALL OTHER AGENCIES") %>% as.character(),
                  indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator) %>% as.character(),
                  indicator = dplyr::case_when(
                    stringr::str_detect(otherdisaggregate, "3 to 5") ~ "TX_MMD3+",
                    stringr::str_detect(otherdisaggregate, "6 or more") ~ "TX_MMD6+",
                    TRUE ~ indicator)
    ) %>%
    dplyr::filter(indicator != "TX_CURR") %>%
    dplyr::group_by_at(.vars = tidyselect::all_of(...)) %>% 
    dplyr::summarise(dplyr::across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup() 
  
  return(vlc_df)
}