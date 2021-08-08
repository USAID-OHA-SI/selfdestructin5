#' Creates the base table for the treatment / VLS MDB tables
#' 
#' @description 
#' Use the [collapse_vlc_tbl()] to combine different data frames needed to calculate VLS and VLC.
#' Filters for mechs with known issues by default, can be controlled with resolve_issues argument
#' Wrapper function to create three core tables needed for the treatment MDB data frame.
#' 
#' First, it creates a TX_CURR table and excludes South Africa from the USAID Global table.
#' Second, [collapse_vlc_tbl()] is called to create the VLS/VLC table needed for derived indicators.
#' Third, Three month + MMD is calculated from the TX_CURR table. 
#' Finally, the tables are combined to be used in creation of the VLS/VLC and MMD+ Share calculations.
#' 
#' 
#' @param df usually and ou_im data frame
#' @param resolve_issues logical indicating whether or not known issues are removed
#' 
#' 
#' TODO: Should this be broken out into 3 separate functions?
#' 
# Create the base TX_CURR table
make_mdb_tx_df <- function(df, resolve_issues = "TRUE") {
  
  if (resolve_issues == TRUE) {
    df <- glamr::resolve_knownissues(df)
  } 
  
  indic <- "TX_CURR"
  
  # Create the base treatment table for TX_CURR
  df_ou_tx <- df %>%
    collapse_base_tbl(indic_list = indic, fiscal_year, agency, indicator, operatingunit) %>% 
    label_aggregation(type = "ou")
  
  df_reg_tx <- df %>% 
    dplyr::filter(stringr::str_detect(operatingunit, "Region")) %>% 
    collapse_base_tbl(indic_list = indic, fiscal_year, agency, indicator, operatingunit, countryname) %>% 
    label_aggregation(type = "regional") 
  
  df_usaid_tx <- df %>% 
    dplyr::filter(operatingunit != "South Africa") %>% 
    dplyr::mutate(operatingunit = ifelse(fundingagency == "USAID", "USAID", "ALL OTHER AGENCIES")) %>% 
    collapse_base_tbl(indic_list = indic, fiscal_year, agency, indicator) %>% 
    label_aggregation(type = "agency")
  
  # Create the base disagg tables
  tx_table <- dplyr::bind_rows(df_ou_tx, df_reg_tx, df_usaid_tx) 
  
  # TX PVLS TABLE
  # Get all the VLC table info needed
  vlc_ou <- df %>% 
    dplyr::filter(operatingunit %in% ou_list) %>% 
    collapse_vlc_tbl(fiscal_year, agency, indicator, operatingunit) %>% 
    label_aggregation(type = "ou") 
  
  vlc_reg <- df %>% 
    dplyr::filter(stringr::str_detect(operatingunit, "Region")) %>% 
    collapse_vlc_tbl(fiscal_year, agency, indicator, operatingunit, countryname) %>% 
    label_aggregation(type = "regional")
  
  vlc_usaid <- df %>% 
    dplyr::mutate(operatingunit = ifelse(fundingagency == "USAID", "USAID", "ALL OTHER AGENCIES")) %>% 
    collapse_vlc_tbl(fiscal_year, agency, indicator) %>% 
    label_aggregation(type = "agency")
  
  vlc_table <- dplyr::bind_rows(vlc_ou, vlc_reg, vlc_usaid)

   # Run once on aggregated info
   # This returns tx_mmd3+ for the agency/operating unit combo level
   mmd3_table <-
     vlc_table %>%
     dplyr::mutate(tx_mmd_group = ifelse(indicator %in% c("TX_MMD3+", "TX_MMD6+"), 1, 0)) %>%
     dplyr::filter(tx_mmd_group == 1) %>%
     dplyr::group_by(fiscal_year, agency, operatingunit, agg_type) %>%
     dplyr::summarise(dplyr::across(tidyselect::matches("tar|cumu|qtr"), sum, na.rm = T)) %>%
     dplyr::ungroup() %>%
     dplyr::mutate(indicator = "TX_MMD3+", .after = operatingunit)
  
   # Now combine the results from VLC_table + TX_table and VLC
   # Remove TX_MMD3+ from VLC table as it's the old version (tx_mmd3)
  
   vlc_mdb_df <-
     vlc_table %>%
     dplyr::filter(!indicator %in% c("TX_MMD3+")) %>%
     dplyr::bind_rows(mmd3_table) %>%
     dplyr::bind_rows(tx_table)

  return(vlc_mdb_df)
}