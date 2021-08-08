#' Create a base table of MDB indicators stacked long 
#' 
#' @description 
#' Makes a single table of core MDB indicators for all OUs, countries and USAID.
#' Uses [fetch_indicators()] to filter indicators and resolves known issues as a default.
#' Output from this function feeds into [reshape_mdb_df()]
#' 
#' @param df data frame from which MDB tables will be constructed for core indicators
#' @param resolve_issues logical that fetches troublesome mechs and omits them from df
#' 
#' @return data frame 
#' @export
#' @seealso [reshape_mdb_df()] to reshape into gt ready data frame;
#'   [fetch_indicators()] to filter indicators

#' 
#' 
#' @examples
#' \dontrun{
#'  mdb_df <- make_mdb_df(ou_im, resolve_issues = F)}
#'  
#'  

make_mdb_df <- function(df, resolve_issues = T) {
  
  if (resolve_issues == TRUE) {
    df <- glamr::resolve_knownissues(df)
  } 
  
  # Get the indicator info you need 
  indicators <- fetch_indicators(df)
  indicator_fltr <- indicators %>% 
    dplyr::distinct(indicator) %>% 
    dplyr::pull()
  
  # Create three dataframes for ou, regional-country, agency
  df_ou <- df %>% 
    dplyr::filter(operatingunit %in% ou_list) %>% 
    collapse_base_tbl(indicator_fltr, fiscal_year, agency, indicator, operatingunit) %>% 
    label_aggregation(type = "ou")
  
  df_reg <- df %>% 
    dplyr::filter(stringr::str_detect(operatingunit, "Region")) %>% 
    collapse_base_tbl(indicator_fltr, fiscal_year, agency, indicator, operatingunit, countryname) %>% 
    label_aggregation(type = "regional")
  
  df_usaid <- df %>% 
    dplyr::mutate(operatingunit = ifelse(fundingagency == "USAID", "USAID", "ALL OTHER AGENCIES")) %>% 
    collapse_base_tbl(indicator_fltr, fiscal_year, agency, indicator, operatingunit) %>% 
    label_aggregation(type = "agency") 
  
  # Bind the tables, set indicator levels
  md_table <- dplyr::bind_rows(df_ou, df_reg, df_usaid) %>% 
    dplyr::left_join(indicators %>% dplyr::select(indicator, indicator_plain)) %>% 
    dplyr::mutate(indicator = forcats::fct_relevel(indicator, indicator_fltr))
  
  return(md_table)
}
