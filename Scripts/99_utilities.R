# Functions for Mission Director Tables Q3


#' Calculate growth between two metrics
#' 
#' Helper function for calculating growth
#' @param x numerator to calculate growth - most recent metric
#' @param y denominator to calculate growth - lagged metric
#' 
calc_growth <- function(x, y) {
  ifelse(x > 0.000, (x / y) - 1, NA_real_)
}



#' Fetches indicators used to make tables, accounting for quarter of MSD and semi-annual indicators
#' 
#' @param df data frame on which indicators are to be fetched
#' @return dataframe of indicators 
#'  
# Fetch indicators
fetch_indicators <- function(df) {
  
  # Determine what quarter you are fetching data from
  pd <- df %>% 
    ICPIutilities::identifypd(.) %>% 
    stringr::str_extract(., ".{2}$")
  cat("Fetching ", crayon::yellow(pd), " indicators\n")
  
  # Pull in indicator list from google drive
  indic_list <- googlesheets4::read_sheet("1Xv9QQp6AkDdKxSYGRQ8UYJidBE8LzMnEzjIlYEur83A")
  
  # Extract the correct vector of indicators for the tables
  if (pd %in% c("Q1", "Q3")) {
    indics <- indic_list %>% 
      dplyr::filter(frequency != "semi-annual") 
  } else {
    indics <- indic_list 
  }
  cat(crayon::yellow(pd), " indicators fetched ", crayon::yellow(indics$indicator), "\n")
  
  return(indics)
}



#' Helper function to collapse data down by a provided grouping
#' Used in the creation of the main MDB table
#' 
#' @param df MSD or Genie data frame
#' @param indic_list list of indicators to be filter on
#' @param ...
#' 
# Munge function to get tables in appendable format
# Function to repeat collapsing process


collapse_base_tbl <- function(df, indic_list, ...){
  df %>% 
    dplyr::filter(indicator %in% indic_list,
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>% 
    dplyr::mutate(agency = ifelse(fundingagency == "USAID", "USAID", "ALL OTHER AGENCIES")) %>% 
    dplyr::group_by(fiscal_year, agency, indicator, ...) %>% 
    dplyr::summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 
}




#' Makes a single table of core MDB indicators for all OUs, countries and USAID
#' 
#'  
#'   
#' @param df data frame from which MDB tables will be constructed for core indicators
#' @param resolve_issues logical that fetches troublesome mechs and omits them from df
#' 
#' @return data frame of munged indicators that are used for creating the MDB briefers
#' 

make_mdb_df <- function(df, resolve_issues = T) {
  
  if (resolve_issues == TRUE) {
    df <- glamr::resolve_knownissues(df)
    print(names(df))
  } 
  
  # Get the indicator info you need 
  indicators <- fetch_indicators(df)
  indicator_fltr <- indicators %>% distinct(indicator) %>% pull()
  
  # Determine what filter you need to apply: ou, country or agency
  ou_list <- glamr::get_outable(datim_user(), datim_pwd()) %>% 
    dplyr::distinct(operatingunit) %>% 
    pull()
  
  # Create three dataframes for ou, regional-country, agency
  df_ou <- df %>% 
    dplyr::filter(operatingunit %in% ou_list) %>% 
    collapse_base_tbl(., indicator_fltr, operatingunit) %>% 
    dplyr::mutate(agg_type = "OU")
  
  df_reg <- df %>% 
    dplyr::filter(str_detect(operatingunit, "Region")) %>% 
    collapse_base_tbl(., indicator_fltr, operatingunit, countryname) %>% 
    dplyr::mutate(agg_type = "Region-Country",
           operatingunit = paste(operatingunit, countryname, sep = "-")) %>% 
    dplyr::select(-countryname)
  
  df_usaid <- df %>% 
    dplyr::mutate(operatingunit = ifelse(fundingagency == "USAID", "USAID", "ALL OTHER AGENCIES")) %>% 
    collapse_base_tbl(., indicator_fltr, operatingunit) %>% 
    dplyr::mutate(agg_type = "Agency", 
                  operatingunit = "Global")  
  
  # Bind the tables, set indictor levels
  md_table <- dplyr::bind_rows(df_ou, df_reg, df_usaid) %>% 
    dplyr::left_join(., indicators %>% select(indicator, indicator_plain)) %>% 
    dplyr::mutate(indicator = fct_relevel(indicator, indicator_fltr))
  
  return(md_table)
}


#' Reshape the base table to a gt() ready table
#' Keeps most recent quarter and cumulative results from previous year
#' Inherits the table from make_md_df
#' 
#' @param df takes the make_md_df results as an input 
#' @return returns a wide formatted table of all OUs, countries and USAID
#' 
#
reshape_mdb_df <- function(df) {
  
  # Need to know the fiscal year and quarters to filter
  pd <- identifypd(df)
  fy_end <-  pd %>% str_sub(., 3, 4) %>% as.numeric() + 2000
  fy_beg <- fy_end - 1 
  max_pd <- pd
  min_pd <- paste0("FY", str_sub(fy_beg, 3, 4), "Q4")
  
  # Filter out years and quarters not needed
  md_tbl <- 
    df %>% 
    reshape_msd("quarters", qtrs_keep_cumulative = TRUE) %>% 
    arrange(agency, operatingunit, indicator, period) %>%
    group_by(agency, operatingunit, indicator) %>% 
    mutate(
      z_change = calc_growth(results_cumulative, lag(results_cumulative, n = 4)),
      z_direction = case_when(
        z_change >= 0  ~ "increase", 
        z_change <= 0  ~ "decrease",
        is.na(z_change) ~ "not applicable",
        TRUE ~ "not applicable")
    ) %>% 
    ungroup() %>% 
    mutate(qtr = str_sub(period, 5, 6)) %>% 
    filter(period %in% c(max_pd, min_pd)) %>% 
    calc_achievement() %>% 
    adorn_achievement()
  
  
  # Now, rename so results are output in order after pivoting
  # Use z_ and tarc_ stubs to ensure ordering of columns is consistent 
  md_tbl <- 
    md_tbl %>% 
    rename(tint_achv = achv_color,
           targets_achievement = achievement, 
           z_aresults = results) %>% 
    select(-c(fiscal_year, cumulative, achievement_qtrly, achv_label)) %>%
    mutate(across(c(z_change, z_direction, tint_achv, z_aresults), ~ case_when(period == pd ~ .x))) %>%
    mutate(order_col = ifelse(str_detect(period, max_pd), "present", "past"),
           period = str_sub(period, 1, 4),
    ) %>%
    pivot_wider(names_from = c(period, order_col),
                names_glue = "{order_col}_{.value}",
                values_from = c(targets, z_aresults, results_cumulative, targets_achievement, tint_achv, z_change, z_direction),
                names_sort = TRUE) %>%
    mutate(indicator2 = ifelse(agency == "USAID", paste(indicator, indicator_plain), paste(indicator)),
           indicator2 = ifelse(agency == "USAID", format_indicator(indicator2), indicator2), 
           agency = fct_relevel(agency, "USAID"), 
           present_z_direction = map(present_z_direction, rank_chg),
           present_tint_achv = map(present_tint_achv, achv_circle)
    ) %>%
    arrange(agency) %>% 
    select(-indicator_plain) %>%
    select(agency, operatingunit, indicator2, indicator, sort(tidyselect::peek_vars())) %>% 
    select(where(~!all(is.na(.x))))  #Drop all columns with NAs
  
  return(md_tbl)
}




#' Remove extra stubs on column names for potential use in mdb table names
#' @param wide formatted dataframe of indicator
#'  
#' 
fmt_tbl_cols <- function(df) {
  names(df) <- names(df) %>% 
    str_replace_all(., "(past_|present_)", "") %>% 
    str_replace_all(., "(targets_|_cumulative|z_a|z_)", "") %>% 
    str_replace_all(., "achievement", "achv")
  return(df)
}


#' Function to assign arrows and colors to year over year change
#' Returns a html formatting circle that can be embedded in gt object
#' @param change_dir variable indicating direction of change or not applicable
#' @export
#' 
#' 
rank_chg <- function(change_dir){
  
  alpha = 0.75
  # Extra if statement to catch NA cells
  if(is.na(change_dir)){
    logo_out <- fontawesome::fa("circle", fill = trolley_grey_light, fill_opacity = alpha)
  } else {
    if (change_dir == "increase") {
      logo_out <- fontawesome::fa("arrow-circle-up", fill = genoa_light, fill_opacity = alpha)
    } else if (change_dir == "decrease"){
      logo_out <- fontawesome::fa("arrow-circle-down", fill = old_rose_light, fill_opacity = alpha)
    } else if (change_dir == "not applicable" | is.na(change_dir)) {
      logo_out <- fontawesome::fa("circle", fill = trolley_grey_light, fill_opacity = alpha)
    } 
  }
  logo_out %>% as.character() %>% gt::html()
}


#' Function to create and color achievement circles
#' Returns a html formatting circle that can be embedded in gt object
#' @param x color hex code created by adorn_achievement
#' 
#' @export
#' 
#'@examples
#' \dontrun{
#'  achv_circle(trolley_grey)
#'  df %>% mutate(achv_color = achv_circle(achv_color)) }

achv_circle <- function(x){
  if(is.na(x)){
    fontawesome::fa("circle", fill = "white") %>% as.character() %>% gt::html()
  } else {
    fontawesome::fa("circle", fill = x) %>% as.character() %>% gt::html()
  }
}



#' GT function to format indicator + indicator plain definition
#' Creates the top part of table that lists indicator and description
#' 
#' @examples
#' \dontrun{
#'  format_indicator("HTS_TST Received HIV testing service and results")
#'  df %>% mutate(indicator2 = format_indicator(indicator2)) }
format_indicator <- function(x){
  name <- word(x, 1)
  name2 <- word(x, 2, -1)
    glue::glue(
    "<div style='line-height:10px'<span style='font-weight:regular;font-variant:small-caps;font-size:13px'>{name}</div>
        <div><span style='font-weight:regular;font-size:11px'>{name2}</br></div>"
    )
}

