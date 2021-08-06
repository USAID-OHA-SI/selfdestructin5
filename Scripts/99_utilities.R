# Functions for Mission Director Tables Q3

#' Identify MSD Period and Type
#'
#' requires having si_path setup from glamr
#' 
#' @param type Type of MSD (OU_IM, PSNU, PSNU_IM, NAT_SUBNAT)
#' @param period can provide period from ICPIutilities::identifypd(), or it will run if NULL
#'
#' @return FY00Q0t MSD
#' @export

msd_period <- function(type = "OU_IM", period = NULL){
  file <- glamr::si_path() %>% 
    glamr::return_latest(type)
  
  
  v <- file %>% 
    basename() %>% 
    stringr::str_extract("(?<=_v).{1}")
  
  v <- ifelse(v == "1", "i", "c")
  
  if(is.null(period)){
    period <- file %>% 
      ICPIutilities::read_msd() %>% 
      ICPIutilities::identifypd()
    
  }
  
  pd <- glue::glue("{period}{v} MSD")
  
  return(pd)
  
}

#' Calculate growth between two metrics that are positive
#' 
#' Helper function for calculating growth.
#' Returns NA_real_ if the 
#' @param x numerator to calculate growth - most recent metric
#' @param y denominator to calculate growth - lagged metric
#' 
#'@examples
#' \dontrun{
#'  calc_growth(110, 100) #should give 10%
#'  df %>% mutate(z_change = calc_growth(results_cumulative, dplyr::lag(results_cumulative, n = 4)))}
#' 
calc_growth <- function(x, y) {
  ifelse(x > 0.000, (x / y) - 1, NA_real_)
}



#' Fetches indicators used to make tables, accounting for quarter of MSD and semi-annual indicators
#' 
#' MDB indicators are stored on google drive. This helper function fetches a desired tab.
#' Used to create filters, sort factor levels, and apply plain labels to indicators.
#' 
#' @param df data frame on which indicators are to be fetched
#' @param table which tab
#' @return dataframe of indicators 
#'  
#'@examples
#' \dontrun{
#'  fetch_indicators(ou_im, tab = "main")
#'  fetch_indicators(ou_im, tab = "treatment")}
#'   
# Fetch indicators
fetch_indicators <- function(df, tab = "main") {
  
  # Determine what quarter you are fetching data from
  pd <- df %>% 
    ICPIutilities::identifypd(.) %>% 
    stringr::str_extract(., ".{2}$")
  cat("Fetching ", crayon::yellow(pd), " indicators\n")
  
  if(!tab %in% c("main", "treatment")) {
    stop("Please select the indicator crosswalk you would like to use: main or treatment")
  }
  
  # Pull in indicator list from google drive
  if (tab == "treatment") {
    indic_list <- googlesheets4::read_sheet("1Xv9QQp6AkDdKxSYGRQ8UYJidBE8LzMnEzjIlYEur83A", sheet = "indicator_tx")
  } else  {
    indic_list <- googlesheets4::read_sheet("1Xv9QQp6AkDdKxSYGRQ8UYJidBE8LzMnEzjIlYEur83A", sheet = "indicator_cw")
  } 
  
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

#' Label the level of aggregation
# Function to label aggreagation level based on input
label_aggregation <- function(df, type = "ou") {
  
  if(!type %in% c("regional", "agency", "ou")) {
    stop("Please select the type of aggregation label to apply: ou, regional or agency")
  }  
  
  if (type == "agency") {
    df %>% 
      dplyr::mutate(agg_type = "Agency", 
                    operatingunit = "Global") 
  } else if (type == "regional") {
    df %>% 
      dplyr::mutate(agg_type = "Region-Country",
                    operatingunit = paste(operatingunit, countryname, sep = "-")) %>% 
      dplyr::select(-countryname)
  } else {
    df %>% dplyr::mutate(agg_type = "OU")
  }
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
  } 
  
  # Get the indicator info you need 
    indicators <- fetch_indicators(df)
    indicator_fltr <- indicators %>% 
      distinct(indicator) %>% 
      pull()
  
  # Create three dataframes for ou, regional-country, agency
  df_ou <- df %>% 
    dplyr::filter(operatingunit %in% ou_list) %>% 
    collapse_base_tbl(., indicator_fltr, operatingunit) %>% 
    label_aggregation(type = "ou")
  
  df_reg <- df %>% 
    dplyr::filter(stringr::str_detect(operatingunit, "Region")) %>% 
    collapse_base_tbl(., indicator_fltr, operatingunit, countryname) %>% 
    label_aggregation(type = "regional")
  
  df_usaid <- df %>% 
    dplyr::mutate(operatingunit = ifelse(fundingagency == "USAID", "USAID", "ALL OTHER AGENCIES")) %>% 
    collapse_base_tbl(., indicator_fltr, operatingunit) %>% 
    label_aggregation(type = "agency") 
  
  # Bind the tables, set indicator levels
  md_table <- dplyr::bind_rows(df_ou, df_reg, df_usaid) %>% 
    dplyr::left_join(., indicators %>% dplyr::select(indicator, indicator_plain)) %>% 
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
  pd <- ICPIutilities::identifypd(df)
  fy_end <-  pd %>% stringr::str_sub(., 3, 4) %>% as.numeric() + 2000
  fy_beg <- fy_end - 1 
  max_pd <- pd
  min_pd <- paste0("FY", str_sub(fy_beg, 3, 4), "Q4")
  
  # Filter out years and quarters not needed
  md_tbl <- 
    df %>% 
    ICPIutilities::reshape_msd("quarters", qtrs_keep_cumulative = TRUE) %>% 
    dplyr::arrange(agency, operatingunit, indicator, period) %>%
    dplyr::group_by(agency, operatingunit, indicator) %>% 
    dplyr::mutate(
      z_change = calc_growth(results_cumulative, dplyr::lag(results_cumulative, n = 4)),
      z_direction = case_when(
        z_change >= 0  ~ "increase", 
        z_change <= 0  ~ "decrease",
        is.na(z_change) ~ "not applicable",
        TRUE ~ "not applicable")
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(qtr = str_sub(period, 5, 6)) %>% 
    dplyr::filter(period %in% c(max_pd, min_pd)) %>% 
    ICPIutilities::calc_achievement() %>% 
    ICPIutilities::adorn_achievement()
  
  
  # Now, rename so results are output in order after pivoting
  # Use z_ and tarc_ stubs to ensure ordering of columns is consistent 
  md_tbl <- 
    md_tbl %>% 
    dplyr::rename(tint_achv = achv_color,
           targets_achievement = achievement, 
           z_aresults = results) %>% 
    dplyr::select(-c(fiscal_year, cumulative, achievement_qtrly, achv_label)) %>%
    dplyr::mutate(across(c(z_change, z_direction, tint_achv, z_aresults), ~ dplyr::case_when(period == pd ~ .x))) %>%
    dplyr::mutate(order_col = ifelse(str_detect(period, max_pd), "present", "past"),
           period = stringr::str_sub(period, 1, 4),
    ) %>%
    tidyr::pivot_wider(names_from = c(period, order_col),
                names_glue = "{order_col}_{.value}",
                values_from = c(targets, z_aresults, results_cumulative, targets_achievement, tint_achv, z_change, z_direction),
                names_sort = TRUE) %>%
    mutate(indicator2 = ifelse(agency == "USAID", paste(indicator, indicator_plain), paste(indicator)),
           indicator2 = ifelse(agency == "USAID", format_indicator(indicator2), indicator2), 
           agency = forcats::fct_relevel(agency, "USAID"), 
           present_z_direction = purrr::map(present_z_direction, rank_chg),
           present_tint_achv = purrr::map(present_tint_achv, achv_circle)
    ) %>%
    dplyr::arrange(agency) %>% 
    dplyr::select(-indicator_plain) %>%
    dplyr::select(agency, operatingunit, indicator2, indicator, sort(tidyselect::peek_vars())) %>% 
    dplyr::select(where(~!all(is.na(.x))))  #Drop all columns with NAs
  
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


### VIRAL LOAD Munging FUNCTIONS


#' Takes an ou_im data frame and preps it for treatment mdb treatment table
#' 
#' @param df an ou_im table
#' @param ... allows for grouping at one of three levels
#' 
#' 
#' 
#' 
#' 
collapse_vlc_tbl  <- function(df, ...) {
  
  vlc_indics <- c("TX_CURR", "TX_PVLS")
  
  vlc_df <-  df %>% 
    dplyr::filter(indicator %in% vlc_indics,
           disaggregate %in%  c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator", "Total Denominator") |
             otherdisaggregate %in% c("ARV Dispensing Quantity - 6 or more months", "ARV Dispensing Quantity - 3 to 5 months"),
           fundingagency != "Dedup") %>% 
    dplyr::mutate(agency = ifelse(fundingagency == "USAID", "USAID", "ALL OTHER AGENCIES"),
           indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
           indicator = dplyr::case_when(
             stringr::str_detect(otherdisaggregate, "3 to 5") ~ "TX_MMD3+",
             stringr::str_detect(otherdisaggregate, "6 or more") ~ "TX_MMD6+",
             TRUE ~ indicator)
    ) %>%
    dplyr::filter(indicator != "TX_CURR") %>%
    dplyr::group_by(fiscal_year, agency, indicator, ...) %>%
    dplyr::summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup() 
  return(vlc_df)
}





#' Creates the base table for the treatment indicators
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
    collapse_base_tbl(., indic_list = indic, operatingunit) %>% 
    label_aggregation(type = "ou")
  
  df_reg_tx <- df %>% 
    dplyr::filter(str_detect(operatingunit, "Region")) %>% 
    collapse_base_tbl(., indic_list = indic, operatingunit, countryname) %>% 
    label_aggregation(., type = "regional") 
  
  df_usaid_tx <- df %>% 
    dplyr::filter(operatingunit != "South Africa") %>% 
    dplyr::mutate(operatingunit = ifelse(fundingagency == "USAID", "USAID", "ALL OTHER AGENCIES")) %>% 
    collapse_base_tbl(., indic_list = indic) %>% 
    label_aggregation(type = "agency")
  
  # Create the base disagg tables
  tx_table <- dplyr::bind_rows(df_ou_tx, df_reg_tx, df_usaid_tx) 
  
  # TX PVLS TABLE
  # Get all the VLC table info needed
  vlc_ou <- df %>% 
    dplyr::filter(operatingunit %in% ou_list) %>% 
    collapse_vlc_tbl(., operatingunit) %>% 
    label_aggregation(type = "ou") 
  
  vlc_reg <- df %>% 
    dplyr::filter(str_detect(operatingunit, "Region")) %>% 
    collapse_vlc_tbl(., operatingunit, countryname) %>% 
    label_aggregation(type = "regional")
  
  vlc_usaid <- df %>% 
    dplyr::mutate(operatingunit = ifelse(fundingagency == "USAID", "USAID", "ALL OTHER AGENCIES")) %>% 
    collapse_vlc_tbl(.) %>% 
    label_aggregation(type = "agency")
  
  vlc_table <- dplyr::bind_rows(vlc_ou, vlc_reg, vlc_usaid)
  
  # Run once on aggregated info
  # This returns tx_mmd3+ for the agency/operating unit combo level
  mmd3_table <- 
    vlc_table %>% 
    dplyr::mutate(tx_mmd_group = if_else(indicator %in% c("TX_MMD3+", "TX_MMD6+"), 1, 0)) %>% 
    dplyr::filter(tx_mmd_group == 1) %>% 
    dplyr::group_by(fiscal_year, agency, operatingunit, agg_type) %>% 
    dplyr::summarise(across(matches("tar|cumu|qtr"), sum, na.rm = T)) %>% 
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




#' Create a wide formatted and sorted table for treatment indicators
#' @param df dataframe that is the result of running make_mdb_tx_df()
#' @param df2 dataframe containing the treatment indicators pulled from drive
#' 
#' 
reshape_mdb_tx_df <- function(df){
  
  indicators <- fetch_indicators(df, tab = "treatment")
  indicator_fltr <- indicators %>% distinct(indicator) %>% pull()
  
  # Should take the output from make_mdb_tx_df
  vlc_mdb_df <- df %>% 
    dplyr::select(-c(targets, cumulative)) %>% 
    ICPIutilities::reshape_msd() %>% 
    tidyr::spread(indicator, value) %>% 
    dplyr::arrange(agency, operatingunit, period) %>%
    # Calculate global TX_CURR for global mmd
    dplyr::group_by(agg_type, period, agency) %>% 
    dplyr::mutate(TX_CURR_VLC = dplyr::case_when(
      agg_type == "OU" ~ sum(TX_CURR, na.rm = T), 
      TRUE ~ NA_real_
    ))  %>% 
    dplyr::ungroup() %>%
    dplyr::group_by(agency, period) %>%
    tidyr::fill(TX_CURR_VLC) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(agency, operatingunit, period) %>%
    dplyr::mutate(TX_MMD3_SHARE = `TX_MMD3+`/TX_CURR,
                  VLS = TX_PVLS / TX_PVLS_D,
                  VLC = dplyr::case_when(
                    operatingunit == "Global" ~ TX_PVLS_D / lag(TX_CURR_VLC, n = 2), 
                    TRUE ~ TX_PVLS_D / lag(TX_CURR, n = 2)
                  )
    ) %>% 
    arrange(agency, operatingunit, period) %>%
    dplyr::select(-c(TX_PVLS_D, TX_PVLS, TX_CURR_VLC)) %>% 
    # Filter out rows you don't need - makes table dynamic
    dplyr::filter(stringr::str_detect(period, paste(stringr::str_sub(fy_end, 3, 4), min_pd, sep = "|"))) %>% 
    tidyr::pivot_longer(cols = TX_CURR:VLC,
                 names_to = "indicator",
                 values_to = "results") %>% 
      dplyr::mutate(period = dplyr::case_when(
      period == min_pd ~ "results",
      TRUE ~ stringr::str_extract(period, ".{2}$")
    )
    ) %>% 
      tidyr::pivot_wider(names_from = period, 
                values_from = results) 
  
  # Determine length of df    
  end_pos <- length(vlc_mdb_df)
  lag_pos <- end_pos - 1
  
  vlc_md_tbl <- 
    vlc_mdb_df %>% 
    dplyr::mutate(`delta*` = .[[end_pos]] - .[[lag_pos]],
           change_dir = dplyr::case_when(
             `delta*` >=  0.005 ~ "increase", 
             `delta*` <= -0.005 ~ "decrease",
             TRUE ~ "not applicable"
           ),
           change_dir = purrr::map(change_dir, rank_chg))%>% 
    dplyr::left_join(., indicators %>% dplyr::select(indicator, indicator_plain)) %>% 
    dplyr::mutate(indicator = forcats::fct_relevel(indicator, indicator_fltr)) %>% 
    dplyr::mutate(indicator2 = ifelse(agency == "USAID", paste(indicator, indicator_plain), paste(indicator)),
           indicator2 = ifelse(agency == "USAID", format_indicator(indicator2), indicator2),
           agency = forcats::fct_relevel(agency, "USAID"),
           indicator = forcats::fct_relevel(indicator, indicator_fltr)) %>% 
    dplyr::arrange(agency, indicator) %>% 
    dplyr::select(-c(indicator_plain, period_type)) %>% 
    dplyr::select(agency, operatingunit, indicator, indicator2, agg_type, everything())
  
  return(vlc_md_tbl)
}


# Generate objects needed

