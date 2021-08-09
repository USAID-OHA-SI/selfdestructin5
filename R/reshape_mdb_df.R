#' Prepare filtered data frame for MSD briefer formatting
#' 
#' `reshape_mdb_df` takes the output from [make_mdb_df()] and creates a gt ready data frame.
#' The transformed data frame retains the most recent quarter and previous year's performance.
#' The resulting table can be passed directly to the [mdb_main_theme()] to create a MDB table.
#' Helper functions format certain columns as svgs to be rendered in the gt call.
#' 
#' @param df takes the [make_mdb_df()] results as an input 
#' @return returns a wide formatted data frame (table) of all OUs, countries and USAID
#' 
#' 
#' @seealso [make_mdb_df()] to see input required and
#'   [mdb_main_theme()] to see the gt theme used to format the table
#'   [make_chg_shape()] creates an svg based on the direction of change
#'   [make_achv_shape()] creates a colored circle based on achievement level
#' 
#' @export
#' @family data frame munging
#' 
#' @examples
#' \dontrun{
#'  mdb_df <- make_mdb_df(ou_im, resolve_issues = F)
#'  mdb_tbl <- reshape_mdb_df(mdb_df)
#'  }
#'  
#
reshape_mdb_df <- function(df) {
  
  # Need to know the fiscal year and quarters to filter
  if(!exists(pd)){
    pd <- gophr::identifypd(df)
  }
  
  fy_end <- pd %>% substr(3, 4) %>% as.numeric() + 2000
  fy_beg <- fy_end - 1 
  max_pd <- pd
  min_pd <- paste0("FY", substr(fy_beg, 3, 4), "Q4")
  
  # Filter out years and quarters not needed
  md_tbl <- 
    df %>% 
    ICPIutilities::reshape_msd("quarters", qtrs_keep_cumulative = TRUE) %>% 
    dplyr::arrange(agency, operatingunit, indicator, period) %>%
    dplyr::group_by(agency, operatingunit, indicator) %>% 
    dplyr::mutate(
      z_change = calc_growth(results_cumulative, dplyr::lag(results_cumulative, n = 4)),
      z_direction = dplyr::case_when(
        z_change >= 0  ~ "increase", 
        z_change <= 0  ~ "decrease",
        is.na(z_change) ~ "not applicable",
        TRUE ~ "not applicable")
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(qtr = substr(period, 5, 6)) %>% 
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
    dplyr::mutate(dplyr::across(c(z_change, z_direction, tint_achv, z_aresults), ~ dplyr::case_when(period == pd ~ .x))) %>%
    dplyr::mutate(order_col = ifelse(stringr::str_detect(period, max_pd), "present", "past"),
                  period = stringr::str_sub(period, 1, 4),
    ) %>%
    tidyr::pivot_wider(names_from = c(period, order_col),
                       names_glue = "{order_col}_{.value}",
                       values_from = c(targets, z_aresults, results_cumulative, targets_achievement, tint_achv, z_change, z_direction),
                       names_sort = TRUE) %>%
    dplyr::mutate(indicator2 = ifelse(agency == "USAID", paste(indicator, indicator_plain), paste(indicator)),
           indicator2 = ifelse(agency == "USAID", format_indicator(indicator2), indicator2), 
           agency = forcats::fct_relevel(agency, "USAID"), 
           present_z_direction = purrr::map(present_z_direction, make_chg_shape),
           present_tint_achv = purrr::map(present_tint_achv, make_achv_shape)
    ) %>%
    dplyr::arrange(agency) %>% 
    dplyr::select(-indicator_plain) %>%
    dplyr::select(agency, operatingunit, indicator2, indicator, sort(tidyselect::peek_vars())) %>% 
    dplyr::select(where(~!all(is.na(.x))))  #Drop all columns with NAs
  
  return(md_tbl)
}
