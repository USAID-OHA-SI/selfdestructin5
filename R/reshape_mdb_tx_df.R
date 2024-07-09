#' Prepare filtered data frame of treatment indicators for MDB formatting
#' 
#' `reshape_mdb_tx_df` takes the output from [make_mdb_tx_df()] and creates a gt ready data frame.
#' The transformed data frame retains the most recent quarter and previous year performance.
#' Viral load coverage and viral load suppression percentages are calculated along with TX MMD shares.
#' The resulting table can be passed directly to the [mdb_treatment_theme()] to create a MDB table.
#' Helper functions format certain columns as svgs to be rendered in the gt call. 
#' 
#' 
#' 
#' Create a wide formatted and sorted table for treatment indicators
#' @param df dataframe that is the result of running make_mdb_tx_df()
#' @param pd metadata from the MSD used to create time variables
#' 
#' @export
#' @return data frame that is pivoted wide for passing to gt() call
#' 
#' @family data frame munging 
#' 
#' @examples
#' \dontrun{
#'  mdb_df <- make_mdb_tx_df(ou_im, resolve_issues = F)
#'  mdb_tbl <- reshape_mdb_tx_df(mdb_df)
#'  }
#' 
#' 
#' 
#' 
reshape_mdb_tx_df <- function(df){
  
  pd <- fetch_metadata()
  
  indicators <- suppressMessages(fetch_indicators(tab = "treatment"))
  indicator_fltr <- indicators %>% dplyr::distinct(indicator) %>% dplyr::pull()
  
  fy_end <- pd$curr_pd %>% substr(3, 4) %>% as.numeric() + 2000
  fy_beg <- fy_end - 1 
  min_pd <- paste0("FY", substr(fy_beg, 3, 4), "Q4")
  
  # Should take the output from make_mdb_tx_df
  vlc_mdb_df <- df %>% 
    dplyr::select(-c(targets, cumulative)) %>% 
    gophr::reshape_msd() %>% 
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
                    operatingunit == "Global" ~ TX_PVLS_D / dplyr::lag(TX_CURR_VLC, n = 2), 
                    TRUE ~ TX_PVLS_D / dplyr::lag(TX_CURR, n = 2)
                  )
    ) %>% 
    dplyr::arrange(agency, operatingunit, period) %>%
    dplyr::select(-c(TX_PVLS_D, TX_PVLS, TX_CURR_VLC)) %>% 
    # Filter out rows you don't need - makes table dynamic
    dplyr::filter(stringr::str_detect(period, paste(substr(fy_end, 3, 4), min_pd, sep = "|"))) %>% 
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
                  change_dir = purrr::map(change_dir, make_chg_shape)) %>% 
    dplyr::left_join(., indicators %>% dplyr::select(indicator, indicator_plain)) %>% 
    dplyr::mutate(indicator = forcats::fct_relevel(indicator, indicator_fltr)) %>% 
    dplyr::mutate(indicator2 = ifelse(agency == "USAID", paste(indicator, indicator_plain), paste(indicator)),
                  indicator2 = ifelse(agency == "USAID", format_indicator(indicator2), indicator2),
                  agency = forcats::fct_relevel(agency, "USAID"),
                  indicator = forcats::fct_relevel(indicator, indicator_fltr)) %>% 
    dplyr::arrange(agency, indicator) %>% 
    dplyr::select(-c(indicator_plain, period_type)) %>% 
    dplyr::select(agency, operatingunit, indicator, indicator2, agg_type, tidyselect::everything())
  
  return(vlc_md_tbl)
}


