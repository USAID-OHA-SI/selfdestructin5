# PURPOSE: Munge and Analysis of FY21 Q2 MD tables for Comparing USAID OUS
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-06-05
# NOTES: New take on the tables for Prime Partners
# Builds on 01_Create_MD_tables


# SOURCE PARTS of 01_Create_MD_tables -------------------------------------

  source_parts("Scripts/01_Create_MD_Tables.R", start = 1, end = 268)


# MUNGE USAID OU ACHIEVEMENT --------------------------------------------


  clean_and_aggregate_ou_comp <- function(df){
    df %>% 
      filter(indicator %in% c(indics, "PrEP_CURR"),
             standardizeddisaggregate %in% c("Total Numerator"),
             fundingagency == "USAID") %>% 
      group_by(fiscal_year, operatingunit, indicator) %>% 
      summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop")
  }  
  
  shape_md_tbl_ou_comp <- function(df) {
    
    # Filter the data down to key indicators defined in indics object
    # Collapsing down to the agency level
    ou_tbl <- 
      df %>% 
      clean_and_aggregate_ou_comp()
    
    
    # Clean up and add up down flags, these will be used in version 1.0   
    md_tbl <- 
      ou_tbl %>% 
      reshape_msd("quarters", qtrs_keep_cumulative = TRUE) %>% 
      group_by(operatingunit, indicator) %>% 
      mutate(value_run = row_number(),
             gap = targets - results_cumulative,
             gap_denom = (4 - (substr(period, 6, 6) %>% as.numeric)),
             gap_pace = gap_calc(gap, gap_denom),
             APR = denom_share(results_cumulative, targets)) %>% 
      ungroup() %>% 
      arrange(operatingunit, indicator, period) %>% 
      group_by(operatingunit, indicator) %>% 
      mutate(
        value_yr_lag = lag(results_cumulative, n = 4),
        q2q_comp = q2q_compare(results_cumulative, value_yr_lag),
        change_dir = if_else(q2q_comp > 0, "increase", "decrease")
      ) %>% 
      ungroup() %>% 
      mutate(indicator = fct_relevel(indicator, indics)) %>% 
      calc_achv(., APR, period) %>% 
      group_by(operatingunit)
    
    # Old table layout
    md_tbl_old <- 
      md_tbl %>% 
      filter(period %in% c("FY20Q4", "FY21Q2")) %>% 
      select(period, operatingunit, indicator, targets, results = cumulative, APR) %>% 
      mutate(period = str_sub(period, 1, 4)) %>%
      pivot_wider(names_from = period, 
                  names_glue = "{period}{.value}",
                  values_from = c(targets, results, APR),
                  names_sort = TRUE) %>% 
      arrange(operatingunit, indicator) %>% 
      select(operatingunit, indicator, FY21APR) %>%
      spread(indicator, FY21APR) %>% 
      relocate(PrEP_CURR, .after = PrEP_NEW)
      
    
    return(md_tbl_old)
  }
  shape_md_tbl_ou_comp(ou_im)
  

# GENERATE OU COMPARISON TABLE --------------------------------------------

  shape_md_tbl_ou_comp(ou_im) %>%
    arrange(desc(TX_CURR)) %>% 
    ungroup() %>% 
    gt() %>% 
    fmt_percent(
      columns = -c(operatingunit),
      decimals = 0
    ) %>% 
    fmt_missing(
      columns = -c(operatingunit),
      missing_text = "-"
     ) %>% 
    tab_spanner(
      label = md("**PREVENTION**"),
      columns = PrEP_NEW:VMMC_CIRC
    ) %>% 
    tab_spanner(
      label = md("**TESTING**"),
      columns = matches("HTS")
    ) %>% 
    tab_spanner(
      label = md("**TREATMENT**"),
      columns = matches("TX")
    ) %>% 
    tab_style(
      style = list("font-variant: small-caps;"),
      locations = cells_column_labels(columns = 1)
    ) %>%
    tab_style(
      style = cell_borders(
        sides = c("left", "right"),
        color = "#ffffff",
        weight = px(1.5),
        style = "solid"),
      locations = cells_body(
        columns = 2:8
        )
      ) %>% 
      tab_style(
        style = list(
          cell_borders(
            sides = "left",
            color = "white",
            weight = px(10)
          )
        ),
        locations = list(
          cells_body(
            columns = vars(HTS_TST, TX_NEW)
          )
        )
      ) %>% 
    tab_header(
      title = glue::glue("FY21 Q2 USAID OU PERFORMANCE SUMMARY")
    ) %>%
    opt_align_table_header(align = c("center")) %>% 
    data_color(columns = 2:7, colors = pal, alpha = 0.50) %>% 
    data_color(columns = 8, colors = pal_tx, alpha = 0.50) %>% 
    tab_source_note(
      source_note = paste("Produced on ",Sys.Date(), "by the Core Analytics Cluster using PEPFAR FY21Q2i MSD released on 2021-05-14")
    ) %>% 
    tab_options(
      source_notes.font.size = 10
    ) %>% 
    gtsave("Images/Global/GLOBAL_FY21Q2_OU_COMPARISON_MD.png")

    