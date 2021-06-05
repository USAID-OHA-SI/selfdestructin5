# PURPOSE: Munge and Analysis of FY21 Q2 MD tables for Prime Partners
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-06-05
# NOTES: New take on the tables for Prime Partners
# Builds on 01_Create_MD_tables


# SOURCE PARTS of 01_Create_MD_tables -------------------------------------

  source_parts("Scripts/01_Create_MD_Tables.R", start = 1, end = 268)


# HELPER FUNCTIONS --------------------------------------------------------
  
  # Helper to do a bit of repetitive munging
  clean_and_aggregate_im <- function(df){
    df %>% 
      filter(indicator %in% indics,
             standardizeddisaggregate %in% c("Total Numerator"),
             fundingagency == "USAID") %>% 
      group_by(fiscal_year, indicator, mech_name, mech_code) %>% 
      summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
      ungroup() %>% 
      mutate(mech_name = paste0(mech_name, " (", mech_code, ")")) %>% 
      select(-mech_code)
  }    

# SHAPE BASE TABLE --------------------------------------------------------
  # Shape the base dataframe from which the table is derived
  #@description shape the msd to wide with key indicators  
  #@param df - base msd from which all manipulations are done
  #@param country_col either countryname or operating unit, depending on table desired
  #@param ou countryname or operating unit
  
  shape_md_tbl_im <- function(df, country_col, ou) {
    
    # Filter the data down to key indicators defined in indics object
    # Collapsing down to the agency level
    ou_tbl <- 
      df %>% 
      filter({{country_col}} %in% ou) %>% 
      clean_and_aggregate_im()
    
    
    # Clean up and add up down flags, these will be used in version 1.0   
    md_tbl <- 
      ou_tbl %>% 
      reshape_msd("quarters", qtrs_keep_cumulative = TRUE) %>% 
      group_by(mech_name, indicator) %>% 
      mutate(value_run = row_number(),
             gap = targets - results_cumulative,
             gap_denom = (4 - (substr(period, 6, 6) %>% as.numeric)),
             gap_pace = gap_calc(gap, gap_denom),
             APR = denom_share(results_cumulative, targets)) %>% 
      ungroup() %>% 
      arrange(mech_name, indicator, period) %>% 
      group_by(mech_name, indicator) %>% 
      mutate(
        value_yr_lag = lag(results_cumulative, n = 4),
        q2q_comp = q2q_compare(results_cumulative, value_yr_lag),
        change_dir = if_else(q2q_comp > 0, "increase", "decrease")
      ) %>% 
      ungroup() %>% 
      mutate(indicator = fct_relevel(indicator, indics)) %>% 
      calc_achv(., APR, period) %>% 
      group_by(mech_name)
    
    # Old table layout
    md_tbl_old <- 
      md_tbl %>% 
      filter(period %in% c("FY20Q4", "FY21Q2")) %>% 
      select(period, mech_name, indicator, targets, results = cumulative, APR) %>% 
      mutate(period = str_sub(period, 1, 4)) %>%
      pivot_wider(names_from = period, 
                  names_glue = "{period}{.value}",
                  values_from = c(targets, results, APR),
                  names_sort = TRUE) %>% 
      arrange(mech_name, indicator) %>% 
      select(mech_name, indicator, sort(tidyselect::peek_vars()))
    
    return(md_tbl_old)
  }
  
  # Test function above
  shape_md_tbl_im(df = ou_im, country_col = operatingunit, ou = "Zambia") %>% prinf()  
  
  # Prettify names
  fix_col_names(shape_md_tbl_im(ou_im, operatingunit, "Zambia"))
  
  # Base Table Generation for IMs
  md_tbl_im <- function(md_tbl_old, tbl_col_names, ou) {
    
    cntry <-  str_to_upper(ou)
    team <- "Core Analytics Cluster"
    
    md_tbl_old %>% 
      ungroup() %>% 
      gt(groupname_col = "mech_name") %>% 
      # Format numbers
      fmt_percent(
        columns = contains("APR"), 
        decimal = 0
      ) %>% 
      fmt_number(
        columns = matches("targ|result"),
        decimal = 0
      ) %>% 
      fmt_missing(
        columns = everything(),
        missing_text = "-"
      ) %>% 
      cols_align(
        align = c("left"),
        columns = "indicator"
      ) %>% 
      tab_options(
        row_group.font.weight = "bold"
      ) %>% 
      opt_all_caps(
        all_caps = TRUE,
        locations = c("row_group")
      ) %>% 
      cols_label(.list = {{tbl_col_names}}) %>% 
      tab_spanner(
        label = md("**FY19**"),
        columns = contains("FY19")
      ) %>% 
      tab_spanner(
        label = md("**FY20**"),
        columns = contains("FY20")
      ) %>% 
      tab_spanner(
        label = md("**FY21 Q2**"),
        columns = contains("FY21")
      ) %>% 
      tab_style(
        style = list("font-variant: small-caps;"),
        locations = cells_column_labels(columns = everything()
        )
      ) %>% 
      tab_header(
        title = glue::glue("{cntry} IMPLEMENTING MECHANISM PERFORMACE SUMMARY")
      ) %>%
      opt_align_table_header(align = c("center")) %>% 
      add_achv_colors() %>% 
      tab_source_note(
        source_note = paste("Produced on ",Sys.Date(), "by the ", team, " using PEPFAR FY21Q2i MSD released on 2021-05-14")
      ) %>% 
      tab_options(
        source_notes.font.size = 10
      ) 
  }
  

  # Test it all together
  md_tbl_old <- shape_md_tbl_im(df = ou_im, country_col = countryname, ou = "Rwanda")
  tbl_col_names <- fix_col_names(md_tbl_old)
  md_tbl_im(md_tbl_old, tbl_col_names, "Rwanda")
  
  

# BATCH OUS and COUNTRIES -------------------------------------------------

  get_md_table_im <- function(df, country_col, ou) {
    
    message(paste('Creating base implementing mechanism MD table for', ou))
    
    # Reproduce MD table data frame
    md_tbl_old <- shape_md_tbl_im(df, {{country_col}}, ou)
    
    # Column labels
    tbl_col_names <- fix_col_names(md_tbl_old)
    
    # Generate the table
    md_ou_tbl <- md_tbl_im(md_tbl_old, tbl_col_names, ou) 
    return(md_ou_tbl)
  }  
  
  # Test for a single OU  
  get_md_table_im(ou_im, country_col = countryname, "Burkina Faso")
  get_md_table_im(ou_im, country_col = operatingunit, "Kenya")
  
  
  # BATCH
  ou_list <- ou_im %>% 
    distinct(operatingunit) %>% 
    pull()
  
  map(ou_list, ~get_md_table_im(ou_im, operatingunit, .x) %>% 
        gtsave(file.path("Images/OU", paste0(.x, "_FY21Q2_KEY_INDICATORS_MD_IMPLEMENTING_MECHANISM.png"))))
  