# PURPOSE: Munge and Analysis of MD tables to produce VLS, VLC and TX_MMD Calculations
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-06-03
# NOTES: Builds on the MD_tables_reboot.r script

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(ICPIutilities)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(here)
    library(extrafont)
    

# LOAD DATA ============================================================================  

  # Builds on ou_im data loaded in MD_tables_reboot, but need 2 month lag of tx_curr
  # South Africa has no national MMD program and has been excluded from MMD coverage rates
  # Keep 2019 values in to calculate running history of VLC and VLS
  ou_im_vlc <- 
    si_path() %>% 
    return_latest("OU_IM_FY19-21_20210514") %>% 
    read_msd() %>% 
    filter(fiscal_year %in% c(2019, 2020, 2021))


# HELPER FUNCTIONS --------------------------------------------------------
  #This has to change to TX_CURR for VLS/MMD TABLES
  
  clean_and_aggregate_tx <- function(df){
    df %>% 
      filter(indicator %in% c("TX_CURR"),
             standardizeddisaggregate %in% c("Total Numerator"),
             fundingagency != "Dedup") %>% 
      clean_agency() %>% 
      mutate(agency = fct_lump(fundingagency, n = 2, other_level = "OTHER"),
             agency = fct_relevel(agency, agency_order_long)) %>% 
      group_by(fiscal_year, agency, indicator) %>% 
      summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop")
  }   
  
  
# Functions to return key data frames needed to patch together tables
# One of the challenges is that when doing the global calculations, 
# South Africa is excluded from the denominator for TX_MMD3+_share
  
  #@description returns the base tx_curr data frame needed for calculations
  #@param - df should contain last three years of msd ou_im data
  #@param - country_col is the column to filter for ou or country
  #@param - ou is the name of the country or operating unit
  
  get_tx_curr_base <- function(df, country_col, ou){
    tx_curr_mmd <-
      df %>% 
      filter({{country_col}} %in% ou) %>% 
      clean_and_aggregate_tx(.)
    return(tx_curr_mmd)
  }
  
  tx_curr_base <- get_tx_curr_base(ou_im_vlc, operatingunit, "Zambia")
  
  get_mmd_vlc_base <- function(df, country_col, ou){
    mmd_vlc <-
      df %>%
      filter(indicator %in% c("TX_CURR", "TX_PVLS"),
             disaggregate %in%  c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator", "Total Denominator") |
               otherdisaggregate %in% c("ARV Dispensing Quantity - 6 or more months", "ARV Dispensing Quantity - 3 to 5 months"),
             fundingagency != "Dedup",
             {{country_col}} %in% ou) %>%
      clean_agency() %>%
      mutate(agency = fct_lump(fundingagency, n = 2, other_level = "OTHER"),
             agency = fct_relevel(agency, agency_order_long),
             indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
             indicator = case_when(
               str_detect(otherdisaggregate, "3 to 5") ~ "TX_MMD3+",
               str_detect(otherdisaggregate, "6 or more") ~ "TX_MMD6+",
               TRUE ~ indicator )
      ) %>%
      filter(indicator != "TX_CURR") %>%
      group_by(fiscal_year, agency, indicator) %>%
      summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>%
      ungroup() %>%
      arrange(agency, fiscal_year, indicator) 
    return(mmd_vlc)
  }
  mmd_vlc <- get_mmd_vlc_base(ou_im_vlc, operatingunit, "Zambia")

  # Calculate TX_MMD3+ (sums TX_MMD3 and TX_MMD6)
  get_tx_mmds <- function(df){
    tx_mmds <- 
      df %>% 
      mutate(tx_mmd_group = if_else(indicator %in% c("TX_MMD3+", "TX_MMD6+"), 1, 0)) %>% 
      filter(tx_mmd_group == 1) %>% 
      group_by(fiscal_year, agency) %>% 
      summarise(across(matches("tar|cumu|qtr"), sum, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(indicator = "TX_MMD3+", .after = agency)
    return(tx_mmds)
  }
  tx_mmds <- get_tx_mmds(mmd_vlc)
  
  # Pull everything together into a table ready data frame
  shape_vlc_tbl <- function(mmd_vlc, tx_mmds, tx_curr_base) {
    
    mmd_vlc_tbl <- 
      mmd_vlc %>% 
      filter(!indicator %in% c("TX_MMD3+")) %>% 
      bind_rows(tx_mmds) %>% 
      bind_rows(tx_curr_base) %>% 
      select(-c(targets, cumulative)) %>% 
      reshape_msd() %>% 
      spread(indicator, value) %>% 
      arrange(agency, period) %>% 
      mutate(TX_MMD3_SHARE = `TX_MMD3+`/TX_CURR,
             VLS = TX_PVLS / TX_PVLS_D,
             VLC = TX_PVLS_D / lag(TX_CURR, n = 2)) %>%
      pivot_longer(cols = -c(period, agency, period_type),
                   names_to = "indicator",
                   values_to = "results") %>% 
      spread(period, results) %>% 
      select(agency, indicator, FY20results = FY20Q4, FY21Q1 = FY21Q1, FY21Q2) %>% 
      mutate(FY20APR = NA_real_,
             FY20targets = NA_integer_,
             FY21APR = NA_real_,
             FY21targets = NA_integer_, .after = FY21Q2,
             delta = FY21Q2 - FY21Q1) %>% 
      relocate(FY20APR, .after = indicator) %>% 
      relocate(FY20targets, .after = FY20results) %>% 
      relocate(FY21Q2, .after = FY21APR) %>% 
      filter(!indicator %in% c("TX_PVLS", "TX_PVLS_D", "TX_CURR_VLC")) %>% 
      mutate(indicator = case_when(
        indicator == "TX_CURR" ~ "TX_CURR",
        indicator == "TX_MMD3_SHARE" ~ "MMD 3+ Share",
        indicator == "TX_MMD3+" ~ "MMD 3+",
        indicator == "TX_MMD6+" ~ "MMD 6+",
        indicator == "VLC" ~ "Viral Load Coverage",
        indicator == "VLS" ~ "Virally Suppressed",
      )) %>% 
      mutate(indicator = fct_relevel(indicator,
                                     "TX_CURR",
                                     "MMD 3+ Share",
                                     "MMD 3+",
                                     "MMD 6+",
                                     "Viral Load Coverage",
                                     "Virally Suppressed"),
             agency = fct_relevel(agency,
                                  agency_order_long)) %>% 
      arrange(agency, indicator)
  return(mmd_vlc_tbl)
  }
  md_vlc_df <- shape_vlc_tbl(mmd_vlc, tx_mmds, tx_curr_base)
  

# PRODUCE VLS and VLC TABLES ----------------------------------------------
  
  make_md_vlc_tbl <- function(df, ou) {
  
      cntry <-  str_to_upper(ou)
      team <- "Core Analytics Cluster"
      
      df %>% 
        gt(groupname_col = "agency") %>% 
        fmt_percent(
          columns = matches("results|Q1|Q2|delta"),
          rows = str_detect(indicator, "(Share|Coverage|Supp)"),
          decimal = 0
        ) %>% 
        fmt_number(
          columns = matches("results|Q1|Q2|delta"),
          rows = indicator %in% c("TX_CURR", "MMD 3+", "MMD 6+"),
          decimal = 0
        ) %>% 
        fmt_missing(
          columns = everything(),
          missing_text = "-"
        ) %>% 
        tab_style(style = list(cell_fill(color = old_rose_light, alpha = 0.25)),
                  locations = cells_body(
                    columns = c(delta),
                    rows = delta <= -0.005)
        ) %>% 
        tab_style(style = list(cell_fill(color = genoa_light, alpha = 0.25)),
                  locations = cells_body(
                    columns = c(delta),
                    rows = delta >= 0.005)
        ) %>% 
        cols_hide(
          columns = c(FY20APR, FY20targets, FY21APR, FY21targets)
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
        tab_spanner(
          label = md("**FY20**"),
          columns = contains("FY20")
        ) %>% 
        tab_spanner(
          label = md("**FY21**"),
          columns = matches("FY21|delta")
        ) %>% 
        cols_label(
          indicator = "",
          FY21Q1 = "Q1",
          FY21Q2 = "Q2"
        ) %>% 
        opt_align_table_header(align = c("center")) %>% 
        tab_options(
          footnotes.font.size = 10,
          source_notes.font.size = 10
        ) %>% 
        tab_header(
          title = glue::glue("{cntry} MULTI-MONTH DISPENSING AND VIRAL LOAD SUMMARY")
        ) %>% 
        tab_source_note(
          source_note = "Viral Load Covererage = TX_PVLS_N / TX_CURR_2_period_lag"
        ) %>% 
        tab_source_note(
          source_note = paste("Produced on ",Sys.Date(), "by the ", team, " using PEPFAR FY21Q2i MSD released on 2021-05-14")
        ) 
  }  
  
  make_md_vlc_tbl(md_vlc_df, "Zambia")
  
  # Wrapper around everything to pull it all together  
  get_md_vls_table <- function(df, country_col, ou) {
    
    # Where are you at in loop?    
    message(paste('Creating base VLS/MMD MD table for', ou))
    
    tx_curr_base <- get_tx_curr_base(df, {{country_col}}, ou)
    mmd_vlc <- get_mmd_vlc_base(df, {{country_col}}, ou)
    
    tx_mmds <- get_tx_mmds(mmd_vlc)
    md_vlc_df <- shape_vlc_tbl(mmd_vlc, tx_mmds, tx_curr_base)
    
    md_ou_vlc_tbl <- make_md_vlc_tbl(md_vlc_df, ou)
    return(md_ou_vlc_tbl)
  }  
  
  # Test it all
  get_md_vls_table(ou_im_vlc, countryname, "Kazakhstan")


# BATCH VLC/MMD TABLES ----------------------------------------------------

  # Distinct list of OUS to loop over
  ou_list <- ou_im_vlc %>% 
    distinct(operatingunit) %>% 
    pull()
  
  map(ou_list, ~get_md_vls_table(ou_im_vlc, operatingunit, .x) %>% 
        gtsave(file.path("Images/OU", paste0(.x, "_FY21Q2_MMD_VL_MD.png"))))    
  
  # Distinct list of Countries in Regional OUS
  
  # TODO - Create a function that flags countries with TX_MMD (many do not report it)
  # Asia
  asia_cntry_list <- 
    ou_im_vlc %>% 
    filter(str_detect(operatingunit, "Asia Region")) %>% 
    filter(!countryname %in% c("China", "Philippines")) %>% 
    distinct(countryname) %>% 
    pull()
  
  map(asia_cntry_list, ~get_md_vls_table(ou_im_vlc, countryname, .x) %>% 
        gtsave(file.path("Images/Regional/Asia", paste0(.x, "_FY21Q2_MMD_VL_MD.png"))))
  
  # West Africa
  westafr_cntry_list <- 
    ou_im_vlc %>% 
    filter(str_detect(operatingunit, "Africa Region")) %>% 
    filter(!countryname %in% c("Sierra Leone")) %>% 
    distinct(countryname) %>% 
    pull()
  
  map(westafr_cntry_list, ~get_md_vls_table(ou_im_vlc, countryname, .x) %>% 
        gtsave(file.path("Images/Regional/WAR", paste0(.x, "_FY21Q2_MMD_VL_MD.png"))))
  
  
  # Western Hemisphere
  # Omitting Guyana and Barbados due to no reporting in FY21
  wh_cntry_list <- 
    ou_im_vlc %>% 
    filter(str_detect(operatingunit, "Western")) %>% 
    filter(!countryname %in% c("Guyana", "Barbados", "Nicaragua", "Suriname")) %>% 
    distinct(countryname) %>% 
    pull()
  
  map(wh_cntry_list, ~get_md_vls_table(ou_im_vlc, countryname, .x) %>% 
        gtsave(file.path("Images/Regional/WesternHemi", paste0(.x, "_FY21Q2_MMD_VL_MD.png"))))

  

#  GENERATE GLOBAL TABLE -- SOUTH AFRICA FLAG -----------------------------


  
  #  Get the TX_CURR data needed for calculations
  #  Excluding South Africa
  tx_curr_mmd <-
    ou_im_vlc %>% 
    filter(operatingunit != "South Africa") %>% 
    clean_and_aggregate_tx(.) %>% 
    mutate(indicator = "TX_CURR_MMD")
  
  tx_curr_vls <-
    ou_im_vlc %>% 
    clean_and_aggregate_tx(.) %>% 
    mutate(indicator = "TX_CURR_VLC")
  
  
  # Pull out TX_MMD info and VLC and VLS info
  mmd_vlc <-
    ou_im_vlc %>%
    filter(indicator %in% c("TX_CURR", "TX_PVLS"),
           disaggregate %in%  c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator", "Total Denominator") |
             otherdisaggregate %in% c("ARV Dispensing Quantity - 6 or more months", "ARV Dispensing Quantity - 3 to 5 months"),
           fundingagency != "Dedup") %>%
    clean_agency() %>%
    mutate(agency = fct_lump(fundingagency, n = 2, other_level = "OTHER"),
           agency = fct_relevel(agency, agency_order_long),
           indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
           indicator = case_when(
             str_detect(otherdisaggregate, "3 to 5") ~ "TX_MMD3+",
             str_detect(otherdisaggregate, "6 or more") ~ "TX_MMD6+",
             TRUE ~ indicator )
    ) %>%
    filter(indicator != "TX_CURR") %>%
    group_by(fiscal_year, agency, indicator) %>%
    summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    arrange(agency, fiscal_year, indicator) 
  
  # Calculate new TX_MMD3+ that contains both 3-6 and 6+ months of ART  
  # Pull in TX_CURR values from agency table
  tx_mmds <- get_tx_mmds(mmd_vlc)
  

  mmd_vlc_tbl <- 
    mmd_vlc %>% 
    filter(!indicator %in% c("TX_MMD3+")) %>% 
    bind_rows(tx_mmds) %>% 
    bind_rows(tx_curr_mmd) %>% 
    bind_rows(tx_curr_vls) %>% 
    select(-c(targets, cumulative)) %>% 
    reshape_msd() %>% 
    spread(indicator, value) %>% 
    arrange(agency, period) %>% 
    mutate(TX_MMD3_SHARE = `TX_MMD3+`/TX_CURR_MMD,
           VLS = TX_PVLS / TX_PVLS_D,
           VLC = TX_PVLS_D / lag(TX_CURR_VLC, n = 2)) %>%
    pivot_longer(cols = -c(period, agency, period_type),
                 names_to = "indicator",
                 values_to = "results") %>% 
    spread(period, results) %>% 
    select(agency, indicator, FY20results = FY20Q4, FY21Q1, FY21Q2) %>% 
    mutate(FY20APR = NA_real_,
           FY20targets = NA_integer_,
           FY21APR = NA_real_,
           FY21targets = NA_integer_, .after = FY21Q2,
           delta = FY21Q2 - FY21Q1) %>% 
    relocate(FY20APR, .after = indicator) %>% 
    relocate(FY20targets, .after = FY20results) %>% 
    relocate(FY21Q2, .after = FY21APR) %>% 
    filter(!indicator %in% c("TX_PVLS", "TX_PVLS_D", "TX_CURR_VLC")) %>% 
    mutate(indicator = case_when(
      indicator == "TX_CURR_MMD" ~ "TX_CURR Adjusted",
      indicator == "TX_MMD3_SHARE" ~ "MMD 3+ Share",
      indicator == "TX_MMD3+" ~ "MMD 3+",
      indicator == "TX_MMD6+" ~ "MMD 6+",
      indicator == "VLC" ~ "Viral Load Coverage",
      indicator == "VLS" ~ "Virally Suppressed",
    )) %>% 
    mutate(indicator = fct_relevel(indicator,
                                   "TX_CURR Adjusted",
                                   "MMD 3+ Share",
                                   "MMD 3+",
                                   "MMD 6+",
                                   "Viral Load Coverage",
                                   "Virally Suppressed"),
           agency = fct_relevel(agency,
                               agency_order_long)) %>% 
    arrange(agency, indicator)
    
    
  # PRoduce Table for Global
  
  mmd_vlc_tbl %>% 
    gt(groupname_col = "agency") %>% 
    fmt_percent(
      columns = matches("results|Q1|Q2|delta"),
      rows = str_detect(indicator, "(Share|Coverage|Supp)"),
      decimal = 0
    ) %>% 
    fmt_number(
      columns = matches("results|Q1|Q2|delta"),
      rows = indicator %in% c("TX_CURR Adjusted", "MMD 3+", "MMD 6+"),
      decimal = 0
    ) %>% 
    fmt_missing(
      columns = everything(),
      missing_text = "-"
    ) %>% 
    tab_style(style = list(cell_fill(color = old_rose_light, alpha = 0.25)),
              locations = cells_body(
                columns = c(delta),
                rows = delta <= -0.005)
    ) %>% 
    tab_style(style = list(cell_fill(color = genoa_light, alpha = 0.25)),
              locations = cells_body(
                columns = c(delta),
                rows = delta >= 0.005)
    ) %>% 
    cols_hide(
      columns = c(FY20APR, FY20targets, FY21APR, FY21targets)
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
    tab_spanner(
      label = md("**FY20**"),
      columns = contains("FY20")
    ) %>% 
    tab_spanner(
      label = md("**FY21**"),
      columns = matches("FY21|delta")
    ) %>% 
    cols_label(
      indicator = "",
      FY21Q1 = "Q1",
      FY21Q2 = "Q2"
    ) %>% 
    opt_align_table_header(align = c("center")) %>% 
    tab_options(
      footnotes.font.size = 10,
      source_notes.font.size = 10
    ) %>% 
    tab_header(
      title = glue::glue("GLOBAL MULTI-MONTH DISPENSING AND VIRAL LOAD SUMMARY")
    ) %>% 
    tab_footnote(
      footnote =  "South Africa has no national MMD program and has been excluded from MMD coverage rates.",
      location = cells_row_groups()
    ) %>% 
    tab_source_note(
      source_note = "Viral Load Covererage = TX_PVLS_N / TX_CURR_2_period_lag"
    ) %>% 
    tab_source_note(
      source_note = paste("Produced on ",Sys.Date(), "by SI Core Analytics Cluster using OU_IM_FY19-21_20210514i MSD")
    )  %>% 
    gtsave("Images/Global/GLOBAL_FY21Q2_MMD_VL_MD.png")
