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
  ou_im_vlc <- 
    si_path() %>% 
    return_latest("OU_IM_FY19-21_20210514") %>% 
    read_msd() %>% 
    filter(fiscal_year %in% c(2019, 2020, 2021))
           

# MUNGE ============================================================================
  
  #  Get the TX_CURR data needed for calculations
  #  Excluding South Africa
  tx_curr_mmd <-
    ou_im_vlc %>% 
    filter(indicator %in% c("TX_CURR"),
           standardizeddisaggregate %in% c("Total Numerator"),
           fundingagency != "Dedup", 
           operatingunit != "South Africa") %>% 
           # operatingunit == {{ou}}) %>% 
    clean_agency() %>% 
    mutate(agency = fct_lump(fundingagency, n = 2, other_level = "OTHER"),
           agency = fct_relevel(agency, agency_order_long)) %>% 
    group_by(fiscal_year, agency, indicator) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(indicator = "TX_CURR_MMD")
  
  tx_curr_vls <-
    ou_im_vlc %>% 
    filter(indicator %in% c("TX_CURR"),
           standardizeddisaggregate %in% c("Total Numerator"),
           fundingagency != "Dedup") %>% 
    # operatingunit == {{ou}}) %>% 
    clean_agency() %>% 
    mutate(agency = fct_lump(fundingagency, n = 2, other_level = "OTHER"),
           agency = fct_relevel(agency, agency_order_long)) %>% 
    group_by(fiscal_year, agency, indicator) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>% 
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
  tx_mmds <- 
    mmd_vlc %>% 
    mutate(tx_mmd_group = if_else(indicator %in% c("TX_MMD3+", "TX_MMD6+"), 1, 0)) %>% 
    filter(tx_mmd_group == 1) %>% 
    group_by(fiscal_year, agency) %>% 
    summarise(across(matches("tar|cumu|qtr"), sum, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(indicator = "TX_MMD3+", .after = agency)
  
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
    select(agency, indicator, FY20results = FY20Q4, FY21results = FY21Q2) %>% 
    mutate(FY20APR = NA_real_,
           FY20targets = NA_integer_,
           FY21APR = NA_real_,
           FY21targets = NA_integer_, .after = FY21results) %>% 
    relocate(FY20APR, .after = indicator) %>% 
    relocate(FY20targets, .after = FY20results) %>% 
    relocate(FY21results, .after = FY21APR) %>% 
    filter(!indicator %in% c("TX_PVLS", "TX_PVLS_D", "TX_CURR_VLC")) %>% 
    mutate(indicator = case_when(
      indicator == "TX_CURR_MMD" ~ "TX_CURR Adjusted",
      indicator == "TX_MMD3_SHARE" ~ "MMD 3+ Share",
      indicator == "TX_MMD3+" ~ "MMD 3+",
      indicator == "TX_MMD6+" ~ "MMD 6+",
      indicator == "VLC" ~ "Viral Load Coverage",
      indicator == "VLS" ~ "Viral Load Suppression",
    )) %>% 
    mutate(indicator = fct_relevel(indicator,
                                   "TX_CURR Adjusted",
                                   "MMD 3+ Share",
                                   "MMD 3+",
                                   "MMD 6+",
                                   "Viral Load Coverage",
                                   "Viral Load Suppression"),
           agency = fct_relevel(agency,
                               agency_order_long)) %>% 
    arrange(agency, indicator)
    
    

  
  mmd_vlc_tbl %>% 
    gt(groupname_col = "agency") %>% 
    fmt_percent(
      columns = matches("results"),
      rows = str_detect(indicator, "(Share|Coverage|Suppression)"),
      decimal = 0
    ) %>% 
    fmt_number(
      columns = matches("results"),
      rows = indicator %in% c("TX_CURR Adjusted", "MMD 3+", "MMD 6+"),
      decimal = 0
    ) %>% 
    fmt_missing(
      columns = everything(),
      missing_text = "-"
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
      label = md("**FY21 Q2**"),
      columns = contains("FY21")
    ) %>% 
    cols_label(
        indicator = "",
        FY20results = "Results",
        FY21results = "Results"
    ) %>% 
  opt_align_table_header(align = c("center")) %>% 
    tab_footnote(
      footnote = "South Africa has no national MMD program and has been excluded from MMD coverage rates",
      locations = cells_row_groups()
    ) %>% 
    tab_options(
    footnotes.font.size = 12
    ) 
  
  
  
  
  
  
  
  

  
  
  
  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

