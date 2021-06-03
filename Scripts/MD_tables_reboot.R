# PURPOSE: Munge and Analysis of FY21 Q2 MD tables
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-05-20
# NOTES: New take on the tables

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
    library(gt)
    library(fontawesome)
    
    
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
    rasdata <- glamr::si_path("path_raster")
    shpdata <- glamr::si_path("path_vector")
    datim   <- glamr::si_path("path_datim")  
      
    merdata <- si_path(type = "path_msd")
    
    # What quarter are we in?
    qtr <- "2"
    
    indics <- c("PrEP_NEW", "VMMC_CIRC", 
                "HTS_TST", "HTS_TST_POS",
                "TX_NEW", "TX_CURR")
    
    indics_mmd <- c("TX_CURR", "TX_PVLS")
    
    cumulative_indic <- c("PrEP_NEW", "VMMC_CIRC", 
                          "HTS_TST", "HTS_TST_POS")
    
    # Mechs that need to be filtered for whatever reason
    mech_list <- c("OVC_SERV")
    
    # Agency order throughout
    agency_order_shrt <- c("USAID", "CDC", "OTHER")
    agency_order_long <- c("USAID", "CDC", "OTHER", "DOD", "HRSA", "PRM", "AF", "PC")
  
    # call required functions
    source("../add_achv_colors_tbl.R")
    source("../MD_tables_reboot_funs.R")

# LOAD DATA ============================================================================  

    ou_im <- 
      si_path() %>% 
      return_latest("OU_IM_FY19-21_20210514") %>% 
      read_msd() %>% 
      filter(fiscal_year %in% c(2020, 2021))
    

# MUNGE ============================================================================
  
  # Base Table  
  # agency cleaned with core indicators
  # Loop over operating units and create tables
    
    get_ou_tbl <- function(df, ou) {
      
      ou_tbl <- 
        df %>% 
        filter(indicator %in% indics,
               standardizeddisaggregate %in% c("Total Numerator"),
               fundingagency != "Dedup",
               operatingunit == {{ou}}) %>% 
        clean_agency() %>% 
        mutate(agency = fct_lump(fundingagency, n = 2, other_level = "OTHER"),
               agency = fct_relevel(agency, agency_order_long)) %>% 
        group_by(fiscal_year, agency, indicator) %>% 
        summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") 
    
    # Clean up and add up down flags, these will be used in version 1.0   
      md_tbl <- 
        ou_tbl %>% 
        reshape_msd("quarters", qtrs_keep_cumulative = TRUE) %>% 
        group_by(agency, indicator) %>% 
        mutate(value_run = row_number(),
               gap = targets - results_cumulative,
               gap_denom = (4 - (substr(period, 6, 6) %>% as.numeric)),
               gap_pace = gap_calc(gap, gap_denom),
               APR = denom_share(results_cumulative, targets)) %>% 
        ungroup() %>% 
        arrange(agency, indicator, period) %>% 
        group_by(agency, indicator) %>% 
        mutate(
          value_yr_lag = lag(results_cumulative, n = 4),
          q2q_comp = q2q_compare(results_cumulative, value_yr_lag),
          change_dir = if_else(q2q_comp > 0, "increase", "decrease")
        ) %>% 
        ungroup() %>% 
        mutate(indicator = fct_relevel(indicator, indics)) %>% 
        calc_achv(., APR, period) %>% 
        group_by(agency)
    
    # Old table layout
      agency_tbl_old <- 
        md_tbl %>% 
        filter(period %in% c("FY20Q4", "FY21Q2")) %>% 
        select(period, agency, indicator, targets, results = cumulative, APR) %>% 
        mutate(period = str_sub(period, 1, 4)) %>%
        pivot_wider(names_from = period, 
                    names_glue = "{period}{.value}",
                    values_from = c(targets, results, APR),
                    names_sort = TRUE) %>% 
        arrange(agency, indicator) %>% 
        select(agency, indicator, sort(tidyselect::peek_vars()))
    
    return(agency_tbl_old)
    }
    
    
    # Get the cleaned OU name to use in title and table header
    get_country_name <- function(ou){
      country <- ou_im %>% 
        filter(operatingunit == {{ou}}) %>% 
        clean_countries(colname = 'operatingunit') %>% 
        distinct(operatingunit) %>% 
        pull() %>% 
        str_to_upper()
    }

    # Fix column names
    fix_col_names <-function(ou_tbl) {  
      table_names <- 
        head(ou_tbl, 1) %>% 
        ungroup() %>% 
      mutate_all(as.character) %>% 
      pivot_longer(everything(), names_to = "column", values_to = "value") %>% 
      select(-value) %>% 
      mutate(label = ifelse(str_detect(column, "FY"), str_sub(column, 5, -1), ""),
             label = ifelse(label == "APR", "achievement", label)) %>% 
      deframe()
      
      return(table_names)
    }
    
    
# BASE TABLE GENERATION ---------------------------------------------------

    # Customize GT table to reproduce results
  ou_md_tbl <- function(agency_tbl_old, cntry, col_names) {
      
    agency_tbl_old %>% 
      gt(groupname_col = "agency") %>% 
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
      cols_label(.list = {{col_names}}) %>% 
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
        title = glue::glue("{cntry} PERFORMACE SUMMARY")
      ) %>%
      opt_align_table_header(align = c("center")) %>% 
      add_achv_colors()
  }
    
    
  # Wrapper around everything to pull it all together  
  get_md_table <- function(df, ou) {
    
    # Grab name for use in table
    cntry <- get_country_name(ou)
    message(paste('Creating base MD table for', cntry))
    
    # Reproduce MD table data frame
    tbl <- get_ou_tbl(df, ou)
    
    # Column labels
    col_names <- fix_col_names(tbl)
    
    # Generate the table
    ou_tbl <- ou_md_tbl(tbl, cntry, col_names) 
    return(ou_tbl)
    
  }  

  # Test for a single OU  
  get_md_table(ou_im, "Vietnam")

# BATCH GENERATE OU TABLES ------------------------------------------------

  # Distinct list of OUS to loop over
  ou_list <- ou_im %>% 
    distinct(operatingunit) %>% 
    pull()
  
  map(ou_list, ~get_md_table(ou_im, .x) %>% 
        gtsave(file.path(images, paste0(.x, "_FY21Q2_KEY_INDICATORS_MD.png"))))


  # Generate global numbers
  return_global_tbl <- function() {
    
    all <- get_ou_tbl(ou_im %>% mutate(operatingunit = "Global"), "Global")
    col_names <- fix_col_names(all)
    ou_tbl <- ou_md_tbl(all, "PEPFAR GLOBAL", col_names)
    return(ou_tbl)
  }
  
  return_global_tbl() %>% 
    gtsave("Images/GLOBAL_FY21Q2_KEY_INDICATORS_MD.png")
    
    
    

# MUNGE TX_MDD and VLS + VLC DATA INTO TABLE OF SIMILAR PROPORTIONS -------
  
  # TX_MMD3+ is actually TX_MMD_3-5 + TX_MMD_6+  
  
  glbl_base <- get_ou_tbl(ou_im %>% mutate(operatingunit = "Global"), "Global") %>% 
    filter(indicator == "TX_CURR")
  
  
  tx_curr <- 
  
  
  mmd_vlc <- 
    ou_im %>% 
    filter(indicator %in% c("TX_CURR", "TX_PVLS"),
           disaggregate %in%  c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator", "Total Denominator") | 
             otherdisaggregate %in% c("ARV Dispensing Quantity - 6 or more months", "ARV Dispensing Quantity - 3 to 5 months"),
           fundingagency != "Dedup") %>% 
    clean_agency() %>% 
    mutate(agency = fct_lump(fundingagency, n = 2), 
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
  tx_mmds <- agency_mmd_vlc %>% 
    mutate(tx_mmd_group = if_else(indicator %in% c("TX_MMD3+", "TX_MMD6+"), 1, 0)) %>% 
    filter(tx_mmd_group == 1) %>% 
    group_by(fiscal_year, agency) %>% 
    summarise(across(matches("tar|cumu|qtr"), sum, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(indicator = "TX_MMD3+", .after = agency)
    
  agency_tx_mmd <- 
    agency_mmd_vlc %>% 
    filter(!indicator %in% c("TX_MMD3+", "TX_PVLS", "TX_PVLS_D", "TX_CURR")) %>% 
    bind_rows(tx_mmds)   

    
    
    
    
    
    
    
    
    
 
    

    

    

  
      
      
    # Calculate Viral Load Coverage and Viral Load Suppression sepately then join in to main data frame
    viral_loads <- 
      agency_mmd_vlc %>% 
      filter(str_detect(indicator, "(TX_CURR|TX_PVLS)"))

      
      

  
  # Create APR values for TX_MMD and VLC and VLS using lags and ordering of data. First,
  #
  
  
  
  
  
  md_tbl %>% filter(indicator %in% c("TX_CURR", "TX_MMD3+", "TX_MMD6+"))
  
  # Calcula
    
  

  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

