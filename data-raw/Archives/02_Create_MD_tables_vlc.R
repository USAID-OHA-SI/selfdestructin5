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
    

# LOAD VARIABLES FULL NAMES -----------------------------------------------

  # Indicator definitions full
  indic_def_tx <- 
    tibble::tribble(
      ~indic_category,    ~indicator,      ~indicator_plain,
      "treatment",        "TX_CURR",       "Currently receiving antiretroviral therapy (ART)",
      "treatment",        "TX_MMD3_SHARE", "Share of all ART dispensed as multi-month therapy",
      "treatment",        "TX_MMD3+",      "Three months or more of ART treatment dispensed",
      "treatment",        "TX_MMD6+",      "Six months or more of ART treatment dispensed",
      "treatment",        "VLC",           "Percent of antiretroviral patients with a viral load result documented in past 12 months",
      "treatment",        "VLS",           "Percent of antiretroviral patients with a suppressed viral load result documented in past 12 months"
    ) 

  # Run 01_Create_MD_tables.R before moving one
    # source("Scripts/add_achv_colors_tbl.R")
    # source("Scripts/MD_tables_reboot_funs.R")

# LOAD DATA ============================================================================  

  # Builds on ou_im data loaded in MD_tables_reboot, but need 2 month lag of tx_curr
  # South Africa has no national MMD program and has been excluded from MMD coverage rates
  # Keep 2019 values in to calculate running history of VLC and VLS
  
    ou_im_vlc <- 
      si_path() %>% 
      return_latest("OU_IM_FY19-21_20210618_v2_1") %>% 
      read_msd() %>% 
      filter(fiscal_year %in% c(2019, 2020, 2021),
             !mech_code %in% omit_mechs) 


# HELPER FUNCTIONS --------------------------------------------------------
  
  # This has to change to TX_CURR for VLS/MMD TABLES
    clean_and_aggregate_tx <- function(df){
      suppressWarnings(
        df %>% 
        filter(indicator %in% c("TX_CURR"),
               standardizeddisaggregate %in% c("Total Numerator"),
               funding_agency != "Dedup") %>% 
        clean_agency() %>% 
        mutate(agency = ifelse(funding_agency == "USAID", "USAID", "ALL OTHER AGENCIES"),
        group_by(fiscal_year, agency, indicator) %>% 
        summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop")
      )
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
        suppressWarnings(
          df %>%
        filter(indicator %in% c("TX_CURR", "TX_PVLS"),
               disaggregate %in%  c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator", "Total Denominator") |
                 otherdisaggregate %in% c("ARV Dispensing Quantity - 6 or more months", "ARV Dispensing Quantity - 3 to 5 months"),
               funding_agency != "Dedup",
               {{country_col}} %in% ou) %>%
        clean_agency() %>%
        mutate(agency = ifelse(funding_agency == "USAID", "USAID", "ALL OTHER AGENCIES"),
               # Lump factors at 3 then apply long agency order b/c of varying nature
               # mutate(agency = fct_lump(funding_agency, n = 2, other_level = "ALL OTHER AGENCIES"),
               agency = fct_relevel(agency, agency_order_shrt), 
               indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
               indicator = case_when(
                 str_detect(otherdisaggregate, "3 to 5") ~ "TX_MMD3+",
                 str_detect(otherdisaggregate, "6 or more") ~ "TX_MMD6+",
                 TRUE ~ indicator)
        ) %>%
        filter(indicator != "TX_CURR") %>%
        group_by(fiscal_year, agency, indicator) %>%
        summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop") %>%
        ungroup() %>%
        arrange(agency, fiscal_year, indicator)
        )
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
      suppressWarnings(
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
      mutate(indicator = fct_relevel(indicator,
                                     "TX_CURR",
                                     "TX_MMD3_SHARE",
                                     "TX_MMD3+",
                                     "TX_MMD6+",
                                     "VLC",
                                     "VLS"),
             agency = fct_relevel(agency,
                                  agency_order_shrt)) %>% 
      arrange(agency, indicator)
      )
    
    # Bring in indicator names
    mmd_vlc_tbl <- 
      mmd_vlc_tbl %>% 
      left_join(indic_def_tx) %>% 
      select(-indic_category) %>% 
    mutate(indicator2 = ifelse(agency == "USAID", paste(indicator, indicator_plain), paste(indicator))) %>% 
    select(-indicator_plain) %>% 
      relocate(indicator2, .after = agency)
      
    
  return(mmd_vlc_tbl)
  }
  
    md_vlc_df <- shape_vlc_tbl(mmd_vlc, tx_mmds, tx_curr_base)
  
    
  

# PRODUCE VLS and VLC TABLES ----------------------------------------------
  
    make_md_vlc_tbl <- function(df, ou) {
  
      cntry <-  str_to_upper(ou)
      team <- "Core Analytics Cluster"
      
      df %>% 
        gt(groupname_col = "agency") %>% 
        fmt_number(
          columns = matches("results|Q1|Q2|delta"),
          rows = str_detect(indicator2, "(TX_CURR|TX_MMD3+|TX_MMD6)"),
          decimal = 0
        ) %>% 
        fmt_percent(
          columns = matches("results|Q1|Q2|delta"),
          rows = str_detect(indicator2, "(TX_MMD3_SHARE|VLC|VLS)"),
          decimal = 0
        ) %>% 

        fmt_missing(
          columns = everything(),
          missing_text = "-"
        ) %>% 
        tab_style(style = list(cell_fill(color = old_rose_light, alpha = 0.25)),
                  locations = cells_body(
                    columns = vars(delta),
                    rows = delta <= -0.005)
        ) %>% 
        tab_style(style = list(cell_fill(color = genoa_light, alpha = 0.25)),
                  locations = cells_body(
                    columns = vars(delta),
                    rows = delta >= 0.005)
        ) %>% 
        cols_hide(
          columns = vars(FY20APR, FY20targets, FY21APR, FY21targets)
        ) %>% 
        cols_align(
          align = c("left"),
          columns = vars(indicator)
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
          indicator2 = "",
          delta = "DELTA",
          FY21Q1 = "Q1",
          FY21Q2 = "Q2",
          FY20results = "RESULTS"
        ) %>% 
        text_transform(
          locations = cells_body(
            columns = vars(indicator2),
            rows = (agency == "USAID")
          ),
          fn = function(x){
            name <- word(x, 1)
            name2 <- word(x, 2, -1)
            glue::glue(
              "<div style='line-height:10px'<span style='font-weight:regular;font-variant:small-caps;font-size:13px'>{name}</div>
        <div><span style='font-weight:regular;font-size:11px'>{name2}</br></div>"
            )
          }
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
          source_note = paste("Produced on ",Sys.Date(), "by the ", team, " using PEPFAR FY21Q2c MSD released on 2021-06-18.")
        ) %>% 
        cols_hide(vars(indicator)) %>% 
        tab_style(
          style = list(
            cell_borders(
              sides = "right",
              color = "white",
              weight = px(10)
            )
          ),
          locations = list(
            cells_body(
              columns = vars(FY20results)
            )
          )
        ) %>% 
        cols_width(
          vars(indicator2) ~ px(500),
        ) %>% 
        tab_options(data_row.padding = px(7)) %>% 
        tab_style(
          style = list("font-variant: small-caps;"),
          locations = cells_column_labels(columns = everything()
          )
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
    get_md_vls_table(ou_im_vlc, country_name, "Malawi")

  # Get just the dataframes for each place and write to a csv
    write_md_vls_df <- function(df, country_col, ou) {
      
      tx_curr_base <- get_tx_curr_base(df, {{country_col}}, ou)
      mmd_vlc <- get_mmd_vlc_base(df, {{country_col}}, ou)
      
      tx_mmds <- get_tx_mmds(mmd_vlc)
      md_vlc_df <- shape_vlc_tbl(mmd_vlc, tx_mmds, tx_curr_base)

      md_vlc_df %>% 
        write_csv(file.path("Dataout/MMD_VLC", paste0({{ou}}, "_FY21Q2_MMD_VL_MD_RAW.csv")))
    }  
    
# BATCH VLC/MMD TABLES ----------------------------------------------------

  # Distinct list of OUS to loop over
  ou_list <- ou_im_vlc %>% 
    distinct(operatingunit) %>% 
    pull()
  
  map(ou_list, ~get_md_vls_table(ou_im_vlc, operatingunit, .x) %>% 
        gtsave(file.path("Images/OU", paste0(.x, "_FY21Q2_MMD_VL_MD.png")))) 
  
  map(ou_list, ~write_md_vls_df(ou_im_vlc, operatingunit, .x))
  
  
  # Distinct list of Countries in Regional OUS
  
  # TODO - Create a function that flags countries with TX_MMD (many do not report it)
  # Asia
  asia_cntry_list <- 
    ou_im_vlc %>% 
    filter(str_detect(operatingunit, "Asia Region")) %>% 
    filter(!country_name %in% c("China", "Philippines")) %>% 
    distinct(country_name) %>% 
    pull()
  
  map(asia_cntry_list, ~get_md_vls_table(ou_im_vlc, country_name, .x) %>% 
        gtsave(file.path("Images/Regional/Asia", paste0(.x, "_FY21Q2_MMD_VL_MD.png"))))
  
  map(asia_cntry_list, ~write_md_vls_df(ou_im_vlc, country_name, .x))
  
  # West Africa
  westafr_cntry_list <- 
    ou_im_vlc %>% 
    filter(str_detect(operatingunit, "Africa Region")) %>% 
    filter(!country_name %in% c("Sierra Leone")) %>% 
    distinct(country_name) %>% 
    pull()
  
  map(westafr_cntry_list, ~get_md_vls_table(ou_im_vlc, country_name, .x) %>% 
        gtsave(file.path("Images/Regional/WAR", paste0(.x, "_FY21Q2_MMD_VL_MD.png"))))
  
  map(westafr_cntry_list, ~write_md_vls_df(ou_im_vlc, country_name, .x))
  
  # Western Hemisphere
  # Omitting Guyana and Barbados due to no reporting in FY21
  wh_cntry_list <- 
    ou_im_vlc %>% 
    filter(str_detect(operatingunit, "Western")) %>% 
    filter(!country_name %in% c("Guyana", "Barbados", "Nicaragua", "Suriname")) %>% 
    distinct(country_name) %>% 
    pull()
  
  map(wh_cntry_list, ~get_md_vls_table(ou_im_vlc, country_name, .x) %>% 
        gtsave(file.path("Images/Regional/WesternHemi", paste0(.x, "_FY21Q2_MMD_VL_MD.png"))))

  map(wh_cntry_list, ~write_md_vls_df(ou_im_vlc, country_name, .x))
  

#  GENERATE GLOBAL TABLE -- SOUTH AFRICA FLAG -----------------------------

  # Need separate labels for SA exclusion
  indic_def_tx_sans_sa <- 
    tibble::tribble(
      ~indic_category,    ~indicator,         ~indicator_plain,
      "treatment",        "TX_CURR",         "Currently receiving antiretroviral therapy (ART) excluding South Africa",
      "treatment",        "TX_MMD3_SHARE",   "Share of all ART dispensed as multi-month therapy",
      "treatment",        "TX_MMD3+",        "Three months or more of ART treatment dispensed",
      "treatment",        "TX_MMD6+",        "Six months or more of ART treatment dispensed",
      "treatment",        "VLC",             "Percent of antiretroviral patients with a viral load result documented in past 12 months",
      "treatment",        "VLS",             "Percent of antiretroviral patients with a suppressed viral load result documented in past 12 months"
    ) 

  
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
             funding_agency != "Dedup") %>%
      clean_agency() %>%
      mutate(agency = ifelse(funding_agency == "USAID", "USAID", "ALL OTHER AGENCIES"),
             agency = fct_relevel(agency, agency_order_shrt), 
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
  

  # Combine everything and prepare indicator labels for table  
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
                   values_to = "results")%>% 
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
      filter(!indicator %in% c("TX_PVLS", "TX_PVLS_D", "TX_CURR_VLC"))%>% 
      mutate(indicator = case_when(
         indicator == "TX_CURR_MMD" ~ "TX_CURR",
         TRUE ~ indicator)
         ) %>% 
      mutate(indicator = fct_relevel(indicator,
                                     "TX_CURR",
                                     "TX_MMD3_SHARE",
                                     "TX_MMD3+",
                                     "TX_MMD6+",
                                     "VLC",
                                     "VLS"),
             agency = fct_relevel(agency, agency_order_shrt)) %>% 
      arrange(agency, indicator) %>% 
      left_join(indic_def_tx_sans_sa) %>% 
      select(-indic_category) %>% 
      mutate(indicator2 = ifelse(agency == "USAID", paste(indicator, indicator_plain), paste(indicator))) %>% 
      select(-indicator_plain) %>% 
      relocate(indicator2, .after = agency)
  
  

# GLOBAL TX / MMD TABLE ---------------------------------------------------

    
  # Produce Table for Global
    mmd_vlc_tbl %>% 
      gt(groupname_col = "agency") %>% 
        cols_hide(vars(indicator)) %>% 
        fmt_number(
          columns = matches("results|Q1|Q2|delta"),
          rows = str_detect(indicator2, "(TX_CURR|TX_MMD3+|TX_MMD6)"),
          decimal = 0
        ) %>% 
        fmt_percent(
          columns = matches("results|Q1|Q2|delta"),
          rows = str_detect(indicator2, "(TX_MMD3_SHARE|VLC|VLS)"),
          decimal = 0
        ) %>% 
        fmt_missing(
          columns = everything(),
          missing_text = "-"
        ) %>% 
        tab_style(style = list(cell_fill(color = old_rose_light, alpha = 0.25)),
                  locations = cells_body(
                    columns = vars(delta),
                    rows = delta <= -0.005)
        ) %>% 
        tab_style(style = list(cell_fill(color = genoa_light, alpha = 0.25)),
                  locations = cells_body(
                    columns = vars(delta),
                    rows = delta >= 0.005)
        ) %>% 
        cols_hide(
          columns = vars(FY20APR, FY20targets, FY21APR, FY21targets)
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
          indicator2 = "",
          FY20results = "RESULTS",
          FY21Q1 = "Q1",
          FY21Q2 = "Q2",
          delta = "DELTA"
        ) %>% 
        opt_all_caps(
          all_caps = TRUE,
          locations = c("row_group")
        ) %>%
        text_transform(
          locations = cells_body(
            columns = vars(indicator2),
            rows = (agency == "USAID")
          ),
          fn = function(x){
            name <- word(x, 1)
            name2 <- word(x, 2, -1)
            glue::glue(
              "<div style='line-height:10px'<span style='font-weight:regular;font-variant:small-caps;font-size:13px'>{name}</div>
               <div><span style='font-weight:regular;font-size:11px'>{name2}</br></div>"
            )
          }
        ) %>% 
        cols_hide(vars(indicator)) %>% 
        tab_style(
          style = list(
            cell_borders(
              sides = "right",
              color = "white",
              weight = px(10)
            )
          ),
          locations = list(
            cells_body(
              columns = vars(FY20results)
            )
          )
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
          source_note = paste("Produced on ",Sys.Date(), "by the Core Analytics Cluster using PEPFAR FY21Q2c MSD released on 2021-06-18.")
        ) %>% 
        tab_source_note(
          source_note = md("Viral Load Covererage = TX_PVLS_N / TX_CURR_2_period_lag. *ALL OTHER AGENCIES* based on aggregates excluding de-duplication.")
          )  %>% 
        cols_width(
          vars(indicator2) ~ px(500),
        ) %>% 
        tab_options(data_row.padding = px(7)) %>% 
        tab_style(
          style = list("font-variant: small-caps;"),
          locations = cells_column_labels(columns = everything()
          )
        ) %>% 
      gtsave("Images/Global/GLOBAL_FY21Q2_MMD_VL_MD.png")
    
    mmd_vlc_tbl %>% write_csv("Dataout/MMD_VLC/GLOBAL_FY21Q2_MMD_VL_MD_RAW.csv")


