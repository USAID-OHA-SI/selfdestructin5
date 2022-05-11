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
    library(gt) #Version 0.2.2 used
    library(fontawesome)
    library(googledrive)
    
  # In case a rollback is required; v.0.3.3 seems to have breaking changes
  #devtools::install_version("gt", version = "0.2.2", repos = "http://cran.us.r-project.org")
    
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
      
    merdata <- si_path(type = "path_msd")
    
  # Create a new folder to house regional country tables
    dir_list <- c("Global", "OU", "Regional")
    map(dir_list, ~dir.create(file.path("Images/", .x)))
    
    folder_list <- c("Asia", "WAR", "WesternHemi")
    map(folder_list, ~dir.create(file.path("Images/Regional/", .x)))
    
  # What quarter are we in?
  # TODO: INCORPORATE THIS INTO FLOW
    qtr <- "2"
    
  # Key indicators for the base tables
    indics <- c("PrEP_NEW", "OVC_SERV", "VMMC_CIRC", 
                "HTS_TST", "HTS_TST_POS",
                "TX_NEW", "TX_CURR")
    
    cumulative_indic <- c("PrEP_NEW", "VMMC_CIRC", 
                          "HTS_TST", "HTS_TST_POS")
    
  # Mechs that need to be filtered for whatever reason
    omit_mechs <- c("16772", "84562", "84566")
    
  # Agency order throughout
  # Use the long order b/c of the varying nature of coverage by diff agencies
    agency_order_shrt <- c("USAID", "ALL OTHER AGENCIES")
    agency_order_long <- c("USAID", "CDC", "OTHER", "DOD", "HRSA", "PRM", "AF", "PC")
  
  # call required functions
    source("Scripts/add_achv_colors_tbl.R")
    source("Scripts/MD_tables_reboot_funs.R")
    
    
  # Indicator Definitions -- THESE MAY CHANGE DEPENENT ON INDICS above
    indic_def <- 
      tibble::tribble(
        ~indic_category,    ~indicator,        ~indicator_plain,
        "prevention",       "PrEP_NEW",       "Newly enrolled on antiretroviral pre-exposure prophylaxis",
        "prevention",       "OVC_SERV",       "Beneficiaries of OVC programs for children/families affected by HIV",
        "prevention",       "VMMC_CIRC",      "Voluntary medical male circumcision for HIV prevention",
        "testing",          "HTS_TST",        "Received HIV testing service and results",
        "testing",          "HTS_TST_POS",    "Received HIV testing service and positive results",
        "treatment",        "TX_NEW",         "Newly enrolled on antiretroviral therapy",
        "treatment",        "TX_CURR",        "Currently receiving antiretroviral therapy"
      )
    

    
    
# LOAD DATA ============================================================================  

    ou_im <- 
      si_path() %>% 
      return_latest("OU_IM_FY19-21_20210618_v2_1") %>% 
      read_msd() %>% 
      filter(fiscal_year %in% c(2020, 2021), 
             !mech_code %in% omit_mechs)
    

# HELPER FUNCTIONS --------------------------------------------------------

  # KEEP ONLY USAID AND ALL OTHER AGENCIES
  # Helper to do a bit of repetitive munging
    clean_and_aggregate <- function(df){
      suppressWarnings(df %>% 
        filter(indicator %in% indics,
               standardizeddisaggregate %in% c("Total Numerator"),
               funding_agency != "Dedup") %>% 
        clean_agency() %>% 
        mutate(agency = ifelse(funding_agency == "USAID", "USAID", "ALL OTHER AGENCIES"),
        # Lump factors at 3 then apply long agency order b/c of varying nature
        # mutate(agency = fct_lump(funding_agency, n = 2, other_level = "ALL OTHER AGENCIES"),
               agency = fct_relevel(agency, agency_order_shrt)) %>% 
        group_by(fiscal_year, agency, indicator) %>% 
        summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop")
      )
    }           
    
    

# SHAPE BASE TABLE --------------------------------------------------------
    # Shape the base dataframe from which the table is derived
    #@description shape the msd to wide with key indicators  
    #@param df - base msd from which all manipulations are done
    #@param country_col either country_name or operating unit, depending on table desired
    #@param ou country_name or operating unit
    
    shape_md_tbl <- function(df, country_col, ou) {
      
      # Filter the data down to key indicators defined in indics object
      # Collapsing down to the agency level
      ou_tbl <- 
        df %>% 
        filter({{country_col}} %in% ou) %>% 
        clean_and_aggregate()

    
    # Clean up and add up down flags, these will be used in version 1.0   
      md_tbl <- 
        suppressWarnings(
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
        )
    
    # Old table layout
      md_tbl_old <- 
        suppressWarnings(
          md_tbl %>% 
        filter(period %in% c("FY20Q4", "FY21Q2")) %>% 
        select(period, agency, indicator, targets, results = cumulative, APR) %>% 
        mutate(period = str_sub(period, 1, 4)) %>%
        pivot_wider(names_from = period, 
                    names_glue = "{period}{.value}",
                    values_from = c(targets, results, APR),
                    names_sort = TRUE) %>% 
        left_join(., indic_def) %>% 
        ungroup() %>% 
        
          mutate(indicator2 = ifelse(agency == "USAID", paste(indicator, indicator_plain), paste(indicator)),
               indicator = fct_relevel(indicator, indics))) %>% 
        arrange(agency, indicator) 
    
      md_tbl_old <- 
        md_tbl_old %>% 
        relocate(indicator2, .before = indicator) %>% 
        select(-indic_category, -indicator_plain) %>% 
        select(agency, indicator2, indicator, sort(tidyselect::peek_vars()))
      
    return(md_tbl_old)
    }
    
  # Test function above
    tst <-  shape_md_tbl(df = ou_im, country_col = operatingunit, ou = "Cameroon") %>% prinf()


# PRETTIFY COLUMN NAMES ---------------------------------------------------

  #@description fix_col_names applies a clean formatting to column names
  #@param md_tbl_old old formatted version of md tables
  # Fix column names
    fix_col_names <-function(md_tbl_old) {  
      
      tbl_col_names <- 
        head(md_tbl_old, 1) %>% 
        ungroup() %>% 
      mutate_all(as.character) %>% 
      pivot_longer(everything(), names_to = "column", values_to = "value") %>% 
      select(-value) %>% 
      mutate(label = ifelse(str_detect(column, "FY"), str_sub(column, 5, -1), ""),
             label = ifelse(label == "APR", "achievement", label)) %>% 
      deframe()
      
      return(tbl_col_names)
    }
    
    fix_col_names(shape_md_tbl(ou_im, operatingunit, "Zambia"))
    
    
# BASE TABLE GENERATION ---------------------------------------------------
    
    
  # Customize GT table to reproduce results
    md_tbl <- function(md_tbl_old, tbl_col_names, ou) {
      
    cntry <-  str_to_upper(ou)
    team <- "Core Analytics Cluster"
    
    md_tbl_old %>% 
      gt(groupname_col = "agency") %>% 
      cols_hide(columns = "indicator") %>% 
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
      text_transform( 
        locations = cells_body(
          columns = c("indicator2"),
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
        title = glue::glue("{cntry} PERFORMANCE SUMMARY")
      ) %>%
      opt_align_table_header(align = c("center")) %>% 
      add_achv_colors() %>% 
      tab_source_note(
        source_note = paste("Produced on ",Sys.Date(), "by the ", team, " using PEPFAR FY21Q2c MSD released on 2021-06-18.")
      ) %>% 
      tab_source_note(
        source_note = md("*ALL OTHER AGENCIES* based on aggregates excluding de-duplication.")
      ) %>% 
      tab_options(
        source_notes.font.size = 10,
        table.font.size = 12
      ) %>% 
      # cols_width(
      #   indicator2 ~ px(340),
      # ) %>% 
      tab_options(data_row.padding = px(5))
  }
    
    

# TEST TABLE GENERATION BY OU OR COUNTRY ----------------------------------

  # Test it all together
    md_tbl_old <- shape_md_tbl(df = ou_im, country_col = operatingunit, ou = "Cameroon")
    tbl_col_names <- fix_col_names(md_tbl_old)
    md_tbl(md_tbl_old, tbl_col_names, "Cameroon")
    
    
  # Wrapper around everything to pull it all together  
    get_md_table <- function(df, country_col, ou) {
      
      message(paste('Creating base MD table for', ou))
      
      # Reproduce MD table data frame
      md_tbl_old <- shape_md_tbl(df, {{country_col}}, ou)
      
      # Column labels
      tbl_col_names <- fix_col_names(md_tbl_old)
      
      # Generate the table
      md_ou_tbl <- md_tbl(md_tbl_old, tbl_col_names, ou) 
      return(md_ou_tbl)
    }  

  # Test for a single OU  
    get_md_table(ou_im, country_col = country_name, "Cameroon")
    get_md_table(ou_im, country_col = operatingunit, "Kenya")

# BATCH GENERATE TABLES ------------------------------------------------
  
    # Generating for the following folders
  # Global - TOTAL PEPFAR
  # OU - Operating Unit level
  # Regional - SNU1 Equivalent but for Regional Programs
  
  
  # Distinct list of OUS to loop over
    ou_list <- ou_im %>% 
      distinct(operatingunit) %>% 
      pull()
  
  #Write locally  
    map(ou_list, ~get_md_table(ou_im, operatingunit, .x) %>% 
          gtsave(file.path("Images/OU", paste0(.x, "_FY21Q2_KEY_INDICATORS_MD.png"))))
    
    # Write raw data to csvs
    map(ou_list, ~shape_md_tbl(ou_im, operatingunit, .x) %>% 
          write_csv(file.path("Dataout/", paste0(.x, "_FY21Q2_KEY_INDICATORS_MD_RAW.csv"))))
    
    
# Distinct list of Countries in Regional OUS
  # Asia
    asia_cntry_list <- 
      ou_im %>% 
      filter(str_detect(operatingunit, "Asia Region")) %>% 
      distinct(country_name) %>% 
      pull()
    
    map(asia_cntry_list, ~get_md_table(ou_im, country_name, .x) %>% 
          gtsave(file.path("Images/Regional/Asia", paste0(.x, "_FY21Q2_KEY_INDICATORS_MD.png"))))
    

    map(asia_cntry_list, ~shape_md_tbl(ou_im, country_name, .x) %>% 
          write_csv(file.path("Dataout/", paste0(.x, "_FY21Q2_KEY_INDICATORS_MD_RAW.csv"))))

  # West Africa
    westafr_cntry_list <- 
      ou_im %>% 
      filter(str_detect(operatingunit, "Africa Region")) %>% 
      distinct(country_name) %>% 
      pull()
    
    map(westafr_cntry_list, ~get_md_table(ou_im, country_name, .x) %>% 
          gtsave(file.path("Images/Regional/WAR", paste0(.x, "_FY21Q2_KEY_INDICATORS_MD.png"))))
    
    map(westafr_cntry_list, ~shape_md_tbl(ou_im, country_name, .x) %>% 
          write_csv(file.path("Dataout/", paste0(.x, "_FY21Q2_KEY_INDICATORS_MD_RAW.csv"))))
  
  # Western Hemisphere
  # Omitting Guyana and Barbados due to no reporting in FY21
    wh_cntry_list <- 
      ou_im %>% 
      filter(str_detect(operatingunit, "Western")) %>% 
      filter(!country_name %in% c("Guyana", "Barbados")) %>% 
      distinct(country_name) %>% 
      pull()
    
    map(wh_cntry_list, ~get_md_table(ou_im, country_name, .x) %>% 
          gtsave(file.path("Images/Regional/WesternHemi", paste0(.x, "_FY21Q2_KEY_INDICATORS_MD.png"))))
    
    map(wh_cntry_list, ~shape_md_tbl(ou_im, country_name, .x) %>% 
          write_csv(file.path("Dataout/", paste0(.x, "_FY21Q2_KEY_INDICATORS_MD_RAW.csv"))))
  
  
  # Generate global numbers
  # Change all operating units to be "Global" to generate
    return_global_tbl <- function() {
      all <- shape_md_tbl(ou_im %>% mutate(operatingunit = "Global"), operatingunit, "Global")
      tbl_col_names <- fix_col_names(all)
      ou_tbl <- md_tbl(all, tbl_col_names, "PEPFAR GLOBAL") %>% 
        
      return(ou_tbl)
    }
    
    return_global_tbl() %>% 
      tab_source_note(
        source_note = md(paste('Omitted mechanisms:', toString(omit_mechs)))
      ) %>% 
      gtsave("Images/Global/GLOBAL_FY21Q2_KEY_INDICATORS_MD.png")
    
    shape_md_tbl(ou_im %>% mutate(operatingunit = "Global"), operatingunit, "Global") %>% 
      write_csv(file.path("Dataout/", "GLOBAL_FY21Q2_KEY_INDICATORS_MD_RAW.csv"))
