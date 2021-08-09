# PURPOSE: Munge and Analysis of FY21 Q3 MD tables
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-08-04
# NOTES: More dynamic process for creating tables

# LOCALS & SETUP ============================================================================ 

# Libraries
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(tidyverse)
  library(scales)
  library(extrafont)
  library(gt) #Version 0.3.0
  library(fontawesome)
  library(googledrive)
  library(selfdestructin5)

# Set paths  
  data   <- "Data"
  dataout <- "Dataout"
  images  <- "Images"
  graphs  <- "Graphics"
  merdata <- glamr::si_path("path_msd")
  
# # Load secrets to access data bases needed
#   load_secrets()

  
  #2) Load OU_IM table
  ou_im <- 
    si_path() %>% 
    return_latest("OU_IM_FY19-21_20210618_v2_1") %>%
    # return_latest("OU_IM_FY18-21_20200918") %>% 
    read_msd() 
  
  #2) Objects needed 
  
  
  # TODO: Determine a better way to incorporate these into the functions
  # GT themes don't seem to like receiving anything other than gt-based parameters
  # Time items are used to make column headers dynamic
  pd <- 
  msd_source <- pd %>% msd_period(period = .)

  # Table notes
  make_footnotes(msd_source)
  
  # authors <- glue::glue("Created by Core Analytics Cluster on", Sys.Date(), "using", {msd_source})

  

# MAKE TABLES - DRAW THE OWL ----------------------------------------------

  #3) Create the long base table
  mdb_df <- make_mdb_df(ou_im, resolve_issues = F)
  
  #4) Reshape the main table into a gt ready format
  mdb_tbl <- reshape_mdb_df(mdb_df, pd)
  
  create_mdb_table <- function(df, ou){
  
  cntry <- stringr::str_to_upper(ou)
  
  df %>% 
    filter(operatingunit %in% c({{ou}})) %>% 
    gt(groupname_col = "agency") %>% 
    mdb_main_theme(pd) %>% 
    tab_header(
      title = glue::glue("{cntry} PERFORMANCE SUMMARY")
    ) 
  }

  
  create_mdb_table(mdb_tbl, c("Zambia"))
  
  # How well does this map over ous
  ous <- unique(mdb_tbl$operatingunit)[15:25]
  purrr::map(ous, ~create_mdb_table(mdb_tbl, ou =.x))
  
  
  
  
  
# MAKE VLS/VLC Tables -----------------------------------------------------

  #3a) Create the long base table for treatment indicators
  mdb_df_tx <- make_mdb_tx_df(ou_im, resolve_issues = F)
  
  #4a) Reshape treatment table into gt ready table
  mdb_tbl_tx <- reshape_mdb_tx_df(mdb_df_tx, pd)
  
  # Generic function to call
  # Generic function to call an OU/Country/Agency
  create_mdb_tx_table <- function(df, ou){
    
    # Define numeric cols for formatting in table
    # numeric_cols <- df %>% select_if(is.numeric) %>% names()
    cntry <- str_to_upper(ou)
    numeric_cols <- extract_num_colnames(df)
    
    df %>% 
      filter(operatingunit %in% c({{ou}})) %>% 
      gt(groupname_col = "agency") %>% 
      mdb_treatment_theme(numeric_cols, pd) %>% 
      tab_header(
        title = glue::glue("{cntry} PERFORMANCE SUMMARY")
      ) 
  }
  

  create_mdb_tx_table(mdb_tbl_tx, c("Malawi"))
  
  ous <- unique(mdb_tbl_tx$operatingunit)[15:35]
  purrr::map(ous, ~create_mdb_tx_table(mdb_tbl_tx, ou =.x))
  

  
  
  
