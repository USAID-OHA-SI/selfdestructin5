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

# Set paths  
  data   <- "Data"
  dataout <- "Dataout"
  images  <- "Images"
  graphs  <- "Graphics"
  merdata <- glamr::si_path("path_msd")
  
# Load secrets to access data bases needed
  load_secrets()

# Steps to make tables
  #1) source the 99_utilities & 
  #2) read in most recent ou_im table & create globals
  #3) create the long base table (make_md_df)
  #4) reshape the base table (reshape_md_tbl)
  #5) call resulting table and apply mdb_main_theme()
  
  #1) 
  purrr::map(list("Scripts/99_utilities.R", "Scripts/mdb_main_theme.R"), ~source(.x))
  
  #2) Load OU_IM table
  ou_im <- 
    si_path() %>% 
    return_latest("OU_IM_FY19-21_20210618_v2_1") %>% 
    read_msd() 
  
  #2) Objects needed 
  present_fy <- paste(identifypd(ou_im), "Cumulative")
  present_qtr <- paste(identifypd(ou_im), "Results")
  past_fy <- paste("FY", identifypd(ou_im) %>% str_sub(., 3, 4) %>% as.numeric() - 1, sep = "")
  
  msd_source <- ou_im %>% identifypd() %>% msd_period(period = .)
  authors <- paste("Created by Core Analytics Cluster on", Sys.Date(), "using", msd_source)
  caveats <- "Certain mechanisms have been omitted. See the Known Issues Tracker for full list of mechanisms omitted."
  dedup_note <- "ALL OTHER AGENCIES based on aggregates excluding de-duplication."
  change_note <- "Number reflects percentage change from the same quarter in the previous year."
  
  

# MAKE TABLES - DRAW THE OWL ----------------------------------------------

  #3) Create the long base table
  mdb_df <- make_mdb_df(ou_im, resolve_issues = T)
  
  #4) Reshape the main table into a gt ready format
  mdb_tbl <- reshape_mdb_df(mdb_df)

  # Generic function to call an OU/Country/Agency
  create_mdb_table <- function(df, ou){
    
    cntry <- str_to_upper(ou)
    
    df %>% 
      filter(operatingunit %in% c({{ou}})) %>% 
      gt(groupname_col = "agency") %>% 
      mdb_main_theme() %>% 
      tab_header(
        title = glue::glue("{cntry} PERFORMANCE SUMMARY")
      ) 
  }
  
  create_mdb_table(mdb_tbl, c("Global"))

