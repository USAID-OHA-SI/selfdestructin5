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
  pd <- create_pd(ou_im)
  msd_source <- pd %>% msd_period(period = .)
  

# MAKE TABLES - DRAW THE OWL ----------------------------------------------

  #3) Create the long base table
  mdb_df <- make_mdb_df(ou_im, resolve_issues = F)
  
  #4) Reshape the main table into a gt ready format
  mdb_tbl <- reshape_mdb_df(mdb_df, pd)

  #5) Make the tables
  create_mdb(mdb_tbl, ou = "Nigeria")
  
  
