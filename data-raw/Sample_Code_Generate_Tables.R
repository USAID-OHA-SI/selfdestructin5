# PURPOSE: Sample code to generate MDB tables
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-08-10
# NOTES: Sample code to generate tables



# GLOBALS -----------------------------------------------------------------

  library(glitr)
  library(glamr)
  library(tidyverse)
  library(gophr)
  library(gt)
  library(selfdestructin5)

  mdb_out <- "../../Sandbox/"
  merdata <- glamr::si_path("path_msd")
  load_secrets()
  
# Load OU_IM table
  ou_im <- 
    si_path() %>% 
    #return_latest("OU_IM_FY19-21_20210618_v2_1") %>%
    return_latest("OU_IM_FY18-21_20200918") %>% 
    read_msd() 
  
# Time metadata needed  
  pd <- create_pd(ou_im)
  msd_source <- pd %>% msd_period(period = .)


# GENERATE MDB TABLES -----------------------------------------------------

  # Main
  mdb_df   <- make_mdb_df(ou_im)
  mdb_tbl  <- reshape_mdb_df(mdb_df, pd)
  
  # Treatment
  mdb_df_tx    <- make_mdb_tx_df(ou_im)
  mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, pd)

  # Create OU comparison table for key indicators
  mdb_tbl

# WRITE RESULTS -----------------------------------------------------------
  
  mdb_tbl %>% 
    filter(operatingunit == "Malawi") %>% 
    gt(groupname_col = "agency") %>% 
    mdb_main_theme(pd, msd_source)
  
  # create batch tables
 distinct_agg_type <- function(df, type = "OU"){
   df %>% 
     filter(agg_type == {{type}}) %>% 
     distinct(operatingunit) %>% 
     pull()
  }
  
  # MAIN
  ous <- distinct_agg_type(mdb_tbl, "OU")
  glb <- distinct_agg_type(mdb_tbl, "Agency")
  rgl <- distinct_agg_type(mdb_tbl, "Region-Country")
  
  map(ous, ~create_mdb(mdb_tbl, ou = .x, type = "main", pd, msd_source) %>% 
        gtsave(., path = mdb_out, filename = glue::glue("{.x}_{pd}_mdb_main.png")))
  
  
  # TREATMENT
  ous_tx <- distinct_agg_type(mdb_tbl_tx, "OU")
  
  map(ous, ~create_mdb(mdb_tbl_tx, ou = .x, type = "treatment", pd, msd_source) %>% 
        gtsave(., path = mdb_out, filename = glue::glue("{.x}_{pd}_mdb_treatment.png")))
  

# FUTURE WORK -------------------------------------------------------------


  # TODO
  #1) Wrapper function that will upload files to a specified folder in google drive (glamr issue)
  #2) Think about whether footnotes are added incrementally with a separate f()
  #3) Determine if there is a better way to integrate pd and msd_source in f()s
  #4) 
  
  
  

