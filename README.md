<!-- badges: start -->
[![R-CMD-check](https://github.com/USAID-OHA-SI/selfdestructin5/workflows/R-CMD-check/badge.svg)](https://github.com/USAID-OHA-SI/selfdestructin5/actions)
<!-- badges: end -->


# selfdestructin5
SI utilities package to create Mission Director Briefers from MSDs

## Overview
The selfdestructin5 package is a convenient way to munge and create high quality tables for a select set of PEPFAR indicators. We created the package to automate the creation of quarterly Mission Director Tables that summarize a Missions performance across a subset of indicators. Focal users are SI staff who are tasked with creating, updating or modifying the Mission Director Briefer tables. 

### Installing `selfdestructin5`
selfdestructin5 is not on CRAN, so you will have to install it directly from Github using `remotes`.

If you do not have `remotes` installed, you will have to run the `install.packages("remotes")` line in the code below as well.

```{r}
## SETUP

  #install
    install.packages("remotes")
    remotes::install_github("USAID-OHA-SI/selfdestructin5")
    
  #load the package
    library(selfdestructin5)
  
## List the functions contained in the package
  ls('package:selfdestructin5')
  
```

### Creating a MDB table
The package is engineered to work with OU_IM MSDs from FY21 and onward. The main table returns a summary of select indicators for the current quarter. The treatment table returns a summary of TX_CURR / VLS /VLC indicators from all quarters of the current fiscal year. 

```{r}

    library(glitr)
    library(glamr)
    library(tidyverse)
    library(gophr)
    library(gt)
    library(selfdestructin5)
    library(fontawesome)

  # Set up paths 
    mdb_out <- "../../Sandbox/"
    merdata <- si_path("path_msd")
    load_secrets()
    
  # Load OU_IM table
    ou_im <- 
      si_path() %>% 
      return_latest("OU_IM_FY19-21_20210618_v2_1") %>%
      read_msd() 
    
  # Time metadata needed  
    pd <- create_pd(ou_im)
    msd_source <- pd %>% msd_period(period = .)

# Main Table
   # Create the long mdb_df of the main summary indicators 
   # This will remove mechs with known issues by default
      mdb_df   <- make_mdb_df(ou_im)
    
   # Create the reshaped df that is gt() ready
      mdb_tbl  <- reshape_mdb_df(mdb_df, pd)
    
   # the `agg_type` column flags the operatingunit as either OU, Region-Country or Agency
      mdb_tbl %>% distinct(agg_type, operatingunit) %>% slice(1:15)
  
   # Generate base table by passing the reshaped dataframe to the main mdb theme
      create_mdb(mdb_tbl, ou = "Global", type = "main", pd, msd_source )
    
# Treatment Table
  
      mdb_df_tx    <- make_mdb_tx_df(ou_im, resolve_issues = F)
      mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, pd)
  
  # Generate treatment table
      mdb_tbl_tx %>% 
        filter(operatingunit == "Malawi") %>% 
        gt(groupname_col = "agency") %>% 
        mdb_treatment_theme(pd, msd_source)
      

---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
