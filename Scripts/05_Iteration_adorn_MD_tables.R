# PURPOSE: Munge and Analysis of FY21 Q2 MD tables Version 1.0
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-06-05
# NOTES: New take on the tables for Prime Partners
# Builds on 01_Create_MD_tables

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
  
    merdata <- si_path(type = "path_msd")
  
  # Create a new folder to house regional country tables
    dir_list <- c("Global", "OU", "Regional")
    map(dir_list, ~dir.create(file.path("Images/", .x)))
    
    folder_list <- c("Asia", "WAR", "WesternHemi")
    map(folder_list, ~dir.create(file.path("Images/Regional/", .x)))
  
  # Key indicators for the base tables
    indics <- c("PrEP_NEW", "OVC_SERV", "VMMC_CIRC", 
                "HTS_TST", "HTS_TST_POS",
                "TX_NEW", "TX_CURR")
    
    cumulative_indic <- c("PrEP_NEW", "VMMC_CIRC", 
                          "HTS_TST", "HTS_TST_POS")
  
  # Agency order throughout
  # Use the long order b/c of the varying nature of coverage by diff agencies
    agency_order_shrt <- c("USAID", "CDC", "OTHER")
    agency_order_long <- c("USAID", "CDC", "OTHER", "DOD", "HRSA", "PRM", "AF", "PC")
  

# LOAD AND MUNGE ----------------------------------------------------------

  # Is table Global, OU or Country? 
    # @param df data frame of latest ou_im
    clean_and_collapse <- function(df){
      df %>% 
        filter(indicator %in% indics,
               standardizeddisaggregate %in% c("Total Numerator"),
               fundingagency != "Dedup") %>% 
        clean_agency() %>% 
        # Lump factors at 3 then apply long agency order b/c of varying nature
        mutate(agency = fct_lump(fundingagency, n = 2, other_level = "OTHER"),
               agency = fct_relevel(agency, agency_order_long)) %>% 
        group_by(fiscal_year, agency, indicator) %>% 
        summarise(across(where(is.double), sum, na.rm = TRUE), .groups = "drop")
    }   


  shape_md_tbl <- function(df, country_col, ou) {
    
    # Filter the data down to key indicators defined in indics object
    # Collapsing down to the agency level
    ou_tbl <- 
      df %>% 
      filter({{country_col}} %in% ou) %>% 
      clean_and_collapse()  

    # Clean up and add up down flags, these will be used in version 1.0   
    md_tbl <- 
      ou_tbl %>% 
      reshape_msd("quarters", qtrs_keep_cumulative = TRUE) %>% 
      group_by(agency, indicator) %>% 
      mutate(value_run = row_number()) %>% 
      rename(pd = period) %>% 
      ungroup() %>%   
      complete(nesting(indicator, agency), value_run = full_seq(value_run, period = 1))
    
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
      arrange(agency, indicator)
  }  
  
  tmp <- shape_md_tbl(ou_im, operatingunit, "Zambia")
  
  # Balance panel

    ou_im %>% 
    filter(operatingunit %in% "Zambia") %>% 
    clean_and_collapse() %>% 
      reshape_msd("quarters", qtrs_keep_cumulative = TRUE) %>% 
      group_by(agency, indicator) %>% 
      mutate(value_run = row_number()) %>% 
      rename(pd = period) %>% 
      ungroup() %>%   
      complete(nesting(indicator, agency), value_run = full_seq(value_run, period = 1)) %>% prinf()
  

  # This will form the basis for the OU table
  bfr <- tmp %>% 
    filter(period %in% c("FY19Q4", "FY20Q4", "FY21Q2")) %>% 
    select(fiscal_year, indicator, agency, targets, cumulative, 
           APR, change_dir, q2q_comp) %>% 
    pivot_wider(names_from = fiscal_year, 
                names_glue = "{.value}_{fiscal_year}",
                values_from = c(targets, cumulative, APR, change_dir, q2q_comp)) %>% 
    select(indicator, agency, sort(colnames(.))) %>%
    select(-matches("comp_2019|comp_2020|dir_2019|dir_2020")) %>% 
    left_join(., indic_def)  %>% 
    mutate(indicator = fct_relevel(indicator, indics)) %>% 
    arrange(agency, indicator)
    
  # Grab most recent value (pry needs a function())
  bfr_qs <- 
    tmp %>% 
    select(indicator, agency, period,  results_cumulative) %>% 
    pivot_wider(names_from = period,
                values_from = results_cumulative) %>% 
    select(indicator, agency, FY21Q2)
  
  # Function to create the sparklines
  spark_plot <- function(df){
    df %>% 
      mutate(indicator = fct_relevel(indicator, indics)) %>% 
      ggplot(aes(x = factor(period), y = results_cumulative)) +
      geom_col(aes(fill = spark_color)) +
      scale_fill_identity() +
      si_style_void() +
      theme(legend.position = "none")
  }
  
  spark_plot2 <- function(df){
    df %>% 
      mutate(indicator2 = fct_relevel(indicator2, indics),
             apr = if_else(apr >= 1.1, 1.1, apr),
             fill_color = if_else(apr <1, genoa_light, genoa)) %>% 
      ggplot(aes(y = indicator2)) +
      geom_col(aes(x = 1), fill = grey20k)+
      geom_col(aes(x = apr, fill = fill_color), alpha = 0.5) +
      geom_vline(xintercept = c(.25, .5, .75), color = "white", size = 6) +
      geom_vline(xintercept = c(1), color = grey90k, size = 8) +
      scale_fill_identity() +
      scale_x_continuous(limits = c(0, 1.1))+
      si_style_void() +
      theme(legend.position = "none")
  }
  
  
  
  # Create the sparklines for table
  lag_position = 4
  md_spark <- 
    tmp %>% 
    group_by(indicator, agency) %>% 
    mutate(spark_color = case_when(
      period == "FY21Q2" & (lag(results_cumulative, n = lag_position) - results_cumulative < 0) ~ genoa,
      period == "FY21Q2" & (lag(results_cumulative, n = lag_position) - results_cumulative >= 0) ~ old_rose,
      TRUE ~ grey20k
    )) %>% 
    ungroup() %>% 
    select(agency, spark_color, indicator, period, results_cumulative, APR, indicator) %>% 
    filter(period == "FY21Q2") %>%
    mutate(fundingagency = agency, 
           indicator2 = indicator,
           apr = APR) %>% 
    arrange(agency, indicator) %>% 
    # nest(spark_nest = c(period, results_cumulative, fundingagency, indicator, spark_color)) %>% 
    # mutate(plot = map(spark_nest, spark_plot))
    nest(spark_nest = c(apr, fundingagency, indicator2, spark_color)) %>% 
    mutate(plot = map(spark_nest, spark_plot2))
  

  bfr %>%     
    # Fix the names using on the indictor names for USAID at top
    mutate(indicator_plain = ifelse(agency == "USAID", paste0(indicator, "\n", str_wrap(indicator_plain, 40)), as.character(indicator))) %>% 
      mutate(indicator = fct_relevel(indicator, indics),
           ggplot = NA) %>% 
    arrange(indicator) %>% 
    relocate(indicator_plain, .before = everything()) %>% 
    select(-indicator) %>% 
    gt(groupname_col = "agency") %>% 
    fmt_percent(columns = contains("APR"), 
                decimals = 0) %>% 
    fmt_number(columns = starts_with(c("cumulative", "targets")), 
               decimals = 0) %>% 
    fmt_missing(columns = everything(), missing_text = "-") %>% 
    text_transform(
      locations = cells_body(columns = c(ggplot)),
      fn = function(x){
        map(md_spark$plot, ggplot_image, height = px(15), aspect_ratio = 4)
      }
    ) 

  
  # Reformat 
  reformat_apr_col <- function(df, apr, rslt, tgt, var) {
    df %>% 
      mutate("{{ var }}" := paste(percent({{apr}}, 1), 
                                  comma({{rslt}}, accuracy = 1, scale = 1e-3, suffix = "K"), 
                                  comma({{tgt}}, accuracy = 1, scale = 1e-3, suffix = "K")))
  }
  
  reformat_fy_col <- function(df, newvar, curr_qtr, yoy_delta){
    df %>% 
      mutate("{{ newvar }}" := paste(comma({{curr_qtr}}, 1), percent({{yoy_delta}}, 1)))
  }
  
  
  
  
  
  
  tmp <- bfr %>% 
    arrange(indicator) %>% 
    reformat_apr_col(., APR_2021, cumulative_2021, targets_2021, APR_2021) %>% 
    reformat_apr_col(., APR_2020, cumulative_2020, targets_2020, APR_2020) %>% 
    left_join(bfr_qs) %>% 
    reformat_fy_col(., FY21Q2, FY21Q2, q2q_comp_2021) %>% 
    relocate(FY21Q2, .after = APR_2021) %>% View()
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
 