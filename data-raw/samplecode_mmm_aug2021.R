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
  library(fontawesome)
  library(ggplot2)

  mdb_out <- "../../Sandbox/"
  merdata <- glamr::si_path("path_msd")
  load_secrets()
  
# Load OU_IM table
  ou_im <- 
    si_path() %>% 
    return_latest("OU_IM_FY19-21_20210813_v1_1") %>%
    read_msd() 
  
# Time metadata needed  
  pd <- create_pd(ou_im)
  msd_source <- pd %>% msd_period(period = .)


# GENERATE MDB TABLES -----------------------------------------------------

  # Main
  mdb_df   <- make_mdb_df(ou_im)
  mdb_tbl  <- reshape_mdb_df(mdb_df, pd)
  
  # Treatment
  mdb_df_tx    <- make_mdb_tx_df(ou_im, resolve_issues = F)
  mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, pd)

#Legend
  
  
 # ggplot_image(plot_object = , height = 100, aspect_ratio = 1)
  # 
  # plot_object <-
  #   ggplot(
  #     data = gtcars,
  #     aes(x = hp, y = trq,
  #         size = msrp)) +
  #   geom_point(color = "blue") +
  #   theme(legend.position = "none")
  
  

  
  
# WRITE RESULTS -----------------------------------------------------------
  # plot_object <- ggplot () html(
  #   web_image(url = "https://github.com/USAID-OHA-SI/selfdestructin5/blob/main/man/figures/legend.png"))
  # 
  # 
  # mdb_tbl %>% 
  #   filter(operatingunit == "Malawi") %>% 
  #   gt(groupname_col = "agency") %>% 
  #   ggplot_image(plot_object, height = 30, aspect_ratio = 2) %>%
  #   mdb_main_theme(pd, msd_source) 
  # 
  # install.packages("png")
  # library(png)
  # 
  # mdb_tbl %>% 
  #   filter(operatingunit == "Malawi") %>% 
  #   gt(groupname_col = "agency") %>% 
  #   text_transform(location = cells_title(groups = "agency"), 
  #                  html(
  #                    web_image(url = "https://github.com/USAID-OHA-SI/selfdestructin5/blob/main/man/figures/legend.png"))) %>%
  #   mdb_main_theme(pd, msd_source)
  
  
  url_img <- "https://github.com/USAID-OHA-SI/selfdestructin5/blob/main/man/figures/legend.png"
  
mdb_tbl %>% 
    filter(operatingunit == "Malawi") %>% 
    gt(groupname_col = "agency") %>% 
    mdb_main_theme(pd, msd_source) %>%
  tab_header(
    title = md("<img src='https://raw.githubusercontent.com/USAID-OHA-SI/selfdestructin5/main/man/figures/legend.png' style='height:40px;'> "),
    subtitle = NULL)



mdb_tbl <-  mdb_tbl %>% 
  dplyr::tibble(
    pixels = px(seq(10, 15, 5)),
    image = seq(10, 15, 5)
  ) 
  gt() %>% 
  text_transform(locations = cells_body(columns = "agency"), 
                 fn = function(x) {
                   web_image(
                     url = url_img,
                     height = as.numeric(x)
                   )
                 }
  )
  
  
    #'     locations = cells_body(columns = image),
                   
  
  #... = 
    tab_header(subtitle = html("<strong>Legend</strong>",
                                    web_image(url = "https://github.com/USAID-OHA-SI/selfdestructin5/blob/main/man/figures/legend.png", 
                                        height = 30)
                                    )))
                  
    #ggplot_image[(plot_object = 
    html("<strong>Legend</strong>",
    web_image("https://github.com/USAID-OHA-SI/selfdestructin5/blob/main/man/figures/legend.png", 
              height = px(40)
              ))
  
  #... = ggplot_image(ggplot_image(plot_object, height = 100, aspect_ratio = 1))  )
                  
  
  mdb_tbl_tx %>% 
    filter(operatingunit == "Malawi") %>% 
    gt(groupname_col = "agency") %>% 
    mdb_treatment_theme(pd, msd_source)
  
  
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
  
  

# USAID OU ACHIEVEMENT COMPARISON -----------------------------------------

  # Create OU comparison table for key indicators
  mdb_tbl %>% 
    filter(agency == "USAID", agg_type == "OU") %>% 
    select(operatingunit, present_targets_achievement, indicator) %>% 
    pivot_wider(names_from = indicator, 
                values_from = present_targets_achievement,
                names_sort = TRUE) %>% 
    arrange(desc(TX_CURR)) %>% 
    gt() %>% 
    fmt_missing(columns = -c("operatingunit"), missing_text = "-") %>% 
    fmt_percent(columns = -c("operatingunit"), decimals = 0) %>% 
    cols_label(operatingunit = "") %>% 
    tab_options(
      source_notes.font.size = 8,
      table.font.size = 13, 
      data_row.padding = gt::px(5)
    ) %>% 
    tab_style(
      style = 
        gt::cell_text(color = "lightblue", weight = "bold", stretch = "ultra-expanded"),  
      locations = gt::cells_stubhead()) %>%
    tab_header(title = glue::glue("USAID OU PERFORMANCE SUMMARY FOR {pd}"), 
               subtitle = md("<img src='https://raw.githubusercontent.com/USAID-OHA-SI/selfdestructin5/main/man/figures/legend.png' style='height:40px;'> ")) 

    
locations = gt::cell
    
    
    
    locations = gt::cells_column_spanners(spanners = tidyselect::everything())
    
    mdb_tbl %>% filter(operatingunit == "Global") %>%
      gt(groupname_col = "agency") %>%
      tab_style(
        style = list(
          cell_text(weight = "bold", locations = cells_column_labels())))
  
  mdb <- create_mdb(mdb_tbl, ou = "Global", type = "main", pd, msd_source) %>% 
  dplyr::select(mdb_tbl) %>%
    gt() %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
        ), 
      locations = cells_body(everything()))
      
mdb_main_theme( 
               ... = create_mdb(mdb_tbl, ou = "Global", type = "main", pd, msd_source))
  dplyr::select(HTS_TST) %>%
                 gt() %>%
                 tab_style(
                   style = list(
                     cell_text(weight = "bold"),
                     locations = cells_body(everything())
                   ))
)



                   
                   locations = cells_body(groupname_col = "agency"))
                 )
    

mdb_gt <-  mdb_tbl %>% 
  filter(operatingunit %in% c("Global")) %>% 
  gt(groupname_col = "agency") %>% 
  tab_style(
    style = 
      gt::cell_text(weight = "bold"), 
    locations = cells_row_groups(everything())
    ) %>%
  mdb_main_theme(pd, msd_source) %>% 
  tab_header(
    title = glue::glue("GLOBAL PERFORMANCE SUMMARY"),
    subtitle = md("<img src='https://raw.githubusercontent.com/USAID-OHA-SI/selfdestructin5/main/man/figures/legend.png' style='height:40px;'> ")
  ) 
  
%>%
  
  
  prinf(mdb_tbl)
  
  
  tab_style(
      style = 
        gt::cell_text(weight = "bold")) 
    tab_header(title = NULL,
               subtitle = md("<img src='https://raw.githubusercontent.com/USAID-OHA-SI/selfdestructin5/main/man/figures/legend.png' style='height:40px;'> "))
    
  %>% filter() %>%
    gt() %>%
  
  mdb_tbl %>% 
    gt() %>% 
    tab_header(title = md("<img src='https://raw.githubusercontent.com/USAID-OHA-SI/selfdestructin5/main/man/figures/legend.png' style='height:40px;'> "),
               subtitle = NULL)
  
  
  mdb_tbl %>% 
    filter(operatingunit == "Global") %>% 
    gt() %>% 
    mdb_main_theme(pd, msd_source) %>%
    tab_header(
      title = glue::glue("GLOBAL PERFORMANCE SUMMARY"),
      subtitle = md("<img src='https://raw.githubusercontent.com/USAID-OHA-SI/selfdestructin5/main/man/figures/legend.png' style='height:40px;'> "))

# FUTURE WORK -------------------------------------------------------------


  # TODO
  #1) Wrapper function that will upload files to a specified folder in google drive (glamr issue)
  #2) Think about whether footnotes are added incrementally with a separate f()
  #3) Determine if there is a better way to integrate pd and msd_source in f()s
  #4) Fix Cote D'Ivoire as google drive does not like it if used in the name of file
  #5) PrEP_NEW was not reported quarterly prior to FY21
  
  
  

