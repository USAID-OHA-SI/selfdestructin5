# PURPOSE: Munge and Analysis of FY21 Q2 MD tables
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2021-05-20
# NOTES: New take on the tables

# Requires running 01 script through line 199
  

# BASE TABLE GENERATION ---------------------------------------------------
  # Main changes are in the groupname_col, the indicator2 labels and 
  # logical statment to not print NA and extra line

  # Customize GT table to reproduce results
  md_tbl <- function(md_tbl_old, tbl_col_names, ou) {
    
    cntry <-  str_to_upper(ou)
    team <- "Core Analytics Cluster"
    
    md_tbl_old %>%
      gt(groupname_col = "agency") %>% 
      cols_hide(indicator) %>% 
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
          columns = c(indicator2),
          rows = str_detect(agency, "Salvador: USAID")
        ),
        fn = function(x){
          name <- word(x, 1)
          name2 <- ifelse(is.na(word(x, 2, -1)), "", word(x, 2, -1))
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
        source_note = paste("Produced on ",Sys.Date(), "by the ", team, " using PEPFAR FY21Q2i MSD released on 2021-05-14.")
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
      # ) %>% \
      # Below controls height
      tab_options(., 
                  data_row.padding = px(1), 
                  source_notes.padding = px(1),
                  heading.padding = px(1),
                  column_labels.padding = px(1),
                  row_group.padding = px(1.5)
                  
      )
  }
  
  
  
# TEST TABLE GENERATION BY OU OR COUNTRY ----------------------------------
  
  # Test it all together
  # Grab Data for all Central American OUs - El Salvador, Guatemala, Honduras, Nicaragua, Panama

  els <- shape_md_tbl(df = ou_im, country_col = countryname, ou = "El Salvador") %>% mutate(country = "El Salvador")
  gta <- shape_md_tbl(df = ou_im, country_col = countryname, ou = "Guatemala") %>% mutate(country = "Guatemala")
  hnd <- shape_md_tbl(df = ou_im, country_col = countryname, ou = "Honduras") %>% mutate(country = "Honduras")
  nic <- shape_md_tbl(df = ou_im, country_col = countryname, ou = "Nicaragua") %>% mutate(country = "Nicaragua")
  pan <- shape_md_tbl(df = ou_im, country_col = countryname, ou = "Panama") %>% mutate(country = "Panama")
  
  caribb <- bind_rows(els, gta, hnd, nic, pan) %>% 
    unite(agency, c(country, agency), sep = ": ") %>% 
    mutate(indicator2 = case_when(
      agency != "El Salvador: USAID" ~ as.character(indicator),
      TRUE ~ indicator2
    ))

  tbl_col_names <- fix_col_names(caribb)
  md_tbl(caribb, tbl_col_names, "Central America") %>% 
    gtsave(file.path("Images/Regional/WesternHemi/", "Central_America_FY21Q2_KEY_INDICATORS.PNG"))
  







