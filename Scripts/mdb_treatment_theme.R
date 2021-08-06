# GT template for tables

mdb_treatment_theme <- function(df, numeric_cols, ...){
  
  df %>% 
    # These columns are not needed so they are hidden
    cols_hide(
      columns = c("operatingunit", "agg_type", "indicator")
    ) %>% 
    fmt_missing(
      columns = everything(),
      missing_text = "-"
    ) %>% 
    fmt_number(
      columns = all_of({{numeric_cols}}),
      rows = str_detect(indicator2, "(TX_CURR|TX_MMD3+|TX_MMD6)"),
      decimal = 0
    ) %>% 
    fmt_percent(
      columns = all_of({{numeric_cols}}),
      rows = str_detect(indicator2, "(TX_MMD3_SHARE|VLC|VLS)"),
      decimal = 0
    ) %>% 
    fmt_markdown(columns = c("indicator2")) %>% 
    tab_spanner(
      label = glue::glue("{past_fy}"),
      columns = contains("results")
    ) %>% 
    tab_spanner(
      label = glue::glue("{present_qtr}"),
      columns = matches("q|delta")
    ) %>% 
    tab_style(
      style = list(
        cell_borders(
          sides = "right",
          color = "white",
          weight = px(15)
        )
      ),
      locations = list(
        cells_body(
          columns = matches("results")
        )
      )
    ) %>% 
    tab_style(
      style = list("font-variant: small-caps;"),
      locations = cells_column_labels(columns = everything()
      )
    ) %>% 
    tab_options(
      source_notes.font.size = 8,
      table.font.size = 12, 
      data_row.padding = px(5),
      source_notes.padding = px(1),
      ...) %>% 
    cols_label(
      change_dir = " ",
      indicator2 = " "
    ) %>% 
    tab_source_note(
      source_note = md(glue::glue("***Delta**: {delta_note}"))
    ) %>% 
    tab_source_note(
      source_note = md(glue::glue("**Notes**: {dedup_note} | {caveats}"))
    ) %>% 
    tab_source_note(
      source_note = md(glue::glue("**Source**: {authors} | si.coreanalytics@usaid.gov"))
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")), 
      locations = cells_column_spanners(spanners = everything())
    ) 
}
    
    
    
    
   