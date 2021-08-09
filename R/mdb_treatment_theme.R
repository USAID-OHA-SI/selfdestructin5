# GT template for tables

#' GT theme for main MDB tables
#' 
#' @description 
#' A gt theme to be applied to a reshaped data frame for the creation of MDB tables.
#' The theme formats all columns and rows for the main MDB tables. 
#' A different theme exists for the main tables as the layout is different.
#' 
#' 
#' @param df a dataframe from [reshape_mdb_tx_df()] output
#' @param numeric_cols an object containing the names of the columns that are numeric
#' @param pd character object of the style FY@@Q@ that is returned from [gophr::identifypd()]
#' @param msd_source source of the data used to generate tables
#' @param ... dot-dot-dot option to pass additional formatting to gt object
#' 
#' @return formatted gt object
#' @export
#' 
#' @family MDB gt themes
#' 
#' @examples
#' \dontrun{
#'  mdb_df <- make_mdb_tx_df(ou_im, resolve_issues = F)
#'  mdb_tbl <- reshape_mdb_tx_df(mdb_df)
#'  numeric_cols <- mdb_tbl %>% select_if(is.numeric) %>% names()
#'  mdb_tbl %>% filter(operatingunit == "Zambia") %>% 
#'  gt(groupname_col = "agency") %>% 
#'  mdb_treatment_theme(numeric_cols)
#'  
#'  }


mdb_treatment_theme <- function(df, numeric_cols, pd, msd_source, ...){
  
  df %>% 
    # These columns are not needed so they are hidden
    gt::cols_hide(
      columns = c("operatingunit", "agg_type", "indicator")
    ) %>% 
    gt::fmt_missing(
      columns = tidyselect::everything(),
      missing_text = "-"
    ) %>% 
    gt::fmt_number(
      columns = tidyselect::all_of({{numeric_cols}}),
      rows = stringr::str_detect(indicator2, "(TX_CURR|TX_MMD3+|TX_MMD6)"),
      decimal = 0
    ) %>% 
    gt::fmt_percent(
      columns = tidyselect::all_of({{numeric_cols}}),
      rows = stringr::str_detect(indicator2, "(TX_MMD3_SHARE|VLC|VLS)"),
      decimal = 0
    ) %>% 
    gt::fmt_markdown(columns = c("indicator2")) %>% 
    gt::tab_spanner(
      label = glue::glue("{past_fy(pd)}"),
      columns = tidyselect::contains("results")
    ) %>% 
    gt::tab_spanner(
      label = glue::glue("{present_qtr(pd)}"),
      columns = tidyselect::matches("q|delta")
    ) %>% 
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = "right",
          color = "white",
          weight = gt::px(15)
        )
      ),
      locations = list(
        gt::cells_body(
          columns = tidyselect::matches("results")
        )
      )
    ) %>% 
    gt:: tab_style(
      style = list("font-variant: small-caps;"),
      locations = gt::cells_column_labels(columns = tidyselect::everything()
      )
    ) %>% 
    gt::tab_options(
      source_notes.font.size = 8,
      table.font.size = 12, 
      data_row.padding = gt::px(5),
      source_notes.padding = gt::px(1),
      ...) %>% 
    gt::cols_label(
      change_dir = " ",
      indicator2 = " "
    ) %>% 
    gt::tab_source_note(
      source_note = gt::md(glue::glue("***Delta**: {delta_footnote()}"))
    ) %>% 
    gt::tab_source_note(
      source_note = gt::md(glue::glue("**Notes**: {dedup_footnote()} | {caveats_footnote()}"))
    ) %>% 
    gt::tab_source_note(
      source_note = gt::md(glue::glue("**Source**: {authors} | si.coreanalytics@usaid.gov"))
    ) %>% 
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")), 
      locations = gt::cells_column_spanners(spanners = tidyselect::everything())
    ) 
}
    
    
    
    
   