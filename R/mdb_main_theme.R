# GT template for tables

#' GT theme for main MDB tables
#' 
#' @description 
#' A gt theme to be applied to a reshaped data frame for the creation of MDB tables.
#' The theme formats all columns and rows for the main MDB tables. 
#' A different theme exists for the treatment tables as the layout is different.
#' 
#' 
#' @param df [reshape_mdb_df()] output
#' @param ... dot-dot-dot option to pass additional formatting to gt object
#' 
#' @return formatted gt object
#' @export
#' 
#' @examples
#' \dontrun{
#'  mdb_df <- make_mdb_df(ou_im, resolve_issues = F)
#'  mdb_tbl <- reshape_mdb_df(mdb_df)
#'  mdb_tbl %>% filter(operatingunit == "Zambia") %>% gt(groupname_col = "agency") %>% mdb_main_theme()
#'  
#'  }


mdb_main_theme <- function(df, ...){
  
  df %>% 
    # These columns are not needed so they are hidden
    gt::cols_hide(
      columns = c("operatingunit", "agg_type", "indicator")
    ) %>% 
    gt::fmt_missing(
      columns = tidyselect::everything(),
      missing_text = "-"
    ) %>% 
    gt::fmt_percent(
      columns = tidyselect::matches("achievement|present_z_change"), 
      decimal = 0
    ) %>% 
    gt::fmt_number(
      columns = c("past_results_cumulative", "past_targets", 
                  "present_results_cumulative", "present_targets",
                  "present_z_aresults"),
      decimal = 0
      ) %>% 
    gt::fmt_markdown(columns = c("indicator2")) %>% 
    gt::tab_spanner(
      label = glue::glue("{past_fy}"),
      columns = tidyselect::contains("past")
    ) %>% 
    gt::tab_spanner(
      label = glue::glue("{present_fy}"),
      columns = tidyselect::contains("present")
    ) %>% 
    gt::tab_spanner(
      label = glue::glue("{pd}"),
      columns = tidyselect::contains("_z_")
    ) %>% 
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")), 
      locations = gt::cells_column_spanners(spanners = tidyselect::everything())
    ) %>% 
    gt::cols_label( # TODO -- Move this to an automated step using a naming f()
      indicator2 = " ",
      past_results_cumulative = "results", 
      past_targets = "targets",
      past_targets_achievement = "achv",
      present_results_cumulative = "results",
      present_targets = "targets", 
      present_targets_achievement = "achv",
      present_tint_achv = " ",
      present_z_aresults = "results",
      present_z_change = "change*",
      present_z_direction = " "
    ) %>% 
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#e6e7e8", alpha = 0.5),
        gt::cell_text(weight = 500)
      ),
      locations = gt::cells_body(
        columns = present_targets_achievement
      )
    ) %>% 
    # Merge Key details into a single source note
    gt::tab_source_note(
      source_note = gt::md(glue::glue("***Change**: {change_note}"))
    ) %>% 
    gt::tab_source_note(
      source_note = gt::md(glue::glue("**Notes**: {dedup_note} | {caveats}"))
    ) %>% 
    gt::tab_source_note(
      source_note = gt::md(glue::glue("**Source**: {authors} | si.coreanalytics@usaid.gov"))
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
          columns = c(past_targets_achievement, present_tint_achv)
        )
      )
    ) %>% 
    gt::tab_style(
      style = list("font-variant: small-caps;"),
      locations = gt::cells_column_labels(columns = tidyselect::everything()
      )
    ) %>% 
    gt::tab_options(
      source_notes.font.size = 8,
      table.font.size = 12, 
      data_row.padding = gt::px(5),
      source_notes.padding = gt::px(1),
      ...) 
}

