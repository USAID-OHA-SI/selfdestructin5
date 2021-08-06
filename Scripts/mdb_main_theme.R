# GT template for tables

mdb_main_theme <- function(df, ...){
  
  df %>% 
    # These columns are not needed so they are hidden
    cols_hide(
      columns = c("operatingunit", "agg_type", "indicator")
    ) %>% 
    fmt_missing(
      columns = everything(),
      missing_text = "-"
    ) %>% 
    fmt_percent(
      columns = matches("achievement|present_z_change"), 
      decimal = 0
    ) %>% 
    fmt_number(
      columns = c("past_results_cumulative", "past_targets", 
                  "present_results_cumulative", "present_targets",
                  "present_z_aresults"),
      decimal = 0
      ) %>% 
    fmt_markdown(columns = c("indicator2")) %>% 
    tab_spanner(
      label = glue::glue("{past_fy}"),
      columns = contains("past")
    ) %>% 
    tab_spanner(
      label = glue::glue("{present_fy}"),
      columns = contains("present")
    ) %>% 
    tab_spanner(
      label = glue::glue("{pd}"),
      columns = contains("_z_")
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")), 
      locations = cells_column_spanners(spanners = everything())
    ) %>% 
    cols_label( # TODO -- Move this to an automated step using a naming f()
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
    tab_style(
      style = list(
        cell_fill(color = "#e6e7e8", alpha = 0.5),
        cell_text(weight = 500)
      ),
      locations = cells_body(
        columns = present_targets_achievement
      )
    ) %>% 
    # Merge Key details into a single source note
    tab_source_note(
      source_note = md(glue::glue("***Change**: {change_note}"))
    ) %>% 
    tab_source_note(
      source_note = md(glue::glue("**Notes**: {dedup_note} | {caveats}"))
    ) %>% 
    tab_source_note(
      source_note = md(glue::glue("**Source**: {authors} | si.coreanalytics@usaid.gov"))
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
          columns = c(past_targets_achievement, present_tint_achv)
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
      ...) 
}

