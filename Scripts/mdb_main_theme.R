# GT template for tables

mdb_main_theme <- function(df, ...){
  
  df %>% 
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
    text_transform(
      locations = cells_body(
        columns = c("indicator2"),
        rows = (agency == "USAID")
      ),
      fn = function(x){
        name <- word(x, 1)
        name2 <- word(x, 2, -1)
        glue::glue(
          "<div style='line-height:10px'<span style='font-weight:regular;font-variant:small-caps;font-size:13px'>{name}</div>
        <div><span style='font-weight:regular;font-size:11px'>{name2}</br></div>"
        )
      }
    ) %>%
    fmt_number(
      columns = c("past_results_cumulative", "past_targets", 
                  "present_results_cumulative", "present_targets",
                  "present_z_aresults"),
      decimal = 0
      ) %>% 
    tab_spanner(
      label = glue::glue("{past_fy}"),
      columns = contains("past")
    ) %>% 
    tab_spanner(
      label = glue::glue("{present_fy}"),
      columns = contains("present")
    ) %>% 
    tab_spanner(
      label = glue::glue("{present_qtr}"),
      columns = contains("_z_")
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")), 
      locations = cells_column_spanners(spanners = everything())
    ) %>% 
    cols_label(
      indicator2 = " ",
      past_results_cumulative = "results", 
      past_targets = "targets",
      past_targets_achievement = "achv",
      present_results_cumulative = "results",
      present_targets = "targets", 
      present_targets_achievement = "achv",
      present_tint_achv = " ",
      present_z_aresults = "results",
      present_z_change = "change",
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
    tab_source_note(
      source_note = md("*ALL OTHER AGENCIES* based on aggregates excluding de-duplication.")
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
    tab_options(
      source_notes.font.size = 10,
      table.font.size = 12, 
      data_row.padding = px(5),
      ...) 
}
