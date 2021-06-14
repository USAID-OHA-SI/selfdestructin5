
# Start of code to add achievment shapes
# Break down into two functions
# 1) function to return the correct acheivement color for indicator, quarter, and achievment
# 2) function to return the appropriate font awesome icon depending on indicator, quarter and achievement
# The latter should be consistent with the former

add_achv_shapes <- function(gt_table) {
  gt_table %>%
    text_transform(
      locations = cells_body(
        columns = c(circle),
        rows = circle < 0.25 & indicator != "TX_CURR"
      ),
      fn = function(x) {
        md(glue::glue("{fontawesome::fa('exclamation-circle', fill = old_rose_light, fill_opacity  = 0.75)} "))
      }
    ) %>%
    text_transform(
      locations = cells_body(
        columns = c(circle),
        rows = circle >= 0.25 & FY21APR < 0.4 & indicator != "TX_CURR"
      ),
      fn = function(x) {
        md(glue::glue("{fontawesome::fa('minus-circle', fill = burnt_sienna_light, fill_opacity  = 0.75)} "))
      }
    ) %>%
    text_transform(
      locations = cells_body(
        columns = c(circle),
        rows = circle >= 0.4 & FY21APR < 0.6 & indicator != "TX_CURR"
      ),
      fn = function(x) {
        md(glue::glue("{fontawesome::fa('check-circle', fill = scooter_light, fill_opacity  = 0.75, stroke_opacity = 1)} "))
      }
    ) %>%
    text_transform(
      locations = cells_body(
        columns = c(circle),
        rows = circle >= 0.6 & indicator != "TX_CURR"
      ),
      fn = function(x) {
        md(glue::glue("{fontawesome::fa('plus-circle', fill = trolley_grey_light, stroke_opacity = 1)} "))
      }
    ) %>% 
    
  # TX_CURR ###
    text_transform(
      locations = cells_body(
        columns = c(circle),
        rows = circle < 0.75 & indicator == "TX_CURR"
      ),
      fn = function(x) {
        md(glue::glue("{fontawesome::fa('exclamation-circle', fill = old_rose_light, fill_opacity  = 0.75)} "))
      }
    ) %>%
    text_transform(
      locations = cells_body(
        columns = c(circle),
        rows = circle >= 0.75 & FY21APR < 0.9 & indicator == "TX_CURR"
      ),
      fn = function(x) {
        md(glue::glue("{fontawesome::fa('minus-circle', fill = burnt_sienna_light, fill_opacity  = 0.75)} "))
      }
    ) %>%
    text_transform(
      locations = cells_body(
        columns = c(circle),
        rows = circle >= 0.9 & FY21APR < 1.1 & indicator == "TX_CURR"
      ),
      fn = function(x) {
        md(glue::glue("{fontawesome::fa('check-circle', fill = scooter_light, fill_opacity  = 0.75)} "))
      }
    ) %>%
    text_transform(
      locations = cells_body(
        columns = c(circle),
        rows = circle >= 1.1 & indicator == "TX_CURR"
      ),
      fn = function(x) {
        md(glue::glue("{fontawesome::fa('plus-circle', fill = trolley_grey_light)} "))
      }
    )
}
