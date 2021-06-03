add_achv_colors <- function(gt_table) {
  
  gt_table %>% 
    
  # Add ACHV colors to all non-TX indicators
    tab_style(style = list(cell_fill(color = old_rose_light, alpha = 0.75)),
            locations = cells_body(
              columns = c(FY21APR),
              rows = FY21APR < 0.25 & indicator != "TX_CURR")
  ) %>% 
    tab_style(style = list(cell_fill(color = burnt_sienna_light, alpha = 0.75)),
              locations = cells_body(
                columns = c(FY21APR),
                rows = FY21APR >= 0.25 & FY21APR < 0.4 & indicator != "TX_CURR")
    ) %>% 
    tab_style(style = list(cell_fill(color = scooter_light, alpha = 0.75)),
              locations = cells_body(
                columns = c(FY21APR),
                rows = FY21APR >= 0.4 & FY21APR < 0.6 & indicator != "TX_CURR")
    ) %>% 
    tab_style(style = list(cell_fill(color = trolley_grey_light, alpha = 0.75)),
              locations = cells_body(
                columns = c(FY21APR),
                rows = FY21APR >= 0.6 & indicator != "TX_CURR")
              ) %>% 
    
  # NOW TX_CURR
  tab_style(style = list(cell_fill(color = old_rose_light, alpha = 0.75)),
            locations = cells_body(
              columns = c(FY21APR),
              rows = FY21APR < 0.75 & indicator == "TX_CURR")
  ) %>% 
    tab_style(style = list(cell_fill(color = burnt_sienna_light, alpha = 0.75)),
              locations = cells_body(
                columns = c(FY21APR),
                rows = FY21APR >= 0.75 & FY21APR < 0.9 & indicator == "TX_CURR")
    ) %>% 
    tab_style(style = list(cell_fill(color = scooter_light, alpha = 0.75)),
              locations = cells_body(
                columns = c(FY21APR),
                rows = FY21APR >= 0.9 & FY21APR < 1.1 & indicator == "TX_CURR")
    ) %>% 
    tab_style(style = list(cell_fill(color = trolley_grey_light, alpha = 0.75)),
              locations = cells_body(
                columns = c(FY21APR),
                rows = FY21APR >= 1.1 & indicator == "TX_CURR")
    )
  
  
}
