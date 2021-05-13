#Maddie Medina
#Mission Director Briefers
#04-30-21

library(glamr)

si_setup()


library(extrafont)
library(tidyverse)
library(sf)
library(glitr)
library(gisr)
library(here)
library(scales)
library(patchwork)
library(ICPIutilities)
library(glamr)
library(janitor)
library(rnaturalearth)
library(rnaturalearthhires)
library(ggtext)
library(ggrepel)
library(tidytext)
library(ggflags)
library(glue)
library(gt)

# MER Data

load_secrets()

# MER Site level import --------------------------------------------------------------------

cntry_ou <-  si_path() %>% 
  return_latest("OU_IM") %>% 
  read_msd()   
  #read_rds()


# MER Data Munge ---------------------------------------------------------------------------------


briefer <- cntry_ou %>%
  filter(fiscal_year %in% c(2021, 2020, 2019),
         countryname == "Tanzania",
         indicator %in% c("HTS_TST", "HTS_TST_POS", "PrEP_NEW", "TX_CURR", "TX_NEW", "VMMC_CIRC"),
         standardizeddisaggregate %in% c("Total Numerator"),
         !fundingagency %in% c("Dedup")) %>%
  glamr::clean_agency() %>%
  group_by(fiscal_year, countryname, indicator, fundingagency) %>%
  summarise_at(vars(targets:cumulative),sum,na.rm=TRUE) %>%
  ungroup() %>%
  select(-c(qtr1:qtr4)) %>%
  mutate(
    achievement = (cumulative/targets), 
    across(c(cumulative, targets), comma),
    across(c(achievement), percent, .1))

  #?across
  #across(c(achievement), ~ percent(.)/100))
  
  

#clean clean up FY
briefer <- briefer %>%
    mutate(fiscal_year = glue("FY{str_sub(fiscal_year, start = 3)}"))

#reshape
  briefer <- briefer %>%
    arrange(indicator) %>%
    #reorder()
    rename(results = cumulative) %>% 
    rename_with(.cols = c(targets, results, achievement), str_to_sentence) %>% 
    pivot_wider(names_from = fiscal_year,
                # names_sep = " ",
                names_glue = "{fiscal_year}<br>{.value}",
                #names_sort = TRUE,
                values_from = c(Targets, Results, Achievement)
                )


 briefer <- briefer[, c("countryname", "indicator", "fundingagency", 
                        "FY19<br>Results", "FY19<br>Targets", "FY19<br>Achievement",
                        "FY20<br>Results", "FY20<br>Targets", "FY20<br>Achievement",
                        "FY21<br>Results", "FY21<br>Targets", "FY21<br>Achievement")]
    
#briefer <- briefer %>% pivot_longer(cols = fundingagency,
                                    #values_to = "indicator")
 

## TO DO
  #reorder indicators (order string variables in R) - DONE
  #reorder columns #reorder columns:DONE
 # Alternative:
 # df <- df %>%
 #   dplyr::relocate(`FY20 Total`, .before = `FY21 Targets`) %>% 
 #   dplyr::relocate(`FY20 Achieved`, .before = `FY21 Targets`) %>%
 #   dplyr::relocate(`Fy20 Targets`, .after = `FY20 Total`) %>% 
 #   mutate(indicator = fct_relevel(indicator, "HTS_TST",
 #                                  "HTS_TST_POS",
 #                                  "TX_NEW",
 #                                  "TX_CURR",
 #                                  "TX_NET_NEW",
 #                                  "VMMC_CIRC",
 #                                  "PrEP_NEW")) %>% 
 # arrange(indicator)
  
  #reorder agency - NEED TO REORDER THEM
  
## Table
  #select 1 country
#WHAT am I doing wrong here
  # cntry_sel <- "Tanzania"
  # 
  # 
  # table_data <- briefer %>% 
  #   filter(cntry_sel)
  
  
#TO DO Table
  #add commas & percents - DONE
  #conditional format with color FY19+FY20 (Q4)
  #conditional format with color FY21 (current year, quarter)
    ICPIutilities::identifypd(cntry_ou)
  #convert all the font to Source Sans Pro
  #headers and agencies to bold
  
    
# as_tibble(briefer)
# 
# m# Make a display table with the `islands_tbl`
# # table; put a heading just above the column labels
# 
# briefer_tbl <-
#   tibble(
#     Indicator = briefer,
#     fiscal_year = briefer,
#     Achievement = briefer,
#     Fundingagency = briefer
#   ) %>%
#   arrange(desc(achievement))
# 
# # Display the table
# briefer_tbl
# 
# gt_tbl <- 
#   gt_tbl %>%
#   tab_header(
#     title = "Large Landmasses of the World",
#     subtitle = "The top ten largest are presented"
#   )
# 
# # Show the gt Table
# gt_tbl
# 
# # Use markdown for the heading's `title` and `subtitle` to
# # add bold and italicized characters
# gt(islands_tbl[1:2,]) %>%
#   tab_header(
#     title = md("**Large Landmasses of the World**"),
#     subtitle = md("The *top two* largest are presented")
#   )
#   
#   
# 
# reformat_apr_col <- function(df, apr, rslt, tgt, var) {
#   df %>% 
#     mutate("{{ var }}" := paste(percent({{apr}}, 1), 
#                                 comma({{rslt}}, accuracy = 1, scale = 1e-3, suffix = "K"), 
#                                 comma({{tgt}}, accuracy = 1, scale = 1e-3, suffix = "K")))
# }


#gt
  #need to fix: sourcesanspro
    
    
    briefer %>%
    gt(groupname_col = "fundingagency",
       rowname_col = "indicator") %>% 
      tab_options(
        table.font.names = "Source Sans Pro"
      ) %>% 
      cols_width(
        vars(indicator) ~ px(140),
        everything() ~ px(80)
      )%>% 
      tab_style(
        style = cell_borders(
          sides = "right",
          weight = px(1.5),
        ),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        ))
      # fmt_number(
      #   columns = vars(
      #     `FY20 Q1`,
      #     `FY20 Q2`,
      #     `FY20 Q3`,
      #     `FY20 Q4`,
      #     `FY20 Total`,
      #     `Fy20 Targets`,
      #     `FY21 Targets`),
      #   decimals = 0
      # ) %>% 
      # fmt_percent(
      #   columns = vars(`FY20 Achieved`),
      #   decimals = 0
      # )
    
    
    
    