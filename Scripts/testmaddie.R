#Maddie Medina
#Mission Director Briefers
#04-30-21

# library(glamr)
# 
# si_setup()
# install.packages("glitr")

#devtools::install_github("USAID-OHA-SI/glitr")

folderpath <- "~/Source_Sans_Pro"
font_import(folderpath)

folderpath

si_path()

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
library(ggtext)
library(ggrepel)
library(tidytext)
library(ggflags)
library(glue)
library(gt)
library(tidyselect)
library(devtools)
library(usethis)








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
    achievement = (cumulative/targets))

  #?across
  


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
                names_glue = "{fiscal_year}\n{.value}",
                #names_sort = TRUE,
                values_from = c(Targets, Results, Achievement)
                )

  #reordering
 briefer <- briefer %>% 
      select(countryname, indicator, fundingagency, ends_with("Achievement"),
             ends_with("Results"), everything())
 

## TO DO
  #reorder indicators (order string variables in R) - DONE
  #reorder agency - done
 #reorder columns #reorder columns - done

 
## Table
  #select 1 country
#Need help with this - i just ended up filtering in the cleaning step but would still like to know how to do this
  cntry_sel <- "Tanzania"


  table_data <- briefer %>%
    filter(countryname == cntry_sel)
  
  
#TO DO Table
  #add commas & percents - done
  #conditional format with color FY19+FY20 (Q4) = help
  #conditional format with color FY21 (current year, quarter) - help
    ICPIutilities::identifypd(cntry_ou) #oh now i see
  #convert all the font to Source Sans Pro - definitely help
  #headers and agencies to bold - Help
  
    #gt
    #need to fix: sourcesanspro
    #HOW do i get fy21 to not override - try combining the two lines
    #need to understand the brackets
    
    
    gt_tbl <- briefer %>%
      gt(groupname_col = "fundingagency") %>% 
      row_group_order(
        groups = c("USAID", "CDC", "DOD")
      ) %>%
      cols_hide(vars(countryname)) %>% 
      tab_spanner(label = md("**Achievement**"), 
                  columns = matches("Achievement")) %>%  
      tab_spanner(label = md("**Results**"), 
                  columns = matches("Results")) %>%  
      tab_spanner(label = md("**Targets**"), 
                  columns = matches("Targets")) %>% 
      cols_label("FY19\nAchievement" = "FY19",
                 "FY20\nAchievement" = "FY20",
                 "FY21\nAchievement" = "FY21",
                 "FY19\nResults" = "FY19",
                 "FY20\nResults" = "FY20",
                 "FY21\nResults" = "FY21",
                 "FY19\nTargets" = "FY19",
                 "FY20\nTargets" = "FY20",
                 "FY21\nTargets" = "FY21")
    
    
    gt_tbl <- gt_tbl %>% 
      cols_width(
        vars(indicator) ~ px(140),
        everything() ~ px(80)
      )%>% 
        tab_style(
          style = cell_borders(
          sides = "right",
          weight = px(1),
        ),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        ))
      # tab_style(
      #   style = cells_data(
      #     weight = 40, #help - maybe not cells data
      #   ),
      #   locations = cells_data(
      #     columns = everything(),
      #     rows = everything()
      #   )) %>%
    
    #get rid of indicator at top
    
    
    
      gt_tbl <- gt_tbl %>% 
      tab_style(style = cell_fill(color = usaid_lightgrey),
                locations = cells_body(
                  columns = matches("Ach"))) %>%
      tab_style(style = cell_text(weight = "bold"),
                  locations = cells_body(
                    columns = everything(),
                    rows = everything())) %>%
        tab_style(style = cell_text(weight = "bold"),
                  locations = cells_title(
                    groups = c("title"))) %>%
      tab_style(style = list(cell_fill(color = old_rose_light, alpha = 0.55)),
                locations = cells_body(
                  columns = vars(`FY20\nAchievement`),
                  rows = `FY20\nAchievement` < .75 & indicator == "TX_CURR")) %>%
      tab_style(style = list(cell_fill(color = old_rose_light, alpha = 0.55)),
                locations = cells_body(
                  columns = vars(`FY20\nAchievement`),
                  rows = `FY20\nAchievement` < .25 & indicator != "TX_CURR")) %>%
      tab_style(style = list(cell_fill(color = old_rose_light, alpha = 0.55)),
                  locations = cells_body(
                    columns = vars(`FY21\nAchievement`),
                    rows = `FY21\nAchievement` < .15)) %>%
      tab_header(title = "TANZANIA PERFORMANCE IN FY21 Q1") %>%
      tab_source_note("Source: DATIM MSD FY21Q1 2020-03-19") %>%   
      fmt_percent(
        columns = vars(`FY19\nAchievement`, `FY20\nAchievement`, `FY21\nAchievement`),
        decimals = 0
      ) %>%
        fmt_number(columns = vars(`FY19\nResults`, `FY20\nResults`, `FY21\nResults`,
                                  `FY19\nTargets`, `FY20\nTargets`, `FY21\nTargets`),
                   drop_trailing_zeros = TRUE, sep_mark = "," )
     #hmmmm......... 
      gt_tbl %>% 
        tab_options(
          table.font.names = "Source Sans Pro"
        )
      
      
  # here()
  # install.packages('webshot')
  library(webshot)
      gtsave(gt_tbl, here("briefer_TZA_table.pdf"))
      gtsave(gt_tbl, here("briefer_TZA_table.png"))
      
      # ----------------------
    
    
# IGNORE
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


#GT code for myself - to be ignored
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
    
    # tab_style(style = cell_fill(color = pal[1]), ## defining the what (the 4th value of the pal object)
    #           locations = cells_body(           ## telling it where (ie, the body of a cell)
    #             columns = vars(`FY20 Achievement`), ## which col this refers to (note `vars()`)
    #             rows = `FY20 Achieved` < .75)) %>% 
    
    #old stuff
      #reordering column names
      # note for myself (please ignore):
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
      