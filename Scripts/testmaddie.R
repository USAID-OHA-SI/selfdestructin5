#Maddie Medina
#Mission Director Briefers
#04-30-21

# library(glamr)
# si_setup()
# install.packages("glitr")
#devtools::install_github("USAID-OHA-SI/glitr")
#folderpath <- "~/Source_Sans_Pro"
#font_import(folderpath)
#folderpath
#si_path()

install.packages("devtools")
devtools::install_github("ICPI/ICPIutilities")

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
         #countryname == "Tanzania",
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


 
## Table
  #select 1 country
  cntry_sel <- "Tanzania"


  table_data <- briefer %>%
    filter(countryname == cntry_sel)
  
  
#TO DO Table
  #conditional format with color FY19+FY20 (Q4) = help
  #conditional format with color FY21 (current year, quarter) - help
    ICPIutilities::identifypd(cntry_ou) #oh now i see
  #convert all the font to Source Sans Pro - definitely help
  #headers and agencies to bold
    #HOW do i get fy21 to not override 
    #get rid of indicator at top

    #turn this into a function
    
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
                 "FY21\nTargets" = "FY21") %>% 
      cols_width(
        vars(indicator) ~ px(140),
        everything() ~ px(80)
      ) %>% 
        # tab_style(
        #   style = cell_borders(
        #   sides = "right",
        #   weight = px(1),
        # ),
        # locations = cells_body(
        #   columns = everything(),
        #   rows = everything()
        # ))
      # tab_style(
      #   style = cells_data(
      #     weight = 40, #help - maybe not cells data
      #   ),
      #   locations = cells_data(
      #     columns = everything(),
      #     rows = everything()
      #   )) %>%
      tab_style(style = cell_fill(color = trolley_grey_light, alpha = 0.75),
                locations = cells_body(
                  columns = matches("Ach"))) %>%
      # tab_style(style = cell_text(weight = "bold"),
      #             locations = cells_body(
      #               columns = everything(),
      #               rows = everything())) %>%
      fmt_percent(
        columns = matches("Ach"),
        decimals = 0
      ) %>%
      fmt_number(
        columns = matches("Tar|Res"),
        decimals = 0
      ) %>% 
      tab_header(title = "TANZANIA PERFORMANCE IN FY21 Q1") %>%
      tab_source_note("Source: DATIM MSD FY21Q1 2020-03-19")  %>%
      
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
        # fmt_number(columns = vars(`FY19\nResults`, `FY20\nResults`, `FY21\nResults`,
        #                           `FY19\nTargets`, `FY20\nTargets`, `FY21\nTargets`),
        #            drop_trailing_zeros = TRUE, sep_mark = "," )
      tab_options(
          table.font.names = "Source Sans Pro"
        )
      
      
  # here()
  # install.packages('webshot')
  library(webshot)
      gtsave(gt_tbl, here("briefer_TZA_table.pdf"))
      gtsave(gt_tbl, here("briefer_TZA_table.png"))
      
  # ----------------------
    
      # Agency order throughout 
      agency_order_shrt <- c("USAID", "CDC", "Other")
      # Grab VLC, VLC and TX_CURR & TX_MMD data
      agency_VLC <- 
        #ou_im %>% 
        country_ou %>% 
        clean_agency() %>% 
        mutate(agency = fct_lump(fundingagency, n = 2), 
               agency = fct_relevel(agency, agency_order_shrt)) %>% 
        filter(indicator %in% c("TX_CURR", "TX_PVLS"),
               standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
        mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
        group_by(fiscal_year, agency, indicator) %>% 
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
        ungroup() %>% 
        reshape_msd() %>% 
        pivot_wider(names_from = indicator,
                    values_from = value) %>% 
        arrange(agency, period) %>% 
        mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period),
               VLS = TX_PVLS / TX_PVLS_D) %>%
        mutate(fy = case_when(
          period == "FY19Q4" ~ "FY19 Achievement",
          period == "FY20Q4" ~ "FY20 Achievement",
          period == "FY21Q1" ~ "FY21 Achievement",
          TRUE ~ NA_character_
        )) %>% 
        filter(!is.na(fy)) %>% 
        select(agency, VLC, VLS, fy) %>% 
        pivot_longer(cols = c(VLC, VLS),
                     names_to = "indicator", 
                     values_to = "value") %>% 
        pivot_wider(names_from = fy, values_from = value)
      
      agency_mmd <- 
        country_ou %>% 
        clean_agency() %>% 
        mutate(agency = fct_lump(fundingagency, n = 2), 
               agency = fct_relevel(agency, agency_order_shrt)) %>% 
        filter(indicator == "TX_CURR",
               disaggregate == "Age/Sex/ARVDispense/HIVStatus",
               otherdisaggregate %in% c("ARV Dispensing Quantity - 6 or more months", "ARV Dispensing Quantity - 3 to 5 months")) %>% 
        group_by(fiscal_year, agency, indicator, otherdisaggregate) %>% 
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
        mutate(indicator = case_when(
          str_detect(otherdisaggregate, "3 to 5") ~ "TX_MMD3+",
          str_detect(otherdisaggregate, "6 or more") ~ "TX_MMD6+",
          TRUE ~ NA_character_
        )) %>% 
        select(-otherdisaggregate) %>% 
        reshape_msd() %>% 
        mutate(fy = case_when(
          period == "FY19Q4" ~ "FY19 Achievement",
          period == "FY20Q4" ~ "FY20 Achievement",
          period == "FY21Q1" ~ "FY21 Achievement",
          TRUE ~ NA_character_
        )) %>% 
        filter(!is.na(fy)) %>% 
        select(-period, -period_type) %>% 
        pivot_wider(names_from = fy, values_from = value) 
      
     bind_rows(agency_VLC, agency_mmd)  
      
     
     
     
      ##function stuff
      
      map_apr <- function(df_apr, ou) {
        
        print(ou)
        
        print(map)
        
        return(map)
      }
      
      cntry_2_peds %>%
        filter(!str_detect(operatingunit, " Region$"),
               !is.na(APR)) %>%
        distinct(operatingunit) %>%
        pull() %>%
        nth(25) %>%
        map(.x, .f = ~map_apr(df_apr = cntry_2_peds, ou = .x))
      
      
      
      
    
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
      