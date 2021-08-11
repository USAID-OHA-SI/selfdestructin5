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

# install.packages("devtools")
# devtools::install_github("ICPI/ICPIutilities")

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

 
 # ----------------------
 
 # Agency order throughout 
 agency_order_shrt <- c("USAID", "CDC", "Other")
 # Grab VLC, VLC and TX_CURR & TX_MMD data
 agency_VLC <- 
   #ou_im %>% 
   cntry_ou %>% 
   clean_agency() %>% 
   mutate(fundingagency = fct_lump(fundingagency, n = 2), 
          fundingagency = fct_relevel(fundingagency, agency_order_shrt)) %>% 
   filter(indicator %in% c("TX_CURR", "TX_PVLS"),
          standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
   mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
   group_by(fiscal_year, fundingagency, indicator) %>% 
   summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
   ungroup() %>% 
   reshape_msd() %>% 
   pivot_wider(names_from = indicator,
               values_from = value) %>% 
   arrange(fundingagency, period) %>% 
   mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period),
          VLS = TX_PVLS / TX_PVLS_D) %>%
   mutate(fy = case_when(
     period == "FY19Q4" ~ "FY19 Achievement",
     period == "FY20Q4" ~ "FY20 Achievement",
     period == "FY21Q1" ~ "FY21 Achievement",
     TRUE ~ NA_character_
   )) %>% 
   filter(!is.na(fy)) %>% 
   select(fundingagency, VLC, VLS, fy) %>% 
   pivot_longer(cols = c(VLC, VLS),
                names_to = "indicator", 
                values_to = "value") %>% 
   pivot_wider(names_from = fy, values_from = value)
 
 agency_mmd <- 
   cntry_ou %>% 
   clean_agency() %>% 
   mutate(fundingagency = fct_lump(fundingagency, n = 2), 
          fundingagency = fct_relevel(fundingagency, agency_order_shrt)) %>% 
   filter(indicator == "TX_CURR",
          disaggregate == "Age/Sex/ARVDispense/HIVStatus",
          otherdisaggregate %in% c("ARV Dispensing Quantity - 6 or more months", "ARV Dispensing Quantity - 3 to 5 months")) %>% 
   group_by(fiscal_year, fundingagency, indicator, otherdisaggregate) %>% 
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
 
 test1 <- bind_rows(agency_VLC, agency_mmd) 
 
 test2 <- bind_rows(agency_VLC, agency_mmd, briefer)


 
# ## Table
#   #select 1 country
   cntry_sel <- "Tanzania"
# 
# 
  table_data <- test %>%
    filter(countryname == cntry_sel)
  
  
#TO DO Table
    ICPIutilities::identifypd(cntry_ou) 
  #convert all the font to Source Sans Pro 
  #headers and agencies to bold
    #HOW do i get fy21 to not override 
    #get rid of indicator at top
    #turn this into a function
    
    gt_table <- function(df_brief, ou) {
      
      print(ou)
      
      df_brief <- df_brief %>% filter(countryname == ou)
      
      # cntry_sel <- ou
      # 
      # df_brief %>%
      # filter(ou == cntry_sel)
      
      #countryname = ou
      
      gt_tbl <- df_brief %>% #combined dfs
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
      tab_style(style = cell_fill(color = trolley_grey_light, alpha = 0.75),
                locations = cells_body(
                  columns = matches("Ach"))) %>%
        fmt_percent(
          columns = matches("Ach"),
          decimals = 0
        ) %>%
        fmt_number(
          columns = matches("Tar|Res"),
          decimals = 0
        ) %>% 
        tab_header(
          title = (paste0(ou, " | PERFORMANCE IN FY21 Q1"))) %>%
         # title = ("TANZANIA | PERFORMANCE IN FY21 Q1")) %>%
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
      
      
      print(gt_tbl)
      
      return(gt_tbl)
    }
    

  
      ##function stuff
      #error: all values provided in 'groups' must correspond to group indices

      
test2 %>%
        filter(
          #!str_detect(countryname, " Region$"),
               !is.na(`FY19 Achievement`)) %>%
        distinct(countryname) %>%
        pull() %>%
        nth(21) %>%
        map(.x, .f = ~gt_table(df_brief = test2, ou = .x))
      
      `gtsave(gt_tbl, here("briefer_TZA_table.pdf"))
      gtsave(gt_tbl, here("briefer_TZA_table.png"))
      
      
      #  library(webshot)
      
      
    
# IGNORE

    
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
      