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

cntry_ou <- list.files(path = si_path(type="path_msd"),
                        pattern = "Structured_.*_OU_IM.*_20210319_v2_1.zip",
                        full.names = TRUE) %>%
  sort() %>%
  last() %>%
  read_msd()


# MER Data Munge ---------------------------------------------------------------------------------

briefer <- cntry_ou %>%
  #rename(countryname = countryname) #Not sure if this is what I need to do?
  filter(fiscal_year %in% c("2021","2020", "2019"),
         indicator %in% c("HTS_TST", "HTS_TST_POS", "PrEP_NEW", "TX_CURR", "TX_NEW", "VMMC_CIRC"),
         standardizeddisaggregate %in% c("Total Numerator"),
        #"Age/Sex/HIVStatus", "Modality/Age/Sex/Result", "KeyPop/Result", 
        #"Age/Sex/ARVDispense/HIVStatus", "PregnantOrBreastfeeding/HIVStatus",
        #"KeyPop/HIVStatus", "Age Aggregated/Sex/HIVStatus", "Modality/Age Aggregated/Sex/Result", "Age/Sex",
         #"KeyPopAbr", "TechFollowUp>14days/Sex", "Technique/Sex", "TechFollowUp/Sex"),
         !fundingagency %in% c("Dedup"),
         !primepartner %in% c("TBD")) %>%
  glamr::clean_agency() %>%
  group_by(fiscal_year, indicator, fundingagency) %>%
  summarise_at(vars(targets:cumulative),sum,na.rm=TRUE) %>%
  ungroup() %>%
  select(-c(qtr1:qtr4)) %>%
  group_by(fiscal_year, indicator, fundingagency) %>%
  mutate(
    achievement = (cumulative/targets)) %>%
  ungroup()
#need to group anything not CDC and USAID into Other
  
  
  #disagg<-briefer %>%
  #   filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "PrEP_NEW", "TX_CURR", "TX_NEW", "VMMC_CIRC")) %>%
  #   distinct(standardizeddisaggregate)
  #   print(disagg)
    
    #GT!!!!!!!!
    
as_tibble(briefer)

# Make a display table with the `islands_tbl`
# table; put a heading just above the column labels

briefer_tbl <-
  tibble(
    Indicator = briefer,
    fiscal_year = briefer,
    Achievement = briefer,
    Fundingagency = briefer
  ) %>%
  arrange(desc(achievement))

# Display the table
briefer_tbl

gt_tbl <- 
  gt_tbl %>%
  tab_header(
    title = "Large Landmasses of the World",
    subtitle = "The top ten largest are presented"
  )

# Show the gt Table
gt_tbl

# Use markdown for the heading's `title` and `subtitle` to
# add bold and italicized characters
gt(islands_tbl[1:2,]) %>%
  tab_header(
    title = md("**Large Landmasses of the World**"),
    subtitle = md("The *top two* largest are presented")
  )
  
  



