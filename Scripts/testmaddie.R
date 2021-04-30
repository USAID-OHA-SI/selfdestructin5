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

# MER Data

load_secrets()

# MER Site level import --------------------------------------------------------------------

cntry_ou <- list.files(path = si_path(type="path_msd"),
                        pattern = "Structured_.*_OU_IM.*_20210319_v2_1.zip",
                        full.names = TRUE) %>%
  sort() %>%
  last() %>%
  read_msd()

# GEO DATA ------------------------------------------------------------

gis_vc_sfc <- return_latest(
  si_path(type="path_vector"),
  pattern = "Vc.*.shp$",
  recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove(".shp")) %>%
  map(read_sf)
#OR
gis_vc_sfc <- list.files(
  si_path(type="path_vector"),
  pattern = "Vc.*.shp$",
  recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove(".shp")) %>%
  map(read_sf)


# MER Data Munge ---------------------------------------------------------------------------------

cntry <- briefer %>%
  rename(countryname = countryname) #Not sure if this is what I need to do?
  filter(fiscal_year %in% c("2021","2020", "2019"),
         indicator %in% c("HTS_TST", "HTS_TST_POS", "PrEP_NEW", "TX_CURR", "TX_NEW", "VMMC_CIRC"),
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         !fundingagency %in% c("Dedup"),
         !primepartner %in% c("TBD")) %>%
  glamr::clean_agency() %>%
  group_by(fiscal_year, countryname) %>%
  summarise_at(vars(targets:cumulative),sum,na.rm=TRUE) %>%
  ungroup() %>%
  select(-c(qtr1:qtr4)) %>%
  #reshape_msd(clean = TRUE) %>%
  group_by(countryname, fundingagency) %>%
  mutate(APR = (cumulative/targets)) %>%
  ungroup() %>%
  mutate(primepartner = paste0(#fundingagency, "-",
    primepartner))
  
  
  
  




