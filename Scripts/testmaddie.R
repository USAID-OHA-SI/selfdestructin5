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


# Global dataset
#double check to make sure there is no duplicate data
#MAY need to update with Q2
#check panorama to check on like guatemala and other countries that don't have PEDS data

cntry <- briefer_ou %>%
  filter(fiscal_year %in% c("2021","2020", "2019"),
         indicator == "TX_CURR",
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         !fundingagency %in% c("Dedup"),
         !primepartner %in% c("TBD")) %>%
  filter(!trendsfine %in% c("15-19","20-24","25-29",
                            "30-34","35-39","40-49","50+")) %>%
  glamr::clean_agency() %>%
  group_by(fiscal_year, operatingunit, snu1, countryname,
           snu1uid, primepartner, fundingagency) %>%
  summarise(across(starts_with("targ"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_msd(clean = TRUE) %>%
  select(-period_type) %>%
  group_by(operatingunit) %>%
  mutate(country_val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(operatingunit, primepartner, fundingagency) %>%
  mutate(primepartner_val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(operatingunit, snu1, snu1uid, #countryname,
           primepartner, fundingagency) %>%
  summarise(val = sum(val, na.rm = TRUE),
            share = val / first(country_val),
            primeshare = primepartner_val/first(country_val)) %>%
  ungroup() %>%
  mutate(primepartner = paste0(primepartner, " (", round(primeshare*100, 2),  "%",")"))



#counting mechs
cntry_peds %>% filter(operatingunit == "Kenya", fundingagency == "CDC") %>% count(primepartner, mech_code, mech_name) %>% arrange(primepartner) %>% prinf()

