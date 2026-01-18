# clean up
rm(list=ls())

library(tidyverse)
library(eurostat)
library(giscoR)


# get information about the datasets in eurostat database
eurostat_database <- get_eurostat_toc() 

# keep only the information about tables and datasets which contain data by NUTS
eurostat_database <- eurostat_database %>% 
  filter(type %in% c(#"table", 
                     "dataset"
                     )) %>%
  filter(str_detect(title, regex("nuts", ignore_case = TRUE)))

# get actual data based on dataset id
dataset <- get_eurostat(id="lfst_r_lfe2eftpt")

names(dataset)

# load NUTS geometries
nuts <- giscoR::gisco_get_nuts(resolution = "20")

# combine data and geometries into an sf object
dataset <- left_join(dataset, nuts)
dataset <- st_as_sf(dataset)

sub_dataset <- dataset %>% 
  mutate(YEAR = substr(TIME_PERIOD, 1, 4)) %>%
  filter(LEVL_CODE == 2) %>%
  filter(YEAR == 2010)

# values is always the relevant variable
# but problem when by multiple other variables, cant select automatically










