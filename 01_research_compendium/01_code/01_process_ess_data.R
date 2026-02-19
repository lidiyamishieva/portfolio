############## BEGIN HEADER ############## 
##
## script name: 01_process_ess_data.R
## input: ESS11.csv
## output: ess11_dataset.csv, ess11_analysis_dataset_mplus_nocolnames.csv
## author: Lidiya Mishieva
## date: 19 February 2026
##
############### END HEADER ###############

# 00 setup -----

# clean up workspace
rm(list=ls())

# load packages
library(tidyverse)
library(sf)
library(giscoR)

# 01 import data -----

# import data from the 11th round of the ESS 
ess11 <- read_csv("00_data/i_raw/ESS11.csv")

# import NUTS regions
nuts <- bind_rows(
  gisco_get_nuts(nuts_level = 1, year = 2021, resolution = "10", cache = TRUE, update_cache = TRUE) %>% mutate(level = 1),
  gisco_get_nuts(nuts_level = 2, year = 2021, resolution = "10", cache = TRUE, update_cache = TRUE) %>% mutate(level = 2),
  gisco_get_nuts(nuts_level = 3, year = 2021, resolution = "10", cache = TRUE, update_cache = TRUE) %>% mutate(level = 3))

# 02 data transformations -----

# collect indicators to be used in the factor model later
indicators <- c(
  "ppltrst", "pplfair", "pplhlp", # interpersonal trust
  "sclmeet", "inprdsc", "sclact", # social relations
  "imbgeco", "imueclt", "imwbcnt", # openness
  "trstprl", "trstplt", "trstprt", # institutional trust
  "stfgov", "stfdem", "stfedu", "stfhlth" # legitimacy institutions 
)

ess11 <- ess11 %>%
  # extract individual and regional ids, weights, and indicators
  dplyr::select(c("idno", "cntry", "region",  "regunit", "dweight", "pweight", all_of(indicators))) %>%
  mutate(
    # create a unique identifier for individuals
    unique_id = paste0(cntry, idno),
    # recode missing values
    across(all_of(indicators), ~ ifelse(.x %in% c(77, 88, 99), NA_real_, .x)),
    sclact = ifelse(sclact %in% c(7, 8, 9), NA, sclact),
    # adjust NUTS ids to match the ones in ess11
    # FI -> FI1D4  (Kainuu) in ESS is FI1D8 in NUTS 16/21/24 
    # FI1D6 (Pohjois-Pohjanmaa) in ESS is FI1D9 in NUTS 16/21/24
    region = case_when(
      region == "FI1D4" ~ "FI1D8", 
      region == "FI1D6" ~ "FI1D9", 
      TRUE ~ region),
    # create variables containing all the available nuts regions 
    # for each respondent based on available information
    nuts1 = case_when(
      regunit == 1 ~ region,
      regunit %in% c(2, 3) ~ substr(region, 1, 3),
      TRUE ~ NA_character_),
    nuts2 = case_when(
      regunit == 2 ~ region,
      regunit == 3 ~ substr(region, 1, 4),
      TRUE ~ NA_character_),
    nuts3 = case_when(
      regunit == 3 ~ region,
      TRUE ~ NA_character_
    )
  ) %>%
  # drop Israel as it does not use NUTS
  filter(!cntry=="IL")

# 03 prepare data for import into mplus -----

# create numeric variables for nuts1 and rowid for later use in mplus, because mplus cannot process strings
ess11$nuts1_num <- as.numeric(as.factor(ess11$nuts1))
ess11$row_id <- 1:nrow(ess11)

# (listwise deletion if inficators are missing, and containing only subset of the variables)
ess11_mplus <- na.omit(ess11[c(indicators, "row_id", "nuts1_num")])

# 04 export data -----

write_csv(ess11, "00_data/ii_processed/ess11_dataset.csv")

# the data is exported without column names, because mplus cannot process strings
# the code used to import the data in mplus preserves the order of the columns in the data

# the order of the columns
names(ess11_mplus)

write.table(ess11_mplus, "02_mplus/ess11_analysis_dataset_mplus_nocolnames.csv", sep=",",  col.names=FALSE, row.names=FALSE)
