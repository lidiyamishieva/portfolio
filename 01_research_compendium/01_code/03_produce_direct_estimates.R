############## BEGIN HEADER ############## 
##
## script name: 03_produce_direct_estimates.R
## input: ess11_analysis_dataset.csv
## output: pop_sizes_eu_2023.csv, all_direct_estimates.csv
## author: Lidiya Mishieva
## date: 19 February 2026
##
############### END HEADER ###############

# 00 setup -----

# clean up workspace
rm(list=ls())

# load packages
library(tidyverse)
library(eurostat)
library(sf)
library(sae)

# 01 import data -----

# import analysis dataset (survey data)
data_analysis <- read_csv("00_data/ii_processed/ess11_analysis_dataset.csv")

# import population sizes from eurostat
dataset_name <- c("Population on 1 January by age group, sex and NUTS 3 region")
search_result <- search_eurostat(dataset_name, type = "dataset")
dataset_id <- search_result$code[1]
estat <- get_eurostat(dataset_id, time_format = "num", stringsAsFactors = TRUE)

# 02 data transformations -----

# min max scale the factor scores

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

data_analysis <- data_analysis %>%
  mutate(across(all_of(c("F1W", "F2W", "F3W", "F4W", "F5W")), ~ range01(.x ), .names = "{.col}_scaled"))

# filter population sizes by year and age to match
# the population surveyed in the ESS (individuals above 15)
estat2 <- estat %>%
  filter(!age %in% c("TOTAL", "UNK", "Y_LT5", "Y5-9", "Y10-14", "Y_GE85")) %>%
  filter(sex == "T") %>% 
  filter(TIME_PERIOD == 2023) %>%
  dplyr::select(c("geo", "values")) %>%
  rename(pop_size = values, region = geo) %>%
  group_by(region) %>%
  summarise(pop_size = sum(pop_size))

# merge administrative data and survey data
data_analysis <- data_analysis %>% left_join(estat2, by="region")

# 03 produce direct estimates (Horvitz-Thompson) -----

# collect sample sizes per area
data_analysis <- data_analysis %>% group_by(region) %>% mutate(n = n())
# adjust design weights to be the inverse of the first-order probability to be sampled in the area
data_analysis$dweight_adj <- (data_analysis$pop_size/data_analysis$n)*data_analysis$dweight

# create a data frame that contatins domain sizes for each region
N <- data_analysis %>% 
  group_by(region) %>% 
  summarise(mean(pop_size)) %>% 
  rename(pop_size = `mean(pop_size)`) %>% 
  drop_na() %>%
  as.data.frame()

# produce direct estimates for all factors
f1_weighted <- direct(y=data_analysis$F1W_scaled, sweight = data_analysis$dweight_adj, domsize = as.data.frame(N[, c("region", "pop_size")]), dom=data_analysis$region)
f2_weighted <- direct(y=data_analysis$F2W_scaled, sweight = data_analysis$dweight_adj, domsize = as.data.frame(N[, c("region", "pop_size")]), dom=data_analysis$region)
f3_weighted <- direct(y=data_analysis$F3W_scaled, sweight = data_analysis$dweight_adj, domsize = as.data.frame(N[, c("region", "pop_size")]), dom=data_analysis$region)
f4_weighted <- direct(y=data_analysis$F4W_scaled, sweight = data_analysis$dweight_adj, domsize = as.data.frame(N[, c("region", "pop_size")]), dom=data_analysis$region)
f5_weighted <- direct(y=data_analysis$F5W_scaled, sweight = data_analysis$dweight_adj, domsize = as.data.frame(N[, c("region", "pop_size")]), dom=data_analysis$region)

# combine all estimates

names(f1_weighted)[1] <- "region"
names(f1_weighted)[3:5] <- paste0("F1_wgt_", names(f1_weighted)[3:5])

for(i in 1:5){
  df_w <- get(paste0("f", i, "_weighted"))
  names(df_w)[1] <- "region"
  names(df_w)[3:ncol(df_w)] <- 
    paste0("F", i, "_wgt_", names(df_w)[3:ncol(df_w)])
  assign(paste0("f", i, "_weighted"), df_w)
}

all_direct_estimates <- reduce(
  list(f1_weighted, f2_weighted, f3_weighted, f4_weighted, f5_weighted),
  function(x, y) bind_cols(x, y[, -c(1,2)])
)

# 04 export data -----

write_csv(estat2, "00_data/ii_processed/pop_sizes_eu_2023.csv")
write_csv(all_direct_estimates, "00_data/ii_processed/all_direct_estimates.csv")