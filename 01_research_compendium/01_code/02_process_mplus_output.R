############## BEGIN HEADER ############## 
##
## script name: 02_process_mplus_output.R
## input: ess11_dataset.csv, ess11_analysis_dataset_mplus_nocolnames_fscores_nuts1.csv
## output: ess11_analysis_dataset.csv
## author: Lidiya Mishieva
## date: 19 February 2026
##
############### END HEADER ###############

# 00 setup -----

# clean up workspace
rm(list=ls())

# load packages
library(tidyverse)
library(stringr)

# 01 import data -----

# import processed ess11 data
ess11 <- read_csv("00_data/ii_processed/ess11_dataset.csv")

# import factor scores computed using mplus
fscores <- read_csv("02_mplus/ess11_analysis_dataset_mplus_nocolnames_fscores_nuts1.csv")

# 02 data transformations -----

# fix csv spacing
fscores <- as.data.frame(str_split_fixed(fscores[[1]], ' +', 44))

# provide column names to the data frame

vars <- c(
  "PPLTRST", "PPLFAIR", "PPLHLP", 
  "SCLMEET", "INPRDSC", "SCLACT", 
  "IMBGECO", "IMUECLT", "IMWBCNT", 
  "TRSTPRL", "TRSTPLT", "TRSTPRT", 
  "STFGOV", "STFDEM", "STFEDU", "STFHLTH", 
  "row_id",
  "F1W", "F2W", "F3W", "F4W", "F5W", 
  "F1B", "F2B", "F3B", "F4B", "F5B",
  "B_PPLTRST", "B_PPLFAIR", "B_PPLHLP", 
  "B_SCLMEET", "B_INPRDSC", "B_SCLACT", 
  "B_IMBGECO", "B_IMUECLT", "B_IMWBCNT", 
  "B_TRSTPRL", "B_TRSTPLT", "B_TRSTPRT", 
  "B_STFGOV", "B_STFDEM", "B_STFEDU", "B_STFHLTH", 
  "NUTS1_NUM")

names(fscores) <- vars

fscores$row_id <- as.integer(fscores$row_id)

# for one subject the scores were not produced:
# which(!(ess11_mplus$row_id %in% fscores$row_id))
# ess11_mplus[11148,] # row_id==12718

# merge factor scores with the rest of the data
ess11 <- full_join(ess11, fscores)

# remove nuts1 and nuts2 variables, then listwise deletion
data_analysis <- na.omit(ess11[, !names(ess11) %in% c("nuts2", "nuts3")])

# only keep rows which contain factor scores
data_analysis <- left_join(data_analysis, ess11)

# change the order of variables
data_analysis <- relocate(data_analysis, nuts1, .before = nuts2)

# remove duplicates and unnecessary columns
data_analysis <- subset(data_analysis, select = -c(row_id, nuts1_num, NUTS1_NUM,
                                                   PPLTRST, PPLFAIR, PPLHLP, 
                                                   SCLMEET, INPRDSC, SCLACT, 
                                                   IMBGECO, IMUECLT, IMWBCNT, 
                                                   TRSTPRL, TRSTPLT, TRSTPRT, 
                                                   STFGOV, STFDEM, STFEDU, STFHLTH,
                                                   B_PPLTRST, B_PPLFAIR, B_PPLHLP, 
                                                   B_SCLMEET, B_INPRDSC, B_SCLACT, 
                                                   B_IMBGECO, B_IMUECLT, B_IMWBCNT, 
                                                   B_TRSTPRL, B_TRSTPLT, B_TRSTPRT, 
                                                   B_STFGOV, B_STFDEM, B_STFEDU, B_STFHLTH,
                                                   F1B, F2B, F3B, F4B, F5B,
                                                   ppltrst, pplfair, pplhlp,
                                                   sclmeet, inprdsc, sclact,
                                                   imbgeco, imueclt, imwbcnt,
                                                   trstprl, trstplt, trstprt,
                                                   stfgov, stfdem, stfedu, stfhlth
                                                   ))


# transform factor scores to numeric vectors
data_analysis <- data_analysis %>%
  mutate(across(all_of(c("F1W","F2W","F3W","F4W","F5W")), ~ as.numeric(.x)))


# 03 export data -----

# export the analysis dataset
write_csv(data_analysis, "00_data/ii_processed/ess11_analysis_dataset.csv")