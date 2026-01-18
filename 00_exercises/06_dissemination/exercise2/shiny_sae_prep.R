# clean up
rm(list=ls())

library(tidyverse)
library(eurostat)

eurostat_database <- get_eurostat_toc() 
eurostat_database <- eurostat_database %>% 
  filter(type %in% c("table", "dataset") & str_detect(title, regex("nuts", ignore_case = TRUE)))


eurostat_datasets <- setNames(eurostat_database$code, eurostat_database$title)

