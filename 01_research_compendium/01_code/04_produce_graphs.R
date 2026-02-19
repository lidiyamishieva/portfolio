############## BEGIN HEADER ############## 
##
## script name: 04_produce_graphs.R
## input: all_direct_estimates.csv
## output: all_direct_estimates.geojson, multiple graphs
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
library(RColorBrewer)
library(classInt)

# 01 import data -----

# import NUTS regions with geography information
nuts <- bind_rows(
  gisco_get_nuts(nuts_level = 1, year = 2021, resolution = "10", cache = TRUE, update_cache = TRUE) %>% mutate(level = 1),
  gisco_get_nuts(nuts_level = 2, year = 2021, resolution = "10", cache = TRUE, update_cache = TRUE) %>% mutate(level = 2),
  gisco_get_nuts(nuts_level = 3, year = 2021, resolution = "10", cache = TRUE, update_cache = TRUE) %>% mutate(level = 3))

# import direct estimates
all_direct_estimates <- read_csv("00_data/ii_processed/all_direct_estimates.csv")

# 02 data transformations -----

# add geometries to the data frame with regional estimates

all_direct_estimates <- all_direct_estimates %>%
  left_join(
    nuts %>% 
      rename(region = NUTS_ID) %>% 
      dplyr::select(c(region, geometry))
  ) %>%
  st_as_sf()

# 03 visualizations -----

# function to produce a map with the color scale matching the quantiles
# of the distribution of the variable to be mapped
plot_quantile_map <- function(
    sf_data,
    var,
    n = 3,
    palette = "Blues",
    main = NULL,
    sub = NULL,
    legend_title = NULL
) {
  # extract variable
  x <- sf_data[[var]]
  
  # palette
  pal <- colorRampPalette(brewer.pal(n-1, palette))(n)
  
  # quantile breaks
  brks <- classIntervals(x, n = n, style = "quantile")
  
  # classify values
  classes <- cut(x, breaks = brks$brks, include.lowest = TRUE)
  
  # default titles
  if (is.null(main)) main <- var
  if (is.null(legend_title)) legend_title <- var
  
  # plot map
  plot(
    st_geometry(sf_data),
    #col = pal[as.numeric(classes)],
    col = ifelse(is.na(classes), "grey80", pal[as.numeric(classes)]),
    border = NA,
    main = main,
    sub = sub,
    cex.main = 0.7,
    xaxs = "i",
    yaxs = "i"
  )
  
  # legend labels
  legend_labels <- paste0(
    format(round(brks$brks[-length(brks$brks)], 3), big.mark = ","),
    " – ",
    format(round(brks$brks[-1], 3), big.mark = ",")
  )
  
  # legend(
  #   "bottomleft",
  #   legend = legend_labels,
  #   fill = pal,
  #   title = "Quantiles", #legend_title,
  #   border = "white",
  #   cex = 0.6,
  #   bty = "n"
  # )
}

# 04 export graphs -----

png("03_output/i_graphs/plot_f1.png", width = 1400, height = 800, units = "px", pointsize = 5, res=500)
par(mfrow = c(1,2), mar = c(1,1,4,1))
plot_quantile_map(all_direct_estimates, var = "F1_wgt_F1_wgt_Direct", palette = "Reds",  main = "F1: Interpersonal trust\n(direct estimate, mean)", n=10)
plot_quantile_map(all_direct_estimates, var = "F1_wgt_F1_wgt_CV", palette = "Reds",  main = "F1: Interpersonal trust\n(direct estimate, cv)", n=10)
dev.off()

png("03_output/i_graphs/plot_f2.png", width = 1400, height = 800, units = "px", pointsize = 5, res=500)
par(mfrow = c(1,2), mar = c(1,1,4,1))
plot_quantile_map(all_direct_estimates, var = "F2_wgt_Direct", palette = "Reds",  main = "F2: Social relations\n(direct estimate, mean)", n=10)
plot_quantile_map(all_direct_estimates, var = "F2_wgt_CV", palette = "Reds",  main = "F2: Social relations\n(direct estimate, cv)", n=10)
dev.off()

png("03_output/i_graphs/plot_f3.png", width = 1400, height = 800, units = "px", pointsize = 5, res=500)
par(mfrow = c(1,2), mar = c(1,1,4,1))
plot_quantile_map(all_direct_estimates, var = "F3_wgt_Direct", palette = "Reds",  main = "F3: Openness\n(direct estimate, mean)", n=10)
plot_quantile_map(all_direct_estimates, var = "F3_wgt_CV", palette = "Reds",  main = "F3: Openness\n(direct estimate, cv)", n=10)
dev.off()

png("03_output/i_graphs/plot_f4.png", width = 1400, height = 800, units = "px", pointsize = 5, res=500)
par(mfrow = c(1,2), mar = c(1,1,4,1))
plot_quantile_map(all_direct_estimates, var = "F4_wgt_Direct", palette = "Reds",  main = "F4: Trust in institutions\n(direct estimate, mean)", n=10)
plot_quantile_map(all_direct_estimates, var = "F4_wgt_CV", palette = "Reds",  main = "F4: Trust in institutions\n(direct estimate, cv)", n=10)
dev.off()

png("03_output/i_graphs/plot_f5.png", width = 1400, height = 800, units = "px", pointsize = 5, res=500)
par(mfrow = c(1,2), mar = c(1,1,4,1))
plot_quantile_map(all_direct_estimates, var = "F5_wgt_Direct", palette = "Reds",  main = "F5: Legitimacy of institutions\n(direct estimate, mean)", n=10)
plot_quantile_map(all_direct_estimates, var = "F5_wgt_CV", palette = "Reds",  main = "F5: Legitimacy of institutions\n(direct estimate, cv)", n=10)
dev.off()

# 05 export data -----

st_write(all_direct_estimates, "00_data/ii_processed/all_direct_estimates.geojson", append = FALSE, delete_dsn = TRUE, quiet = TRUE)
