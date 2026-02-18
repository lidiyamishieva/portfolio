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
  )
  
  # legend labels
  legend_labels <- paste0(
    format(round(brks$brks[-length(brks$brks)], 3), big.mark = ","),
    " – ",
    format(round(brks$brks[-1], 3), big.mark = ",")
  )
  
  legend
  legend(
    "bottomleft",
    legend = legend_labels,
    fill = pal,
    title = "Quantiles", #legend_title,
    border = "white",
    cex = 0.6,
    bty = "n"
  )
}

# 04 export graphs -----

par(mfrow=c(1,1))

png("02_output/i_r/plot_f1_est.png", width = 700, height = 700, units = "px", pointsize = 5, res=300)
plot_quantile_map(all_direct_estimates, var = "F1_wgt_F1_wgt_Direct", palette = "Reds",  main = "F1: Interpersonal trust\n(direct estimate, mean)", n=10)
dev.off()

png("02_output/i_r/plot_f1_cv.png", width = 700, height = 700, units = "px", pointsize = 5, res=300)
plot_quantile_map(all_direct_estimates, var = "F1_wgt_F1_wgt_CV", palette = "Reds",  main = "F1: Interpersonal trust\n(direct estimate, cv)", n=10)
dev.off()

png("02_output/i_r/plot_f2_est.png", width = 700, height = 700, units = "px", pointsize = 5, res=300)
plot_quantile_map(all_direct_estimates, var = "F2_wgt_Direct", palette = "Reds",  main = "F2: Social relations\n(direct estimate, mean)", n=10)
dev.off()

png("02_output/i_r/plot_f2_cv.png", width = 700, height = 700, units = "px", pointsize = 5, res=300)
plot_quantile_map(all_direct_estimates, var = "F2_wgt_CV", palette = "Reds",  main = "F2: Social relations\n(direct estimate, cv)", n=10)
dev.off()

png("02_output/i_r/plot_f3_est.png", width = 700, height = 700, units = "px", pointsize = 5, res=300)
plot_quantile_map(all_direct_estimates, var = "F3_wgt_Direct", palette = "Reds",  main = "F3: Openness\n(direct estimate, mean)", n=10)
dev.off()

png("02_output/i_r/plot_f3_cv.png", width = 700, height = 700, units = "px", pointsize = 5, res=300)
plot_quantile_map(all_direct_estimates, var = "F3_wgt_CV", palette = "Reds",  main = "F3: Openness\n(direct estimate, cv)", n=10)
dev.off()

png("02_output/i_r/plot_f4_est.png", width = 700, height = 700, units = "px", pointsize = 5, res=300)
plot_quantile_map(all_direct_estimates, var = "F4_wgt_Direct", palette = "Reds",  main = "F4: Trust in institutions\n(direct estimate, mean)", n=10)
dev.off()

png("02_output/i_r/plot_f4_cv.png", width = 700, height = 700, units = "px", pointsize = 5, res=300)
plot_quantile_map(all_direct_estimates, var = "F4_wgt_CV", palette = "Reds",  main = "F4: Trust in institutions\n(direct estimate, cv)", n=10)
dev.off()

png("02_output/i_r/plot_f5_est.png", width = 700, height = 700, units = "px", pointsize = 5, res=300)
plot_quantile_map(all_direct_estimates, var = "F5_wgt_Direct", palette = "Reds",  main = "F5: Legitimacy of institutions\n(direct estimate, mean)", n=10)
dev.off()

png("02_output/i_r/plot_f5_cv.png", width = 700, height = 700, units = "px", pointsize = 5, res=300)
plot_quantile_map(all_direct_estimates, var = "F5_wgt_CV", palette = "Reds",  main = "F5: Legitimacy of institutions\n(direct estimate, cv)", n=10)
dev.off()

# 05 export data -----

st_write(all_direct_estimates, "00_data/ii_processed/all_direct_estimates.geojson", append = FALSE)