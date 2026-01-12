# clean up workspace
rm(list=ls())

# load packages
library(tidyverse)
library(sf)
library(osmextract)

# wait 1000 seconds for the internet operation to complete
options(timeout = 1000)

# retrieve a data extract from a provider
castles <- oe_get(
  # define a geographical area to be matched with a .osm.pbf file
  place = "France",
  # define a provider
  provider = "geofabrik",
  # query the database locally (!) from a downloaded odm extract
  # only layer is filtered before downloading the extract
  query = "
    SELECT * FROM multipolygons
    WHERE historic = 'castle'
  ",
  # specify additional columns corresponding to OSM tags
  extra_tags = c("castle_type"),
  # define directory for the osm extract 
  download_directory = getwd(),
  # no messages
  quiet = TRUE
)

# export the resulting dataset
write_rds(castles, "week2/exercise2/castles_france.Rds")

# import the dataset
castles <- read_rds("week2/exercise2/castles_france.Rds")

# check out which categories of castles are there
table(castles$castle_type)

# use only the biggest categories of castles for the map
castle_cats <- c(
  "citadel","fortress","defensive", "castle", 
  "manor", "palace", "chateau", "stately")

castles <- castles %>%
  # remove all NA's for now
  filter(!is.na(castle_type)) %>%
  # define the "other" category
  # and the levels (sorted by defense grade)
  mutate(
    castle_type2 = if_else(castle_type %in% castle_cats, castle_type, "other"),
    castle_type2 = factor(castle_type2, levels = c(castle_cats, "other"))
  ) %>%
  # clean up the geometries
  st_make_valid() %>%
  # only use centroids of the polygons
  st_centroid()

# plot the castles
ggplot(castles) +
  geom_sf(aes(color = castle_type2), size = 1) +
  scale_color_brewer(palette = "Spectral") +
  theme_minimal()




