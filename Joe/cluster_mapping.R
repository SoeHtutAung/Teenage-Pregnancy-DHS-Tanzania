getwd() # setwd to 'Geospatial Data' folder

# The GC folder has a guidebook and a csv file which shows attributes of each cluster
tz_clusters <- read.csv("TZGC81FL/TZGC81FL.csv")

# install.packages("sf")
library(sf) # new package for spatial data
# replaces 'rgdal' package which was discontinued Oct 2023

# The GE folder has the geospatial files
# We can join to the csv above by DHSID if necessary
tz_map <- st_read("TZGE81FL/TZGE81FL.shp")

library(ggplot2)

bound2 <-
  bound2 %>%
  mutate(
    p32 = 100*stillb$`p32 == 2TRUE`
  )

ggplot(data = bound2) +
  geom_sf(aes(fill = p32)) +
  geom_sf(data = tz_map,
          aes(x = LONGNUM, y = LATNUM, color = URBAN_RURA)
  )

# Each point represents a cluster (e.g. a village or city neighbourhood)
# This was a very basic attempt at demonstrating where the clusters are

ggplot(tz_map, aes(x = LONGNUM, y = LATNUM, color = DHSREGNA)) + geom_point()
# by region

ggplot(tz_map, aes( x = LONGNUM, y = LATNUM, col=ALT_DEM )) +
  geom_point() + scale_color_viridis_c()
# by altitude

# We can learn more about ggplot2 polygons from the following website:
# https://ggplot2.tidyverse.org/reference/ggsf.html
