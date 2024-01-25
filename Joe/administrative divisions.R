library(sf)
library(ggplot2)
# Looking through the different .shp files provided by DHS

# setwd("...")

# National: Country Outline
natbound <- st_read(
  "SDR data/National Data/shps/sdr_national_data_dhs_2022.shp"
)
ggplot(natbound) + geom_sf()

# Subnational 1: Zones
subnat1 <- st_read(
  "SDR data/Subnational Data/shps/sdr_subnational_data_dhs_2022_lvl_1.shp"
)
ggplot(subnat1) + geom_sf()

# Subnational 2: Regions
subnat2 <- st_read(
  "SDR data/Subnational Data/shps/sdr_subnational_data_dhs_2022_lvl_2.shp"
)
ggplot(subnat2) + geom_sf()
