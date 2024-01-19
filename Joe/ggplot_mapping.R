library(sf)
library(ggplot2)

# Looking through the different .shp files provided by DHS

bound1 <- st_read(
  "SDR data/DHS Boundaries/shps/sdr_subnational_boundaries.shp"
)
ggplot(bound1) + geom_sf()

bound2 <- st_read(
  "SDR data/DHS Boundaries/shps/sdr_subnational_boundaries2.shp"
)
ggplot(bound2) + geom_sf()

natbound <- st_read(
  "SDR data/National Data/shps/sdr_national_data_dhs_2022.shp"
)
ggplot(natbound) + geom_sf()

subnat1 <- st_read(
  "SDR data/Subnational Data/shps/sdr_subnational_data_dhs_2022_lvl_1.shp"
)
ggplot(subnat1) + geom_sf()

subnat2 <- st_read(
  "SDR data/Subnational Data/shps/sdr_subnational_data_dhs_2022_lvl_2.shp"
)
ggplot(subnat2) + geom_sf()

pop0 <- st_read(
  "SDR data/Population Estimates/shps/Tanzania_adm0_uscb_2022.shp"
)
ggplot(pop0) + geom_sf()

pop1 <- st_read(
  "SDR data/Population Estimates/shps/Tanzania_adm1_uscb_2022.shp"
)
ggplot(pop1) + geom_sf()

pop2 <- st_read(
  "SDR data/Population Estimates/shps/Tanzania_adm2_uscb_2022.shp"
)
ggplot(pop2) + geom_sf()
