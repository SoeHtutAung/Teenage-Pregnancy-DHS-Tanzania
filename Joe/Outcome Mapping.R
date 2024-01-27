getwd() # setwd to 'TZDHS' folder
setwd("...")

### The GC folder has a guidebook and a csv file which shows attributes of each cluster
### tz_clusters <- read.csv("Geospatial Data/TZGC81FL/TZGC81FL.csv")

### The GE folder has the geospatial files
### We can join to the csv above by DHSID if necessary
### tz_map <- st_read("Geospatial Data/TZGE81FL/TZGE81FL.shp")

# install.packages("sf")
library(sf) # new package for spatial data
# replaces 'rgdal' package which was discontinued Oct 2023

library(haven) # to read '.dta' files
library(ggplot2)
library(dplyr)
library(patchwork)
library(survey)

# Extract the '.shp' file
# Bound 2: Regions
bound2 <- st_read("Geospatial Data/SDR data/DHS Boundaries/shps/sdr_subnational_boundaries2.shp")
# ggplot(bound2) + geom_sf() # basic map of Tanzania regions

# Extract the '.dta' file for "Pregnancy and Postnatal Care"
# I've named this "perinatal"
perinatal <- read_dta("Survey Data/Pregnancy and Postnatal Care/TZNR82FL.DTA")
# 7281 obs. of 928 variables; 6220 unique caseid

# create a data frame that has the required variables for survey,
# as well as 'stillbirth' as a binary outcome
sb_perinatal <-
  perinatal %>%
  mutate( sb=(p32==2) ) %>%
  reframe(
    wt = v005/1e6,
    cluster = v021,
    stratification = v023,
    region = v024,
    residence = v025,
    stillbirth = p32==2
  )

# Create the survey design
sb_design <-
  svydesign(
    id = ~cluster,
    strata = ~stratification,
    weights = ~wt,
    data = sb_perinatal
  )

# Use the survey package to get rates
sb_ratio <- as.data.frame(
  svyby(~stillbirth, ~region, sb_design, svymean)
) %>%
  mutate(
    sb_rate = round( 1000*stillbirthTRUE , 1 )
  )

# Now add those stillbirth rates to the '.shp' data frame using a join
sb_regional <-
  inner_join( bound2, sb_ratio, by = join_by(REGCODE==region) )
ggplot(sb_regional) +
  geom_sf(aes(fill=sb_rate)) +
  scale_fill_continuous(name = "Stillbirth Rate")

##### URBAN

# create a larger rate table stratifying by both residence AND region
sb_resreg <- svyby(~stillbirth, ~residence+region, sb_design, svymean)

sb_urban <- 
  sb_resreg[which(sb_resreg$residence==1),c(2,4)] %>%
  mutate(
    sb_rate = round( 1000*stillbirthTRUE , 1),
    sb_rate_bins = cut(
      round( 1000*stillbirthTRUE , 1),
      breaks = c(-0.01,1,10,25,50,110)
    )
  )

urban_map <-
  inner_join(
    bound2,
    sb_urban,
    by = join_by(REGCODE==region)
  )

urban_map <- 
  ggplot(sb_urban) +
  geom_sf(aes(fill=sb_rate_bins)) +
  scale_fill_manual(
    values = c("darkblue","skyblue","white","pink","darkred")
  ) +
  # scale_fill_continuous(name = "Stillbirth Rate") +
  ggtitle("Urban Stillbirth Rates in Tanzania","by Region")

##### RURAL

sb_rural <- 
  sb_resreg[which(sb_resreg$residence==2),c(2,4)] %>%
  mutate(
    sb_rate = round( 1000*stillbirthTRUE , 1),
    sb_rate_bins = cut(
      round( 1000*stillbirthTRUE , 1),
      breaks = c(-0.01,1,10,25,50,110)
    )
  )

rural_map <-
  inner_join(
    bound2[-7,], # Need to take out Dar Es Salaam, which is visually small anyway
    sb_rural,
    by = join_by(REGCODE==region)
  )

rural_map <- 
  ggplot(sb_rural) +
  geom_sf(aes(fill=sb_rate_bins)) +
  scale_fill_manual(
    values = c("darkblue","skyblue","white","pink","darkred")
  ) +
  # scale_fill_viridis(name = "Stillbirth Rate") +
  ggtitle("Rural Stillbirth Rates in Tanzania","by Region")

################################3

sb_reg_dif <-
  inner_join(
    sb_urban[,1:2],
    sb_rural[,1:2],
    by = join_by(region)
  ) %>%
  mutate(
    sb_dif = cut(
      1000 * (stillbirthTRUE.x - stillbirthTRUE.y),
      breaks = c(-50,-10,0,10,50,100)
    )
  )

sb_dif_map <-
  inner_join(
    bound2,
    sb_reg_dif,
    by = join_by(REGCODE==region)
  )

sb_dif_map <-
  ggplot(sb_dif_map) +
  geom_sf(aes(fill=sb_dif)) +
  scale_fill_manual(
    name = 'Urban - Rural',
    values = c("darkblue","skyblue","white","pink","darkred")
  ) +
  ggtitle("Increase in Stillbirths in Urban Areas","Per 1000 Pregnancy Outcomes by Region")

########################################################################

sb_reg_OR <-
  inner_join(
    sb_urban[,1:2],
    sb_rural[,1:2],
    by = join_by(region)
  ) %>%
  mutate(
    sb_OR = cut(
      stillbirthTRUE.x / stillbirthTRUE.y ,
      c(-.01,0,1,2,5,Inf)
    )
  )

sb_OR_map <-
  inner_join(
    bound2,
    sb_reg_OR,
    by = join_by(REGCODE==region)
  )

sb_OR_map <-
  ggplot(sb_OR_map) +
  geom_sf(aes(fill=sb_OR)) +
  scale_fill_manual(
    name = 'Urban / Rural',
    values = c("darkblue","skyblue","white","pink","darkred")
  ) +
  ggtitle("Increase in Stillbirth Ratio in Urban Areas","by Region")

########################################################################

urban_map + rural_map + plot_layout(nrow = 2)

sb_dif_map + sb_OR_map + plot_layout(ncol = 2)
